package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.util._
import chisel3.experimental.{withClock, withClockAndReset}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldAccessType, RegReadFn, RegWriteFn}
import freechips.rocketchip.tilelink.{Pattern, WritePattern, ReadPattern, ReadExpectPattern}
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams, AsyncResetShiftReg, SynchronizerShiftReg}
import scala.collection.mutable.ArrayBuffer

trait TLControllerPattern {
    def name: String
    def size: Int
    def toPattern(base: BigInt, map: Map[String, Int]): Pattern
}

case class TLControllerWritePattern(name: String, data: BigInt) extends TLControllerPattern {
    def size = 1
    def toPattern(base: BigInt, map: Map[String, Int]) = WritePattern(map(name) + base, size, data)
}

case class TLControllerReadPattern(name: String) extends TLControllerPattern {
    def size = 1
    def toPattern(base: BigInt, map: Map[String, Int]) = ReadPattern(map(name) + base, size)
}

case class TLControllerReadExpectPattern(name: String, data: BigInt) extends TLControllerPattern {
    def size = 1
    def toPattern(base: BigInt, map: Map[String, Int]) = ReadExpectPattern(map(name) + base, size, data)
}

trait HasTLController {
    this: Lane =>

    private def allios = Seq(ssio, encoderio, decoderio, packetizerio) ++ debugio
    def regmap = allios.foldLeft((Seq[(Int, Seq[RegField])](), 0))({ case ((seq, base), cio) =>
        val mapped = ioToRegMap(cio, base)
        (seq ++ mapped._1, mapped._2)
    })._1

    private def bytesPerReg = 8

    private def ioToRegMap(cio: Option[ControlBundle], base: Int): (Seq[(Int, Seq[RegField])], Int) = {
        if (cio.isDefined) {
            val ins = cio.get.inputMap.values.toSeq.sortWith(_.name < _.name).zipWithIndex.map { case (x, i) =>
                require(x.width <= 8*bytesPerReg, s"The bit width for input register ${x.name} is too large (${x.width} > ${8*bytesPerReg}). Reconfigure the TLController or change your signals")
                ((base + i*bytesPerReg) -> Seq(inputCrossing(x)))
            }
            val outBase = base + ins.length*bytesPerReg
            val outs = cio.get.outputMap.values.toSeq.sortWith(_.name < _.name).zipWithIndex.map { case (x, i) =>
                require(x.width <= 8*bytesPerReg, s"The bit width for output register ${x.name} is too large (${x.width} > ${8*bytesPerReg}). Reconfigure the TLController or change your signals")
                ((outBase + i*bytesPerReg) -> Seq(outputCrossing(x)))
            }
            return ((outs ++ ins), (outBase + outs.length*bytesPerReg))
        } else {
            return (Seq[(Int, Seq[RegField])](), base)
        }
    }

    private def inputCrossing(x: ControlInput): RegField = {
        val width = x.width
        val (toClock, toReset) = x.clock match {
            case OuterClock => (this.clock, this.reset.toBool)
            case TxClock => (this.io.txClock, this.io.txReset)
            case RxClock => (this.io.rxClock, this.io.rxReset)
            case _ => ???
        }
        val reg = withClockAndReset(toClock, toReset) { if (x.default.isDefined) RegInit(x.default.get.U(width.W)) else Reg(UInt(width.W)) }
        x.signal := reg
        reg.suggestName(s"hbwif_scr_${x.name}")
        x.clock match {
            case OuterClock => {
                x.signal := reg
                RegField(width, reg, RegFieldDesc(x.name, x.desc.getOrElse(""), reset = x.default))
            }
            case _ => {
                if (width == 1) {
                    // Use a synchronizer
                    val outerReg = if (x.default.isDefined) RegInit(x.default.get.U(width.W)) else Reg(UInt(width.W))
                    outerReg.suggestName(s"hbwif_scrouter_${x.name}")
                    reg := withClockAndReset(toClock, toReset) { AsyncResetShiftReg(outerReg, 3, x.default.getOrElse(BigInt(0)).toInt, Some(s"hbwif_scrsync_${x.name}")) }
                    RegField(width, outerReg, RegFieldDesc(x.name, x.desc.getOrElse(""), reset = x.default))
                } else {
                    // Use a small async FIFO
                    val q = Module(new AsyncQueue(chiselTypeOf(x.signal), AsyncQueueParams(1, 3, false, false))).suggestName(s"hbwif_scrinqueue_${x.name}")
                    q.io.enq_clock := this.clock
                    q.io.deq_clock := toClock
                    q.io.enq_reset := this.reset.toBool
                    q.io.deq_reset := toReset
                    q.io.deq.ready := true.B
                    when (q.io.deq.fire()) { reg := q.io.deq.bits }
                    // Shadow for reading- hopefully this just gets optimized away since it is the same function as the Async Queue memory reg
                    val outerReg = Reg(UInt(width.W)).suggestName(s"hbwif_scrinreg_${x.name}")
                    when (q.io.enq.fire()) { outerReg := q.io.enq.bits }
                    RegField(width, RegReadFn(outerReg), RegWriteFn(q.io.enq), RegFieldDesc(x.name, x.desc.getOrElse(""), reset = x.default))
                }
            }
        }
    }

    private def outputCrossing(x: ControlOutput): RegField = {
        val width = x.width
        val (fromClock, fromReset) = x.clock match {
            case OuterClock => (this.clock, this.reset.toBool)
            case TxClock => (this.io.txClock, this.io.txReset)
            case RxClock => (this.io.rxClock, this.io.rxReset)
            case _ => ???
        }
        val readfn = x.clock match {
            case OuterClock => {
                RegReadFn(x.signal)
            }
            case _ => {
                if (width == 1) {
                    val reg = Reg(Bool())
                    reg.suggestName(s"hbwif_scrouter_${x.name}")
                    reg := SynchronizerShiftReg(x.signal, 3, Some(s"hbwif_scrsync_${x.name}"))
                    RegReadFn(reg)
                } else {
                    // Use a small async FIFO and have the FIFO constantly churning
                    val q = Module(new AsyncQueue(chiselTypeOf(x.signal), AsyncQueueParams(1, 3, false, false))).suggestName(s"hbwif_scroutqueue_${x.name}")
                    q.io.enq_clock := fromClock
                    q.io.deq_clock := this.clock
                    q.io.enq_reset := fromReset
                    q.io.deq_reset := this.reset.toBool
                    q.io.deq.ready := true.B
                    q.io.enq.valid := true.B // always grab data from the other domain
                    // ignore q.io.deq.valid
                    // ignore q.io.enq.ready
                    q.io.enq.bits := x.signal
                    RegReadFn(q.io.deq.bits)
                }
            }
        }
        RegField.r(width, readfn, RegFieldDesc(
            name = x.name,
            desc = x.desc.getOrElse(""),
            access = RegFieldAccessType.R,
            volatile = true
        ))
    }

}



object TLController {

    def toAddrmap(regmap: Seq[(Int, Seq[RegField])]) = regmap.map({ x: (Int, Seq[RegField]) =>
        require(x._2.length == 1)
        (x._2(0).desc.get.name -> x._1)
    }).toMap

    def toPattern(addrmap: Map[String, Int], base: BigInt, seq: Seq[TLControllerPattern]): Seq[Pattern] = seq.map(_.toPattern(base, addrmap))

}
