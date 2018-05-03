package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset


class BitStufferDebugIO(val numModes: Int)(implicit c: SerDesConfig) extends DebugIO()(c) {
    val mode = Input(UInt(log2Ceil(numModes).W))
}

class BitStufferDebug(val numModes: Int)(implicit c: SerDesConfig) extends Debug()(c) {

    require(numModes > 1)
    require(c.dataWidth % (1 << (numModes - 1)) == 0)

    val io = IO(new BitStufferDebugIO(numModes))

    val txStuffer = withClockAndReset(io.txClock, io.txReset) { Module(new TxBitStuffer(numModes)) }
    val rxStuffer = withClockAndReset(io.rxClock, io.rxReset) { Module(new RxBitStuffer(numModes)) }

    txStuffer.io.mode := io.mode
    rxStuffer.io.mode := io.mode

    txStuffer.io.enq <> io.txIn
    io.txOut <> txStuffer.io.deq
    rxStuffer.io.enq <> io.rxIn
    io.rxOut <> rxStuffer.io.deq

    def connectController(builder: ControllerBuilder) {
        builder.w("bit_stuff_mode", io.mode, 0)
    }

}


class TxBitStuffer(val numModes: Int)(implicit val c: SerDesConfig) extends Module {

    require(numModes > 1)
    require(c.dataWidth % (1 << (numModes - 1)) == 0)

    val io = IO(new Bundle {
        val enq = Flipped(Ready(UInt(c.dataWidth.W)))
        val deq = Ready(UInt(c.dataWidth.W))
        val mode = Input(UInt(log2Ceil(numModes).W))
    })

    io.deq.bits := io.enq.bits
    io.enq.ready := io.deq.ready

    val buf = Reg(UInt(c.dataWidth.W))
    val count = RegInit(0.U((numModes - 1).W))
    when (io.deq.ready) {
        (1 until numModes).foreach { i =>
            val divisor = 1 << i
            val divBitWidth = c.dataWidth/divisor
            when (io.mode === i.U) {
                io.deq.bits := FillInterleaved(divisor, buf(c.dataWidth - 1, c.dataWidth - divBitWidth))
                when (count === (divisor - 1).U) {
                    count := 0.U
                    buf := io.enq.bits
                } .otherwise {
                    buf := buf << divBitWidth
                    count := count + 1.U
                    io.enq.ready := false.B
                }
            }
        }
    } .otherwise {
        io.enq.ready := false.B
    }
}

class RxBitStuffer(val numModes: Int)(implicit val c: SerDesConfig) extends Module {

    require(numModes > 1)
    require(c.dataWidth % (1 << (numModes - 1)) == 0)

    val io = IO(new Bundle {
        val enq = Flipped(Valid(UInt(c.dataWidth.W)))
        val deq = Valid(UInt(c.dataWidth.W))
        val mode = Input(UInt(log2Ceil(numModes).W))
    })

    io.deq.bits := io.enq.bits
    io.deq.valid := io.enq.valid

    val buf = Reg(UInt(c.dataWidth.W))
    val count = RegInit(0.U((numModes - 1).W))
    when (io.enq.valid) {
        (1 until numModes).foreach { i =>
            val divisor = 1 << i
            val divBitWidth = c.dataWidth/divisor
            when(io.mode === i.U) {
                io.deq.bits := buf
                buf := Cat(buf(c.dataWidth - divBitWidth - 1, 0),
                    io.enq.bits.toBools.reverse.zipWithIndex.filter(_._2 % divisor == (divisor - 1)).map(_._1.asUInt).reduceLeft(Cat(_,_)))
                when (count === (divisor - 1).U) {
                    count := 0.U
                } .otherwise {
                    count := count + 1.U
                    io.deq.valid := false.B
                }
            }
        }
    } .otherwise {
        io.deq.valid := false.B
    }

}

trait HasBitStufferDebug4Modes extends HasDebug {
    this: Lane =>
    def numModes = 4
    implicit val c: SerDesConfig
    abstract override def genDebug() = Seq(Module(new BitStufferDebug(numModes)(c))) ++ super.genDebug()
}
