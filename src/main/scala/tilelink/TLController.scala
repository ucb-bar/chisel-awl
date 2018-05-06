package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.util._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldAccessType, RegReadFn}
import freechips.rocketchip.tilelink.{Pattern, WritePattern, ReadPattern, ReadExpectPattern}

trait TLControllerPattern {
    def name: String
    def size: Int
    def toPattern(base: BigInt, map: Map[String, Int]): Pattern
}

case class TLControllerWritePattern(name: String, size: Int, data: BigInt) extends TLControllerPattern {
    def toPattern(base: BigInt, map: Map[String, Int]) = WritePattern(map(name) + base, size, data)
}

case class TLControllerReadPattern(name: String, size: Int) extends TLControllerPattern {
    def toPattern(base: BigInt, map: Map[String, Int]) = ReadPattern(map(name) + base, size)
}

case class TLControllerReadExpectPattern(name: String, size: Int, data: BigInt) extends TLControllerPattern {
    def toPattern(base: BigInt, map: Map[String, Int]) = ReadExpectPattern(map(name) + base, size, data)
}

trait HasTLController {
    this: Lane =>

    private def allios = Seq(ssio, encoderio, decoderio, packetizerio) ++ debugio
    def regmap = allios.foldLeft((Seq[(Int, Seq[RegField])](), 0))({ case ((seq, base), cio) =>
        val mapped = ioToRegMap(cio, base)
        (seq ++ mapped._1, mapped._2)
    })._1

    private def ioToRegMap(cio: Option[ControlBundle], base: Int): (Seq[(Int, Seq[RegField])], Int) = {
        if (cio.isDefined) {
            val bytesPerReg = 8
            val ins = cio.get.inputMap.values.toSeq.sortWith(_.name < _.name).zipWithIndex.map { case (x, i) =>
                val width = x.signal.getWidth
                val reg = if(x.default.isDefined) RegInit(x.default.get.U(width.W)) else Reg(UInt(width.W))
                reg.suggestName(s"hbwif_scr_${x.name}")
                x.signal := reg
                ((base + i*bytesPerReg) -> Seq(RegField(width, reg, RegFieldDesc(x.name, x.desc.getOrElse(""), reset = x.default))))
            }
            val outBase = base + ins.length*bytesPerReg
            val outs = cio.get.outputMap.values.toSeq.sortWith(_.name < _.name).zipWithIndex.map { case (x, i) =>
                val width = x.signal.getWidth
                ((outBase + i*bytesPerReg) -> Seq(RegField.r(width, RegReadFn(x.signal), RegFieldDesc(
                    name = x.name,
                    desc = x.desc.getOrElse(""),
                    access = RegFieldAccessType.R,
                    volatile = true
                ))))
            }
            return ((outs ++ ins), (outBase + outs.length*bytesPerReg))
        } else {
            return (Seq[(Int, Seq[RegField])](), base)
        }
    }


}

object TLController {

    def toAddrmap(regmap: Seq[(Int, Seq[RegField])]) = regmap.map({ x: (Int, Seq[RegField]) =>
        require(x._2.length == 1)
        (x._2(0).desc.get.name -> x._1)
    }).toMap

    def toPattern(addrmap: Map[String, Int], base: BigInt, seq: Seq[TLControllerPattern]): Seq[Pattern] = seq.map(_.toPattern(base, addrmap))

}
