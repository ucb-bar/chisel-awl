package hbwif2

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArraySeq


class DebugIO()(implicit val c: SerDesConfig) extends Bundle {
    val txIn = Flipped(Ready(UInt(c.dataWidth.W)))
    val txOut = Ready(UInt(c.dataWidth.W))
    val rxIn = Flipped(Valid(UInt(c.dataWidth.W)))
    val rxOut = Valid(UInt(c.dataWidth.W))
}

abstract class Debug()(implicit val c: SerDesConfig) extends Module with HasControllerConnector {

    val io: DebugIO

}

trait HasDebug {

    def genDebug(): Seq[Debug] = Seq[Debug]()

}
