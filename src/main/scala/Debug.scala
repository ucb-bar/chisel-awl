package hbwif2

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArraySeq


class DebugIO()(implicit val c: SerDesConfig) extends Bundle {
    val txIn = Flipped(Ready(UInt(c.dataWidth.W)))
    val txOut = Ready(UInt(c.dataWidth.W))
    val txClock = Input(Clock())
    val txReset = Input(Bool())
    val rxIn = Flipped(Valid(UInt(c.dataWidth.W)))
    val rxOut = Valid(UInt(c.dataWidth.W))
    val rxClock = Input(Clock())
    val rxReset = Input(Bool())
}

abstract class Debug()(implicit val c: SerDesConfig) extends Module with HasControllerConnector {

    val io: DebugIO

}

abstract class TxDebug()(implicit c: SerDesConfig) extends Debug()(c) {
    io.rxIn <> io.rxOut
}

abstract class RxDebug()(implicit c: SerDesConfig) extends Debug()(c) {
    io.txIn <> io.txOut
}

trait HasDebug {

    def genDebug(): Seq[Debug] = Seq[Debug]()

}
