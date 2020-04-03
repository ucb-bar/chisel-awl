package awl.serial

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

abstract class Debug()(implicit val c: SerDesConfig) extends RawModule with HasControllerConnector {

    val io = IO(new DebugIO)

}

abstract class TxDebug()(implicit c: SerDesConfig) extends Debug()(c) {
    io.rxIn <> io.rxOut
}

abstract class RxDebug()(implicit c: SerDesConfig) extends Debug()(c) {
    io.txIn <> io.txOut
}

trait HasDebug {
    this: Lane =>

    // The standard to ovveride this is to use
    // abstract override def genDebug(): Seq[Debug] = super.genDebug() ++ Seq(YOUR CODE HERE)
    // When mixing in Debug traits, the last traits to be included are the closest to the transceiver
    // e.g.
    // with HasBertDebug
    // with HasPatternMemDebug <- closest to the transceiver
    def genDebug(): Seq[Debug] = Seq[Debug]()

}
