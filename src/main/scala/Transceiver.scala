package hbwif

import chisel3._
import chisel3.core.{IntParam, Param}
import chisel3.experimental.Analog

case class SerDesConfig(
    dataWidth: Int = 16,
    numWays: Int = 2
)

class Differential extends Bundle {
    val p = Analog(1.W)
    val n = Analog(1.W)
}

class TransceiverDataIF()(implicit val c: SerDesConfig) extends Bundle {

    // internal data interface
    val rx = Output(UInt(c.dataWidth.W))
    val tx = Input(UInt(c.dataWidth.W))

}

class TransceiverIO()(implicit val c: SerDesConfig) extends Bundle {

    // Data
    val data = new TransceiverDataIF

    // low speed clock output
    val clock_rx = Output(Clock())

    // low speed clock output
    val clock_tx = Output(Clock())

    // reference clock input
    val clock_ref = Input(Clock())

    // async reset input
    val async_reset_in = Input(Bool())

    // RX pad inputs
    val rx = Flipped(new Differential)

    // TX pad outputs
    val tx = new Differential

}

abstract class Transceiver(params: Map[String, Param] = Map.empty[String, Param])(implicit val c: SerDesConfig) extends BlackBox(params) {

    val io: TransceiverIO

    def transceiverName: String

    override def desiredName = transceiverName

}

class GenericTransceiver()(implicit c: SerDesConfig) extends Transceiver(Map("SERDES_BITS" -> IntParam(c.dataWidth)))(c) {

    val io = IO(new TransceiverIO)

    def transceiverName = "generic_transceiver"

}
