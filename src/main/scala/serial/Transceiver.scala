package awl.serial

import chisel3._
import chisel3.core.{BaseModule, IntParam, Param}
import chisel3.util.{HasBlackBoxResource}
import chisel3.experimental.Analog
import chisel3.util.HasBlackBoxResource

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

trait TransceiverIO extends Bundle {
    implicit val c: SerDesConfig

    // Data
    val data = new TransceiverDataIF

    // low speed clock output
    val clock_rx_div = Output(Clock())

    // low speed clock output
    val clock_tx_div = Output(Clock())

    // reference clock input
    val clock_ref = Input(Clock())

    // async reset input
    val async_reset_in = Input(Bool())

    // RX pad inputs
    val rx = Flipped(new Differential)

    // TX pad outputs
    val tx = new Differential

}

class GenericTransceiverIO()(implicit val c: SerDesConfig) extends Bundle with TransceiverIO

trait HasTransceiverIO extends BaseModule {

    val io: TransceiverIO

}

class GenericTransceiver()(implicit c: SerDesConfig) extends BlackBox(Map("SERDES_BITS" -> IntParam(c.dataWidth))) with HasTransceiverIO with HasBlackBoxResource {

    val io = IO(new GenericTransceiverIO)

    addResource("/awl/vsrc/generic_transceiver.sv")

    override def desiredName = "generic_transceiver"
    addResource("/generic_transceiver.sv")

}
