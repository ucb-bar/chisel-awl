package hbwif

import chisel3._
import chisel3.experimental.Analog
import chisel3.util.HasBlackBoxResource

case class SerDesConfig(
    dataWidth: Int = 16,
    numWays: Int = 2,
    cdrHasOverride: Boolean = true,
    cdrIWidth: Int = 8,
    cdrPWidth: Int = 8,
    dfeNumTaps: Int = 4,
    dfeTapWidth: Int = 4,
    dfeHasOverride: Boolean = true,
    dlevDACWidth: Int = 4,
    dlevHasOverride: Boolean = true,
    bitStuffModes: Int = 1 // TODO test more modes
)

class Differential extends Bundle {
    val p = Analog(1.W)
    val n = Analog(1.W)
}

class TransceiverDataIF()(implicit val c: SerDesConfig) extends Bundle {

    // internal data interface
    val dlev = Output(UInt(c.dataWidth.W))
    val rx = Output(UInt(c.dataWidth.W))
    val tx = Input(UInt(c.dataWidth.W))

}

class TransceiverIO()(implicit val c: SerDesConfig) extends Bundle {

    // CDR stuff
    val cdri = Input(UInt(c.cdrIWidth.W))
    val cdrp = Input(UInt(c.cdrPWidth.W))

    // Clock dither for CDR
    val dither_clock = Input(Bool())

    // Data
    val data = new TransceiverDataIF

    // DFE stuff
    val dfe_taps = Input(Vec(c.dfeNumTaps, UInt(c.dfeTapWidth.W)))
    val dlev_dac = Input(UInt(c.dlevDACWidth.W))

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

abstract class Transceiver()(implicit val c: SerDesConfig) extends BlackBox {

    val io: TransceiverIO

    def transceiverName: String

    override def desiredName = transceiverName

}

class GenericTransceiver()(implicit c: SerDesConfig) extends Transceiver()(c) {

    val io = IO(new TransceiverIO)

    def transceiverName = "generic_transceiver"

}

trait HasGenericTransceiver {
    this: TransceiverSubsystem =>
    implicit val c: SerDesConfig

    def genTransceiver() = new GenericTransceiver()(c)

}
