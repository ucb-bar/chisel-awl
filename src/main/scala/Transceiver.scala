package hbwif2

import chisel3._
import chisel3.experimental.Analog
import chisel3.util.HasBlackBoxResource

class Differential extends Bundle {
  val p = Analog(1.W)
  val n = Analog(1.W)
}

class TransceiverDataIF()(implicit val c: SerDesGeneratorConfig) extends Bundle {

  // internal data interface
  val dlev = Output(UInt(c.dataWidth.W))
  val rx = Output(UInt(c.dataWidth.W))
  val tx = Input(UInt(c.dataWidth.W))

}

// This is a container for the stuff that is shared between the TransceiverIO and TransceiverSubsystemIO
// i.e. not the dlev, dfe, and cdr override stuff
trait TransceiverSharedIF {
  implicit val c: SerDesGeneratorConfig

  // reference clock input
  val clock_ref = Input(Clock())

  // low speed clock output
  val clock_digital = Output(Clock())

  // reset
  val async_reset_in = Input(Bool())
  val reset_out = Output(Bool())

  // RX pad inputs
  val rx = Flipped(new Differential)

  // TX pad outputs
  val tx = new Differential

  // Data
  val data = new TransceiverDataIF

  //val config = TODO

  //val debug = TODO

  // reference current (if any)
  val bias = Analog(c.transceiverNumIrefs.W)

}

class TransceiverIO()(implicit val c: SerDesGeneratorConfig) extends Bundle with TransceiverSharedIF {

  // CDR stuff
  val cdri = Input(UInt(c.cdrIWidth.W))
  val cdrp = Input(UInt(c.cdrPWidth.W))

  // Clock dither for CDR
  val dither_clock = Input(Bool())

  // DFE stuff
  val dfe_taps = Input(Vec(c.dfeNumTaps, UInt(c.dfeTapWidth.W)))
  val dlev_dac = Input(UInt(c.dlevDACWidth.W))

}

class Transceiver()(implicit val c: SerDesGeneratorConfig) extends BlackBox with HasBlackBoxResource {

  val io = IO(new TransceiverIO)

  override def desiredName = c.transceiverName

  setResource(c.transceiverResource)

}

