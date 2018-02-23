package hbwif2

import chisel3._
import chisel3.experimental._

class TransceiverOverrideIF()(implicit val c: SerDesGeneratorConfig) extends Bundle {

  // CDR override
  val cdri_value = Input(UInt((if (c.cdrHasOverride) c.cdrIWidth else 0).W))
  val cdrp_value = Input(UInt((if (c.cdrHasOverride) c.cdrPWidth else 0).W))
  val cdr = Input(UInt((if (c.cdrHasOverride) 1 else 0).W))

  def get_cdri(default: UInt): UInt = (if(c.cdrHasOverride) Mux(cdr === 1.U, cdri_value, default) else default)
  def get_cdrp(default: UInt): UInt = (if(c.cdrHasOverride) Mux(cdr === 1.U, cdrp_value, default) else default)

  // DFE override
  val dfe_taps_value = Input(Vec(if (c.dfeHasOverride) c.dfeNumTaps else 0, UInt(c.dfeTapWidth.W)))
  val dfe = Input(UInt((if (c.dfeHasOverride) 1 else 0).W))

  def get_dfe_taps(default: Vec[UInt]): Vec[UInt] = (if(c.dfeHasOverride) Mux(dfe === 1.U, dfe_taps_value, default) else default)

  // DLEV override
  val dlev_dac_value = Input(UInt((if (c.dlevHasOverride) c.dlevDACWidth else 0).W))
  val dlev = Input(UInt((if (c.dlevHasOverride) 1 else 0).W))

  def get_dlev_dac(default: UInt): UInt = (if(c.dlevHasOverride) Mux(dlev === 1.U, dlev_dac_value, default) else default)

  val loopback_mode = Input(UInt((if (c.hasDigitalLoopback) 1 else 0).W))

}

class TransceiverSubsystemIO()(implicit val c: SerDesGeneratorConfig) extends Bundle with TransceiverSharedIF {

  val overrides = new TransceiverOverrideIF

}

class TransceiverSubsystem()(implicit val c: SerDesGeneratorConfig) extends Module {

  val io = IO(new TransceiverSubsystemIO)

  // Transceiver <> top level connections
  val txrx = Module(new Transceiver)

  txrx.io.clock_ref := io.clock_ref
  txrx.io.async_reset_in := io.async_reset_in

  io.tx <> txrx.io.tx
  io.rx <> txrx.io.rx

  io.data.dlev := txrx.io.data.dlev
  io.data.rx := txrx.io.data.rx

  txrx.io.data.tx := if (c.hasDigitalLoopback) Mux(io.loopback_mode, io.data.tx, txrx.io.data.rx) else io.data.tx

  io.bias <> txrx.io.bias

  withClockAndReset(txrx.io.clock_digital, txrx.io.reset_out) {

    // Transceiver <> CDR Loop
    val cdr = Module(new CDR)

    txrx.io.cdri := io.overrides.get_cdri(cdr.io.i)
    txrx.io.cdrp := io.overrides.get_cdrp(cdr.io.p)
    txrx.io.dither_clock := cdr.io.dither_clock
    cdr.io.data_dlev := txrx.io.data.dlev
    cdr.io.data_rx := txrx.io.data.rx

    // Transceiver <> DFE Loop
    if (c.dfeNumTaps > 0) {
      val dfe = Module(new DFE)
      txrx.io.dfe_taps := io.overrides.get_dfe_taps(dfe.io.taps)
      dfe.io.data_dlev := txrx.io.data.dlev
      dfe.io.data_rx := txrx.io.data.rx
    }

    // Transceiver <> DLEV Loop
    val dlev = Module(new DLEV)
    txrx.io.dlev_dac := io.overrides.get_dlev_dac(dlev.io.code)
    dlev.io.data_rx := txrx.io.data.rx
    dlev.io.data_dlev := txrx.io.data.dlev

  }

}
