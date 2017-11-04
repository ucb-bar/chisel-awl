package hbwif2

import chisel3._
import chisel3.experimental._

class TransceiverSubsystemIO()(implicit c: SerDesGeneratorConfig) extends TransceiverSharedIO()(c) {

  // CDR override
  val cdri_override = Input(UInt((if (c.cdrHasOverride) c.cdrIWidth else 0).W))
  val cdrp_override = Input(UInt((if (c.cdrHasOverride) c.cdrPWidth else 0).W))
  val cdr_override = Input(UInt((if (c.cdrHasOverride) 1 else 0).W))

  // DFE override
  val dfe_taps_override = Input(Vec(if (c.dfeHasOverride) c.dfeNumTaps else 0, UInt(c.dfeTapWidth.W)))
  val dfe_override = Input(UInt((if (c.dfeHasOverride) 1 else 0).W))

  // DLEV override
  val dlev_dac_override = Input(UInt((if (c.dlevHasOverride) dlevDACWidth else 0).W))
  val dlev_override = Input(UInt((if (c.dlevHasOverride) 1 else 0).W))

}


class TransceiverSubsystem()(implicit c: SerDesGeneratorConfig) extends Module {

  val io = IO(new TransceiverSubsystemIO)

  // Transceiver <> top level connections
  val txrx = Module(new Transceiver)

  txrx.io.clock_ref := io.clock_ref
  txrx.io.async_reset_in := io.async_reset_in

  io.tx <> txrx.io.tx
  io.rx <> txrx.io.rx

  io.data_dlev := txrx.io.data_dlev
  io.data_rx := txrx.io.data_rx
  txrx.io.data_tx := io.data_tx

  io.bias <> txrx.io.bias

  withClockAndReset(txrx.io.clock_digital, txrx.io.reset_out) {

    // Transceiver <> CDR Loop
    val cdr = Module(new CDR)

    txrx.io.cdri := if(c.cdrHasOverride) Mux(io.cdr_override === 1.U, io.cdri_override, cdr.io.i) else cdr.io.i
    txrx.io.cdrp := if(c.cdrHasOverride) Mux(io.cdr_override === 1.U, io.cdrp_override, cdr.io.p) else cdr.io.p
    txrx.io.dither_clock := cdr.io.dither_clock
    cdr.io.data_dlev := txrx.io.data_dlev
    cdr.io.data_rx := txrx.io.data_rx

    // Transceiver <> DFE Loop
    if (c.dfeNumTaps > 0) {
      val dfe = Module(new DFE)
      txrx.io.dfe_taps := if(c.dfeHasOverride) Mux(io.dfe_override === 1.U, io.dfe_taps_override, dfe.io.dfe_taps) else dfe.io.dfe_taps
      dfe.io.data_dlev := txrx.io.data_dlev
      dfe.io.data_rx := txrx.io.data_rx
    }

    // Transceiver <> DLEV Loop
    val dlev = Module(new DLEV)
    txrx.io.dlev_dac := if(c.dlevHasOverride) Mux(io.dlev_override === 1.U, io.dlev_dac_override, dlev.io.dlev_dac) else dlev.io.dlev_dac
    dlev.io.data_rx := txrx.io.data_rx
    dlev.io.data_dlev := txrx.io.data_dlev

  }

}
