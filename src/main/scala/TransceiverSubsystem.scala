package hbwif2

import chisel3._
import chisel3.experimental._

class TransceiverSubsystemIO(
  transceiverDataWidth: Int,
  transceiverNumIrefs: Int,
  cdrHasOverride: Boolean,
  cdrIWidth: Int,
  cdrPWidth: Int,
  dfeNumTaps: Int,
  dfeTapWidth: Int,
  dfeHasOverride: Boolean
) extends Bundle {

  // high speed clock input
  val fastClk = Input(Clock())

  // low speed clock output
  val slowClk = Output(Clock())

  // reset
  val resetIn = Input(Bool())
  val resetOut = Output(Bool())

  // RX pad inputs
  val rx = Flipped(new Differential)

  // TX pad outputs
  val tx = new Differential

  // internal data interface
  val dataDLev = Output(UInt(transceiverDataWidth.W))
  val dataRx = Output(UInt(transceiverDataWidth.W))
  val dataTx = Input(UInt(transceiverDataWidth.W))

  // reference current (if any)
  val iref = Analog(transceiverNumIrefs.W)

  // CDR override
  val cdrIOverride = Input(UInt((if (cdrHasOverride) cdrIWidth else 0).W))
  val cdrPOverride = Input(UInt((if (cdrHasOverride) cdrPWidth else 0).W))
  val cdrOverride = Input(UInt((if (cdrHasOverride) 1 else 0).W))

  // DFE override
  val dfeTapsOverride = Input(Vec(if (dfeHasOverride) dfeNumTaps else 0, UInt(dfeTapWidth.W)))
  val dfeOverride = Input(UInt((if (dfeHasOverride) 1 else 0).W))

  // DLEV override
  val dLevDACOverride = Input(UInt((if (dLevHasOverride) dLevDACWidth else 0).W))
  val dLevOverride = Input(UInt((if (dLevHasOverride) 1 else 0).W))

  //val config = TODO

  //val debug = TODO

}


class TransceiverSubsystem(
  transceiverDataWidth: Int,
  transceiverNumIrefs: Int,
  cdrHasOverride: Boolean,
  cdrIWidth: Int,
  cdrPWidth: Int,
  dfeNumTaps: Int,
  dfeTapWidth: Int,
  dfeHasOverride: Boolean,
  dLevDACWidth: Int,
  dLevHasOverride: Boolean
) extends Module {

  val io = IO(new TransceiverSubsystemIO(
    transceiverDataWidth,
    transceiverNumIrefs,
    cdrHasOverride,
    cdrIWidth,
    cdrPWidth,
    dfeNumTaps,
    dfeTapWidth,
    dfeHasOverride,
    dLevDACWidth,
    dLevHasOverride
  ))

  // Transceiver <> top level connections
  val txrx = Module(new Transceiver(
    transceiverDataWidth,
    transceiverNumIrefs,
    cdrIWidth,
    cdrPWidth,
    dfeNumTaps,
    dfeTapWidth,
    dLevDACWidth
  ))

  txrx.io.fastClock := io.fastClock
  txrx.io.resetIn := io.resetIn

  io.tx <> txrx.io.tx
  io.rx <> txrx.io.rx

  io.dataDLev := txrx.io.dataDLev
  io.dataRx := txrx.io.dataRx
  txrx.io.dataTx := io.dataTx

  io.iref <> txrx.io.iref

  withClockAndReset(txrx.io.slowClock, txrx.io.resetOut) {

    // Transceiver <> CDR Loop
    val cdr = Module(new CDR(
      transceiverDataWidth,
      cdrIWidth,
      cdrPwidth
    ))

    txrx.io.cdrI := if(cdrHasOverride) Mux(io.cdrOverride === 1.U, io.cdrIOverride, cdr.io.i) else cdr.io.i
    txrx.io.cdrP := if(cdrHasOverride) Mux(io.cdrOverride === 1.U, io.cdrPOverride, cdr.io.p) else cdr.io.p
    txrx.io.clockDither := cdr.io.clockDither
    cdr.io.dataDLev := txrx.io.dataDLev
    cdr.io.dataRx := txrx.io.dataRx

    // Transceiver <> DFE Loop
    if (dfeNumTaps > 0) {
      val dfe = Module(new DFE(
        transceiverDataWidth,
        dfeNumTaps,
        dfeTapWidth
      ))
      txrx.io.dfeTaps := if(dfeHasOverride) Mux(io.dfeOverride === 1.U, io.dfeTapsOverride, dfe.io.dfeTaps) else dfe.io.dfeTaps
      dfe.io.dataDLev := txrx.io.dataDLev
      dfe.io.dataRx := txrx.io.dataRx
    }

    // Transceiver <> DLEV Loop
    val dLev = Module(new DLEV(
      transceiverDataWidth,
      dLevDACWidth
    ))
    txrx.io.dLevDAC := if(dLevHasOverride) Mux(io.dLevOverride === 1.U, io.dLevDACOverride, dLev.io.dLevDAC) else dLev.io.dLevDAC
    dLev.io.dataRx := txrx.io.dataRx
    dLev.io.dataDLev := txrx.io.dataDLev

  }

}
