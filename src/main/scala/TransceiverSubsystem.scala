package hbwif2

import chisel3._
import chisel3.experimental._

class TransceiverOverrideIF()(implicit val c: SerDesConfig) extends Bundle {

  // Note that these all have an "unsafe" crossing from the TX into RX domain

  // CDR override
  val cdriValue = Input(UInt((if (c.cdrHasOverride) c.cdrIWidth else 0).W))
  val cdrpValue = Input(UInt((if (c.cdrHasOverride) c.cdrPWidth else 0).W))
  val cdr = Input(UInt((if (c.cdrHasOverride) 1 else 0).W))

  def getCDRI(default: UInt): UInt = (if(c.cdrHasOverride) Mux(cdr === 1.U, cdriValue, default) else default)
  def getCDRP(default: UInt): UInt = (if(c.cdrHasOverride) Mux(cdr === 1.U, cdrpValue, default) else default)

  // DFE override
  val dfeTapsValue = Input(Vec(if (c.dfeHasOverride) c.dfeNumTaps else 0, UInt(c.dfeTapWidth.W)))
  val dfe = Input(UInt((if (c.dfeHasOverride) 1 else 0).W))

  def getDFETaps(default: Vec[UInt]): Vec[UInt] = (if(c.dfeHasOverride) Mux(dfe === 1.U, dfeTapsValue, default) else default)

  // DLEV override
  val dlevDACValue = Input(UInt((if (c.dlevHasOverride) c.dlevDACWidth else 0).W))
  val dlev = Input(UInt((if (c.dlevHasOverride) 1 else 0).W))

  def getDlevDAC(default: UInt): UInt = (if(c.dlevHasOverride) Mux(dlev === 1.U, dlevDACValue, default) else default)

}

trait TransceiverOuterIF extends Bundle {
  implicit val c: SerDesConfig

  // asynchronous reset
  val asyncResetIn = Input(Bool())

  // reference clock
  val clockRef = Input(Clock())

  // RX pad inputs
  val rx = Flipped(new Differential)

  // TX pad outputs
  val tx = new Differential

}

class TransceiverSubsystemIO()(implicit val c: SerDesConfig) extends Bundle with TransceiverOuterIF {

  // override interface
  val overrides = new TransceiverOverrideIF

  // data interface
  val data = new TransceiverDataIF

  // clock and reset for the rest of the digital
  val slowClock = Output(Clock())
  val syncReset = Output(Bool())

}

class TransceiverSubsystem()(implicit val c: SerDesConfig) extends Module with HasControllerConnector {

  val io = IO(new TransceiverSubsystemIO)

  // Transceiver <> top level connections
  val txrx = Module(new Transceiver)

  txrx.io.clock_ref := io.clockRef
  txrx.io.async_reset_in := io.asyncResetIn

  io.tx <> txrx.io.tx
  io.rx <> txrx.io.rx

  io.data.dlev := txrx.io.data.dlev // TODO do we want to be able to observe this?
  io.data.rx := txrx.io.data.rx    // XXX TODO retime and add plesiochronous buffer

  txrx.io.data.tx := io.data.tx

  val txSyncReset = AsyncResetSynchronizer(txrx.io.clock_tx, io.asyncResetIn)
  val rxSyncReset = AsyncResetSynchronizer(txrx.io.clock_rx, io.asyncResetIn)

  io.slowClock := txrx.io.clock_tx
  io.syncReset := txSyncReset

  withClockAndReset(txrx.io.clock_rx, rxSyncReset) {

    // Transceiver <> CDR Loop
    val cdr = Module(new CDR)

    txrx.io.cdri := io.overrides.getCDRI(cdr.io.i)
    txrx.io.cdrp := io.overrides.getCDRP(cdr.io.p)
    txrx.io.dither_clock := cdr.io.dither_clock
    cdr.io.data_dlev := txrx.io.data.dlev
    cdr.io.data_rx := txrx.io.data.rx

    // Transceiver <> DFE Loop
    if (c.dfeNumTaps > 0) {
      val dfe = Module(new DFE)
      txrx.io.dfe_taps := io.overrides.getDFETaps(dfe.io.taps)
      dfe.io.data_dlev := txrx.io.data.dlev
      dfe.io.data_rx := txrx.io.data.rx
    }

    // Transceiver <> DLEV Loop
    val dlev = Module(new DLEV)
    txrx.io.dlev_dac := io.overrides.getDlevDAC(dlev.io.code)
    dlev.io.data_rx := txrx.io.data.rx
    dlev.io.data_dlev := txrx.io.data.dlev

  }

  def connectController(builder: ControllerBuilder) {
    //TODO
  }

}
