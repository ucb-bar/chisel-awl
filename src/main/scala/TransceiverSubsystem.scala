package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental._

class TransceiverOverrideIF()(implicit val c: SerDesConfig) extends Bundle {

  // Note that these all have an "unsafe" crossing from the TX into RX domain

  // CDR override
  val cdriValue = if (c.cdrHasOverride) Some(Input(UInt(c.cdrIWidth.W))) else None
  val cdrpValue = if (c.cdrHasOverride) Some(Input(UInt(c.cdrPWidth.W))) else None
  val cdr       = if (c.cdrHasOverride) Some(Input(Bool())) else None

  def getCDRI(default: UInt): UInt = (if(c.cdrHasOverride) Mux(cdr.get, cdriValue.get, default) else default)
  def getCDRP(default: UInt): UInt = (if(c.cdrHasOverride) Mux(cdr.get, cdrpValue.get, default) else default)

  // DFE override
  val dfeTapsValue = if (c.dfeHasOverride) Some(Input(Vec(c.dfeNumTaps, UInt(c.dfeTapWidth.W)))) else None
  val dfe          = if (c.dfeHasOverride) Some(Input(Bool())) else None

  def getDFETaps(default: Vec[UInt]): Vec[UInt] = (if(c.dfeHasOverride) Mux(dfe.get, dfeTapsValue.get, default) else default)

  // DLEV override
  val dlevDACValue = if (c.dlevHasOverride) Some(Input(UInt(c.dlevDACWidth.W))) else None
  val dlev         = if (c.dlevHasOverride) Some(Input(Bool())) else None

  def getDlevDAC(default: UInt): UInt = (if(c.dlevHasOverride) Mux(dlev.get, dlevDACValue.get, default) else default)

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

class TransceiverSubsystemDataIF()(implicit val c: SerDesConfig) extends Bundle {

    val rx = Valid(UInt(c.dataWidth.W))
    val tx = Flipped(Ready(UInt(c.dataWidth.W)))

}

class TransceiverSubsystemIO()(implicit val c: SerDesConfig) extends Bundle with TransceiverOuterIF {

  // override interface
  val overrides = new TransceiverOverrideIF

  // data interface
  val data = new TransceiverSubsystemDataIF

  // clock and reset for the rest of the digital
  val txClock = Output(Clock())
  val txReset = Output(Bool())
  val rxClock = Output(Clock())
  val rxReset = Output(Bool())

  // bit stuff mode
  val bitStuffMode = if(c.bitStuffModes > 1) Some(Input(UInt(log2Ceil(c.bitStuffModes).W))) else None

}

class TransceiverSubsystem()(implicit val c: SerDesConfig) extends Module with HasControllerConnector {

  val io = IO(new TransceiverSubsystemIO)

  // Transceiver <> top level connections
  val txrx = Module(new Transceiver)

  txrx.io.clock_ref := io.clockRef
  txrx.io.async_reset_in := io.asyncResetIn

  io.tx <> txrx.io.tx
  io.rx <> txrx.io.rx

  // TODO do we want observability on dlev
  // TODO do we need to do anything special re: bit stuffing
  // io.dlev := txrx.io.data.dlev

  val txSyncReset = AsyncResetSynchronizer(txrx.io.clock_tx, io.asyncResetIn)
  val rxSyncReset = AsyncResetSynchronizer(txrx.io.clock_rx, io.asyncResetIn)

  val rxBitStuffer = withClockAndReset(txrx.io.clock_rx, rxSyncReset) { Module(new RxBitStuffer) }
  val txBitStuffer = withClockAndReset(txrx.io.clock_tx, txSyncReset) { Module(new TxBitStuffer) }
  rxBitStuffer.io.raw := txrx.io.data.rx
  txrx.io.data.tx := txBitStuffer.io.raw
  txBitStuffer.io.enq <> io.data.tx
  io.data.rx <> rxBitStuffer.io.deq
  rxBitStuffer.io.mode.map(_ := io.bitStuffMode.get)
  txBitStuffer.io.mode.map(_ := io.bitStuffMode.get)

  io.txClock := txrx.io.clock_tx
  io.txReset := txSyncReset
  io.rxClock := txrx.io.clock_rx
  io.rxReset := rxSyncReset

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
    io.bitStuffMode.map(x => builder.w("bit_stuff_mode", x))
    io.overrides.cdriValue.map(x => builder.w("cdr_i_value", x))
    io.overrides.cdrpValue.map(x => builder.w("cdr_p_value", x))
    io.overrides.cdr.map(x => builder.w("cdr_override", x))
    io.overrides.dfeTapsValue.map(x => builder.w("dfe_value", x))
    io.overrides.dfe.map(x => builder.w("dfe_override", x))
    io.overrides.dlevDACValue.map(x => builder.w("dlev_dac_value", x))
    io.overrides.dlev.map(x => builder.w("dlev_override", x))
  }

}

class TxBitStuffer()(implicit val c: SerDesConfig) extends Module {

    val io = IO(new Bundle {
        val enq = Flipped(Ready(UInt(c.dataWidth.W)))
        val raw = Output(UInt(c.dataWidth.W))
        val mode = if (c.bitStuffModes > 1) Some(Input(UInt(log2Ceil(c.bitStuffModes).W))) else None
    })

    require(c.bitStuffModes > 0)
    require(c.dataWidth % (1 << (c.bitStuffModes - 1)) == 0)

    if (c.bitStuffModes == 1) {
        io.raw := io.enq.bits
        io.enq.ready := true.B
    } else {
        val buf = Reg(UInt(c.dataWidth.W))
        val count = RegInit(0.U((c.bitStuffModes - 1).W))
        io.raw := buf
        (0 until c.bitStuffModes).foreach { i =>
            when (io.mode.get === i.U) {
                io.raw := FillInterleaved((1 << i), buf(c.dataWidth - 1, c.dataWidth - (c.dataWidth/(1 << i))))
            }
        }
        when ((count +& 1.U)(io.mode.get) === true.B) {
            count := 0.U
            io.enq.ready := true.B
            buf := io.enq.bits
        } .otherwise {
            count := count + 1.U
            io.enq.ready := false.B
            (0 until c.bitStuffModes).foreach { i =>
                when(io.mode.get === i.U) {
                    if (i > 0) {
                        buf := buf << (c.dataWidth/(1 << i))
                    } else {
                        // but we shouldn't get here
                        buf := io.enq.bits
                    }
                }
            }
        }
    }

}

class RxBitStuffer()(implicit val c: SerDesConfig) extends Module {

    val io = IO(new Bundle {
        val raw = Input(UInt(c.dataWidth.W))
        val deq = Valid(UInt(c.dataWidth.W))
        val mode = if (c.bitStuffModes > 1) Some(Input(UInt(log2Ceil(c.bitStuffModes).W))) else None
    })

    // TODO implement this so that we don't incur a register delay in mode 0
    require(c.bitStuffModes > 0)
    require(c.dataWidth % (1 << (c.bitStuffModes - 1)) == 0)

    if (c.bitStuffModes == 1) {
        io.deq.bits := io.raw
        io.deq.valid := true.B
    } else {
        val buf = Reg(UInt(c.dataWidth.W))
        val count = RegInit(0.U(log2Ceil(c.bitStuffModes).W))
        io.deq.bits := buf
        (0 until c.bitStuffModes).foreach { i =>
            when(io.mode.get === i.U) {
                val tmp = Wire(UInt((c.dataWidth / (1 << i)).W))
                tmp := io.raw.toBools.zipWithIndex.filter(_._2 % (1 << i) == 0).map(_._1.asUInt).reduceLeft(Cat(_,_))
                if (i > 0) {
                    buf := Cat(buf(c.dataWidth - 1 - (c.dataWidth/(1 << i)),0), tmp)
                } else {
                    buf := tmp
                }
            }
        }
        when ((count +& 1.U)(io.mode.get) === true.B) {
            count := 0.U
            io.deq.valid := true.B
        } .otherwise {
            count := count + 1.U
            io.deq.valid := false.B
        }
    }

}
