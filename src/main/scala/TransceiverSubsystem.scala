package hbwif2

import chisel3._
import chisel3.util._
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

class TransceiverSubsystemDataIF()(implicit val c: SerDesConfig) extends Bundle {

    val rx = Valid(UInt(c.dataWidth.W))
    val tx = Input(UInt(c.dataWidth.W))
    val txReady = Output(Bool())

}

class TransceiverSubsystemIO()(implicit val c: SerDesConfig) extends Bundle with TransceiverOuterIF {

  // override interface
  val overrides = new TransceiverOverrideIF

  // data interface
  val data = new TransceiverSubsystemDataIF

  // clock and reset for the rest of the digital
  val slowClock = Output(Clock())
  val syncReset = Output(Bool())

  // bit stuff mode (can be 0 width)
  val bitStuffMode = Input(UInt(log2Ceil(c.bitStuffModes).W))

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
  rxBitStuffer.io.mode := io.bitStuffMode
  txBitStuffer.io.mode := io.bitStuffMode

  val rxRetimer = Module(new RxRetimer)
  rxRetimer.io.dataIn <> rxBitStuffer.io.deq
  rxRetimer.io.clockIn := txrx.io.clock_rx
  io.data.rx <> rxRetimer.io.dataOut
  rxRetimer.io.clockOut := txrx.io.clock_tx

  io.slowClock := txrx.io.clock_tx
  io.syncReset := txSyncReset

  withClockAndReset(txrx.io.clock_rx, txSyncReset) {

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
    //TODO more signals need to go here
    builder.w("bit_stuff_mode", io.bitStuffMode)
  }

}

class TxBitStuffer()(implicit val c: SerDesConfig) extends Module {

    val io = IO(new Bundle {
        val enq = Flipped(Ready(UInt(c.dataWidth.W)))
        val raw = Output(UInt(c.dataWidth.W))
        val mode = Input(UInt(log2Ceil(c.bitStuffModes).W))
    })

    require(c.bitStuffModes > 0)
    require(c.dataWidth % (1 << (c.bitStuffModes - 1)) == 0)

    if (c.bitStuffModes == 1) {
        io.raw := io.enq.bits
        io.enq.ready := true.B
    } else {
        val buf = Reg(UInt(c.dataWidth.W))
        val count = RegInit(0.U((c.bitStuffModes - 1).W))
        (0 until c.bitStuffModes).foldLeft(when(false.B) { io.raw := buf } ) { (w, i) =>
            w.elsewhen (io.mode === i.U) {
                io.raw := FillInterleaved(i, buf(c.dataWidth - 1, c.dataWidth - (c.dataWidth/(1 << i))))
            }
        } .otherwise {
            io.raw := buf
        }
        when ((count +& 1.U)(io.mode) === true.B) {
            count := 0.U
            io.enq.ready := true.B
            buf := io.enq.bits
        } .otherwise {
            count := count + 1.U
            io.enq.ready := false.B
            (0 until c.bitStuffModes).foreach { i =>
                when(io.mode === i.U) {
                    if (i > 0) {
                        buf := Cat(buf(c.dataWidth - 1 - (c.dataWidth/(1 << i)), 0), buf((c.dataWidth/(1 << i)) - 1, 0))
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
        val mode = Input(UInt((c.bitStuffModes - 1).W))
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
        (0 until c.bitStuffModes).foreach { i =>
            when(io.mode === i.U) {
                val tmp = Wire(UInt((c.dataWidth / (1 << i)).W))
                tmp := io.raw.toBools.zipWithIndex.filter(_._2 % (1 << i) == 0).map(_._1.asUInt).reduceLeft(Cat(_,_))
                if (i > 0) {
                    buf := Cat(buf(c.dataWidth - 1 - (c.dataWidth/(1 << i)),0), tmp)
                } else {
                    buf := tmp
                }
            }
        }
        when ((count +& 1.U)(io.mode) === true.B) {
            count := 0.U
            io.deq.valid := true.B
        } .otherwise {
            count := count + 1.U
            io.deq.valid := false.B
        }
    }

}

class RxRetimer()(implicit val c: SerDesConfig) extends Module {

    val io = IO(new Bundle {
        val dataIn = Flipped(Valid(UInt(c.dataWidth.W)))
        val clockIn = Input(Clock())
        val dataOut = Valid(UInt(c.dataWidth.W))
        val clockOut = Input(Clock())
        val asyncReset = Input(Bool())
    })

    // TODO


}
