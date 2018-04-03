package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental._

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

    // data interface
    val data = new TransceiverSubsystemDataIF

    // clock and reset for the rest of the digital
    val txClock = Output(Clock())
    val txReset = Output(Bool())
    val rxClock = Output(Clock())
    val rxReset = Output(Bool())

    // RX invert bit
    val rxInvert = Input(Bool())

    // TX invert bit
    val txInvert = Input(Bool())
}

abstract class TransceiverSubsystem()(implicit val c: SerDesConfig) extends Module with HasControllerConnector {


    type T <: Transceiver
    type I <: TransceiverSubsystemIO

    def genTransceiver(): T
    def genTransceiverSubsystemIO(): I

    val txrx = Module(genTransceiver())
    val io = IO(genTransceiverSubsystemIO())

}

class GenericTransceiverSubsystem()(implicit c: SerDesConfig) extends TransceiverSubsystem()(c) with HasTransceiverSubsystemConnections {

    type T = GenericTransceiver
    type I = TransceiverSubsystemIO

    def genTransceiver() = new GenericTransceiver
    def genTransceiverSubsystemIO() = new TransceiverSubsystemIO

}

trait HasTransceiverSubsystemConnections {
    this: TransceiverSubsystem =>
    val txrx: Transceiver

    txrx.io.clock_ref := io.clockRef
    txrx.io.async_reset_in := io.asyncResetIn

    io.tx <> txrx.io.tx
    io.rx <> txrx.io.rx

    val txSyncReset = AsyncResetSynchronizer(txrx.io.clock_tx, io.asyncResetIn)
    val rxSyncReset = AsyncResetSynchronizer(txrx.io.clock_rx, io.asyncResetIn)

    io.data.rx.bits := txrx.io.data.rx ^ Fill(c.dataWidth, io.rxInvert)
    txrx.io.data.tx := io.data.tx.bits ^ Fill(c.dataWidth, io.txInvert)
    io.data.rx.valid := true.B
    io.data.tx.ready := true.B

    io.txClock := txrx.io.clock_tx
    io.txReset := txSyncReset
    io.rxClock := txrx.io.clock_rx
    io.rxReset := rxSyncReset

    def connectController(builder: ControllerBuilder) {
        builder.w("tx_invert", io.txInvert, 0)
        builder.w("rx_invert", io.rxInvert, 0)
    }

}

trait HasGenericTransceiverSubsystem {
    this: Lane =>
    implicit val c: SerDesConfig

    def genTransceiverSubsystem() = new GenericTransceiverSubsystem()(c)

}
