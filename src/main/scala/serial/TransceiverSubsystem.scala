package awl.serial

import chisel3._
import chisel3.util._
import chisel3.experimental._

import freechips.rocketchip.util.ResetCatchAndSync

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
}

class GenericTransceiverSubsystemControlBundle extends ControlBundle {

    // RX invert bit
    val rxInvert = input(Bool(), 0, "rx_invert",
        "RX polarity select, 0 = normal, 1 = inverted", RxClock)

    // TX invert bit
    val txInvert = input(Bool(), 0, "tx_invert",
        "TX polarity select, 0 = normal, 1 = inverted", TxClock)

}

abstract class TransceiverSubsystem()(implicit val c: SerDesConfig) extends MultiIOModule with HasControllerConnector {


    type T <: HasTransceiverIO
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

    override val controlIO = Some(IO(new GenericTransceiverSubsystemControlBundle))

    connectInvert
}

trait HasTransceiverSubsystemConnections {
    this: TransceiverSubsystem =>
    val txrx: HasTransceiverIO

    val controlIO: Option[GenericTransceiverSubsystemControlBundle]

    txrx.io.clock_ref := io.clockRef
    txrx.io.async_reset_in := io.asyncResetIn

    io.tx <> txrx.io.tx
    io.rx <> txrx.io.rx

    val txSyncReset = ResetCatchAndSync(txrx.io.clock_tx_div, io.asyncResetIn, 3)
    val rxSyncReset = ResetCatchAndSync(txrx.io.clock_rx_div, io.asyncResetIn, 3)

    def connectInvert {
        val ctrl = controlIO.get.asInstanceOf[GenericTransceiverSubsystemControlBundle]
        io.data.rx.bits := txrx.io.data.rx ^ Fill(c.dataWidth, ctrl.rxInvert)
        txrx.io.data.tx := io.data.tx.bits ^ Fill(c.dataWidth, ctrl.txInvert)
    }
    io.data.rx.valid := true.B
    io.data.tx.ready := true.B

    io.txClock := txrx.io.clock_tx_div
    io.txReset := txSyncReset
    io.rxClock := txrx.io.clock_rx_div
    io.rxReset := rxSyncReset

}

trait HasGenericTransceiverSubsystem {
    this: Lane =>
    implicit val c: SerDesConfig

    def genTransceiverSubsystem() = new GenericTransceiverSubsystem()(c)

}
