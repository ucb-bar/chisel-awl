package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental._

final class LaneIO[P <: Bundle, T <: Data](portFactory: () => P, dataFactory: () => T)(implicit val c: SerDesConfig)
    extends Bundle with TransceiverOuterIF {
    val control = portFactory()
    val data = dataFactory()
    val laneClock = Output(Clock())
    val laneReset = Output(Bool())
}

abstract class Lane extends Module with HasDebug {

    def decodedSymbolsPerCycle: Int

    type T <: Data

    implicit val c: SerDesConfig

    def genEncoder(): Encoder
    def genDecoder(): Decoder
    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S): Packetizer[S, T]
    def genBuilder(): ControllerBuilder
    def genTransceiverSubsystem(): TransceiverSubsystem

    val txrxss = Module(genTransceiverSubsystem())

    val txClock = txrxss.io.txClock
    val txReset = txrxss.io.txReset
    val rxClock = txrxss.io.rxClock
    val rxReset = txrxss.io.rxReset

    val decoder        = withClockAndReset(rxClock, rxReset) { Module(genDecoder()) }
    val decoderAdapter = withClockAndReset(rxClock, rxReset) { Module(new DecoderWidthAdapter(c.dataWidth, decoder.encodedWidth)) }

    val encoder        = withClockAndReset(txClock, txReset) { Module(genEncoder()) }
    val encoderAdapter = withClockAndReset(txClock, txReset) { Module(new EncoderWidthAdapter(encoder.encodedWidth, c.dataWidth)) }
    val packetizer     = withClockAndReset(txClock, txReset) { Module(genPacketizer(encoder.symbolFactory)) }

    val builder        = genBuilder()
    val debugBus       = genDebug().map { x =>
        x.io.txClock := txClock
        x.io.txReset := txReset
        x.io.rxClock := rxClock
        x.io.rxReset := rxReset
        x
    }

    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    require(encoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")
    require(decoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    val (txDebug, rxDebug) = debugBus.foldLeft((encoderAdapter.io.deq, decoderAdapter.io.enq)) { case ((tx, rx), debug) =>
        debug.io.txIn <> tx
        rx <> debug.io.rxOut
        (debug.io.txOut, debug.io.rxIn)
    }

    // TX chain
    txDebug <> txrxss.io.data.tx
    encoderAdapter.io.enq <> encoder.io.encoded
    packetizer.io.symbolsTxReady := encoder.io.decodedReady
    encoder.io.decoded <> packetizer.io.symbolsTx

    // RX into TX domain crossing
    val decoderQueue = Module(new DecoderQueue(decodedSymbolsPerCycle, encoder.symbolFactory))
    decoderQueue.io.enqClock := rxClock
    decoderQueue.io.deqClock := txClock
    decoderQueue.io.enqReset := rxReset
    decoderQueue.io.deqReset := txReset

    // RX chain
    rxDebug <> txrxss.io.data.rx
    decoder.io.encoded <> decoderAdapter.io.deq
    decoderQueue.io.enq <> decoder.io.decoded
    packetizer.io.symbolsRx <> decoderQueue.io.deq

    // Be careful with clock crossings here
    withClockAndReset(txClock, txReset) {
        txrxss.connectController(builder)
        encoder.connectController(builder)
        decoder.connectController(builder)
        packetizer.connectController(builder)
        debugBus.foreach(_.connectController(builder))
    }

    val io = IO(new LaneIO[builder.P, T](builder.createPort _, packetizer.dataFactory))

    // async crossings live in here
    builder.generate(txClock, txReset, clock, reset.toBool, io.control)

    // async crossings live in here
    packetizer.connectData(clock, reset.toBool, io.data)

    txrxss.io.clockRef := io.clockRef
    txrxss.io.asyncResetIn := io.asyncResetIn

    txrxss.io.rx <> io.rx
    io.tx <> txrxss.io.tx

    io.laneClock := txClock
    io.laneReset := txReset

}
