package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.MultiIOModule
import freechips.rocketchip.util.ResetCatchAndSync

final class LaneIO[T <: Data](val dataFactory: () => T)(implicit val c: SerDesConfig)
    extends Bundle with TransceiverOuterIF {
    val data = dataFactory()
    val dataClock = Input(Clock())
    val txClock = Output(Clock())
    val txReset = Output(Bool())
    val rxClock = Output(Clock())
    val rxReset = Output(Bool())
}

abstract class Lane extends MultiIOModule with HasDebug {

    def decodedSymbolsPerCycle: Int

    type T <: Data

    implicit val c: SerDesConfig

    def genEncoder(): Encoder
    def genDecoder(): Decoder
    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S): Packetizer[S, T]
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
    private def iomap[T <: Data] = { x:T => IO(chiselTypeOf(x)).asInstanceOf[T] }
    val ssio         = txrxss.controlIO.map(iomap)
    val encoderio    = encoder.controlIO.map(iomap)
    val decoderio    = decoder.controlIO.map(iomap)
    val packetizerio = packetizer.controlIO.map(iomap)
    val debugio      = debugBus.map(_.controlIO.map(iomap))

    txrxss.controlIO.foreach(_.attach(ssio))
    encoder.controlIO.foreach(_.attach(encoderio))
    decoder.controlIO.foreach(_.attach(decoderio))
    packetizer.controlIO.foreach(_.attach(packetizerio))
    debugBus.zip(debugio).foreach { case (d, i) => d.controlIO.foreach(_.attach(i)) }

    val io = IO(new LaneIO[T](packetizer.dataFactory))

    // async crossings live in here

    // async crossings live in here
    val packetizerReset = ResetCatchAndSync(io.dataClock, reset.toBool())
    packetizer.connectData(io.dataClock, packetizerReset, io.data)

    txrxss.io.clockRef := io.clockRef
    txrxss.io.txAsyncResetIn := io.txAsyncResetIn
    txrxss.io.rxAsyncResetIn := io.rxAsyncResetIn

    txrxss.io.rx <> io.rx
    io.tx <> txrxss.io.tx

    io.txClock := txClock
    io.txReset := txReset
    io.rxClock := rxClock
    io.rxReset := rxReset

}
