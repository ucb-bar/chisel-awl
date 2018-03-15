package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental._

final class LaneIO[P <: Bundle, T <: Data](portFactory: () => P, dataFactory: () => T)(implicit val c: SerDesConfig)
    extends Bundle with TransceiverOuterIF {
    val control = portFactory()
    val data = dataFactory()
}

abstract class Lane extends Module with HasDebug {

    def decodedSymbolsPerCycle: Int

    type T <: Data

    implicit val c: SerDesConfig

    def genEncoder(): Encoder
    def genDecoder(): Decoder
    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S): Packetizer[S, T]
    def genBuilder(): ControllerBuilder

    val txrxss = Module(new TransceiverSubsystem)

    val slowClock = txrxss.io.slowClock
    val syncReset = txrxss.io.syncReset

    val encoder    = withClockAndReset(slowClock, syncReset) { Module(genEncoder()) }
    val decoder    = withClockAndReset(slowClock, syncReset) { Module(genDecoder()) }
    val packetizer = withClockAndReset(slowClock, syncReset) { Module(genPacketizer(encoder.symbolFactory)) }
    val builder    = genBuilder()
    val debugBus   = genDebug().map(x => withClockAndReset(slowClock, syncReset) { Module(x) })

    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    require(encoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")
    require(decoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    val encoderAdapter = withClockAndReset(slowClock, syncReset) { Module(new EncoderWidthAdapter(encoder.encodedWidth, c.dataWidth)) }
    val decoderAdapter = withClockAndReset(slowClock, syncReset) { Module(new DecoderWidthAdapter(c.dataWidth, decoder.encodedWidth)) }

    withClockAndReset(slowClock, syncReset) {

        encoderAdapter.io.enq := encoder.io.encoded
        encoder.io.next := encoderAdapter.io.next
        txrxss.io.data.tx := debugBus.foldLeft(encoderAdapter.io.deq) { (in, debug) =>
            debug.io.txIn := in
            debug.io.rxIn := txrxss.io.data.rx
            debug.io.txOut
        }

        decoder.io.encoded := decoderAdapter.io.deq
        decoderAdapter.io.enq := txrxss.io.data.rx

        encoder.io.decoded <> packetizer.io.symbolsTx
        packetizer.io.symbolsTxReady := encoder.io.decodedReady
        packetizer.io.symbolsRx := decoder.io.decoded

        txrxss.connectController(builder)
        encoder.connectController(builder)
        decoder.connectController(builder)
        packetizer.connectController(builder)
        debugBus.foreach(_.connectController(builder))

    }

    // Any async crossings need to live in here
    val port = builder.generate(slowClock, syncReset)

    val io = IO(new LaneIO[builder.P, T](builder.createPort _, packetizer.dataFactory))

    io.control <> port
    // Any async crossings need to live in here
    //packetizer.connectData(io.data)

    txrxss.io.clockRef := io.clockRef
    txrxss.io.asyncResetIn := io.asyncResetIn

    io.rx <> txrxss.io.rx
    io.tx <> txrxss.io.rx

}
