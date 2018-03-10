package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental._

final class LaneIO[P <: Bundle, T <: Data](portFactory: () => P, dataFactory: () => T)(implicit val c: SerDesGeneratorConfig)
    extends Bundle with TransceiverOuterIF {
    val port = portFactory()
    val data = dataFactory()
}

abstract class Lane extends Module {

    type C <: Controller

    implicit val c: SerDesGeneratorConfig

    def genEncoder(): Encoder
    def genDecoder(): Decoder
    def genPacketizer[S <: DecodedSymbol, T <: Data](symbolFactory: () => S, decodedSymbolsPerCycle: Int): Packetizer[S, T]
    def genBuilder(): ControllerBuilder[C]

    val builder = genBuilder()
    val txrxss = Module(new TransceiverSubsystem)
    val encoder = Module(genEncoder())
    val decoder = Module(genDecoder())
    val packetizer = Module(genPacketizer(encoder.symbolFactory, encoder.decodedSymbolsPerCycle))

    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    require(encoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")
    require(decoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    val encoderAdapter = Module(new EncoderWidthAdapter(encoder.encodedWidth, c.dataWidth))
    val decoderAdapter = Module(new DecoderWidthAdapter(c.dataWidth, decoder.encodedWidth))

    // TODO mux in BERT

    encoderAdapter.io.enq := encoder.io.encoded
    encoder.io.next := encoderAdapter.io.next
    txrxss.io.data.tx := encoderAdapter.io.deq

    decoder.io.encoded := decoderAdapter.io.deq
    decoderAdapter.io.enq := txrxss.io.data.rx

    // TODO add overrideIF

    encoder.io.decoded <> packetizer.io.symbolsTx
    packetizer.io.symbolsTxReady := encoder.io.decodedReady
    // packetizer.io.packetTx
    packetizer.io.symbolsRx := decoder.io.decoded
    // packetizer.io.packetRx

    txrxss.connectController(builder)
    encoder.connectController(builder)
    decoder.connectController(builder)
    packetizer.connectController(builder)

    val controller = builder.generate()

    val io = IO(new LaneIO(controller.portFactory, packetizer.dataFactory))

    io.port <> controller.io.port

    txrxss.io.clock_ref := io.clock_ref
    txrxss.io.async_reset_in := io.async_reset_in
    io.bias <> txrxss.io.bias
    io.rx <> txrxss.io.rx
    io.tx <> txrxss.io.rx

}
