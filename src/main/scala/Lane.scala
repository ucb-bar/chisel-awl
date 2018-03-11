package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental._

final class LaneIO[P <: Bundle, T <: Data](portFactory: () => P, dataFactory: () => T)(implicit val c: SerDesGeneratorConfig)
    extends Bundle with TransceiverOuterIF {
    val port = portFactory()
    val data = dataFactory()
}

abstract class Lane(val decodedSymbolsPerCycle: Int) extends Module with HasDebug {

    type C <: Controller
    type T <: Data

    implicit val c: SerDesGeneratorConfig

    def genEncoder(): Encoder
    def genDecoder(): Decoder
    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S): Packetizer[S, T]
    def genBuilder(): ControllerBuilder[C]

    val encoder = Module(genEncoder())
    val decoder = Module(genDecoder())
    val builder = genBuilder()
    val txrxss = Module(new TransceiverSubsystem)
    val packetizer = Module(genPacketizer(encoder.symbolFactory))
    val debugBus = genDebug().map(Module(_))

    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    require(encoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")
    require(decoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    val encoderAdapter = Module(new EncoderWidthAdapter(encoder.encodedWidth, c.dataWidth))
    val decoderAdapter = Module(new DecoderWidthAdapter(c.dataWidth, decoder.encodedWidth))


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

    // TODO add overrideIF

    txrxss.connectController(builder)
    encoder.connectController(builder)
    decoder.connectController(builder)
    packetizer.connectController(builder)
    debugBus.foreach(_.connectController(builder))

    val controller = builder.generate()

    val io = IO(new LaneIO[controller.P, T](controller.portFactory, packetizer.dataFactory))

    // TODO why is this broken
    //io.data <> packetizer.io.data
    io.port <> controller.io.port

    txrxss.io.clock_ref := io.clock_ref
    txrxss.io.async_reset_in := io.async_reset_in
    io.bias <> txrxss.io.bias
    io.rx <> txrxss.io.rx
    io.tx <> txrxss.io.rx

}
