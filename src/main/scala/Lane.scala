package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental._

final class LaneIO[T <: Bundle, U <: Data, V <: Data](portFactory: () => T, txFactory: () => U, rxFactory: () => V)(implicit val c: SerDesGeneratorConfig)
    extends Bundle with TransceiverOuterIF {
    val port = portFactory()
    val dataTx = Decoupled(txFactory())
    val dataRx = Flipped(Decoupled(rxFactory()))
}

abstract class Lane extends Module {

    type DecodedSymbolType <: DecodedSymbol
    type TxDataType <: Data
    type RxDataType <: Data
    type ControllerPortType <: Bundle
    type ControllerType <: Controller
    val portFactory: () => ControllerPortType
    val txFactory: () => TxDataType
    val rxFactory: () => RxDataType
    val controllerFactory: (ControlSpec) => ControllerType

    val io = IO(new LaneIO(portFactory, txFactory, rxFactory))

    implicit val c: SerDesGeneratorConfig

    val txrxss = Module(new TransceiverSubsystem)
    val encoder: Encoder
    val decoder: Decoder
    val packetizer: Packetizer
    val builder = new ControllerBuilder(controllerFactory)

    val encoderAdapter = Module(new EncoderWidthAdapter(encoder.encodedWidth, c.dataWidth))
    val decoderAdapter = Module(new DecoderWidthAdapter(c.dataWidth, decoder.encodedWidth))

    txrxss.io.clock_ref := io.clock_ref
    txrxss.io.async_reset_in := io.async_reset_in
    io.bias <> txrxss.io.bias
    io.rx <> txrxss.io.rx
    io.tx <> txrxss.io.rx

    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    require(encoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")
    require(decoder.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    // TODO mux in BERT

    encoderAdapter.io.enq := encoder.io.encoded
    encoder.io.next := encoderAdapter.io.next
    txrxss.io.data.tx := encoderAdapter.io.deq

    decoder.io.encoded := decoderAdapter.io.deq
    decoderAdapter.io.enq := txrxss.io.data.rx

    // TODO add overrideIF

    encoder.io.decoded := packetizer.io.symbolsTx
    // packetizer.io.packetTx
    packetizer.io.symbolsRx := decoder.io.decoded
    // packetizer.io.packetRx

    def connectController() = { io.port <> builder.generate().io.port }

}
