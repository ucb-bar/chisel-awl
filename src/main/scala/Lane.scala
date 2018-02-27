package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental._

trait LaneTypes {
    type DecodedSymbolType <: DecodedSymbol
    type TxDataType <: Data
    type RxDataType <: Data
    type ControlPortType <: Bundle
    type ControlType <: Control
    val portFactory: () => ControlPortType
    val txFactory: () => TxDataType
    val rxFactory: () => RxDataType
    val controlFactory: (ControlSpec) => ControlType
}


abstract class LaneIO extends Bundle with TransceiverOuterIF with LaneTypes {
    val port = portFactory()
    val dataTx = Decoupled(txFactory())
    val dataRx = Flipped(Decoupled(rxFactory()))
}

abstract class Lane extends Module with LaneTypes {

    override val io: LaneIO

    implicit val c: SerDesGeneratorConfig

    val txrxss = Module(new TransceiverSubsystem)
    def encoder: Encoder
    def decoder: Decoder
    def packetizer: Packetizer
    val encoderModule = Module(encoder)
    val decoderModule = Module(decoder)
    val packetizerModule = Module(packetizer)
    val builder = new ControlBuilder(controlFactory)

    val encoderAdapter = Module(new EncoderWidthAdapter(encoderModule.encodedWidth, c.dataWidth))
    val decoderAdapter = Module(new DecoderWidthAdapter(c.dataWidth, decoderModule.encodedWidth))

    txrxss.io.clock_ref := io.clock_ref
    txrxss.io.async_reset_in := io.async_reset_in
    io.bias <> txrxss.io.bias
    io.rx <> txrxss.io.rx
    io.tx <> txrxss.io.rx

    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    require(encoderModule.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")
    require(decoderModule.encodedWidth >= c.dataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    // TODO mux in BERT

    encoderAdapter.io.enq := encoderModule.io.encoded
    encoderModule.io.next := encoderAdapter.io.next
    txrxss.io.data.tx := encoderAdapter.io.deq

    decoderModule.io.encoded := decoderAdapter.io.deq
    decoderAdapter.io.enq := txrxss.io.data.rx

    // TODO add overrideIF

    encoderModule.io.decoded := packetizerModule.io.symbolsTx
    // packetizer.io.packetTx
    packetizerModule.io.symbolsRx := decoderModule.io.decoded
    // packetizer.io.packetRx


    val ctrl = builder.generate()
    io.port <> ctrl.io.port

}

