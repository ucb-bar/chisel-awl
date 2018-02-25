package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental._


abstract class LaneIO[U <: Data, V <: Data, W <: Bundle, X <: Control[W]](val txFactory: () => U, val rxFactory: () => V, val controlFactory: (Seq[(String, UInt, Option[UInt])], Seq[(String, UInt)]) => X) extends Bundle with TransceiverOuterIF {
    val port = controlFactory(Seq(),Seq()).portFactory()
    val dataTx = Decoupled(txFactory())
    val dataRx = Flipped(Decoupled(rxFactory()))
}

abstract class Lane[T <: DecodedSymbol, U <: Data, V <: Data, W <: Bundle, X <: Control[W]](val txFactory: () => U, val rxFactory: () => V, val controlFactory: (Seq[(String, UInt, Option[UInt])], Seq[(String, UInt)]) => X) extends Module {

    override val io: LaneIO[U,V,W,X]

    implicit val c: SerDesGeneratorConfig

    val txrxss = Module(new TransceiverSubsystem)
    val encoder: Encoder[T]
    val decoder: Decoder[T]
    val packetizer: Packetizer[T, U, V]
    val builder = new ControlBuilder[W, X](controlFactory)

    val encoderAdapter = Module(new EncoderWidthAdapter(encoder.encodedWidth, c.dataWidth))
    val decoderAdapter = Module(new DecoderWidthAdapter(c.dataWidth, decoder.encodedWidth))

    // TODO clean up the snake_case names
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


    val ctrl = builder.generate()
    io.port <> ctrl.io.port

}
