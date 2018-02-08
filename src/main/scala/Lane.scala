package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental._


abstract class LaneIO[T <: Control](val controlFactory: () => T) extends Bundle with TransceiverOuterIF {
    val port = controlFactory().portFactory()
}

abstract class Lane[T <: DecodedSymbol, U <: Data, V <: Data, W <: Control](val controlFactory: () => W) extends Module {

    override val io: LaneIO[W]

    implicit val c: SerDesGeneratorConfig

    val txrxss = Module(new TransceiverSubsystem)
    val encoder: Encoder[T]
    val decoder: Decoder[T]
    val packetizer: Packetizer[T, U, V]
    val builder = new ControlBuilder(controlFactory)

    // TODO clean up the snake_case names
    txrxss.io.clock_ref := io.clock_ref
    txrxss.io.async_reset_in := io.async_reset_in
    io.bias <> txrxss.io.bias
    io.rx <> txrxss.io.rx
    io.tx <> txrxss.io.rx

    // TODO mux in BERT
    txrxss.io.data.tx := encoder.io.tx
    // TODO change name from encoded to rx
    decoder.io.encoded := txrxss.io.data.rx

    // TODO add overrideIF

    encoder.io.decoded := packetizer.io.symbolsTx
    // packetizer.io.packetTx
    packetizer.io.symbolsRx := decoder.io.decoded
    // packetizer.io.packetRx


    val ctrl = builder.generate()
    io.port <> ctrl.io.port

}
