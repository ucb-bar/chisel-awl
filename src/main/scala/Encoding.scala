package hbwif2

import chisel3._
import chisel3.util._

abstract class DecodedSymbol extends Bundle {

    val decodedWidth: Int
    val encodedWidth: Int
    val bits = UInt(decodedWidth.W)

    // Minimum set of literals needed for the protocol to work
    def comma: DecodedSymbol
    //def ack: this.type
    //def nack: this.type
}

abstract class Encoder[T <: DecodedSymbol](val symbolFactory: () => T) extends Module {

    // encoded => decoded width map
    val numSymbols: Int
    assert(numSymbols >= 1, "Cannot have 0- or negative-width Encoder")

    val io = IO(new Bundle {
        val encoded = Output(UInt((symbolFactory().encodedWidth * numSymbols).W))
        val decoded = Input(Vec(numSymbols, Decoupled(symbolFactory())))
    })

    // TODO insert widget here to allow for encoded width mismatches
}

abstract class Decoder[T <: DecodedSymbol](val symbolFactory: () => T) extends Module {

    // decoded => encoded width map
    val numSymbols: Int
    assert(numSymbols >= 1, "Cannot have 0- or negative-width Decoder")

    val io = IO(new Bundle {
        val encoded = Input(UInt((symbolFactory().encodedWidth * numSymbols).W))
        val decoded = Output(Vec(numSymbols, Valid(symbolFactory())))
    })

    // TODO insert widget here to allow for encoded width mismatches
}
