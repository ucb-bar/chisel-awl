package hbwif2

import chisel3._
import chisel3.util._

abstract class DecodedSymbol extends Bundle {

    val decodedWidth: Int
    val encodedWidth: Int
    final val bits = UInt(decodedWidth.W)

    // Comparison
    def ===(other: DecodedSymbol): Bool = {
        if (this.decodedWidth == other.decodedWidth) {
            this.toBits === other.toBits
        } else {
            false.B
        }
    }

    // Minimum set of literals needed for the protocol to work
    def comma: DecodedSymbol
    def ack: DecodedSymbol
    def nack: DecodedSymbol
    def sync: DecodedSymbol

    // How to convert data to/from (NOT the encoding, how do we convert this type to a data UInt)
    def fromData(d: UInt): DecodedSymbol
    def isData: Bool
}

abstract class Encoder[T <: DecodedSymbol](val symbolFactory: () => T) extends Module {

    val serdesDataWidth: Int = symbolFactory().encodedWidth * numSymbols
    val numSymbols: Int
    assert(numSymbols >= 1, "Cannot have 0- or negative-width Encoder")

    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    assert(symbolFactory().encodedWidth * numSymbols >= serdesDataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    final val io = IO(new Bundle {
        val tx = Output(UInt(serdesDataWidth.W))
        val decoded = Input(Vec(numSymbols, Decoupled(symbolFactory())))
    })

    final val encoded = UInt((symbolFactory().encodedWidth * numSymbols).W)
    if ((symbolFactory().encodedWidth * numSymbols) == serdesDataWidth) {
        io.tx := encoded
    } else {
        assert(false, "FIXME, add a queue here")
    }
}

abstract class Decoder[T <: DecodedSymbol](val symbolFactory: () => T) extends Module {

    // decoded => encoded width map
    val numSymbols: Int
    assert(numSymbols >= 1, "Cannot have 0- or negative-width Decoder")

    final val io = IO(new Bundle {
        val encoded = Input(UInt((symbolFactory().encodedWidth * numSymbols).W))
        val decoded = Output(Vec(numSymbols, Valid(symbolFactory())))
    })

    // TODO insert widget here to allow for encoded width mismatches
}
