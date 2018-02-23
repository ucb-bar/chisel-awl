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

    val serdesDataWidth: Int
    val numSymbols: Int
    assert(numSymbols >= 1, "Cannot have 0- or negative-width Encoder")

    val encodedWidth = symbolFactory().encodedWidth * numSymbols
    // ensure we can always keep the line busy if we want to (have more data to send than bandwidth to send it)
    assert(encodedWidth >= serdesDataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")


    final val io = IO(new Bundle {
        val dataOut = Output(UInt(serdesDataWidth.W))
        val decoded = Input(Vec(numSymbols, Decoupled(symbolFactory())))
    })

    final val adapter = Module(new EncoderWidthAdapter(encodedWidth, serdesDataWidth))

    // Write your encoded output to this wire
    final val encoded = Wire(UInt(encodedWidth.W))
    // Use this signal to move to the next symbol(s)
    final val next: Bool = adapter.io.next

    adapter.io.enq := encoded
    io.dataOut := adapter.io.deq

}

abstract class Decoder[T <: DecodedSymbol](val symbolFactory: () => T) extends Module {

    val serdesDataWidth: Int
    val numSymbols: Int
    assert(numSymbols >= 1, "Cannot have 0- or negative-width Decoder")

    // ensure we have enough bandwidth to handle all of the data coming in
    assert(symbolFactory().encodedWidth * numSymbols >= serdesDataWidth, "The bitwidth of the physical interface (serdesDataWidth) must not be larger than the aggregate bitwidth of the encoded interface")

    final val io = IO(new Bundle {
        val dataIn = Input(UInt((symbolFactory().encodedWidth * numSymbols).W))
        val decoded = Output(Vec(numSymbols, Valid(symbolFactory())))
    })

    final val adapter = Module(new DecoderWidthAdapter(serdesDataWidth, encodedWidth))

    // Encoded output goes to this Valid[UInt] bundle
    final val encoded: Valid[UInt] = adapter.io.deq

    adapter.io.enq := io.dataIn

}

class EncoderWidthAdapter(val enqBits, val deqBits) extends Module {

    val cycles = lcm(enqBits, deqBits) / enqBits
    val stalls = lcm(enqBits, deqBits) / deqBits - cycles

    val numStates = lcm(enqBits, deqBits) / deqBits

    // Assume that ENQ always has valid data we can consume
    val io = IO(new Bundle {
        val enq = Input(UInt(enqBits.W)))
        val next = Output(Bool())
        val deq = Valid(UInt(deqBits.W))
    })

    if (enqBits == deqBits) {
        io.deq := io.enq
        io.next := true.B
    } else {
        assert(enqBits > deqBits, "Cannot have more enqBits than deqBits for the Encoder")
        //val count = RegInit(numStates.U)
        io.next := false.B
        io.deq := buf(deqBits-1,0)
    }
}

class DecoderWidthAdapter(val enqBits, val deqBits) extends Module {

    val numStates = lcm(enqBits, deqBits) / enqBits

    val io = IO(new Bundle {
        val enq = Input(UInt(enqBits.W)))
        val deq = Valid(UInt(deqBits.W))
    })

    if (enqBits == deqBits) {
        io.deq.bits := io.enq
        io.deq.valid := true.B
    } else {
        assert(deqBits > enqBits, "Cannot have more deqBits than enqBits for the Decoder")
        io.deq.bits := 0.U
        io.deq.valid := false.B
    }
}

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
def lcm(a: Int, b: Int): Int = a*b / gcd(a, b)
