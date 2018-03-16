package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset
import scala.math.max

// TODO get rid of this rocketchip dependency
import freechips.rocketchip.util.AsyncQueue

// rate is how many decoded symbols are there per encoded symbol
abstract class DecodedSymbol(val decodedWidth: Int, val encodedWidth: Int, val rate: Int) extends Bundle {

    final val bits = Output(UInt(decodedWidth.W))

    // Comparison
    def ===(other: DecodedSymbol): Bool = {
        if (this.decodedWidth == other.decodedWidth) {
            this.toBits === other.toBits
        } else {
            false.B
        }
    }
    def =/=(other: DecodedSymbol): Bool = !(this === other)

    // Define a minimum set of control symbols that must be implemented
    def comma: DecodedSymbol
    def ack: DecodedSymbol
    def nack: DecodedSymbol
    def sync: DecodedSymbol

    // How to convert data to/from (NOT the encoding, how do we convert this type to a data UInt)
    def fromData(d: UInt): DecodedSymbol
    def isData: Bool
}

trait HasEncoderParams {

    type S <: DecodedSymbol
    def symbolFactory(): S
    val decodedSymbolsPerCycle: Int

    require(decodedSymbolsPerCycle >= 1, "Cannot have 0- or negative-width Encoder/Decoder")

}

trait HasEncodedWidth[S <: DecodedSymbol] {
    val decodedSymbolsPerCycle: Int
    val symbolFactory: () => S
    final val encodedWidth = max(1, decodedSymbolsPerCycle / symbolFactory().rate) * symbolFactory().encodedWidth
}

class EncoderIO[S <: DecodedSymbol](val symbolFactory: () => S, val decodedSymbolsPerCycle: Int) extends Bundle with HasEncodedWidth[S] {
    final val encoded = Ready(UInt(encodedWidth.W))
    final val decoded = Vec(decodedSymbolsPerCycle, Flipped(Valid(symbolFactory())))
    final val decodedReady = Output(Bool())
}

abstract class Encoder(val decodedSymbolsPerCycle: Int) extends Module with HasControllerConnector with HasEncoderParams {

    val io: EncoderIO[S]

    final def encodedWidth = io.encodedWidth

}

class DecoderIO[S <: DecodedSymbol](val symbolFactory: () => S, val decodedSymbolsPerCycle: Int) extends Bundle with HasEncodedWidth[S] {
    final val encoded = Flipped(Valid(UInt(encodedWidth.W)))
    final val decoded = Vec(decodedSymbolsPerCycle, Valid(symbolFactory()))
}

abstract class Decoder(val decodedSymbolsPerCycle: Int) extends Module with HasControllerConnector with HasEncoderParams {

    val io: DecoderIO[S]

    final def encodedWidth = io.encodedWidth

}

final class EncoderWidthAdapter(val enqBits: Int, val deqBits: Int) extends Module {

    private val numStates = LCM(enqBits, deqBits) / deqBits

    // Assume that ENQ always has valid data we can consume
    final val io = IO(new Bundle {
        val enq = Flipped(Ready(UInt(enqBits.W)))
        val deq = Ready(UInt(deqBits.W))
    })

    if (enqBits == deqBits) {
        io.deq <> io.enq
    } else {
        require(enqBits > deqBits, "Cannot have more deqBits than enqBits for the Encoder")
        val state = RegInit(0.U(log2Ceil(numStates).W))
        val buf = Reg(UInt((2*enqBits - 1).W)) // This can be reduced in some cases, but it should get optimized away
        io.deq.bits := buf(deqBits - 1, 0)
        var rem = 0
        (0 until numStates) foreach { x =>
            when (io.deq.ready) {
                when (state === x.U) {
                    rem = rem + enqBits - deqBits
                    if (rem >= deqBits) {
                        rem = rem - enqBits
                        io.enq.ready := false.B
                        buf := Cat(buf(2*enqBits-2, deqBits), buf(2*deqBits-1, deqBits))
                    } else {
                        io.enq.ready := true.B
                        buf := Cat(buf(2*enqBits-2, enqBits-1-rem-deqBits), io.enq.bits, buf(deqBits+rem-1, deqBits))

                    }
                    state := ((x+1) % numStates).U
                }
            } .otherwise {
                io.enq.ready := false.B
            }
        }
    }
}

final class DecoderWidthAdapter(val enqBits: Int, val deqBits: Int) extends Module {

    private val numStates = LCM(enqBits, deqBits) / enqBits

    final val io = IO(new Bundle {
        val enq = Flipped(Valid(UInt(enqBits.W)))
        val deq = Valid(UInt(deqBits.W))
    })

    if (enqBits == deqBits) {
        io.deq <> io.enq
    } else {
        require(deqBits > enqBits, "Cannot have more enqBits than deqBits for the Decoder")
        val state = RegInit(0.U(log2Ceil(numStates).W))
        val buf = Reg(UInt((deqBits + enqBits - 1).W)) // This can be reduced in some cases, but it should get optimized away
        when (io.enq.valid) {
            buf := Cat(buf(deqBits - 2, 0), io.enq.bits)
        }
        io.deq.bits := buf(deqBits - 1, 0)
        var filled = 0
        (0 until numStates) foreach { x =>
            when (io.enq.valid) {
                when (state === x.U) {
                    filled = filled + enqBits
                    if (filled >= deqBits) {
                        filled = filled - deqBits
                        io.deq.valid := true.B
                    } else {
                        io.deq.valid := false.B
                    }
                    state := ((x+1) % numStates).U
                }
            } .otherwise {
                io.deq.valid := false.B
            }
        }
    }
}

final class DecoderQueue[S <: DecodedSymbol](val decodedSymbolsPerCycle: Int,  val symbolFactory: () => S) extends Module {

    final val io = IO(new Bundle {
        val enqClock = Input(Clock())
        val enqReset = Input(Bool())
        val deqClock = Input(Clock())
        val deqReset = Input(Bool())
        val enq = Vec(decodedSymbolsPerCycle, Flipped(Valid(symbolFactory())))
        val deq = Vec(decodedSymbolsPerCycle, Valid(symbolFactory()))
    })

    val multiQueue = withClockAndReset(io.enqClock, io.enqReset) {
        Module(new MultiQueue(symbolFactory(), 1 << (log2Ceil(decodedSymbolsPerCycle) + 1), decodedSymbolsPerCycle, decodedSymbolsPerCycle))
    }
    multiQueue.io.enq.zip(io.enq).foreach { case (m,i) =>
        m.bits := i.bits
        m.valid := i.valid
        assert(m.ready, "Buffer overrun")
    }

    val async = Module(new AsyncQueue(Vec(decodedSymbolsPerCycle, Valid(symbolFactory())), 4, 3, true))
    async.io.enq_clock := io.enqClock
    async.io.enq_reset := io.enqReset
    async.io.deq_clock := io.deqClock
    async.io.deq_reset := io.deqReset
    async.io.enq <> multiQueue.io.deq
    async.io.deq.bits.zip(io.deq).foreach { case (a,i) =>
        i.bits := a.bits
        i.valid := a.valid && async.io.deq.valid
    }
    async.io.deq.ready := true.B



}
