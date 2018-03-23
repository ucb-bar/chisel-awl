package hbwif

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
        val bufWidth = 2*enqBits - 1
        val buf = Reg(UInt(bufWidth.W))
        io.deq.bits := buf(deqBits - 1, 0)
        io.enq.ready := false.B
        (0 until numStates).foldLeft(0) { case (empty, s) =>
            val nextEmpty = empty + deqBits - (if ((empty + deqBits) < enqBits) 0 else enqBits)
            when (io.deq.ready) {
                when (state === s.U) {
                    if (nextEmpty < enqBits) {
                        io.enq.ready := false.B
                        buf := buf >> deqBits
                    } else {
                        io.enq.ready := true.B
                        val filled = bufWidth - empty
                        require(filled >= 0)
                        val mask = ((1 << (filled - deqBits)) - 1)
                        require(filled >= deqBits, s"Should not get here, filled=$filled, deqBits=$deqBits")
                        buf := ((buf >> deqBits) & mask.U) | (io.enq.bits << (filled - deqBits))
                    }
                    state := ((s+1) % numStates).U
                }
            }
            nextEmpty
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
        val buf = Reg(UInt((2*deqBits - 1).W)) // This can be reduced in some cases, but it should get optimized away TODO
        when (io.enq.valid) {
            buf := ((buf << enqBits) | io.enq.bits)
        }
        io.deq.bits := buf(deqBits-1, 0)
        io.deq.valid := false.B
        (0 until numStates).foldLeft(0) { case (filled, s) =>
            when (io.enq.valid) {
                when (state === s.U) {
                    if (filled >= deqBits) {
                        io.deq.valid := true.B
                        io.deq.bits := buf(filled - 1, filled - deqBits)
                    } else {
                        io.deq.valid := false.B
                    }
                    state := ((s+1) % numStates).U
                }
            }
            filled + enqBits - (if (filled < deqBits) 0 else deqBits)
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
        assert(m.ready || !m.valid, "Buffer overrun")
    }

    val async = Module(new AsyncQueue(Vec(decodedSymbolsPerCycle, Valid(symbolFactory())), 4, 3, true))
    async.io.enq_clock := io.enqClock
    async.io.enq_reset := io.enqReset
    async.io.deq_clock := io.deqClock
    async.io.deq_reset := io.deqReset
    async.io.enq.bits.zip(multiQueue.io.deq).foreach { case (a,m) =>
        a.valid := m.valid
        a.bits := m.bits
        m.ready := async.io.enq.ready
    }
    async.io.enq.valid := multiQueue.io.deq.map(_.valid).reduce(_||_)

    async.io.deq.bits.zip(io.deq).foreach { case (a,i) =>
        i.bits := a.bits
        i.valid := a.valid && async.io.deq.valid
    }
    async.io.deq.ready := true.B



}
