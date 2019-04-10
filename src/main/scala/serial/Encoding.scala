package awl.serial

import chisel3._
import chisel3.util._
import chisel3.experimental.{withClockAndReset, MultiIOModule}
import scala.math.max

// TODO get rid of this rocketchip dependency
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}

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
    def isComma: Bool
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

// io.decoded(MSB) is the LEAST recent time symbol
// io.decoded(LSB) is the MOST recent time symbol
// e.g. if we send A B C D E F G with decodedSymbolsPerCycle = 3,
// io.decoded(2) = A
// io.decoded(1) = B
// io.decoded(0) = C
//
// Similarly, this expects the encoded interface to go MSB..LSB with MSB being sent over the line first
abstract class Encoder(val decodedSymbolsPerCycle: Int) extends MultiIOModule with HasControllerConnector with HasEncoderParams {

    val io: EncoderIO[S]

    final def encodedWidth = io.encodedWidth

}

class DecoderIO[S <: DecodedSymbol](val symbolFactory: () => S, val decodedSymbolsPerCycle: Int) extends Bundle with HasEncodedWidth[S] {
    final val encoded = Flipped(Valid(UInt(encodedWidth.W)))
    final val decoded = Vec(decodedSymbolsPerCycle, Valid(symbolFactory()))
}

abstract class Decoder(val decodedSymbolsPerCycle: Int) extends MultiIOModule with HasControllerConnector with HasEncoderParams {

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
        val bufWidth = 2*enqBits - 1 // this is too much, but does it get optimized away?
        val buf = Reg(UInt(bufWidth.W))
        io.deq.bits := buf(bufWidth - 1, bufWidth - deqBits)
        io.enq.ready := false.B
        (0 until numStates).foldLeft(0) { case (empty, s) =>
            val canTake = empty + deqBits >= enqBits
            val nextEmpty = empty + deqBits - (if (canTake) enqBits else 0)
            when (io.deq.ready) {
                when (state === s.U) {
                    if (canTake) {
                        io.enq.ready := true.B
                        val filled = bufWidth - empty
                        require(filled >= deqBits, s"Should not get here, filled=$filled, deqBits=$deqBits")
                        val mask = ((BigInt(1) << (filled - deqBits)) - 1) << (bufWidth - filled + deqBits)
                        buf := ((buf << deqBits) & mask.U) | (io.enq.bits << (bufWidth - filled + deqBits - enqBits))
                    } else {
                        io.enq.ready := false.B
                        buf := buf << deqBits
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
        val bufWidth = 2*deqBits - 1 // this is too much, but does it get optimized away?
        val buf = Reg(UInt(bufWidth.W))
        io.deq.bits := buf(bufWidth - 1, bufWidth - deqBits)
        io.deq.valid := false.B
        (0 until numStates).foldLeft(enqBits) { case (filled, s) =>
            val canPut = filled >= deqBits
            val nextFilled = filled + enqBits - (if (canPut) deqBits else 0)
            when (io.enq.valid) {
                when (state === s.U) {
                    io.deq.valid := canPut.B
                    val shiftBits = if (canPut) deqBits else 0
                    val mask = ((BigInt(1) << (filled - shiftBits)) - 1) << (bufWidth - filled + shiftBits)
                    buf := ((buf << shiftBits) & mask.U) | (io.enq.bits << (bufWidth - filled + shiftBits - enqBits))
                    state := ((s+1) % numStates).U
                }
            }
            nextFilled
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
        m.valid := i.valid && !i.bits.isComma // filter out commas
        assert(m.ready || !m.valid, "Buffer overrun")
    }

    val async = Module(new AsyncQueue(Vec(decodedSymbolsPerCycle, Valid(symbolFactory())), AsyncQueueParams(8, 2, true)))
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
