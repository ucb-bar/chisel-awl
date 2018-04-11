package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset
import freechips.rocketchip.unittest._

class BitStufferTest(val mode: Int, timeout: Int = 50000) extends UnitTest(timeout) {

    this.suggestName(s"BitStufferTest_mode${mode}")

    val numModes = 4
    implicit val c = SerDesConfig()
    val decodedSymbolsPerCycle = (c.dataWidth + 9)/10

    val str = s"BitStuffer mode=$mode"
    val ss = Module(new GenericTransceiverSubsystem)

    val encoder = withClockAndReset(ss.io.txClock, ss.io.txReset) { Module(new Encoder8b10b(decodedSymbolsPerCycle, 0)) }
    val decoder = withClockAndReset(ss.io.rxClock, ss.io.rxReset) { Module(new Decoder8b10b(decodedSymbolsPerCycle)) }
    val encoderAdapter = withClockAndReset(ss.io.txClock, ss.io.txReset) { Module(new EncoderWidthAdapter(encoder.encodedWidth, c.dataWidth)) }
    val decoderAdapter = withClockAndReset(ss.io.rxClock, ss.io.rxReset) { Module(new DecoderWidthAdapter(c.dataWidth, decoder.encodedWidth)) }

    val txBitStuffer = withClockAndReset(ss.io.txClock, ss.io.txReset) { Module(new TxBitStuffer(numModes)) }
    val rxBitStuffer = withClockAndReset(ss.io.rxClock, ss.io.rxReset) { Module(new RxBitStuffer(numModes)) }

    txBitStuffer.io.mode := mode.U
    rxBitStuffer.io.mode := mode.U

    val (txPBool, txPTee) = DifferentialToBool(ss.io.tx)

    ss.io.rx <> txPTee
    ss.io.rxInvert := false.B
    ss.io.txInvert := false.B
    ss.io.clockRef := clock
    ss.io.asyncResetIn := reset.toBool

    encoderAdapter.io.enq <> encoder.io.encoded
    txBitStuffer.io.enq <> encoderAdapter.io.deq
    ss.io.data.tx <> txBitStuffer.io.deq
    decoderAdapter.io.enq <> rxBitStuffer.io.deq
    rxBitStuffer.io.enq <> ss.io.data.rx
    decoder.io.encoded <> decoderAdapter.io.deq

    val edge = txPBool ^ RegNext(txPBool)
    val safe = RegInit(true.B)
    val edgeCount = RegInit(0.U(mode.U))

    if (mode > 1) {
        when (edge) {
            edgeCount := 0.U
            safe := false.B
            //assert(safe, s"This edge happened too quickly for this mode $mode")
        } .elsewhen (edgeCount === ((1 << (mode-1)) - 1).U) {
            safe := true.B
        } .otherwise {
            edgeCount := edgeCount + 1.U
        }
    }

    withClockAndReset(ss.io.txClock, ss.io.txReset) {
        val txCount = RegInit(0.U(4.W))
        txCount.suggestName("txCount")
        val txStarted = RegInit(false.B)
        txStarted.suggestName("txStarted")

        txCount := txCount + encoder.io.decodedReady
        when (txCount === 0xf.U) {
            txStarted := true.B
        }

        encoder.io.decoded.reverse.zipWithIndex.foreach { case (x, i) =>
            x.valid := txStarted
            x.bits := Decoded8b10bSymbol(i.U + (txCount * decodedSymbolsPerCycle.U), false.B)
        }
        decoder.io.clearError := false.B
    }

    withClockAndReset (ss.io.rxClock, ss.io.rxReset) {
        val rxCount = RegInit(0.U(4.W))
        rxCount.suggestName("rxCount")
        val offset = RegInit(0.U(log2Ceil(decodedSymbolsPerCycle).W))
        offset.suggestName("offset")
        val rxStarted = RegInit(false.B)
        rxStarted.suggestName("rxStarted")
        val finished = RegInit(false.B)
        io.finished := finished

        when (rxStarted && rxCount === 0xf.U) {
            finished := true.B
        }

        rxCount := rxCount + (rxStarted && decoder.io.decoded(0).valid)
        val check = (rxCount * decodedSymbolsPerCycle.U) - offset
        check.suggestName("check")
        decoder.io.decoded.reverse.zipWithIndex.foreach { case (x, i) =>
            when (x.valid) {
                when (x.bits.bits === 0.U) {
                    offset := i.U
                    rxStarted := true.B
                    rxCount := 1.U
                }
                when (rxStarted) {
                    assert((x.bits.bits === (check + i.U)) || finished)
                }
            }
        }
    }
}


object BitStufferTests {

    val modes = List(0,1,2,3)

    def apply(timeout: Int = 50000):Seq[UnitTest] = for (x <- modes) yield Module(new BitStufferTest(x, timeout))

}
