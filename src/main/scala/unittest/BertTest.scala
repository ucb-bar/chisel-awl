package hbwif

import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import chisel3.experimental.withClockAndReset
import freechips.rocketchip.unittest._

class BertTest(delay: Int = 1, per1k: Int = 0, prbs: Int = 0, berMode: Boolean = true, numSamples: Int = 1000, timeout: Int = 50000) extends UnitTest(timeout) {

    this.suggestName(s"BertTest_delay${delay}_per1k${per1k}_prbs${prbs}_berMode${if (berMode) 1 else 0}_numSamples${numSamples}")

    implicit val b: BertConfig = BertConfig()
    implicit val c: SerDesConfig = SerDesConfig()

    val bert = Module(new BertDebug()(c, b) with HasAllPRBS)
    val txrxss = Module(new GenericTransceiverSubsystem()(c))
    val errorInjector = Module(new ErrorInjector(per1k))
    val delayLine = Module(new DifferentialDelayLine(delay))

    errorInjector.io.in <> txrxss.io.tx
    delayLine.io.in <> errorInjector.io.out
    txrxss.io.rx <> delayLine.io.out
    errorInjector.io.clock := this.clock
    delayLine.io.clock := this.clock

    txrxss.io.asyncResetIn := this.reset.toBool
    txrxss.io.clockRef := this.clock
    txrxss.io.txInvert := false.B
    txrxss.io.rxInvert := false.B

    bert.io.txClock := txrxss.io.txClock
    bert.io.txReset := txrxss.io.txReset
    bert.io.rxClock := txrxss.io.rxClock
    bert.io.rxReset := txrxss.io.rxReset
    bert.io.txIn.bits := 0.U
    txrxss.io.data.tx <> bert.io.txOut
    bert.io.rxIn <> txrxss.io.data.rx

    val sDisabled :: sClear :: sLoad :: sSeed :: sErrors :: sWait :: sCheck :: sDone :: Nil = Enum(8)


    withClockAndReset(txrxss.io.txClock, txrxss.io.txReset) {

        val state = RegInit(sDisabled)
        state.suggestName("state")
        val counter = Counter(8)
        counter.value.suggestName("counter")
        val onesCount = RegInit(0.U(64.W))
        onesCount.suggestName("onesCount")
        val margin = 2 // Account for cycles of delay between errorInjector and rxOut
        val sampleCounter = Counter(numSamples - margin)
        sampleCounter.value.suggestName("sampleCounter")

        // Set defaults
        bert.io.enable         := false.B
        bert.io.clear          := true.B
        bert.io.prbsLoad       := 1.U
        bert.io.prbsModeTx     := PRBS.sStop
        bert.io.prbsModeRx     := PRBS.sStop
        bert.io.prbsSelect     := prbs.U
        bert.io.sampleCount    := numSamples.U
        bert.io.berMode        := berMode.B
        errorInjector.io.reset := true.B
        errorInjector.io.stop  := false.B

        when (bert.io.sampleCountOut < numSamples.U && bert.io.enable && !bert.io.clear) {
            onesCount := onesCount + PopCount(bert.io.rxOut.bits)
        }

        switch (state) {
            is (sDisabled) {
                state := sClear
            }
            is (sClear) {
                bert.io.enable := true.B
                state := sLoad
            }
            is (sLoad) {
                bert.io.enable := true.B
                bert.io.prbsModeTx := PRBS.sLoad
                bert.io.prbsModeRx := PRBS.sSeed
                when (counter.inc()) {
                    state := sSeed
                }
            }
            is (sSeed) {
                bert.io.enable := true.B
                bert.io.prbsModeTx := PRBS.sRun
                bert.io.prbsModeRx := PRBS.sSeed
                when (counter.inc()) {
                    state := sErrors
                }
            }
            is (sErrors) {
                bert.io.enable := true.B
                bert.io.clear  := false.B
                errorInjector.io.reset := false.B
                bert.io.prbsModeTx := PRBS.sRun
                bert.io.prbsModeRx := PRBS.sRun
                assert(bert.io.prbsSeedGoods.reduce(_&&_))
                when (sampleCounter.inc()) {
                    state := sWait
                }
            }
            is (sWait) {
                bert.io.enable := true.B
                bert.io.clear  := false.B
                errorInjector.io.reset := false.B
                bert.io.prbsModeTx := PRBS.sRun
                bert.io.prbsModeRx := PRBS.sRun
                errorInjector.io.stop := true.B
                when (counter.inc()) {
                    state := sCheck
                }
            }
            is (sCheck) {
                bert.io.enable := true.B
                bert.io.clear  := false.B
                errorInjector.io.reset := false.B
                errorInjector.io.stop := true.B
                bert.io.prbsModeTx := PRBS.sRun
                bert.io.prbsModeRx := PRBS.sRun
                assert(bert.io.sampleCountOut === numSamples.U)
                if (berMode) {
                    assert(bert.io.errorCounts.reduce(_+_) === errorInjector.io.errors)
                } else {
                    assert(bert.io.errorCounts.reduce(_+_) === onesCount)
                }
                state := sDone
            }
            is (sDone) {
                bert.io.enable := true.B
                bert.io.clear  := false.B
                errorInjector.io.reset := false.B
                errorInjector.io.stop := true.B
            }
        }

        io.finished := state === sDone
    }

}

object BertTests {

    val per1ks     = List(0, 1, 50, 60)
    val prbss      = List(0, 1, 2)
    val numSamples = List(1000)
    val delays     = List(1, 5)

    def apply(timeout: Int = 50000):Seq[UnitTest] =
        (for (w <- delays; x <- per1ks; y <- prbss; z <- numSamples) yield Module(new BertTest(w, x, y, true, z, timeout))) ++
        (for (w <- delays; z <- numSamples) yield Module(new BertTest(w, 0, 0, false, z, timeout)))

}
