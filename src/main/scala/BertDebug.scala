package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset
import freechips.rocketchip.util.AsyncResetShiftReg

case class BertConfig(
    bertSampleCounterWidth: Int = 32,
    bertErrorCounterWidth: Int = 32
)

object BertConfig {

    def fromBER(c: SerDesConfig, minBER: Double): BertConfig = {
        val maxSamples = BigInt((math.ceil(1.0/minBER) / c.dataWidth).toLong)
        val maxErrors = maxSamples * c.dataWidth
        BertConfig(log2Ceil(maxSamples), log2Ceil(maxErrors))
    }

}

class BertDebug()(implicit c: SerDesConfig, implicit val b: BertConfig) extends Debug()(c) with HasPRBS {

    require((c.dataWidth % c.numWays) == 0)

    override val controlIO = Some(IO(new ControlBundle {
        val enable         = input(Bool(), 0, "bert_enable", TxClock)
        val clear          = input(Bool(), 1, "bert_clear", RxClock)
        val prbsLoad       = input(UInt(prbs.map(_._1).max.W), "bert_prbs_load") // Not resynchronized, load this value while disabled
        val prbsModeTx     = input(UInt(2.W), "bert_prbs_mode_tx", TxClock)
        val prbsModeRx     = input(UInt(2.W), "bert_prbs_mode_rx", RxClock)
        val prbsSelect     = input(UInt(log2Ceil(prbs.length).W), "bert_prbs_select") // Not resynchronized, load this value while disabled
        val prbsSeedGoods  = output(UInt(c.numWays.W), "bert_prbs_seed_goods", TxClock)
        val sampleCount    = input(UInt(b.bertSampleCounterWidth.W), "bert_sample_count", RxClock)
        val sampleCountOut = output(UInt(b.bertSampleCounterWidth.W), "bert_sample_count_out", RxClock)
        val errorCounts    = output(Vec(c.numWays, UInt(b.bertErrorCounterWidth.W)), "bert_error_counts", RxClock)
        val berMode        = input(Bool(), "bert_ber_mode", RxClock)
    }))
    val ctrl = controlIO.get

    withClockAndReset(io.txClock, io.txReset) {
        val prbsModulesTx = prbs().map(PRBS(_, c.dataWidth))
        prbsModulesTx.foreach { x => x.suggestName(s"tx_PRBS${x.prbsWidth}") }

        io.txOut.bits := Mux(ctrl.enable, MuxLookup(ctrl.prbsSelect, 0.U, prbsModulesTx.zipWithIndex.map { x => (x._2.U, x._1.io.out.asUInt) }), io.txIn.bits)
        io.txIn.ready := Mux(ctrl.enable, false.B, io.txOut.ready)

        prbsModulesTx.foreach { p =>
            p.io.seed := 1.U
            p.io.load := ctrl.prbsLoad
            p.io.mode := Mux(io.txOut.ready, ctrl.prbsModeTx, PRBS.sStop)
        }
    }

    val enableSync = withClockAndReset(io.rxClock, io.rxReset) { AsyncResetShiftReg(ctrl.enable, 3, 0) }

    // These clock crossings are unsafe, but most of these signals will be constant when sampled, so we won't worry about them for now
    withClockAndReset(io.rxClock, io.rxReset) {
        val prbsModulesRx = Seq.fill(c.numWays) { prbs().map(PRBS(_, c.dataWidth/c.numWays)) }
        prbsModulesRx.zipWithIndex.foreach { x => x._1.foreach { y => y.suggestName(s"rx_PRBS${y.prbsWidth}_way${x._2}") } }
        val errorCounts = RegInit(VecInit(Seq.fill(c.numWays) {0.U(b.bertErrorCounterWidth.W)}))
        val sampleCount = RegInit(0.U(b.bertSampleCounterWidth.W))
        val wayData = (0 until c.numWays).map { w => (0 until c.dataWidth/c.numWays).map({ i => io.rxIn.bits(i*c.numWays + w).asUInt }).reverse.reduce(Cat(_,_)) }

        ctrl.sampleCountOut := sampleCount
        val done = ctrl.sampleCount === ctrl.sampleCountOut

        ctrl.prbsSeedGoods := prbsModulesRx.map({ m => MuxLookup(ctrl.prbsSelect, false.B, m.zipWithIndex.map { x => (x._2.U, x._1.io.seedGood) }) }).map(_.asUInt).reduce(Cat(_,_))
        ctrl.errorCounts := errorCounts

        val prbsRxData = prbsModulesRx.zip(wayData).map { case (w, d) =>
            w.foreach { p =>
                p.io.seed := d
                p.io.load := ctrl.prbsLoad
                p.io.mode := Mux(io.rxIn.valid, ctrl.prbsModeRx, PRBS.sStop)
            }
            MuxLookup(ctrl.prbsSelect, 0.U, w.zipWithIndex.map { x => (x._2.U, x._1.io.out ) })
        }

        val wayErrors = prbsRxData.zip(wayData).map { case (p, d) => PopCount(Mux(ctrl.berMode, p ^ d, d)) }

        io.rxOut <> io.rxIn

        when (ctrl.clear) {
            sampleCount := 0.U
            errorCounts.foreach(_ := 0.U)
        } .otherwise {
            when (!done && enableSync && io.rxIn.valid) {
                sampleCount := sampleCount + 1.U
                errorCounts.zip(wayErrors).foreach { case (e, w) => e := e + w }
            }
        }
    }

}

object PRBS {

    val sLoad :: sSeed :: sRun :: sStop :: Nil = Enum(4)

    def apply(prbs: (Int, Int), parallel: Int): PRBS = Module(new PRBS(prbs._1, prbs._2, parallel))
}

class PRBS(val prbsWidth: Int, polynomial: Int, parallel: Int) extends Module {

    val io = IO(new Bundle {
        val out = Output(UInt(parallel.W))
        val seedGood = Output(Bool())
        val seed = Input(UInt(parallel.W))
        val load = Input(UInt(prbsWidth.W))
        val mode = Input(UInt(2.W))
    })

    val lfsr = RegInit(1.U(prbsWidth.W))

    val out = Wire(Vec(parallel, Bool()))

    val nextLfsr = (0 until parallel).foldRight(lfsr)({ (i, stage) =>
        val nextBit = (stage & polynomial.U).xorR
        out(i) := nextBit
        Cat(stage(prbsWidth-2,0), nextBit)
    })

    io.out := out.asUInt
    io.seedGood := lfsr.orR

    switch (io.mode) {
        is (PRBS.sLoad) {
            lfsr := io.load
        }
        is (PRBS.sSeed) {
            if (parallel >= prbsWidth) {
                lfsr := io.seed(prbsWidth-1, 0)
            } else {
                lfsr := Cat(lfsr(prbsWidth-parallel-1,0), io.seed)
            }
        }
        is (PRBS.sRun) {
            lfsr := nextLfsr
        }
    }

}

trait HasPRBS {
    this: BertDebug =>
    // each entry is a tuple containing (width, polynomial)
    def prbs(): Seq[(Int, Int)] = Seq()
    require(prbs.length > 0, "must override prbs!")
}

trait HasPRBS7 extends HasPRBS {
    this: BertDebug =>
    abstract override def prbs() = super.prbs() ++ Seq((7, 0x60))
}

trait HasPRBS15 extends HasPRBS {
    this: BertDebug =>
    abstract override def prbs() = super.prbs() ++ Seq((15, 0x6000))
}

trait HasPRBS31 extends HasPRBS {
    this: BertDebug =>
    abstract override def prbs() = super.prbs() ++ Seq((31, 0x48000000))
}

trait HasAllPRBS extends HasPRBS7 with HasPRBS15 with HasPRBS31 { this: BertDebug => }

trait HasBertDebug extends HasDebug {
    this: Lane =>
    implicit val c: SerDesConfig
    implicit val b: BertConfig
    abstract override def genDebug() = super.genDebug() ++ Seq(Module(new BertDebug()(c, b) with HasAllPRBS))
}
