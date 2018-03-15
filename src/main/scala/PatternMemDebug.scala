package hbwif2

import chisel3._
import chisel3.util._

case class PatternMemConfig(
    val patternDepth: Int = 16,
    val snapshotDepth: Int = 16,
    val patternUseSeqMem: Boolean = false,
    val snapshotUseSeqMem: Boolean = true
)

class PatternMemDebugIO()(implicit c: SerDesConfig, implicit val m: PatternMemConfig) extends DebugIO {
    val patternEnable  = Input(Bool())
    val snapshotEnable = Input(Bool())
    val pattern        = Input(Vec((if (m.patternUseSeqMem) 1 else m.patternDepth), UInt(c.dataWidth.W)))
    val snapshot       = Output(Vec((if (m.snapshotUseSeqMem) 1 else m.snapshotDepth), UInt(c.dataWidth.W)))
    val patternWrEn    = if (m.patternUseSeqMem) Some(Input(Bool())) else None
    val patternWrAddr  = if (m.patternUseSeqMem) Some(Input(UInt(log2Ceil(m.patternDepth).W))) else None
    val snapshotRdEn   = if (m.snapshotUseSeqMem) Some(Input(Bool())) else None
    val snapshotRdAddr = if (m.snapshotUseSeqMem) Some(Input(UInt(log2Ceil(m.snapshotDepth).W))) else None
}

class PatternMemDebug()(implicit c: SerDesConfig, implicit val m: PatternMemConfig) extends Debug {

    val io = IO(new PatternMemDebugIO)

    val snapshotCounter = Counter(m.snapshotDepth)
    val patternCounter = Counter(m.patternDepth)
    val finished = RegInit(false.B)

    when (io.snapshotEnable) {
        when (!finished) {
            finished := snapshotCounter.inc()
        }
    } .otherwise {
        finished := false.B
    }

    when (io.patternEnable) {
        patternCounter.inc()
    } .otherwise {
        patternCounter.value := 0.U
    }

    val patternOut = Wire(UInt(c.dataWidth.W))

    // TODO deal with rx valid and tx ready

    if (m.snapshotUseSeqMem) {
        val snapshotMem = SeqMem(m.snapshotDepth, UInt(c.dataWidth.W))
        io.snapshot(0) := snapshotMem.read(io.snapshotRdAddr.get, io.snapshotRdEn.get)
        when (io.snapshotEnable && !finished) {
            snapshotMem.write(snapshotCounter.value, io.rxIn.bits)
        }
    } else {
        val snapshotData = Reg(Vec(m.snapshotDepth, UInt(c.dataWidth.W)))
        io.snapshot := snapshotData
        when (io.snapshotEnable && !finished) {
            snapshotData(snapshotCounter.value) := io.rxIn.bits
        }
    }

    if (m.patternUseSeqMem) {
        val patternMem = SeqMem(m.patternDepth, UInt(c.dataWidth.W))
        patternOut := patternMem.read(patternCounter.value, io.patternEnable)
        when (io.patternWrEn.get) {
            patternMem.write(io.patternWrAddr.get, io.pattern(0))
        }
    } else {
        patternOut := io.pattern(patternCounter.value)
    }

    io.txOut.bits := Mux(io.patternEnable, patternOut, io.txIn.bits)
    io.txIn.ready := Mux(io.patternEnable, false.B, io.txOut.ready)
    io.rxOut <> io.rxIn

    def connectController(builder: ControllerBuilder) {
        // Ideally we figure this out automatically but for now this works
        if (m.snapshotUseSeqMem) {
            builder.rSeqMem("pattern_mem_snapshot", m.snapshotDepth, io.snapshot(0), io.snapshotRdAddr.get, io.snapshotRdEn.get)
        } else {
            builder.r(s"pattern_mem_snapshot", io.snapshot)
        }
        if (m.patternUseSeqMem) {
            builder.wSeqMem("pattern_mem_pattern", m.patternDepth, io.pattern(0), io.patternWrAddr.get, io.patternWrEn.get)
        } else {
            builder.w(s"pattern_mem_pattern", io.pattern)
        }
    }
}

trait HasPatternMemDebug extends HasDebug {
    implicit val c: SerDesConfig
    implicit val m: PatternMemConfig
    abstract override def genDebug() = Seq(new PatternMemDebug) ++ super.genDebug()
}
