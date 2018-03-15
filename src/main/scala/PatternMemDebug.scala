package hbwif2

import chisel3._
import chisel3.util._
/*
case class PatternMemConfig(
    val patternDepth: Int = 16,
    val snapshotDepth: Int = 16,
    val useSeqMem: Boolean = true
)

class PatternMemDebugIO()(implicit c: SerDesConfig, implicit val m: PatternMemConfig) extends DebugIO {
    val patternEnable  = Input(Bool())
    val snapshotEnable = Input(Bool())
    val pattern        = Input(Vec((if (m.useSeqMem) 1 else m.patternDepth), UInt(c.dataWidth.W)))
    val snapshot       = Output(Vec((if (m.useSeqMem) 1 else m.snapshotDepth), UInt(c.dataWidth.W)))
    val patternWrEn    = if (m.useSeqMem) Some(Input(Bool())) else None
    val patternWrAddr  = if (m.useSeqMem) Some(Input(UInt(log2Ceil(m.patternDepth).W))) else None
    val snapshotRdAddr = if (m.useSeqMem) Some(Input(UInt(log2Ceil(m.snapshotDepth).W))) else None
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

    if (m.useSeqMem) {
        io.snapshot(0) := io.rxIn
        io.snapshotWrAddr.get := snapshotCounter.value
        io.snapshotWrEn.get := io.snapshotEnable && !finished
        patternOut := io.pattern(0)
        io.patternRdAddr.get := patternCounter.value
    } else {
        val snapshotData = Reg(Vec(m.snapshotDepth, UInt(c.dataWidth.W)))
        io.snapshot := snapshotData
        when (io.snapshotEnable && !finished) {
            snapshotData(snapshotCounter.value)
        }
        patternOut := io.pattern(patternCounter.value)
    }

    io.txOut := Mux(io.patternEnable, patternOut, io.txIn)

    def connectController(builder: ControllerBuilder) {
        // Ideally we figure this out automatically but for now this works
        if (m.useSeqMem) {
            // Instead of wiring this to registers, add it to the memory map
            //b.rSeqMem("snapshot", io.snapshot(0), m.snapshotDepth)
            //b.wSeqMem("pattern", io.pattern(0), m.patternDepth)
            ???
        } else {
            (0 until m.snapshotDepth).foreach { i =>
                b.r(s"snapshot$i", io.snapshot(i))
            }
            (0 until m.patternDepth).foreach { i =>
                b.w(s"pattern$i", io.pattern(i))
            }
        }
    }
}

*/
