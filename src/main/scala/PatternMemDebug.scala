package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset

case class PatternMemConfig(
    val patternDepth: Int = 16,
    val snapshotDepth: Int = 16
)

class PatternMemDebugIO()(implicit c: SerDesConfig, implicit val m: PatternMemConfig) extends DebugIO {
    val patternEnable  = Input(Bool())
    val snapshotEnable = Input(Bool())
    val pattern        = Input(Vec(m.patternDepth, UInt(c.dataWidth.W)))
    val snapshot       = Output(Vec(m.snapshotDepth, UInt(c.dataWidth.W)))
    val snapshotValid  = Output(Bool())
}

class PatternMemDebug()(implicit c: SerDesConfig, implicit val m: PatternMemConfig) extends Debug {

    val io = IO(new PatternMemDebugIO)


    val snapshotValid   = withClockAndReset(io.rxClock, io.rxReset) { RegInit(false.B) }
    val snapshotData    = Reg(Vec(m.snapshotDepth, UInt(c.dataWidth.W)))
    val snapshotCounter = withClockAndReset(io.rxClock, io.rxReset) { Counter(m.snapshotDepth) }
    val patternCounter  = withClockAndReset(io.txClock, io.txReset) { Counter(m.patternDepth) }

    io.snapshotValid := snapshotValid
    io.snapshot := snapshotData

    withClockAndReset(io.rxClock, io.rxReset) {
        when (io.snapshotEnable) {
            when (!snapshotValid && io.rxIn.valid) {
                snapshotValid := snapshotCounter.inc()
                snapshotData(snapshotCounter.value) := io.rxIn.bits
            }
        } .otherwise {
            snapshotValid := false.B
            snapshotCounter.value := 0.U
        }
    }

    withClockAndReset(io.txClock, io.txReset) {
        when (io.patternEnable) {
            when (io.txOut.ready) {
                patternCounter.inc()
            }
        } .otherwise {
            patternCounter.value := 0.U
        }
    }

    io.txOut.bits := Mux(io.patternEnable, io.pattern(patternCounter.value), io.txIn.bits)
    io.txIn.ready := Mux(io.patternEnable, false.B, io.txOut.ready)

    io.rxOut.bits := io.rxIn.bits
    io.rxOut.valid := io.rxIn.valid

    def connectController(builder: ControllerBuilder) {
        builder.w("pattern_mem_snapshot_en", io.snapshotEnable, 0)
        builder.w("pattern_mem_pattern_en", io.patternEnable, 0)
        builder.r("pattern_mem_snapshot", io.snapshot)
        builder.r("pattern_mem_snapshot_valid", io.snapshotValid)
        builder.w("pattern_mem_pattern", io.pattern)
    }
}

trait HasPatternMemDebug extends HasDebug {
    this: Lane =>
    implicit val c: SerDesConfig
    implicit val m: PatternMemConfig
    abstract override def genDebug() = Seq(Module(new PatternMemDebug()(c, m))) ++ super.genDebug()
}

