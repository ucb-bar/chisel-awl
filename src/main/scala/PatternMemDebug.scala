package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset

case class PatternMemConfig(
    val patternDepth: Int = 16,
    val snapshotDepth: Int = 16
)

class PatternMemDebug()(implicit c: SerDesConfig, implicit val m: PatternMemConfig) extends Debug {

    override val controlIO = Some(IO(new ControlBundle {
        val patternEnable  = input(Bool(), 0, "pattern_mem_pattern_enable")
        val snapshotEnable = input(Bool(), 0, "pattern_mem_snapshot_enable")
        val pattern        = input(Vec(m.patternDepth, UInt(c.dataWidth.W)), "pattern_mem_pattern")
        val snapshot       = output(Vec(m.snapshotDepth, UInt(c.dataWidth.W)), "pattern_mem_snapshot")
        val snapshotValid  = output(Bool(), "pattern_mem_snapshot_valid")
    }))
    val ctrl = controlIO.get

    val snapshotValid   = withClockAndReset(io.rxClock, io.rxReset) { RegInit(false.B) }
    val snapshotData    = withClockAndReset(io.rxClock, io.rxReset) { Reg(Vec(m.snapshotDepth, UInt(c.dataWidth.W))) }
    val snapshotCounter = withClockAndReset(io.rxClock, io.rxReset) { Counter(m.snapshotDepth) }
    val patternCounter  = withClockAndReset(io.txClock, io.txReset) { Counter(m.patternDepth) }

    ctrl.snapshotValid := snapshotValid
    ctrl.snapshot := snapshotData

    withClockAndReset(io.rxClock, io.rxReset) {
        when (ctrl.snapshotEnable) {
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
        when (ctrl.patternEnable) {
            when (io.txOut.ready) {
                patternCounter.inc()
            }
        } .otherwise {
            patternCounter.value := 0.U
        }
    }

    io.txOut.bits := Mux(ctrl.patternEnable, ctrl.pattern(patternCounter.value), io.txIn.bits)
    io.txIn.ready := Mux(ctrl.patternEnable, false.B, io.txOut.ready)

    io.rxOut.bits := io.rxIn.bits
    io.rxOut.valid := io.rxIn.valid

}

trait HasPatternMemDebug extends HasDebug {
    this: Lane =>
    implicit val c: SerDesConfig
    implicit val m: PatternMemConfig
    abstract override def genDebug() = Seq(Module(new PatternMemDebug()(c, m))) ++ super.genDebug()
}

