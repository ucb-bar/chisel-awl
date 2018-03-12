package hbwif2

import chisel3._
import chisel3.util._

class PatternMemDebugIO(width: Int) extends DebugIO(width) {
    val snapshotEn         = Input(Bool())
    val snapshotOut        = Output(Bool())
}

// TODO have a parameter to use a SeqMem vs. registers (and do the smart thing with a TLController)

