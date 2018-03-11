package hbwif2

import chisel3._
import chisel3.util._

class SnapshotDebugIO(width: Int) extends DebugIO(width) {
    val snapshotEn         = Input(Bool())
}
