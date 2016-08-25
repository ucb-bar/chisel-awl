// johnwright@eecs.berkeley.edu
//
// This is a black box wrapping a hand-synthesized clock divider
// using only clock network cells in the clock path

package hbwif

import Chisel._
import ChiselError._

class ClockDivider5IO extends Bundle {
  val clk_in  = Bool(INPUT)
  val clk_out = Bool(OUTPUT)
}

class ClockDivider5 extends BlackBox {
  val io = new ClockDivider5IO

  moduleName = "hurricane_hbwif_clk_div5"
}
