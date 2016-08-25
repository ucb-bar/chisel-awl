// johnwright@eecs.berkeley.edu
//
// Supports only DDR (de)serializers
// TODO parameterize number of rx slices

package hbwif

import Chisel._
import ChiselError._

class SerDes(
  val divideBy: Int = 8
) extends BlackBox {
  val io = new Bundle {
    val rx_in  = Vec.fill(3) { Bits(INPUT, width = 2) }
    val tx_in  = Bits(INPUT, width = 2*divideBy)
    val clks   = Bool(INPUT)
    val reset  = Bool(INPUT)
    val config_rx_edge_sel = Bool(INPUT)
    val config_rx_sel = Bits(INPUT, width = 2)
    val tx_out = Bits(OUTPUT, width = 2)
    val rx_out = Bits(OUTPUT, width = 2*divideBy)
  }
  // [jcw]: remove verilog parameters with hardened serdes
  setVerilogParameters("#(.NDIVBY("+divideBy+"),.WDIVBY("+log2Up(divideBy)+"))")
  moduleName = "hurricane_hbwif_serdes"
}

