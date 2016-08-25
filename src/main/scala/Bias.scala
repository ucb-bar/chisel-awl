// johnwright@eecs.berkeley.edu

package hbwif

import Chisel._
import ChiselError._

class BiasConfig extends Bundle {
  val pmos = Vec.fill(4) { Bits(INPUT, width = 3) }
  val txmon = Bits(INPUT, width = 8)
  val rxmon = Bits(INPUT, width = 8)
}

class Bias() extends BlackBox {
  val io = new Bundle {
    val rx_iref       = Bool(INPUT)
    val rx_ioff       = Bool(INPUT)
    val tx_iref       = Bool(INPUT)
    val config        = new BiasConfig
    val rx_iref_out   = Bits(OUTPUT, width = 8)
    val rx_ioff_out   = Bits(OUTPUT, width = 8)
    val tx_iref_out   = Bits(OUTPUT, width = 8)
    val tx_imon       = Bool(OUTPUT)
    val tx_vmon       = Bool(OUTPUT)
    val rx_imon       = Bool(OUTPUT)
    val rx_vmon       = Bool(OUTPUT)
    val onebyf_clk    = Bool(INPUT)
  }
  moduleName = "hurricane_hbwif_bias_top"
}
