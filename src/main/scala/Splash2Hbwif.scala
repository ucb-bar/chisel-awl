// johnwright@eecs.berkeley.edu

package splash2_hbwif

import hbwif._

import Chisel._
import ChiselError._
import cde.{Parameters, Field}
import junctions.ParameterizedBundle

case object Splash2HbwifKey extends Field[Splash2HbwifParameters]

case class Splash2HbwifParameters(
  val maxLatency: Int = 63,
  val maxSkew: Int = 15,
  val genPoly: Int = 0x03,
  val prbsWidth: Int = 7)

trait HasSplash2HbwifParameters extends HasSplash2LinkTopParameters with HasBertParameters {
  implicit val p: Parameters
  val splash2HbwifMaxLatency = p(Splash2HbwifKey).maxLatency
  val splash2HbwifMaxSkew = p(Splash2HbwifKey).maxSkew
  val splash2HbwifGenPoly = p(Splash2HbwifKey).genPoly
  val splash2HbwifPrbsWidth = p(Splash2HbwifKey).prbsWidth
  val splash2HbwifWordBits = 4
  val splash2HbwifDataWords = bertCountWidth*linkTopNumLanes/splash2HbwifWordBits
  val splash2HbwifXorCountWidth = log2Up((splash2HbwifMaxLatency+1)/(2*bertCountWidth))
  val splash2HbwifCoarseSkewBits = math.max(log2Up(splash2HbwifMaxSkew/bertCountWidth),0)+1
}

abstract class Splash2HbwifModule(implicit val p: Parameters) extends Module
  with HasSplash2HbwifParameters

abstract class Splash2HbwifBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasSplash2HbwifParameters


class Splash2HbwifIO(implicit p: Parameters) extends Splash2HbwifBundle()(p) {
  // high speed clock
  val clks             = Bool(INPUT)
  val clkd             = Bool(INPUT)
  val flicker_clk      = Bool(INPUT)

  // early reset for serdes
  val reset_e          = Bool(INPUT)

  // Data to SPLASH2
  val data             = Vec.fill(splash2HbwifDataWords) { UInt(OUTPUT, width = splash2HbwifWordBits) }

  // Configuration Bundles
  val rx_config        = Vec.fill(linkTopNumLanes) { new TransceiverRxConfig }
  val tx_config        = Vec.fill(linkTopNumLanes) { new TransceiverTxConfig }
  val pads             = new Splash2LinkTopPadIO
  val bias_config      = new BiasConfig
  val link_config      = new Splash2LinkTopConfig
  val adc_config       = new Splash2ADCConfig
}

class Splash2Hbwif(implicit p: Parameters) extends Splash2HbwifModule()(p) {

  val io = new Splash2HbwifIO
  val link_top = Module(new Splash2LinkTop)
  val splash2_interface = Module(new Splash2ADCInterface)

  // connect link_top
  link_top.io.clks        <> io.clks
  link_top.io.clkd        <> io.clkd
  link_top.io.reset_e     <> io.reset_e
  link_top.io.link        <> splash2_interface.io.interface.link
  link_top.io.rx_config   <> io.rx_config
  link_top.io.tx_config   <> io.tx_config
  link_top.io.bias_config <> io.bias_config
  link_top.io.link_config <> io.link_config
  link_top.io.pads        <> io.pads
  link_top.io.prbs_out    <> splash2_interface.io.prbs_in
  link_top.io.flicker_clk <> io.flicker_clk

  // connect splash2_interface
  splash2_interface.io.interface.data <> io.data
  splash2_interface.io.config <> io.adc_config

}
