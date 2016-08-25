// johnwright@eecs.berkeley.edu

package splash2_hbwif

import hbwif._

import Chisel._
import ChiselError._
import cde.{Parameters, Field}
import junctions.ParameterizedBundle

case object Splash2LinkTopKey extends Field[Splash2LinkTopParameters]

case class Splash2LinkTopParameters(
  // TODO add parameter to change ddr bit ordering
  dataBits: Int = 16,
  numLanes: Int = 8,
  rxOutsPerLane: Int = 3,
  isDDR: Boolean = true,
  interfaceBufferDepth: Int = 2,
  numClocks: Int = 4)

trait HasSplash2LinkTopParameters {
  implicit val p: Parameters
  val linkTopDataBits = p(Splash2LinkTopKey).dataBits
  val linkTopNumLanes = p(Splash2LinkTopKey).numLanes
  val linkTopRxOutsPerLane = p(Splash2LinkTopKey).rxOutsPerLane
  val linkTopIsDDR = p(Splash2LinkTopKey).isDDR
  val linkTopInterfaceBufferDepth = p(Splash2LinkTopKey).interfaceBufferDepth
  val linkTopNumClocks = p(Splash2LinkTopKey).numClocks
  val linkTopDivideBy = linkTopDataBits/(if(linkTopIsDDR) 2 else 1)
}

abstract class Splash2LinkTopModule(implicit val p: Parameters) extends Module
  with HasSplash2LinkTopParameters
  with HasBertParameters
abstract class Splash2LinkTopBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasSplash2LinkTopParameters
  with HasBertParameters

class Splash2LinkTopPadIO(implicit p: Parameters) extends Splash2LinkTopBundle()(p) {
  // Analog biases
  val tx_imon          = Bool(OUTPUT)
  val tx_vmon          = Bool(OUTPUT)
  val rx_imon          = Bool(OUTPUT)
  val rx_vmon          = Bool(OUTPUT)
  val rx_vcm           = Bool(INPUT)
  val rx_iref          = Bool(INPUT)
  val rx_ioff          = Bool(INPUT)
  val tx_iref          = Bool(INPUT)

  // High speed digital
  val tx_outp          = Vec.fill(linkTopNumLanes) { Bool(OUTPUT) }
  val tx_outn          = Vec.fill(linkTopNumLanes) { Bool(OUTPUT) }
  val rx_inp           = Vec.fill(linkTopNumLanes) { Bool(INPUT) }
  val rx_inn           = Vec.fill(linkTopNumLanes) { Bool(INPUT) }

}

class Splash2LinkInterfaceIO(implicit p: Parameters) extends Splash2LinkTopBundle()(p) {
  val rx  = Vec.fill(linkTopNumLanes) { UInt(INPUT, width = linkTopDataBits) }
  val tx  = Vec.fill(linkTopNumLanes) { UInt(OUTPUT, width = linkTopDataBits) }
}

class Splash2LinkTopConfig(implicit p: Parameters) extends Splash2LinkTopBundle()(p) {
  val bert_enable      = Bool(INPUT)
  val bert_clear       = Bool(INPUT)
  val snapshot_en      = Bool(INPUT)
  val lane_select      = Bits(INPUT, width = log2Up(linkTopNumLanes))
  val prbs_load_data   = Bits(INPUT, width = 31)
  val rx_selects       = Vec.fill(linkTopNumLanes) { Bits(INPUT, width = log2Up(linkTopRxOutsPerLane)) }
  val rx_prbs_mode     = Bits(INPUT, width = 2)
  val tx_prbs_mode     = Bits(INPUT, width = 2)
  val ber_mode         = Bool(INPUT)
  val shutoff_select   = Bits(INPUT, width = log2Up(bertShutoffPoints))
  val tx_sources       = Vec.fill(linkTopNumLanes) { Bool(INPUT) }
  val prbs_select      = Bits(INPUT, width = 2)
  val edge_selects     = Vec.fill(linkTopNumLanes) { Bool(INPUT) }
  val rx_xors          = Vec.fill(linkTopNumLanes) { Bool(INPUT) }
  val rx_flips         = Vec.fill(linkTopNumLanes) { Bool(INPUT) }
  val tx_flips         = Vec.fill(linkTopNumLanes) { Bool(INPUT) }

  val seed_good        = Bool(OUTPUT)
  val error_counts     = Vec.fill(if(linkTopIsDDR) 2 else 1) { UInt(OUTPUT, width = bertCountWidth) }
  val bit_count        = UInt(OUTPUT, width = bertCountWidth)
  val snapshot         = Bits(OUTPUT, width = bertSnapshotWidth)
}

class Splash2LinkTopIO(implicit p: Parameters) extends Splash2LinkTopBundle()(p) {
  // SERDES fast clock
  val clks             = Bool(INPUT)
  val clkd             = Bool(INPUT)
  val flicker_clk      = Bool(INPUT)

  // Internal data interface
  val link             = new Splash2LinkInterfaceIO().flip

  // Configuration Bundles
  val rx_config        = Vec.fill(linkTopNumLanes) { new TransceiverRxConfig }
  val tx_config        = Vec.fill(linkTopNumLanes) { new TransceiverTxConfig }
  val pads             = new Splash2LinkTopPadIO
  val bias_config      = new BiasConfig
  val link_config      = new Splash2LinkTopConfig

  // parallel output for feeding to the SPLASH backend (basically just to reuse the PRBS15)
  val prbs_out         = Bits(OUTPUT, width = linkTopDataBits)

  // early reset for serdes
  val reset_e          = Bool(INPUT)
}

class Splash2LinkTop(implicit p: Parameters) extends Splash2LinkTopModule()(p) {

  val io = new Splash2LinkTopIO

  // Don't support non-DDR configs yet
  if (!linkTopIsDDR) {
    ChiselError.error("LinkTop doesn't support non-DDR configs yet")
  }
  // TODO parameterize this
  if (linkTopRxOutsPerLane != 3) {
    ChiselError.error("LinkTop requires linkTopRxOutsPerLane == 3")
  }

  // bias network
  val bias = Module(new Bias)
  bias.io.rx_iref <> io.pads.rx_iref
  bias.io.rx_ioff <> io.pads.rx_ioff
  bias.io.tx_iref <> io.pads.tx_iref
  bias.io.config  <> io.bias_config
  bias.io.tx_imon <> io.pads.tx_imon
  bias.io.tx_vmon <> io.pads.tx_vmon
  bias.io.rx_imon <> io.pads.rx_imon
  bias.io.rx_vmon <> io.pads.rx_vmon
  bias.io.onebyf_clk <> io.flicker_clk

  // transceiver instances
  val transceivers = List.fill(linkTopNumLanes) { Module(new Transceiver(linkTopNumClocks)) }
  // wire clocks
  transceivers.zipWithIndex.foreach { x =>
    for(i <- 0 until linkTopNumClocks) {
      x._1.io.clks(i) := io.clks
    }
    // use _b for the last one
    if(x._2 == 7) { x._1.moduleName = "hurricane_hbwif_top_b" }
  }
  transceivers.zip(io.pads.tx_outp).map ( x => x._2 := x._1.io.tx_outp )
  transceivers.zip(io.pads.tx_outn).map ( x => x._2 := x._1.io.tx_outn )
  transceivers.zip(io.pads.rx_inp).map ( x => x._1.io.rx_inp := x._2 )
  transceivers.zip(io.pads.rx_inn).map ( x => x._1.io.rx_inn := x._2 )
  transceivers.zip(io.rx_config).map ( x => x._1.io.rx_config := x._2 )
  transceivers.zip(io.tx_config).map ( x => x._1.io.tx_config := x._2 )
  transceivers.foreach ( _.io.rx_vcm := io.pads.rx_vcm )
  // TODO parameterize this
  val serializer_outs = Vec.fill(linkTopNumLanes) { UInt(width = if(linkTopIsDDR) 2 else 1) }
  transceivers.zip(serializer_outs).map ( x => x._1.io.tx_in := x._2 )
  val raw_rx_data = Vec.fill(linkTopNumLanes) { Vec.fill(linkTopRxOutsPerLane) { Bits(INPUT, width = if(linkTopIsDDR) 2 else 1) } }
  transceivers.zip(raw_rx_data).foreach { x =>
    x._2(0) := x._1.io.rx_out1
    x._2(1) := x._1.io.rx_out2
    x._2(2) := x._1.io.rx_out3
  }
  transceivers.zipWithIndex.foreach { x =>
    x._1.io.rx_iref := bias.io.rx_iref_out(x._2)
    x._1.io.rx_ioff := bias.io.rx_ioff_out(x._2)
    x._1.io.tx_iref := bias.io.tx_iref_out(x._2)
  }

  // instantiate the PRBS31
  val tx_prbs31 = Module(new PRBS(prbsWidth = 31, parallelOutBits = linkTopDataBits, generatorPolynomial = 0x09))
  tx_prbs31.io.mode <> io.link_config.tx_prbs_mode
  tx_prbs31.io.load_in <> io.link_config.prbs_load_data
  tx_prbs31.io.seed_in := UInt(1)

  // instantiate the PRBS15
  val tx_prbs15 = Module(new PRBS(prbsWidth = 15, parallelOutBits = linkTopDataBits, generatorPolynomial = 0x03))
  tx_prbs15.io.mode <> io.link_config.tx_prbs_mode
  tx_prbs15.io.load_in := io.link_config.prbs_load_data(14,0)
  tx_prbs15.io.seed_in := UInt(1)

  // instantiate the PRBS7
  val tx_prbs7 = Module(new PRBS(prbsWidth = 7, parallelOutBits = linkTopDataBits, generatorPolynomial = 0x03))
  tx_prbs7.io.mode <> io.link_config.tx_prbs_mode
  tx_prbs7.io.load_in := io.link_config.prbs_load_data(6,0)
  tx_prbs7.io.seed_in := UInt(1)

  val tx_prbs = Mux(io.link_config.prbs_select(0),Mux(io.link_config.prbs_select(1),tx_prbs15.io.out,tx_prbs31.io.out),tx_prbs7.io.out)

  // delay the tx_prbs signal by a clock cycle to help with QOR- this decreases fanout of the prbs_select signal
  val tx_prbs_d = Reg(init = UInt(0, width = linkTopDataBits), next = tx_prbs)
  io.prbs_out := tx_prbs_d

  // serdes
  val serdes = List.fill(linkTopNumLanes) { Module(new SerDes(divideBy = linkTopDivideBy)) }
  val deserializer_outs = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }
  // QOR improvement
  val deserializer_outs_d = Vec.fill(linkTopInterfaceBufferDepth) { Vec.fill(linkTopNumLanes) { Reg(UInt(width = linkTopDataBits))} }
  val serializer_ins = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }
  val serializer_ins_d = Vec.fill(linkTopInterfaceBufferDepth) { Vec.fill(linkTopNumLanes) { Reg(UInt(width = linkTopDataBits))} }
  val tx_flip_mux_outs = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }

  deserializer_outs_d(0) := deserializer_outs
  serializer_ins_d(0) := tx_flip_mux_outs
  for(i <- 1 until linkTopInterfaceBufferDepth) {
    serializer_ins_d(i) := serializer_ins_d(i-1)
    deserializer_outs_d(i) := deserializer_outs_d(i-1)
  }

  ////// flip logic failsafe
  val deserializer_outs_d_flip = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }
  val serializer_ins_flip = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }

  for(j <- 0 until linkTopNumLanes) {
    var rx_flip_wire = Cat(deserializer_outs_d(linkTopInterfaceBufferDepth-1)(j)(0),deserializer_outs_d(linkTopInterfaceBufferDepth-1)(j)(1))
    var tx_flip_wire = Cat(serializer_ins(j)(0),serializer_ins(j)(1))
    for(i <- 1 until linkTopDataBits/2) {
      rx_flip_wire = Cat(deserializer_outs_d(linkTopInterfaceBufferDepth-1)(j)(2*i),deserializer_outs_d(linkTopInterfaceBufferDepth-1)(j)(2*i+1),rx_flip_wire)
      tx_flip_wire = Cat(serializer_ins(j)(2*i),serializer_ins(j)(2*i+1),tx_flip_wire)
    }
    deserializer_outs_d_flip(j) := rx_flip_wire
    serializer_ins_flip(j) := tx_flip_wire
  }
  val rx_flip_mux_outs = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }
  rx_flip_mux_outs.zipWithIndex.foreach ( x => x._1 := Mux(io.link_config.rx_flips(x._2),deserializer_outs_d_flip(x._2),deserializer_outs_d(linkTopInterfaceBufferDepth-1)(x._2)) )
  tx_flip_mux_outs.zipWithIndex.foreach ( x => x._1 := Mux(io.link_config.tx_flips(x._2),serializer_ins_flip(x._2),serializer_ins(x._2)) )
  val rx_xor_outs = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }
  rx_xor_outs.zipWithIndex.foreach ( x => x._1 := Fill(linkTopDataBits,io.link_config.rx_xors(x._2)) ^ rx_flip_mux_outs(x._2) )




  serdes.zip(io.link_config.rx_selects).foreach ( x => x._1.io.config_rx_sel := x._2 )
  serdes.zip(io.link_config.edge_selects).foreach ( x => x._1.io.config_rx_edge_sel := x._2 )
  serdes.zip(serializer_ins_d(linkTopInterfaceBufferDepth-1)).foreach ( x => x._1.io.tx_in := x._2 )
  serdes.zip(serializer_outs).foreach ( x => x._2 := x._1.io.tx_out )
  serdes.zip(deserializer_outs).foreach ( x => x._2 := x._1.io.rx_out )
  serdes.foreach ( _.io.clks := io.clks )
  val reset_vec = List.fill(linkTopNumLanes) { Module(new SoReg()) }
  reset_vec.foreach ( _.io.d := io.reset_e )
  serdes.zip(reset_vec).foreach ( x => x._1.io.reset := x._2.io.q )
  serdes.zip(raw_rx_data).foreach ( x => x._1.io.rx_in := x._2 )

  // tx mux
  for (i <- 0 until linkTopNumLanes) {
    when (io.link_config.tx_sources(i)) {
      serializer_ins(i) := io.link.tx(i)
    } .otherwise {
      serializer_ins(i) := tx_prbs_d
    }
  }

  // wide output from the deserializers to another backend
  io.link.rx := rx_xor_outs

  // instantiate BERT
  val bert = Module(new Bert)

  bert.io.enable <> io.link_config.bert_enable
  bert.io.clear <> io.link_config.bert_clear
  bert.io.snapshot_en <> io.link_config.snapshot_en
  bert.io.data_in := io.link.rx(io.link_config.lane_select)
  bert.io.prbs_load_data <> io.link_config.prbs_load_data
  bert.io.prbs_mode <> io.link_config.rx_prbs_mode
  bert.io.shutoff_select <> io.link_config.shutoff_select
  bert.io.prbs_select <> io.link_config.prbs_select
  bert.io.ber_mode <> io.link_config.ber_mode
  bert.io.seed_good <> io.link_config.seed_good
  bert.io.error_counts <> io.link_config.error_counts
  bert.io.bit_count <> io.link_config.bit_count
  bert.io.snapshot <> io.link_config.snapshot

}

// stevo
class SoReg extends BlackBox {
  val io = new Bundle {
    val d = Bool(INPUT)
    val q = Bool(OUTPUT)
  }
  val reg = Reg(next=io.d)
  io.q := reg
  moduleName = "so_reg_wrap"
}
