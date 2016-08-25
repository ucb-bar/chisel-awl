// johnwright@eecs.berkeley.edu

package hurricane_hbwif
import hbwif._

import Chisel._
import ChiselError._
import cde.{Parameters, Field}
import junctions._
import uncore.HasHtifParameters
import Serial._

case object HurricaneHbwifKey extends Field[HurricaneHbwifParameters]

case class HurricaneHbwifParameters(
  val numLanes: Int = 8,
  val bufferDepth: Int = 10, // Calculated based on worst-case transmission line delay and codec, serdes, etc. latency
  val nMemChannels: Int = 8) {
  }

trait HasHurricaneHbwifParameters extends HasBertParameters with HasNastiParameters with HasAtosParameters {
  implicit val p: Parameters
  val hbwifBufferDepth  = p(HurricaneHbwifKey).bufferDepth
  val hbwifNumLanes     = p(HurricaneHbwifKey).numLanes
  val hbwifBertDataBits = 10
  val hbwifNMemChannels  = p(HurricaneHbwifKey).nMemChannels
  // Map the memory channels to lanes and evenly distribute them for better QOR
  // e.g. 4 memory channels go to lanes 0, 2, 4, and 6
  val hbwifLaneMap = (0 until hbwifNMemChannels) map { i => ((i % hbwifNMemChannels)*hbwifNumLanes) / hbwifNMemChannels }
}

abstract class HurricaneHbwifModule(implicit val p: Parameters) extends Module
  with HasHurricaneHbwifParameters

abstract class HurricaneHbwifBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasHurricaneHbwifParameters

class HurricaneHbwifPadIO(implicit p: Parameters) extends HurricaneHbwifBundle()(p) {
  val txp = Vec(hbwifNumLanes, Bool(OUTPUT))
  val txn = Vec(hbwifNumLanes, Bool(OUTPUT))
  val rxp = Vec(hbwifNumLanes, Bool(INPUT))
  val rxn = Vec(hbwifNumLanes, Bool(INPUT))
  val tx_iref = Bool(INPUT)
  val rx_iref = Bool(INPUT)
  val rx_ioff = Bool(INPUT)
  val rx_vcm  = Bool(INPUT)
  // val rx_vmon = Bool(OUTPUT)
  // val rx_imon = Bool(OUTPUT)
}

class HurricaneHbwifIO(implicit p: Parameters) extends HurricaneHbwifBundle()(p) with HasHtifParameters {
  val mem         = Vec(hbwifNMemChannels, new NastiIO().flip)
  val pad         = new HurricaneHbwifPadIO
  val scrs        = Vec(hbwifNumLanes, new SmiIO(scrDataBits, scrAddrBits).flip)
  val flicker_clk = Bool(INPUT)
  val bias_config = new BiasConfig
  val lane_fast_clk = Vec(hbwifNumLanes, Bool(INPUT))
  // These are wired by TopGenerator.py
  val lane_slow_clk = Vec(hbwifNumLanes, Bool(INPUT))
  val lane_reset    = Vec(hbwifNumLanes, Bool(INPUT))
  val mem_en        = Bool(INPUT)
  val alignment_lock = Bool(INPUT)
}

class HurricaneHbwif(baseAddrs: Seq[BigInt])(implicit p: Parameters) extends HurricaneHbwifModule()(p) with HasHtifParameters {

  require(hbwifNumLanes <= 8, "Bias network only supports 8 or fewer lanes")
  require(hbwifNMemChannels <= hbwifNumLanes, "Cannot have more Memory channels than lanes")

  val io = new HurricaneHbwifIO

  val bias = Module(new Bias)
  bias.io.tx_iref <> io.pad.tx_iref
  bias.io.rx_iref <> io.pad.rx_iref
  bias.io.rx_ioff <> io.pad.rx_ioff
  // bias.io.rx_vmon <> io.pad.rx_vmon
  // bias.io.rx_imon <> io.pad.rx_imon
  // bias.io.tx_vmon unconnected
  // bias.io.tx_imon unconnected
  bias.io.onebyf_clk <> io.flicker_clk
  bias.io.config <> io.bias_config

  val lane = List.tabulate(hbwifNumLanes) { i => Module(new HurricaneHbwifLane(s"hbwif_lane_$i",baseAddrs(i),hbwifLaneMap contains i)) }
  val a2s = List.fill(hbwifNMemChannels) { Module(new AtosClientConverter) }

  (0 until hbwifNumLanes).foreach { i =>
    lane(i).io.bias.tx_iref := bias.io.tx_iref_out(i)
    lane(i).io.bias.rx_iref := bias.io.rx_iref_out(i)
    lane(i).io.bias.rx_ioff := bias.io.rx_ioff_out(i)
    lane(i).io.bias.rx_vcm  := io.pad.rx_vcm
    lane(i).io.clks         := io.lane_fast_clk(i)
    lane(i).io.pad.rx_inp   := io.pad.rxp(i)
    lane(i).io.pad.rx_inn   := io.pad.rxn(i)
    lane(i).reset           := io.lane_reset(i)
    lane(i).io.alignment_lock := io.alignment_lock
    if (hbwifLaneMap contains i) {
      lane(i).io.mem_en     := io.mem_en
    } else {
      lane(i).io.mem_en     := Bool(false)
    }
    io.pad.txp(i)        := lane(i).io.pad.tx_outp
    io.pad.txn(i)        := lane(i).io.pad.tx_outn



    // Connect the SCR memory port
    val scr_req_fifo  = AsyncFifoTX(new SmiReq(scrDataBits, scrAddrBits))
    val scr_resp_fifo = AsyncFifoRX(Bits(width = scrDataBits))
    lane(i).io.scr_req <> scr_req_fifo.io.to_rx
    lane(i).io.scr_resp <> scr_resp_fifo.io.from_tx
    scr_req_fifo.io.enq <> io.scrs(i).req
    scr_resp_fifo.io.deq <> io.scrs(i).resp

  }

  (0 until hbwifNMemChannels).foreach { i =>

    // Build the other half of the Async FIFOs and connect to the Nasti-to-Serial converter
    val tx_fifo = AsyncFifoTX(new AtosRequest)
    val rx_fifo = AsyncFifoRX(new AtosResponse)
    lane(hbwifLaneMap(i)).io.uncore.tx <> tx_fifo.io.to_rx
    lane(hbwifLaneMap(i)).io.uncore.rx <> rx_fifo.io.from_tx
    tx_fifo.io.enq.bits  := a2s(i).io.atos.req.bits
    tx_fifo.io.enq.valid := a2s(i).io.atos.req.valid
    a2s(i).io.atos.req.ready := tx_fifo.io.enq.ready
    rx_fifo.io.deq <> a2s(i).io.atos.resp

    io.mem(i) <> a2s(i).io.nasti
  }

}
