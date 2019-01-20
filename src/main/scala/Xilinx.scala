// This is the "inverse" HBWIF (or FIWBH) that goes on the FPGA to translate serial back into TileLink
// This is specifically for the Xilinx ZC706

package hbwif

import Chisel._
import cde._
import util._
import rocketchip._
import junctions._
import uncore.tilelink._
import testchipip._

case object XilinxKey extends Field[XilinxParameters]

case class XilinxParameters(
    val gtxIPName: String = "gtx_hbwif",
    val gtxNumLanes: Int = 8,
    val gtxDataWidth: Int = 20,
    val snapshotDepth: Int = 128
)

trait HasXilinxGTXClock {
    def clockInput(dummy: Int = 0): Differential
}

trait HasXilinxGTXClockQ1 extends HasXilinxGTXClock {

    val q1_clk1_gtrefclk_pad_n_in = Bool(INPUT)
    val q1_clk1_gtrefclk_pad_p_in = Bool(INPUT)

    def clockInput(dummy: Int = 0) = {
        val out = Wire(new Differential)
        q1_clk1_gtrefclk_pad_n_in := out.n
        q1_clk1_gtrefclk_pad_p_in := out.p
        out
    }

}

trait HasDRPInterface {
    // DRP interface
    val drpaddr_in = UInt(INPUT, width=9)
    val drpdi_in   = UInt(INPUT, width=16)
    val drpdo_out  = UInt(OUTPUT, width=16)
    val drpen_in   = Bool(INPUT)
    val drprdy_out = Bool(OUTPUT)
    val drpwe_in   = Bool(INPUT)

    def connectDRP(other: HasDRPInterface) {
        this.drpaddr_in  := other.drpaddr_in
        this.drpdi_in    := other.drpdi_in
        other.drpdo_out  := this.drpdo_out
        this.drpen_in    := other.drpen_in
        other.drprdy_out := this.drprdy_out
        this.drpwe_in    := other.drpwe_in
    }
}

class DRPBundle extends Bundle with HasDRPInterface

class GTXBundle(implicit val p: Parameters) extends util.ParameterizedBundle()(p) with HasDRPInterface {
    // resetFSM done
    val tx_fsm_reset_done_out = Bool(OUTPUT)
    val rx_fsm_reset_done_out = Bool(OUTPUT)

    // Data valid (tie high)
    val data_valid_in         = Bool(INPUT)

    // Data
    val rxdata_out = UInt(OUTPUT, width=p(XilinxKey).gtxDataWidth)
    val txdata_in  = UInt(INPUT, width=p(XilinxKey).gtxDataWidth)

    // Physical TX/RX interface
    val gtxrxp_in  = Bool(INPUT)
    val gtxrxn_in  = Bool(INPUT)
    val gtxtxp_out = Bool(OUTPUT)
    val gtxtxn_out = Bool(OUTPUT)

    // Loopback mode
    val loopback_in = UInt(INPUT, width=3)

    // PLL info
    val cpllfbclklost_out     = Bool(OUTPUT)
    val cplllock_out          = Bool(OUTPUT)
    val cpllreset_in          = Bool(INPUT)

    // Output clocks
    val txusrclk_out  = Clock(OUTPUT)
    val txusrclk2_out = Clock(OUTPUT)
    val rxusrclk_out  = Clock(OUTPUT)
    val rxusrclk2_out = Clock(OUTPUT) // We only use this one

    // Monitor port (unused)
    val dmonitorout_out = Bool(OUTPUT)

    // RX ready
    val rxuserrdy_in = Bool(INPUT)

    // RX CDR hold
    val rxcdrhold_in = Bool(INPUT)

    // Eye monitor (unused)
    val eyescanreset_in      = Bool(INPUT)
    val eyescandataerror_out = Bool(OUTPUT)
    val eyescantrigger_in    = Bool(INPUT)

    // RX misc stuff
    val rxdfelpmreset_in   = Bool(INPUT)
    val rxmonitorout_out   = Bool(OUTPUT)
    val rxmonitorsel_in    = UInt(INPUT, width=2)
    val rxoutclkfabric_out = Bool(OUTPUT)

    // RX polarity
    val rxpolarity_in = Bool(INPUT)

    // RX PRBS checker
    val rxprbserr_out     = Bool(OUTPUT)
    val rxprbssel_in      = UInt(INPUT, width=3)
    val rxprbscntreset_in = Bool(INPUT)

    // RX initialization and reset
    val gtrxreset_in    = Bool(INPUT)
    val rxpmareset_in   = Bool(INPUT)
    val rxresetdone_out = Bool(OUTPUT)

    // TX driver configuration
    val txpostcursor_in = UInt(INPUT, width=5)
    val txprecursor_in  = UInt(INPUT, width=5)
    val txelecidle_in   = Bool(INPUT)
    val txdiffctrl_in   = UInt(INPUT, width=4)
    val txinhibit_in    = Bool(INPUT)
    val txpolarity_in   = Bool(INPUT)

    // TX initialization and reset
    val gttxreset_in    = Bool(INPUT)
    val txuserrdy_in    = Bool(INPUT)
    val txresetdone_out = Bool(OUTPUT)

    // TX misc stuff
    val txoutclkfabric_out = Bool(OUTPUT)
    val txoutclkpcs_out    = Bool(OUTPUT)

    def rx(dummy: Int = 0): Differential = {
        val d = Wire(new Differential)
        this.gtxrxp_in := d.p
        this.gtxrxn_in := d.n
        d
    }

    def tx(dummy: Int = 0): Differential = {
        val d = Wire(new Differential)
        d.p := this.gtxtxp_out
        d.n := this.gtxtxn_out
        d
    }
}

trait HasXilinxGTX extends HasXilinxGTXClock {
    def toSeq(dummy: Int = 0): Seq[GTXBundle]
    def rxData(dummy: Int = 0): Seq[UInt]
    def txData(dummy: Int = 0): Seq[UInt]
}

trait HasXilinxGTX8Lanes extends HasXilinxGTX {
    implicit val p: Parameters

    require(p(XilinxKey).gtxNumLanes == 8)

    val gt0 = new GTXBundle
    val gt1 = new GTXBundle
    val gt2 = new GTXBundle
    val gt3 = new GTXBundle
    val gt4 = new GTXBundle
    val gt5 = new GTXBundle
    val gt6 = new GTXBundle
    val gt7 = new GTXBundle

    def toSeq(dummy: Int = 0) = Seq(gt0, gt1, gt2, gt3, gt4, gt5, gt6, gt7)
    def rxData(dummy: Int = 0) = this.toSeq().map { _.rxdata_out }
    def txData(dummy: Int = 0) = this.toSeq().map { _.txdata_in }

}

abstract class XilinxTransceiverIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
    with HasXilinxGTX8Lanes {
    val soft_reset_tx_in = Bool(INPUT)
    val soft_reset_rx_in = Bool(INPUT)

    val dont_reset_on_data_error_in = Bool(INPUT)

    val sysclk_in = Clock(INPUT)
}

class XilinxTransceiverIO8Lanes(implicit p: Parameters) extends XilinxTransceiverIO()(p)
    with HasXilinxGTX8Lanes
    with HasXilinxGTXClockQ1

class XilinxTransceiver(implicit val p: Parameters) extends BlackBox {

    val io: XilinxTransceiverIO = new XilinxTransceiverIO8Lanes

    override def desiredName = p(XilinxKey).gtxIPName

    def connectJunk() {
        io.toSeq().foreach { lane =>
            lane.data_valid_in     := Bool(true)
            lane.eyescanreset_in   := Bool(false)
            lane.eyescantrigger_in := Bool(false)
            lane.rxdfelpmreset_in  := Bool(false)
            lane.rxmonitorsel_in   := UInt(2)
            lane.gtrxreset_in      := Bool(false)
            lane.rxpmareset_in     := Bool(false)
            lane.gttxreset_in      := Bool(false)
            lane.rxprbssel_in      := UInt(0)
            lane.rxprbscntreset_in := Bool(false)
        }
        io.dont_reset_on_data_error_in := Bool(true)
    }

}

class XilinxFiwbhIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  require(hbwifNumLanes == p(XilinxKey).gtxNumLanes)

  // high speed clock input
  val refClk = (new Differential).flip

  // RX Pad inputs
  val rx = Vec(hbwifNumLanes, new Differential).flip

  // TX pad outputs
  val tx = Vec(hbwifNumLanes, new Differential)

  // TileLink port for memory
  val mem = Vec(hbwifNumLanes, new ClientUncachedTileLinkIO()(memParams))

  // DRP port for programming the transceiver
  val drp = Vec(hbwifNumLanes, new DRPBundle)

  // Top-level IO
  val tx_soft_reset = Bool(INPUT)
  val rx_soft_reset = Bool(INPUT)
  val loopback_mode = UInt(INPUT, width=3)

  // Per-lane IO
  val tx_fsm_reset_done = Vec(hbwifNumLanes, Bool(OUTPUT))
  val rx_fsm_reset_done = Vec(hbwifNumLanes, Bool(OUTPUT))
  val cpll_lock         = Vec(hbwifNumLanes, Bool(OUTPUT))
  val cpll_reset        = Vec(hbwifNumLanes, Bool(OUTPUT)).flip
  val rx_cdr_hold       = Vec(hbwifNumLanes, Bool(OUTPUT)).flip
  val rx_polarity       = Vec(hbwifNumLanes, Bool(OUTPUT)).flip
  val tx_postcursor     = Vec(hbwifNumLanes, UInt(OUTPUT, width=5)).flip
  val tx_precursor      = Vec(hbwifNumLanes, UInt(OUTPUT, width=5)).flip
  val tx_elec_idle      = Vec(hbwifNumLanes, Bool(OUTPUT)).flip
  val tx_diff_ctrl      = Vec(hbwifNumLanes, UInt(OUTPUT, width=4)).flip
  val tx_inhibit        = Vec(hbwifNumLanes, Bool(OUTPUT)).flip
  val tx_polarity       = Vec(hbwifNumLanes, Bool(OUTPUT)).flip

  val soft_loopback     = Vec(hbwifNumLanes, Bool(OUTPUT)).flip
  val snapshot_en       = Vec(hbwifNumLanes, Bool(OUTPUT)).flip
  val snapshot          = Vec(hbwifNumLanes, UInt(OUTPUT, width=p(XilinxKey).snapshotDepth))
}

class XilinxFiwbh(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new XilinxFiwbhIO

  val gtx = Module(new XilinxTransceiver)

  val q = p.alterPartial({ case TransceiverKey =>
    TransceiverParameters(fpgaName = "gtx_hbwif",
    numIrefs = 0,
    divideBy = 10,
    isDDR = true)
  })

  val lanes = gtx.io.toSeq().map { g =>
    val m = Module(new XilinxFiwbhLane()(q))
    m.io.slowClk := g.rxusrclk2_out
    m.io.transceiverReset := reset // TODO synchronize me
    m
  }

  gtx.connectJunk()
  gtx.io.sysclk_in := this.clock

  require(hbwifNumLanes == p(XilinxKey).gtxNumLanes)

  gtx.io.clockInput() := io.refClk
  gtx.io.soft_reset_tx_in := io.tx_soft_reset
  gtx.io.soft_reset_rx_in := io.rx_soft_reset
  gtx.io.toSeq().zipWithIndex.foreach { case (g, i) =>
    // globals
    g.loopback_in := io.loopback_mode

    // per-lane
    io.tx_fsm_reset_done(i) := g.tx_fsm_reset_done_out
    io.rx_fsm_reset_done(i) := g.rx_fsm_reset_done_out
    io.cpll_lock(i)         := g.cplllock_out
    g.cpllreset_in    := io.cpll_reset(i)
    g.rxcdrhold_in    := io.rx_cdr_hold(i)
    g.rxpolarity_in   := io.rx_polarity(i)
    g.txpostcursor_in := io.tx_postcursor(i)
    g.txprecursor_in  := io.tx_precursor(i)
    g.txelecidle_in   := io.tx_elec_idle(i)
    g.txdiffctrl_in   := io.tx_diff_ctrl(i)
    g.txinhibit_in    := io.tx_inhibit(i)
    g.txpolarity_in   := io.tx_polarity(i)

    // other wiring
    g.rxuserrdy_in := g.cplllock_out
    g.txuserrdy_in := g.cplllock_out
  }

  gtx.io.toSeq().map(_.rx()).zip(io.rx).foreach { case (g, top) => g <> top }
  gtx.io.toSeq().map(_.tx()).zip(io.tx).foreach { case (g, top) => top <> g }
  gtx.io.toSeq().zip(io.drp).foreach { case (g, drp) => g.connectDRP(drp) }

  lanes.map(_.io.rxData).zip(gtx.io.rxData()).foreach { case (lane, top) => lane <> top }
  lanes.map(_.io.txData).zip(gtx.io.txData()).foreach { case (lane, top) => top <> lane }
  lanes.map(_.io.mem).zip(io.mem).foreach { case (lane, top) => top <> lane }

  lanes.map(_.io.snapshotEn).zip(io.snapshot_en).foreach { case (lane, top) => lane <> top }
  lanes.map(_.io.loopbackEn).zip(io.soft_loopback).foreach { case (lane, top) => lane <> top }
  lanes.map(_.io.snapshot).zip(io.snapshot).foreach { case (lane, top) => top <> lane }

}

class XilinxFiwbhLaneIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  // low speed clock input
  val slowClk = Clock(INPUT)

  // reset from transceiver
  val transceiverReset = Bool(INPUT)

  // RX Pad inputs
  val rxData = UInt(INPUT, width=p(XilinxKey).gtxDataWidth)

  // TX pad outputs
  val txData = UInt(OUTPUT, width=p(XilinxKey).gtxDataWidth)

  // TileLink port for memory
  val mem = new ClientUncachedTileLinkIO()(memParams)

  // Snapshot stuff
  val snapshot = UInt(OUTPUT, width=p(XilinxKey).snapshotDepth)
  val snapshotEn = Bool(INPUT)

  // Soft loopback
  val loopbackEn = Bool(INPUT)

  // TX data select
  // TODO

}

class XilinxFiwbhLane(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  require(transceiverDataWidth == 20)
  require(transceiverDivideBy == 10)
  require(transceiverIsDDR)

  val io = new XilinxFiwbhLaneIO

  val syncReset = io.transceiverReset

  val backend = Module(new FiwbhLaneBackend(io.slowClk, syncReset)(memParams))

  backend.io.loopback := io.loopbackEn
  backend.io.transceiverData.rx := io.rxData
  io.txData := backend.io.transceiverData.tx

  val snapshot = Module(new XilinxSnapshotModule(backend.clock, backend.reset))
  io.snapshot := snapshot.io.dataOut
  snapshot.io.dataIn := backend.io.transceiverData.rx
  snapshot.io.en := io.snapshotEn

  io.mem <> AsyncUTileLinkFrom(backend.clock, backend.reset, backend.io.mem, depth = 4)

}

class XilinxSnapshotModule(c: Clock, r: Bool)(implicit val p: Parameters) extends Module(_clock = c, _reset = r) {

    val snapshotDepth = p(XilinxKey).snapshotDepth

    val io = new Bundle {
        val dataOut = UInt(OUTPUT, width=snapshotDepth)
        val dataIn = UInt(INPUT, width=p(XilinxKey).gtxDataWidth)
        val en = Bool(INPUT)
    }

    val enSync = Reg(next = Reg(next = io.en))

    val regData = Reg(UInt(width=snapshotDepth))

    when (enSync) {
        regData := (regData << p(XilinxKey).gtxDataWidth) | io.dataIn
    }

}
