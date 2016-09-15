package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._

class LaneIO(implicit p: Parameters) extends HbwifBundle()(p) {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // RX pad inputs
  val rx = new Differential

  // TX pad outputs
  val tx = (new Differential).flip

  // TileLink port for memory
  val mem = (new ClientUncachedTileLinkIO()(outermostParams)).flip

  // Configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(outermostMMIOParams)).flip

}

class Lane(implicit val p: Parameters) extends Module {

  val io = new LaneIO

  // Transceiver
  val transceiver = Module(new Transceiver)

  // Lane Backend
  val backend = Module(new LaneBackend(transceiver.io.slowClk))

  transceiver.io.data    <> backend.io.transceiverData
  transceiver.io.rx      <> io.rx
  transceiver.io.fastClk <> io.fastClk
  io.tx                  <> transceiver.io.tx

  backend.io.mem <> io.mem
  backend.io.scr <> io.scr

  if (!(p(TransceiverKey).extraInputs.isEmpty)) {
    transceiver.io.extraInputs <> backend.io.transceiverExtraInputs
  }

  if (!(p(TransceiverKey).extraOutputs.isEmpty)) {
    transceiver.io.extraOutputs <> backend.io.transceiverExtraOutputs
  }

}
