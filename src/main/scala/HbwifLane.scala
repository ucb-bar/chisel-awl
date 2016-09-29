package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._

class HbwifLaneIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // RX pad inputs
  val rx = new Differential

  // TX pad outputs
  val tx = (new Differential).flip

  // TileLink port for memory
  val mem = (new ClientUncachedTileLinkIO()(edgeMemParams)).flip

  // configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(edgeMMIOParams)).flip

  // optional reference for the transceiver
  val iref = if (transceiverHasIRef) Some(Bool(INPUT)) else None

}

class HbwifLane(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new HbwifLaneIO

  // Transceiver
  val transceiver = Module(new Transceiver)

  // Lane Backend
  val backend = Module(new HbwifLaneBackend(transceiver.io.slowClk))

  backend.io.transceiverData <> transceiver.io.data
  io.rx <> transceiver.io.rx
  transceiver.io.fastClk := io.fastClk
  transceiver.io.tx <> io.tx

  transceiver.io.reset := backend.io.transceiverReset

  backend.io.mem <> io.mem
  backend.io.scr <> io.scr

  if (!(p(TransceiverKey).extraInputs.isEmpty)) {
    transceiver.io.extraInputs.get <> backend.io.transceiverExtraInputs.get
  }

  if (!(p(TransceiverKey).extraOutputs.isEmpty)) {
    backend.io.transceiverExtraOutputs.get <> transceiver.io.extraOutputs.get
  }

  if (transceiverHasIRef) {
    transceiver.io.iref.get <> io.iref.get
  }

}
