package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._
import testchipip._

class HbwifLaneIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // RX pad inputs
  val rx = new Differential

  // TX pad outputs
  val tx = (new Differential).flip

  // TileLink port for memory
  val mem = (new ClientUncachedTileLinkIO()(memParams)).flip

  // configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(mmioParams)).flip

  // optional reference for the transceiver
  val iref = if (transceiverHasIRef) Some(Bool(INPUT)) else None

  // un-synchronized HBWIF reset
  val hbwifReset = Bool(INPUT)

}

class HbwifLane(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new HbwifLaneIO

  // Transceiver
  val transceiver = Module(new Transceiver)
  transceiver.suggestName("transceiverInst")

  // Synchronous reset
  val syncReset = ResetSync(io.hbwifReset, transceiver.io.slowClk)

  // Lane Backend
  val backend = Module(new HbwifLaneBackend(transceiver.io.slowClk, syncReset))
  backend.suggestName("backendInst")

  backend.io.transceiverData <> transceiver.io.data
  io.rx <> transceiver.io.rx
  transceiver.io.fastClk := io.fastClk
  transceiver.io.tx <> io.tx

  transceiver.io.reset := backend.io.transceiverReset

  backend.io.mem <> AsyncUTileLinkTo(backend.clock, backend.reset, io.mem)
  backend.io.scr <> AsyncUTileLinkTo(backend.clock, backend.reset, io.scr)

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
