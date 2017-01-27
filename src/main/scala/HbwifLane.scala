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
  val rx = (new Differential).flip

  // TX pad outputs
  val tx = new Differential

  // TileLink port for memory
  val mem = (new ClientUncachedTileLinkIO()(memParams)).flip

  // configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(mmioParams)).flip

  // optional reference for the transceiver
  val iref = if (transceiverHasIref) Some(Seq.fill(transceiverNumIrefs) { Bool(INPUT) } ) else None

  // un-synchronized HBWIF reset
  val hbwifReset = Bool(INPUT)

  // Switch to bypass reset synchronizers and wire reset directly
  val hbwifResetOverride = Bool(INPUT)

  // Counter for the slow clock, read by outside
  val slowClockCounter = UInt(OUTPUT,64)
}

class HbwifLane(id: Int)(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new HbwifLaneIO

  // Transceiver
  val transceiver = Module(new Transceiver)
  transceiver.suggestName("transceiverInst")

  // Synchronous reset
  val syncReset = transceiver.io.resetOut

  // Lane Backend
  val backend = Module(new HbwifLaneBackend(transceiver.io.slowClk, Mux(io.hbwifResetOverride, io.hbwifReset, syncReset), id))
  backend.suggestName("backendInst")

  backend.io.transceiverData <> transceiver.io.data
  transceiver.io.rx <> io.rx
  transceiver.io.fastClk := io.fastClk
  io.tx <> transceiver.io.tx

  transceiver.io.resetIn := io.hbwifReset

  backend.io.mem <> AsyncUTileLinkTo(backend.clock, backend.reset, io.mem)
  backend.io.scr <> AsyncUTileLinkTo(backend.clock, backend.reset, io.scr)

  if (!(p(TransceiverKey).extraInputs.isEmpty)) {
    transceiver.io.extraInputs.get := backend.io.transceiverExtraInputs.get
  }

  if (!(p(TransceiverKey).extraOutputs.isEmpty)) {
    backend.io.transceiverExtraOutputs.get <> transceiver.io.extraOutputs.get
  }

  //transceiver.io.iref.get.zip(io.iref.get).foreach { x => x._1 := x._2 }

  io.slowClockCounter := WordSync(WideCounterModule(64, transceiver.io.slowClk, syncReset),transceiver.io.slowClk)

}

