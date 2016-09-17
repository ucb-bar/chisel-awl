package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._

class LaneBackendIO(implicit p: Parameters) extends HbwifBundle()(p) {

  // data from/to the transceiver
  val transceiverData = (new TransceiverData).flip

  // TileLink port for memory
  val mem = (new ClientUncachedTileLinkIO()(outermostParams)).flip

  // Configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(outermostMMIOParams)).flip

  // parameterizable configuration bundle
  val transceiverExtraInputs = p(TransceiverKey).extraInputs.map { _.cloneType.asOutput }

  // parameterizable configuration bundle
  val transceiverExtraOutputs = p(TransceiverKey).extraOutputs.map { _.cloneType.asInput }

}

class LaneBackend(val c: Clock)(implicit val p: Parameters) extends Module(_clock = c)
  with HasHbwifParameters {

  val io = new LaneBackendIO

  val mod = Module(new FakeLaneBackendThing)

  mod.io.transceiverData <> io.transceiverData
  mod.io.mem <> io.mem
  mod.io.scr <> io.scr


  if (!(p(TransceiverKey).extraInputs.isEmpty)) {
    io.transceiverExtraInputs.get <> mod.io.transceiverExtraInputs.get
  }

  if (!(p(TransceiverKey).extraOutputs.isEmpty)) {
    mod.io.transceiverExtraOutputs.get <> io.transceiverExtraOutputs.get
  }

}

class FakeLaneBackendThing(implicit val p: Parameters) extends BlackBox {

  val io = new LaneBackendIO

}
