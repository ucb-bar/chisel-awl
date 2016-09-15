package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._

class LaneBackendIO(implicit p: Parameters) extends HbwifBundle()(p) {

  // data from/to the transceiver
  val transceiverData = new Bundle {
    val rx = UInt(INPUT, width = transceiverDataWidth)
    val tx = UInt(OUTPUT, width = transceiverDataWidth)
  }

  // TileLink port for memory
  val mem = (new ClientUncachedTileLinkIO()(outermostParams)).flip

  // Configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(outermostMMIOParams)).flip

  // parameterizable configuration bundle
  val transceiverExtraInputs = p(TransceiverKey).extraInputs.getOrElse({new Bundle}).cloneType.asOutput

  // parameterizable configuration bundle
  val transceiverExtraOutputs = p(TransceiverKey).extraOutputs.getOrElse({new Bundle}).cloneType.asInput

}

class LaneBackend(val c: Clock)(implicit val p: Parameters) extends Module(_clock = c)
  with HasHbwifParameters {

  val io = new LaneBackendIO

}

