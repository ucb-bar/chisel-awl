package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._
import testchipip._

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

  val scrBuilder = new SCRBuilder

  if (!(p(TransceiverKey).extraInputs.isEmpty)) {
    // TODO this needs to handle nested Bundles
    io.transceiverExtraInputs.get.elements.foreach { case (name: String, data: Data) =>
      data := scrBuilder.control(name, UInt(0))
    }
  }

  if (!(p(TransceiverKey).extraOutputs.isEmpty)) {
    // TODO this needs to handle nested Bundles
    io.transceiverExtraOutputs.get.elements.foreach { case (name: String, data: Data) =>
      scrBuilder.status(name) := data
    }
  }

  // generate the SCR File and attach it to our SCR TileLink port
  scrBuilder.generate(io.scr)(outermostMMIOParams)

}
