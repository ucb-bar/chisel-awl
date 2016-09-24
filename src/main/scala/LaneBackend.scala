package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._
import testchipip._

class LaneBackendIO(implicit p: Parameters) extends HbwifBundle()(p) {

  // data from/to the transceiver
  val transceiverData = (new TransceiverData).flip

  // tilelink port for memory
  val mem = (new ClientUncachedTileLinkIO()(outermostParams)).flip

  // Configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(outermostMMIOParams)).flip

  // parameterizable configuration bundle
  val transceiverExtraInputs = p(TransceiverKey).extraInputs.map { _.cloneType.asOutput }

  // parameterizable configuration bundle
  val transceiverExtraOutputs = p(TransceiverKey).extraOutputs.map { _.cloneType.asInput }

  // reset for the transceiver
  val transceiverReset = Bool(OUTPUT)

}

class LaneBackend(val c: Clock)(implicit val p: Parameters) extends Module(_clock = c)
  with HasHbwifParameters {

  val io = new LaneBackendIO

  require(transceiverDataWidth == 10)
  val encoder = Module(new Encoder8b10b)
  val decoder = Module(new Decoder8b10b)

  io.transceiverData.tx := encoder.io.encoded
  decoder.io.encoded := io.transceiverData.rx

  val memSerDes = Module(new HbwifTileLinkMemSerDes)
  encoder.io.decoded <> memSerDes.io.down
  memSerDes.io.up <> decoder.io.decoded
  memSerDes.io.mem <> io.mem

  val scrBuilder = new SCRBuilder

  scrBuilder.control("reset", UInt(1))

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
