package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._
import testchipip._

class HbwifLaneBackendIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  // data from/to the transceiver
  val transceiverData = (new TransceiverData).flip

  // tilelink port for memory
  val mem = (new ClientUncachedTileLinkIO()(memParams)).flip

  // Configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(mmioParams)).flip

  // parameterizable configuration bundle
  val transceiverExtraInputs = p(TransceiverKey).extraInputs.map { _.cloneType.asOutput }

  // parameterizable configuration bundle
  val transceiverExtraOutputs = p(TransceiverKey).extraOutputs.map { _.cloneType.asInput }

  // reset for the transceiver
  val transceiverReset = Bool(OUTPUT)

}

class HbwifLaneBackend(val c: Clock)(implicit val p: Parameters) extends Module(_clock = c)
  with HasHbwifParameters {

  val io = new HbwifLaneBackendIO

  require(transceiverDataWidth == 10)
  val encoder = Module(new Encoder8b10b)
  val decoder = Module(new Decoder8b10b)

  io.transceiverData.tx := encoder.io.encoded
  decoder.io.encoded := io.transceiverData.rx

  val memSerDes = Module(new HbwifTileLinkMemSerDes)
  encoder.io.decoded <> memSerDes.io.down
  memSerDes.io.up <> decoder.io.decoded
  memSerDes.io.mem <> io.mem

  val scrBuilder = new SCRBuilder("hbwif_lane")

  scrBuilder.addControl("reset", UInt(1))

  // TODO this needs to handle nested Bundles
  io.transceiverExtraInputs.map(_.elements.keys.foreach {
    name => scrBuilder.addControl(name)
  })

  // TODO this needs to handle nested Bundles
  io.transceiverExtraOutputs.map(_.elements.keys.foreach {
    name => scrBuilder.addStatus(name)
  })

  // generate the SCR File and attach it to our SCR TileLink port
  val scr = scrBuilder.generate(mmioParams)
  scr.io.tl <> io.scr

  // TODO this needs to handle nested Bundles
  io.transceiverExtraInputs.map(_.elements.foreach {
    case (name: String, data: Data) => data := scr.control(name)
  })

  // TODO this needs to handle nested Bundles
  io.transceiverExtraOutputs.map(_.elements.foreach {
    case (name: String, data: Data) => scr.status(name) := data
  })

  io.transceiverReset := scr.control("reset")
}