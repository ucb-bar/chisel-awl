// This is the "inverse" HBWIF (or FIWBH) that goes on the FPGA to translate serial back into TileLink

package hbwif

import Chisel._
import cde._
import util._
import rocketchip._
import junctions._
import uncore.tilelink._
import testchipip._


class FiwbhIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // loopback mode
  val loopback = Bool(INPUT)

  // RX Pad inputs
  val rx = Vec(hbwifNumLanes, new Differential).flip

  // TX pad outputs
  val tx = Vec(hbwifNumLanes, new Differential)

  // TileLink port for memory
  val mem = Vec(hbwifNumLanes, new ClientUncachedTileLinkIO()(memParams))

}

class Fiwbh(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new FiwbhIO

  val lanes = (0 until hbwifNumLanes) map { i => Module(new FiwbhLane) }

  lanes.foreach(_.io.fastClk := io.fastClk)
  lanes.foreach(_.io.loopback := io.loopback)
  lanes.map(_.io.rx).zip(io.rx).foreach { case (lane, top) => lane <> top }
  lanes.map(_.io.tx).zip(io.tx).foreach { case (lane, top) => top <> lane }
  lanes.map(_.io.mem).zip(io.mem).foreach { case (lane, top) => top <> lane }

}

class FiwbhLaneIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // loopback mode
  val loopback = Bool(INPUT)

  // RX Pad inputs
  val rx = (new Differential).flip

  // TX pad outputs
  val tx = new Differential

  // TileLink port for memory
  val mem = new ClientUncachedTileLinkIO()(memParams)

}

class FiwbhLane(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new FiwbhLaneIO

  val transceiver = Module(new FPGATransceiver())

  val syncReset = transceiver.io.resetOut
  val syncLoopback = ResetSync(io.loopback, transceiver.io.slowClk)

  val backend = Module(new FiwbhLaneBackend(transceiver.io.slowClk, syncReset)(memParams))

  backend.io.loopback := syncLoopback
  backend.io.transceiverData <> transceiver.io.data
  transceiver.io.rx <> io.rx
  transceiver.io.fastClk := io.fastClk
  io.tx <> transceiver.io.tx

  transceiver.io.resetIn := reset

  io.mem <> AsyncUTileLinkFrom(backend.clock, backend.reset, backend.io.mem, depth = 4)

}

class FiwbhLaneBackendIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  // loopback mode
  val loopback = Bool(INPUT)

  // data from/to the transceiver
  val transceiverData = (new TransceiverData).flip

  // tilelink port for memory
  val mem = new ClientUncachedTileLinkIO

}

class FiwbhLaneBackend(c: Clock, r: Bool)(implicit val p: Parameters) extends Module(_clock = c, _reset = r)
  with HasHbwifTileLinkParameters {

  val io = new FiwbhLaneBackendIO

  require(transceiverDataWidth == 10)
  val encoder = Module(new Encoder8b10b)
  val decoder = Module(new Decoder8b10b)

  decoder.io.encoded := io.transceiverData.rx

  val memDesSer = Module(new FiwbhTileLinkMemDesSer)

  encoder.io.decoded <> memDesSer.io.tx
  memDesSer.io.rx <> decoder.io.decoded
  io.mem <> memDesSer.io.mem

  io.transceiverData.tx := Mux(io.loopback, io.transceiverData.rx, encoder.io.encoded)

}

class FiwbhTileLinkMemDesSerIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val tx = (new Decoded8b10bSymbol).asOutput
  val rx = (new Decoded8b10bSymbol).asInput

  val mem = new ClientUncachedTileLinkIO

}

class FiwbhTileLinkMemDesSer(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new FiwbhTileLinkMemDesSerIO

  val acquireBuffer = Module(new HellaFlowQueue(hbwifBufferDepth * tlDataBeats)(new Acquire))
  val acquireFilter = Module(new HbwifFilter(new Acquire))
  val acquireDeserializer = Module(new HbwifDeserializer(new Acquire))

  val grantSerializer = Module(new HbwifSerializer(new Grant))

  acquireBuffer.io.enq.bits := acquireFilter.io.out.bits
  acquireBuffer.io.enq.valid := acquireFilter.io.out.valid
  io.mem.acquire <> acquireBuffer.io.deq

  acquireFilter.io.in <> acquireDeserializer.io.data
  acquireDeserializer.io.serial <> io.rx

  grantSerializer.io.data <> io.mem.grant
  io.tx <> grantSerializer.io.serial

}
