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

  val syncReset = ResetSync(reset, transceiver.io.slowClk)
  val syncLoopback = ResetSync(io.loopback, transceiver.io.slowClk)

  val backend = Module(new FiwbhLaneBackend(transceiver.io.slowClk, syncReset)(memParams))

  backend.io.loopback := syncLoopback
  backend.io.transceiverData <> transceiver.io.data
  transceiver.io.rx <> io.rx
  transceiver.io.fastClk := io.fastClk
  io.tx <> transceiver.io.tx

  transceiver.io.reset := reset

  io.mem <> AsyncUTileLinkTo(backend.clock, backend.reset, backend.io.mem)

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
  val acquireFilter = Module(new FiwbhAcquireFilter)
  val acquireDeserializer = Module(new FiwbhAcquireDeserializer)

  val grantSerializer = Module(new FiwbhGrantSerializer)

  acquireBuffer.io.enq.bits := acquireFilter.io.out.bits
  acquireBuffer.io.enq.valid := acquireFilter.io.out.valid
  io.mem.acquire <> acquireBuffer.io.deq

  acquireFilter.io.in <> acquireDeserializer.io.acquire
  acquireDeserializer.io.serial <> io.rx

  grantSerializer.io.grant <> io.mem.grant
  io.tx <> grantSerializer.io.serial

}

class FiwbhAcquireFilterIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val in = Valid(new Acquire).flip
  val out = Valid(new Acquire)

}

class FiwbhAcquireFilter(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  // TODO this can be scala-ized
  // class HbwifFilter[T <: Data] ...

  val io = new FiwbhAcquireFilterIO

  val sWait :: sValid :: sFill :: Nil = Enum(UInt(), 3)
  val state = Reg(init = sWait)

  val buffer = Reg(Vec(tlDataBeats, new Acquire()))
  val count = Reg(UInt(width = tlBeatAddrBits))

  io.out.bits  := buffer(UInt(tlDataBeats) - count)
  io.out.valid := (state === sValid)

  when(state === sWait) {
    when(io.in.valid) {
      when(io.in.bits.hasMultibeatData()) {
        when (io.in.bits.addr_beat === UInt(0)) {
          state := sFill
          count := UInt(1)
          buffer(UInt(tlDataBeats - 1)) := io.in.bits
        }
      } .otherwise {
        state := sValid
        count := UInt(0)
        buffer(UInt(tlDataBeats - 1)) := io.in.bits
      }
    }
  } .elsewhen (state === sValid) {
    assert(!io.in.valid, "I got a valid Acquire before I could handle it")
    when(count === UInt(0)) {
      state := sWait
    } .otherwise {
      count := count - UInt(1)
    }
  } .elsewhen (state === sFill) {
    when (io.in.valid) {
      when ((io.in.bits.client_xact_id === buffer(UInt(tlDataBeats - 1)).client_xact_id) && (io.in.bits.addr_beat === count)) {
        when (count === UInt(tlDataBeats - 1)) {
          state := sValid
        } .otherwise {
          count := count + UInt(1)
        }
        buffer(UInt(tlDataBeats - 1) - count) := io.in.bits
      } .otherwise {
        // we lost a packet somewhere, abort this transaction
        state := sWait
      }
    }
  } .otherwise {
    state := sWait
  }

}

class FiwbhAcquireDeserializerIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val serial = (new Decoded8b10bSymbol).asInput
  val acquire = Valid(new Acquire())

}

class FiwbhAcquireDeserializer(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  // TODO this can be scala-ized
  // class HbwifDeserializer[T <: Data] ...

  val io = new FiwbhAcquireDeserializerIO

  val buffer = Reg(Vec(hbwifAcquireBytes-1, UInt(width = 8)))
  val checksum = Reg(init = UInt(0, width = 8))
  val valid = Reg(init = Bool(false))
  val count = Reg(UInt(width = log2Up(hbwifAcquireBytes)))

  val cat = buffer.asUInt()

  io.acquire.bits := (new Acquire).fromBits(buffer.asUInt()(hbwifRawAcquireBits-1,0))
  io.acquire.valid := valid

  val sIdle :: sFill :: Nil = Enum(UInt(), 2)

  val state = Reg(init = sIdle)

  when (state === sIdle) {
    when (io.serial.isData()) {
      state := sFill
    }
    checksum := io.serial.data
    buffer(UInt(0)) := io.serial.data
    count := UInt(1)
    valid := Bool(false)
  } .elsewhen (state === sFill) {
    when (io.serial.isData()) {
      when (count === UInt(hbwifAcquireBytes-1)) {
        count := UInt(0)
        when ((checksum + ~io.serial.data + UInt(1)) === UInt(0)) {
          // checksum pass
          valid := Bool(true)
        } .otherwise {
          valid := Bool(false)
        }
        checksum := UInt(0)
      } .otherwise {
        count := count + UInt(1)
        buffer(count) := io.serial.data
        checksum := checksum + io.serial.data
        valid := Bool(false)
      }
    } .otherwise {
      state := sIdle
      count := UInt(0)
      checksum := UInt(0)
      valid := Bool(false)
    }
  } .otherwise {
    state := sIdle
    count := UInt(0)
    checksum := UInt(0)
    valid := Bool(false)
  }

}

class FiwbhGrantSerializerIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val grant = Decoupled(new Grant).flip
  val serial = (new Decoded8b10bSymbol).asOutput

}

class FiwbhGrantSerializer(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  // TODO this can be scala-ized
  // class HbwifSerializer[T <: Data] ...

  val io = new FiwbhGrantSerializerIO

  val buffer = Reg(Vec(hbwifGrantBytes-1, UInt(width = 8)))
  val checksum = Reg(init = UInt(0, width = 8))
  val count = Reg(init = UInt(0, width = log2Up(hbwifGrantBytes-1)))

  val sIdle :: sFill :: sChecksum :: Nil = Enum(UInt(), 3)

  val state = Reg(init = sIdle)

  io.grant.ready := (state === sIdle | state === sChecksum)

  io.serial.valid := (state === sFill | state === sChecksum)
  io.serial.control := Bool(false)
  io.serial.rd := Bool(false) // don't care here

  when (state === sIdle) {
    io.serial.data := buffer(count)
  } .elsewhen (state === sFill) {
    io.serial.data := buffer(count)
  } .elsewhen (state === sChecksum) {
    io.serial.data := ~checksum + UInt(1)
  } .otherwise {
    io.serial.data := buffer(count)
  }

  when (state === sIdle) {
    when (io.grant.fire()) {
      state := sFill
      count := UInt(0)
      (0 until hbwifGrantBytes-2).foreach { i => buffer(i) := Cat(UInt(0, width=hbwifGrantPadBits),io.grant.bits.asUInt())(i*8+7,i*8) }
    }
  } .elsewhen (state === sFill) {
    when (count === UInt(hbwifGrantBytes-2)) {
      state := sChecksum
    } .otherwise {
      state := sFill
      count := count + UInt(1)
    }
  } .elsewhen (state === sChecksum) {
    when (io.grant.fire()) {
      state := sFill
      (0 until hbwifGrantBytes-2).foreach { i => buffer(i) := Cat(UInt(0, width=hbwifGrantPadBits),io.grant.bits.asUInt())(i*8+7,i*8) }
      count := UInt(0)
    } .otherwise {
      state := sIdle
    }
  } .otherwise {
    state := sIdle
  }

}
