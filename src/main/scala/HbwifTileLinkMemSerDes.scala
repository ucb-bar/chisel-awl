package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._
import util.HellaFlowQueue

class HbwifTileLinkMemSerDesIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  val tx = (new Decoded8b10bSymbol).asOutput
  val rx = (new Decoded8b10bSymbol).asInput

  val mem = (new ClientUncachedTileLinkIO).flip

}

class HbwifTileLinkMemSerDes(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifTileLinkMemSerDesIO

  assert(io.mem.acquire.bits.is_builtin_type, "Only builtin types allowed in HBWIF")
  require(hbwifBufferDepth <= (1 << tlClientXactIdBits), "HBWIF buffer depth should be <= (1 << tlClientXactIdBits)")
  require(hbwifBufferDepth == (1 << log2Up(hbwifBufferDepth)), "HBWIF buffer depth should be a power of 2")

  val grantBuffer = Module(new HellaFlowQueue(hbwifBufferDepth)(new Grant))
  val grantFilter = Module(new HbwifGrantFilter)
  val grantDeserializer = Module(new HbwifGrantDeserializer)

  val acquireTable = Module(new HbwifAcquireTable)
  val acquireSerializer = Module(new HbwifAcquireSerializer)

  assert(grantBuffer.io.enq.ready, "The grantBuffer must always be ready")
  grantBuffer.io.enq.bits := grantFilter.io.out.bits
  grantBuffer.io.enq.valid := grantFilter.io.out.valid
  io.mem.grant <> grantBuffer.io.deq

  grantFilter.io.in <> grantDeserializer.io.grant
  grantDeserializer.io.serial <> io.rx

  val dec = (io.mem.grant.fire() && io.mem.grant.bits.last())
  val inc = (io.mem.acquire.fire() && io.mem.acquire.bits.last())
  val slots = Reg(init = UInt(0, width = log2Up(hbwifBufferDepth)))
  val full = Reg(init = Bool(false))
  when (dec && ~inc) {
    slots := slots - UInt(1)
    full := Bool(false)
  } .elsewhen (inc && ~dec) {
    slots := slots + UInt(1)
    when (slots === UInt(hbwifBufferDepth - 1)) {
      full := Bool(true)
    }
  }

  io.mem.acquire.ready := !full && acquireTable.io.in.ready
  acquireTable.io.in.valid := !full && io.mem.acquire.valid
  acquireTable.io.in.bits := io.mem.acquire.bits

  acquireSerializer.io.acquire <> acquireTable.io.out
  io.tx <> acquireSerializer.io.serial

}

class HbwifGrantFilterIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  val in  = Valid(new Grant).flip
  val out = Valid(new Grant)

}

class HbwifGrantFilter(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifGrantFilterIO

  val sWait :: sValid :: sFill :: Nil = Enum(UInt(), 3)
  val state = Reg(init = sWait)

  val buffer = Reg(Vec(tlDataBeats, new Grant()))
  val count = Reg(UInt(width = tlBeatAddrBits))

  io.out.bits  := buffer(UInt(tlDataBeats) - count)
  io.out.valid := (state === sValid)

  when(state === sWait) {
    when(io.in.valid) {
      when((io.in.bits.g_type === Acquire.getBlockType) && (io.in.bits.addr_beat === UInt(0))) {
        state := sFill
        count := UInt(1)
        buffer(UInt(tlDataBeats - 1)) := io.in.bits
      } .otherwise {
        state := sValid
        count := UInt(0)
        buffer(UInt(tlDataBeats - 1)) := io.in.bits
      }
    }
  } .elsewhen (state === sValid) {
    assert(!io.in.valid, "I got a valid Grant before I could handle it")
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

class HbwifGrantDeserializerIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  val serial = (new Decoded8b10bSymbol).asInput
  val grant = Valid(new Grant())

}

class HbwifGrantDeserializer(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifGrantDeserializerIO

  val buffer = Reg(Vec(hbwifGrantBytes-1, UInt(width = 8)))
  val checksum = Reg(init = UInt(0, width = 8))
  val valid = Reg(init = Bool(false))
  val count = Reg(UInt(width = log2Up(hbwifGrantBytes)))

  val cat = buffer.asUInt()

  io.grant.bits := (new Grant).fromBits(buffer.asUInt()(hbwifRawGrantBits-1,0))
  io.grant.valid := valid

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
      when (count === UInt(hbwifGrantBytes-1)) {
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

class HbwifAcquireTableIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val in = Decoupled(new Acquire).flip
  val out = Decoupled(new Acquire)

}

class HbwifAcquireTable(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifAcquireTableIO

  val timestamps = Reg(Vec(hbwifBufferDepth, UInt(width = log2Up(hbwifMaxRetransmitCycles))))
  val valids = Reg(init = Wire(Vec(hbwifBufferDepth, UInt(0, width=1))))
  val types = Reg(Vec(hbwifBufferDepth, UInt(width = 2)))
  val xactIds = Reg(Vec(hbwifBufferDepth, UInt(width = tlClientXactIdBits)))

  /*
   input  io_mem_acquire_bits_addr_block,      - block address
   input  io_mem_acquire_bits_client_xact_id,  - client transaction id
   input  io_mem_acquire_bits_addr_beat,       - beat address
   input  io_mem_acquire_bits_is_builtin_type, - should be true
   input  io_mem_acquire_bits_a_type,          - look at definitions (should be one of 4)
   input  io_mem_acquire_bits_union,           - extra information (write mask, etc)
   input  io_mem_acquire_bits_data,            - write data for puts and put blocks
  */

  // TODO

}

class HbwifAcquireSerializerIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val acquire = Decoupled(new Acquire).flip
  val serial = (new Decoded8b10bSymbol).asOutput

}

class HbwifAcquireSerializer(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifAcquireSerializerIO

  // TODO

}
