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

  val retransmitEnable = Bool(INPUT)
  val retransmitCycles = UInt(INPUT, width = log2Up(hbwifMaxRetransmitCycles))

}

class HbwifTileLinkMemSerDes(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifTileLinkMemSerDesIO

  when (io.mem.acquire.valid) {
    assert(io.mem.acquire.bits.is_builtin_type, "Only builtin types allowed in HBWIF")
  }
  require(hbwifBufferDepth <= (1 << tlClientXactIdBits), "HBWIF buffer depth should be <= (1 << tlClientXactIdBits)")
  require(hbwifBufferDepth == (1 << log2Up(hbwifBufferDepth)), "HBWIF buffer depth should be a power of 2")

  val grantBuffer = Module(new HellaFlowQueue(hbwifBufferDepth * tlDataBeats)(new Grant))
  val grantFilter = Module(new HbwifFilter(new Grant))
  val grantDeserializer = Module(new HbwifDeserializer(new Grant))

  val acquireTable = Module(new HbwifAcquireTable)
  val acquireSerializer = Module(new HbwifSerializer(new Acquire))

  assert(grantBuffer.io.enq.ready, "The grantBuffer must always be ready")
  grantBuffer.io.enq.bits := grantFilter.io.out.bits
  grantBuffer.io.enq.valid := grantFilter.io.out.valid
  io.mem.grant <> grantBuffer.io.deq

  grantFilter.io.in <> grantDeserializer.io.data
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
  acquireTable.io.retransmitEnable := io.retransmitEnable
  acquireTable.io.retransmitCycles := io.retransmitCycles

  acquireTable.io.clear.valid := grantFilter.io.out.valid & grantFilter.io.out.bits.first()
  acquireTable.io.clear.bits := grantFilter.io.out.bits.client_xact_id

  acquireSerializer.io.data <> acquireTable.io.out
  io.tx <> acquireSerializer.io.serial

}

class HbwifFilterIO[T <: Bundle](gen: T)(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val in = Valid(gen.cloneType).flip
  val out = Valid(gen.cloneType)

}

class HbwifFilter[T <: TileLinkChannel with HasClientTransactionId with HasTileLinkBeatId](gen: T)(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifFilterIO(gen)

  val sWait :: sBeatValid :: sBlockValid :: sFill :: Nil = Enum(UInt(), 4)
  val state = Reg(init = sWait)

  val buffer = Reg(Vec(tlDataBeats, gen.cloneType))
  val count = Reg(UInt(width = tlBeatAddrBits))

  io.out.bits  := buffer(count)
  io.out.valid := (state === sBlockValid) || (state === sBeatValid)

  when(state === sWait) {
    when(io.in.valid) {
      when(io.in.bits.hasMultibeatData()) {
        when (io.in.bits.addr_beat === UInt(0)) {
          state := sFill
          count := UInt(1)
        }
      } .otherwise {
        state := sBeatValid
        count := UInt(0)
      }
      buffer(UInt(0)) := io.in.bits
    }
  } .elsewhen (state === sBeatValid) {
    assert(!io.in.valid, "I got a valid Acquire before I could handle it")
    state := sWait
  } .elsewhen (state === sBlockValid) {
    assert(!io.in.valid, "I got a valid Acquire before I could handle it")
    when(count === UInt(tlDataBeats - 1)) {
      state := sWait
    } .otherwise {
      count := count + UInt(1)
    }
  } .elsewhen (state === sFill) {
    when (io.in.valid) {
      when ((io.in.bits.client_xact_id === buffer(UInt(0)).client_xact_id) && (io.in.bits.addr_beat === count)) {
        when (count === UInt(tlDataBeats - 1)) {
          state := sBlockValid
          count := UInt(0)
        } .otherwise {
          count := count + UInt(1)
        }
        buffer(count) := io.in.bits
      } .otherwise {
        // we lost a packet somewhere, abort this transaction
        state := sWait
      }
    }
  } .otherwise {
    state := sWait
  }

}

class HbwifDeserializerIO[T <: Bundle](gen: T)(implicit val p: Parameters) extends util.ParameterizedBundle()(p) {

  val serial = (new Decoded8b10bSymbol).asInput
  val data = Valid(gen.cloneType)

}

class HbwifDeserializer[T <: Bundle](gen: T)(implicit val p: Parameters) extends Module {

  val size = gen.cloneType.fromBits(UInt(0)).asUInt().getWidth
  val bytes = if (size % 8 == 0) size/8 else size/8 + 1

  val io = new HbwifDeserializerIO(gen)

  val buffer = Reg(Vec(bytes, UInt(width = 8)))
  val checksum = Reg(init = UInt(0, width = 8))
  val valid = Reg(init = Bool(false))
  val count = Reg(UInt(width = log2Up(bytes+1)))

  io.data.bits := gen.cloneType.fromBits(buffer.asUInt()(size-1,0))
  io.data.valid := valid

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
      when (count === UInt(bytes)) {
        count := UInt(0)
        when ((checksum + io.serial.data) === UInt(0)) {
          // checksum pass
          valid := Bool(true)
        } .otherwise {
          valid := Bool(false)
        }
        checksum := UInt(0)
        state := sIdle
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

  val retransmitEnable = Bool(INPUT)
  val retransmitCycles = UInt(INPUT, width = log2Up(hbwifMaxRetransmitCycles))

  val clear = Valid(UInt(width = tlClientXactIdBits)).flip

}

class HbwifAcquireTable(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifAcquireTableIO

  val sReady :: sFill :: sRetransmit :: sRetransmitFill :: Nil = Enum(UInt(), 4)
  val state = Reg(init = sReady)

  val timestamps = Reg(Vec(hbwifBufferDepth, UInt(width = log2Up(hbwifMaxRetransmitCycles))))
  val timeouts = Reg(Vec(hbwifBufferDepth, Bool()))
  val valids = Reg(init = Vec.fill(hbwifBufferDepth) { Bool(false) } )
  val xactIdToAddr = Reg(Vec((1 << tlClientXactIdBits), UInt(width = log2Up(hbwifBufferDepth))))

  val table = SeqMem(hbwifBufferDepth * tlDataBeats, new Acquire())
  table.suggestName("Hbwif_table")

  val count = Reg(UInt(width = tlBeatAddrBits))
  val blockAddress = Reg(UInt(width = log2Up(hbwifBufferDepth)))

  val full = valids.reduce(_&_)
  val nextAddress = PriorityEncoder(valids.map(!_))
  val timeoutsMasked = timeouts.zip(valids).map { case (t,v) => t & v }
  val timeout = timeoutsMasked.reduce(_|_)
  val timeoutAddress = PriorityEncoder(timeoutsMasked)

  val retransmitData = Wire(new Acquire())

  io.in.ready := !full & (state =/= sRetransmit) & io.out.ready
  io.out.valid := state === sFill | state === sRetransmit | state === sRetransmitFill | (((state === sReady) | !io.retransmitEnable) & io.in.valid)
  io.out.bits := Mux(state === sRetransmit | state === sRetransmitFill, retransmitData, io.in.bits)

  // table SRAM stuff
  val wen = (state === sReady | state === sFill) & io.in.fire()
  val waddr = Mux(state === sFill, Cat(blockAddress, count), Cat(nextAddress, UInt(0, width = tlBeatAddrBits)))
  val raddr = Mux(state === sReady, Cat(timeoutAddress, UInt(0, width = tlBeatAddrBits)), Cat(blockAddress, count))

  retransmitData := table.read(raddr, !wen)
  // TODO the timing here is off

  when (wen) { table.write(waddr, io.in.bits) }

  // TODO need to fix TIMEOUT on the same cycle as a clear XXX

  ///////////////////////////// timeouts and timestamps
  (0 until hbwifBufferDepth).foreach { i =>
    when ((nextAddress === UInt(i)) & (state === sReady) & io.in.fire()) {
      timestamps(UInt(i)) := io.retransmitCycles
      timeouts(UInt(i)) := Bool(false)
    } .elsewhen(!io.in.fire() & timeout & (timeoutAddress === UInt(i))) {
      timestamps(UInt(i)) := io.retransmitCycles
      timeouts(UInt(i)) := UInt(0)
    } .otherwise {
      when (timestamps(UInt(i)) === UInt(0)) {
        timeouts(UInt(i)) := Bool(true)
      } .otherwise {
        timestamps(UInt(i)) := timestamps(UInt(i)) - UInt(1)
      }
    }
  }

  ///////////////////////////// valids
  when (io.retransmitEnable) {
    assert(!io.clear.valid | valids(xactIdToAddr(io.clear.bits)), "Trying to clear a valid bit that isn't set")
    (0 until hbwifBufferDepth).foreach { i =>
      when ((state === sReady) & io.in.fire() & (nextAddress === UInt(i))) {
        valids(UInt(i)) := Bool(true)
      } .elsewhen (io.clear.valid & (xactIdToAddr(io.clear.bits) === UInt(i))) {
        valids(UInt(i)) := Bool(false)
      }
    }
  } .otherwise {
    valids.foreach { _ := Bool(false) }
  }

  ///////////////////////////// xactIdToAddr
  when (state === sReady) {
    when (io.in.fire()) {
      assert(!valids(xactIdToAddr(io.in.bits.client_xact_id)), "Should not get the same client_xact_id before the valid bit is cleared")
      xactIdToAddr(io.in.bits.client_xact_id) := nextAddress
    }
  }

  ///////////////////////////// state
  when (io.retransmitEnable) {
    when (state === sReady) {
      when (io.in.fire()) {
        blockAddress := nextAddress
        count := UInt(1)
        when (!io.in.bits.last()) {
          assert(io.in.bits.addr_beat === UInt(0), "Got a non-zero beat address as first beat")
          state := sFill
        }
      } .elsewhen (timeout) {
        blockAddress := timeoutAddress
        count := UInt(1)
        state := sRetransmit
      }
    } .elsewhen (state === sFill) {
      when (io.in.fire()) {
        assert(io.in.bits.addr_beat === count, "Got a beat out of order")
        count := count + UInt(1)
        when (count === UInt(tlDataBeats-1)) {
          state := sReady
        }
      }
    } .elsewhen (state === sRetransmit) {
      when (io.out.bits.last()) {
        state := sReady
      } .otherwise {
        count := UInt(1)
        state := sRetransmitFill
      }
    } .elsewhen (state === sRetransmitFill) {
      when (io.out.fire()) {
        count := count + UInt(1)
        when (count === UInt(tlDataBeats-1)) {
          state := sReady
        }
      }
    }
  } .otherwise {
    state := sReady
  }
}

class HbwifSerializerIO[T <: Bundle](gen: T)(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val data = Decoupled(gen.cloneType).flip
  val serial = (new Decoded8b10bSymbol).asOutput

}

class HbwifSerializer[T <: Bundle](gen: T)(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifSerializerIO(gen)

  val tobits = io.data.bits.asUInt()
  val size = gen.cloneType.fromBits(UInt(0)).asUInt().getWidth
  val bytes = if (size % 8 == 0) size/8 else size/8 + 1
  val padBits = if (size % 8 == 0) 0 else 8 - (size % 8)

  val buffer = Reg(Vec(bytes, UInt(width = 8)))
  val checksum = Reg(init = UInt(0, width = 8))
  val count = Reg(init = UInt(0, width = log2Up(bytes)))

  val sIdle :: sFill :: sChecksum :: Nil = Enum(UInt(), 3)

  val state = Reg(init = sIdle)

  val raw = if(padBits == 0) tobits else Cat(UInt(0, width=padBits), tobits)

  io.data.ready := (state === sIdle | state === sChecksum)

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
    when (io.data.fire()) {
      state := sFill
      count := UInt(0)
      checksum := UInt(0)
      (0 until bytes).foreach { i => buffer(i) := raw(i*8+7,i*8) }
    }
  } .elsewhen (state === sFill) {
    checksum := checksum + buffer(count)
    when (count === UInt(bytes-1)) {
      state := sChecksum
    } .otherwise {
      state := sFill
      count := count + UInt(1)
    }
  } .elsewhen (state === sChecksum) {
    when (io.data.fire()) {
      state := sFill
      (0 until bytes).foreach { i => buffer(i) := raw(i*8+7,i*8) }
      count := UInt(0)
      checksum := UInt(0)
    } .otherwise {
      state := sIdle
    }
  } .otherwise {
    state := sIdle
  }
}

///////////////////////// XXX This is just a hack to get H2 working

class HbwifSerializer2IO[T <: Bundle](gen: T)(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifTileLinkParameters {

  val data = Decoupled(gen.cloneType).flip
  val serial = Vec(2, new Decoded8b10bSymbol).asOutput

}

class HbwifSerializer2[T <: Bundle](gen: T)(implicit val p: Parameters) extends Module
  with HasHbwifTileLinkParameters {

  val io = new HbwifSerializer2IO(gen)

  val tobits = io.data.bits.asUInt()
  val size = gen.cloneType.fromBits(UInt(0)).asUInt().getWidth
  val bytes = if (size % 8 == 0) size/8 else size/8 + 1
  val padBits = if (size % 8 == 0) 0 else 8 - (size % 8)
  require(bytes > 2)

  val buffer = Reg(Vec(bytes, UInt(width = 8)))
  val checksum = Reg(init = UInt(0, width = 8))
  val count = Reg(init = UInt(0, width = log2Up(bytes)))

  val sIdle :: sFill :: sChecksum :: sFillChecksum :: Nil = Enum(UInt(), 4)

  val state = Reg(init = sIdle)

  val raw = if(padBits == 0) tobits else Cat(UInt(0, width=padBits), tobits)

  io.data.ready := (state === sIdle || state === sChecksum || state === sFillChecksum)

  io.serial(0).valid := (state === sFill || state === sChecksum || state === sFillChecksum)
  io.serial(1).valid := (state === sFill || state === sFillChecksum)
  io.serial(0).control := Bool(false)
  io.serial(1).control := Bool(false)
  io.serial(0).rd := Bool(false) // don't care here
  io.serial(1).rd := Bool(false) // don't care here

  when (state === sChecksum) {
    io.serial(0).data := ~checksum + UInt(1)
    io.serial(1).data := UInt(0) // lose a bit of performance here but don't care
  } .elsewhen (state === sFillChecksum) {
    io.serial(0).data := buffer(count)
    io.serial(1).data := ~(checksum + buffer(count)) + UInt(1)
  } .otherwise {
    io.serial(0).data := buffer(count)
    io.serial(1).data := buffer(count + UInt(1))
  }

  when (state === sIdle) {
    when (io.data.fire()) {
      state := sFill
      count := UInt(0)
      checksum := UInt(0)
      (0 until bytes).foreach { i => buffer(i) := raw(i*8+7,i*8) }
    }
  } .elsewhen (state === sFill) {
    checksum := checksum + buffer(count) + buffer(count + UInt(1))
    when (count === UInt(bytes-1)) {
      state := sChecksum
    } .elsewhen (count === UInt(bytes-2)) {
      state := sFillChecksum
    } .otherwise {
      state := sFill
      count := count + UInt(2)
    }
  } .elsewhen (state === sChecksum || state === sFillChecksum) {
    when (io.data.fire()) {
      state := sFill
      (0 until bytes).foreach { i => buffer(i) := raw(i*8+7,i*8) }
      count := UInt(0)
      checksum := UInt(0)
    } .otherwise {
      state := sIdle
    }
  } .otherwise {
    state := sIdle
  }
}

class HbwifDeserializer2IO[T <: Bundle](gen: T)(implicit val p: Parameters) extends util.ParameterizedBundle()(p) {

  val serial = Vec(2, new Decoded8b10bSymbol).asInput
  val data = Valid(gen.cloneType)

}

class HbwifDeserializer2[T <: Bundle](gen: T)(implicit val p: Parameters) extends Module {

  val size = gen.cloneType.fromBits(UInt(0)).asUInt().getWidth
  val bytes = if (size % 8 == 0) size/8 else size/8 + 1
  require(bytes > 2)
  val odd = (bytes % 2) == 1

  val io = new HbwifDeserializer2IO(gen)

  val buffer = Reg(Vec(bytes+1, UInt(width = 8)))
  val checksum = Reg(init = UInt(0, width = 8))
  val valid = Reg(init = Bool(false))
  val count = Reg(UInt(width = log2Up(bytes+1)))
  val offset = Reg(Bool())

  io.data.bits := Mux(offset,
    gen.cloneType.fromBits(buffer.asUInt()(size+7,8)),
    gen.cloneType.fromBits(buffer.asUInt()(size-1,0)))
  io.data.valid := valid

  val sIdle :: sFill :: Nil = Enum(UInt(), 2)

  val state = Reg(init = sIdle)

  when (state === sIdle) {
    when (io.serial(0).isData()) {
      state := sFill
      checksum := io.serial(0).data + io.serial(1).data
      if (odd) {
          buffer(UInt(0)) := io.serial(0).data
          buffer(UInt(1)) := io.serial(1).data
          offset := Bool(false)
      } else {
          buffer(UInt(1)) := io.serial(0).data
          buffer(UInt(2)) := io.serial(1).data
          offset := Bool(true)
      }
      count := UInt(2)
      valid := Bool(false)
    } .elsewhen (io.serial(1).isData()) {
      state := sFill
      checksum := io.serial(1).data
      if (odd) {
          buffer(UInt(1)) := io.serial(1).data
          offset := Bool(true)
      } else {
          buffer(UInt(0)) := io.serial(1).data
          offset := Bool(false)
      }
      count := UInt(1)
      valid := Bool(false)
    }
  } .elsewhen (state === sFill) {
    when (io.serial(0).isData()) {
      when (count === UInt(bytes)) {
        // io.serial(0) is checksum
        when (io.serial(1).isData()) {
          count := UInt(1)
          when ((checksum + io.serial(0).data) === UInt(0)) {
            valid := Bool(true)
          } .otherwise {
            valid := Bool(false)
          }
          checksum := io.serial(1).data
          buffer(UInt(0)) := io.serial(1).data
          offset := Bool(false)
        } .otherwise {
          // ignore io.serial(1)
          count := UInt(0)
          when ((checksum + io.serial(0).data) === UInt(0)) {
            valid := Bool(true)
          } .otherwise {
            valid := Bool(false)
          }
          checksum := UInt(0)
          state := sIdle
        }
      } .elsewhen (count === UInt(bytes-1)) {
        // io.serial(1) is checksum
        count := UInt(0)
        when ((checksum + io.serial(1).data + io.serial(0).data) === UInt(0)) {
          valid := Bool(true)
        } .otherwise {
          valid := Bool(false)
        }
        buffer(count+offset) := io.serial(0).data
        checksum := UInt(0)
        state := sIdle
      } .otherwise {
        count := count + UInt(2)
        buffer(count+offset) := io.serial(0).data
        buffer(count+UInt(1)+offset) := io.serial(1).data
        checksum := checksum + io.serial(0).data + io.serial(1).data
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
