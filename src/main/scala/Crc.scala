// johnwright@eecs.berkeley.edu

package hurricane_hbwif

import Chisel._
import ChiselError._
import cde.{Parameters, Field}
import junctions._
import Serial._

// Note that there isn't actually a CRC in this block yet- but the interface shouldn't change
class CrcIO(implicit p: Parameters) extends HurricaneHbwifBundle()(p) {
  val atos = new AtosIO().flip
  val crc = new SerialChannel(8).flip
  val sync = Bool(OUTPUT)
  val mem_en = Bool(INPUT)
  val err = Bool(OUTPUT)
}

class Crc(implicit p: Parameters) extends HurricaneHbwifModule()(p) {

  val io = new CrcIO

  val tx = Module(new CrcTx)
  val rx = Module(new CrcRx)

  tx.io.mem_en := io.mem_en

  // this tracks the total space required for all outstanding transactions
  val free = Reg(init = UInt(hbwifBufferDepth))
  val buf = Module(new Queue(new AtosResponse, hbwifBufferDepth, true, true))

  // calculate how much space is left
  when(io.atos.req.fire()) {
    when(buf.io.deq.fire()) {
      free := free - io.atos.req.bits.resp_len() + UInt(1)
    } .otherwise {
      free := free - io.atos.req.bits.resp_len()
    }
  } .otherwise {
    when(buf.io.deq.fire()) {
      free := free + UInt(1)
    }
  }

  val space_left = (free >= io.atos.req.bits.resp_len())

  io.atos.req  <> tx.io.in

  tx.io.space_left := space_left
  tx.io.sync := rx.io.sync
  io.sync := tx.io.sync
  io.err := rx.io.err

  rx.io.in <> io.crc.rx
  buf.io.enq <> rx.io.out
  io.atos.resp <> buf.io.deq
  io.crc.tx <> tx.io.out


}

class CrcTx(implicit p: Parameters) extends HurricaneHbwifModule()(p) {

  val io = new Bundle {
    val out = Decoupled(new SerialSymbol(8))
    val in  = Decoupled(new AtosRequest).flip
    val sync = Bool(INPUT)
    val mem_en = Bool(INPUT)
    val space_left = Bool(INPUT)
  }

  val cnt = Reg(init = UInt(0, width = log2Up(atosRequestBytes)))
  val buf = Reg(init = UInt(0, width = atosRequestBits))
  val (s_reset :: s_sync :: s_ack :: s_idle :: s_busy :: s_last :: Nil) = Enum(Bits(), 6)

  val state = Reg(init = s_reset)

  when (state === s_reset) {
    when (io.mem_en) {
      state := s_sync
    }
  }.elsewhen (state === s_sync) {
    // send the sync pulse as soon as we are out of reset
    state := s_ack
  } .elsewhen(state === s_ack) {
    // wait for the ack
    when(io.sync) {
      state := s_idle
    }
  } .elsewhen(state === s_idle) {
    when(~io.sync) {
      state := s_sync // lost sync
    } .elsewhen(io.in.fire()) {
      buf := io.in.bits.toBits
      state := s_busy
      cnt := UInt(0)
    }
  } .elsewhen(state === s_busy) {
    when(~io.sync) {
      state := s_sync // lost sync
    } .elsewhen(cnt === UInt(atosRequestBytes-2)) {
      state := s_last
    }
    cnt := cnt + UInt(1)
  } .elsewhen(state === s_last) {
    cnt := UInt(0)
    when(~io.sync) {
      state := s_sync // lost sync
    } .elsewhen(io.in.fire()) {
      buf := io.in.bits.toBits
      state := s_busy
    } .otherwise {
      state := s_idle
    }
  } .otherwise {
    // should never get here
    state := s_reset
  }

  // note we ignore io.out.ready since it's not "really" decoupled (ready is always 1)
  // FIXME we should use Valid instead of Decoupled here
  io.out.valid := (state === s_sync | state === s_busy | state === s_last)
  val lo = Cat(cnt, UInt(0, width=3))
  val buf_extracted = (buf >> lo) & UInt(0xff, width = 8)
  io.out.bits.bits := Mux(state === s_sync,Crc.SYNC,buf_extracted)
  io.out.bits.control := (state === s_sync)

  io.in.ready := (state === s_idle | state === s_last) & io.space_left

}


class CrcRx(implicit p: Parameters) extends HurricaneHbwifModule()(p) {

  val io = new Bundle {
    val in  = Decoupled(new SerialSymbol(8)).flip
    val out = Decoupled(new AtosResponse)
    val sync = Bool(OUTPUT)
    val err = Bool(OUTPUT)
  }


  val cnt = Reg(init = UInt(0, width = log2Up(atosResponseBytes)))
  val (s_align :: s_sync :: s_idle :: s_rcv :: s_resp :: Nil) = Enum(Bits(), 5)

  val state = Reg(init = s_align)
  val buf   = Reg(Vec(atosResponseBytes,UInt(width = 8)))

  val err   = Reg(init = Bool(false))


  when(state === s_align) {
    when(io.in.fire() & io.in.bits.isComma()) {
      state := s_sync
    }
  } .elsewhen(state === s_sync) {
    // wait for an ACK
    when(io.in.fire() & Crc.isAck(io.in.bits)) {
      state := s_idle
    }
    cnt := UInt(0)
  } .elsewhen(state === s_idle) {
    when(io.in.fire()) {
      when(io.in.bits.isComma()) {
        // keep waiting for a valid symbol
        state := s_idle
      } .elsewhen(io.in.bits.isControl()) {
        // we got some unknown control symbol, reset
        state := s_align
        err := Bool(true)
      } .otherwise {
        // got a valid data symbol
        state := s_rcv
        cnt := UInt(1)
      }
    }
  } .elsewhen(state === s_rcv) {
    when(io.in.fire() & io.in.bits.isData()) {
      // got a valid data byte
      when(cnt === UInt(atosResponseBytes-1)) {
        state := s_resp
      } .otherwise {
        state := s_rcv
        cnt := cnt + UInt(1)
      }
    } .otherwise {
      // something strange happened, reset
      // we only need this since the Serial interface isn't "really" decoupled
      state := s_align
      err := Bool(true)
      //assert(Bool(false), "Got an invalid data or control packet during a transaction")
    }

  } .elsewhen(state === s_resp) {
    when(io.out.fire()) {
      when(io.in.bits.isComma()) {
        state := s_idle
      } .elsewhen(io.in.bits.isControl()) {
        state := s_align
        err := Bool(true)
      } .elsewhen(io.in.bits.isData()) {
        // skip straight to receive with the next data
        state := s_rcv
        cnt := UInt(1)
      }
    } .elsewhen(io.in.valid & io.in.bits.isData()) {
      // this shouldn't happen, but it means something screwed up
      // if we implement error correction, this needs to be handled
      assert(Bool(false), "Got a valid data packet without a ready queue")
      err := Bool(true)
    }
  } .otherwise {
    // should never get here
    state := s_align
  }

  when(state === s_rcv | ((state === s_resp | state === s_idle) & io.in.valid & io.in.bits.isData())) {
    buf(atosResponseBytes-1) := io.in.bits.bits
    for(i <- 0 until atosResponseBytes-1) { buf(i) := buf(i+1) }
  }

  io.in.ready := UInt(1) // we cannot apply backpressure to the encoding engine

  io.out.bits := (new AtosResponse).fromBits(buf.toBits)
  io.out.valid := (state === s_resp)

  io.sync := (state === s_rcv | state === s_idle | state === s_resp)
  io.err := err

}

object Crc {
  // Define the sync control symbols (we won't use all of these yet)
  val SYNC = Consts8b10b.K_28_0
  val ACK  = Consts8b10b.K_28_4
  val NACK = Consts8b10b.K_28_3
  def isSync(k: SerialSymbol): Bool = { k.isControl() & k.bits === SYNC }
  def isAck(k: SerialSymbol): Bool = { k.isControl() & k.bits === ACK }
  def isNack(k: SerialSymbol): Bool = { k.isControl() & k.bits === NACK }
}
