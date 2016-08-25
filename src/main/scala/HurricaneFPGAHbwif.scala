// johnwright@eecs.berkeley.edu
//
// This is a fake AXI deserializer for testing the HBWIF links
//
// TODO: wire up en

package hurricane_hbwif
import hbwif._

import Chisel._
import ChiselError._
import cde.{Parameters, Field}
import junctions._
import uncore.HasHtifParameters
import Serial._

class FPGAQuadIO extends Bundle {
  val txp = Bool(OUTPUT)
  val txn = Bool(OUTPUT)
  val rxp = Bool(INPUT)
  val rxn = Bool(INPUT)
}

class HurricaneFPGAHbwifIO(implicit p: Parameters) extends HurricaneHbwifBundle()(p) {
  val pad = Vec(hbwifNumLanes, new FPGAQuadIO)
  val fast_clk = Bool(INPUT)
  val mem = Vec(hbwifNMemChannels, new NastiIO)
}

class HurricaneFPGAHbwif(implicit p: Parameters) extends HurricaneHbwifModule()(p) {
  val io = new HurricaneFPGAHbwifIO

  val lane = List.fill(hbwifNumLanes) { Module(new HurricaneFPGAHbwifLane) }

  for(i <- 0 until hbwifNumLanes) {
    lane(i).io.pad <> io.pad(i)
    lane(i).io.fast_clk <> io.fast_clk
  }

  // Connect two Nasti interfaces with queues in-between
  def connectNasti(outer: NastiIO, inner: NastiIO)(implicit p: Parameters) {
    val mifDataBeats = p(MIFDataBeats)*8
    outer.ar <> Queue(inner.ar)
    outer.aw <> Queue(inner.aw)
    outer.w  <> Queue(inner.w, mifDataBeats)
    inner.r  <> Queue(outer.r, mifDataBeats)
    inner.b  <> Queue(outer.b)
  }

  for(i <- 0 until hbwifNMemChannels) {
    connectNasti(io.mem(i), lane(hbwifLaneMap(i)).io.mem)
  }
}

class HurricaneFPGAHbwifLane(implicit p: Parameters) extends HurricaneHbwifModule()(p) {
  val io = new Bundle {
    val pad = new FPGAQuadIO
    val en = Bool(INPUT)
    val fast_clk = Bool(INPUT)
    val mem = new NastiIO
  }

  // AtoS
  val atos = Module(new AtosManagerConverter)
  atos.io.nasti <> io.mem

  // Crc
  val crc = Module(new FPGACrc)
  crc.io.atos <> atos.io.atos

  // 8b/10b
  val codec = Module(new Channel8b10bController)
  codec.io.ctl <> crc.io.crc
  codec.io.on  := UInt(1)
  codec.io.lock := crc.io.sync

  // SerDes
  val serdes = Module(new SerDes(divideBy = 5))
  serdes.io.tx_in      := codec.io.phy.tx_data
  codec.io.phy.rx_data := serdes.io.rx_out
  serdes.io.clks       := io.fast_clk
  serdes.io.reset      := reset

  // Unused SerDes ports
  serdes.io.config_rx_edge_sel := UInt(0)
  serdes.io.config_rx_sel      := UInt(0)
  serdes.io.rx_in(1)           := UInt(0)
  serdes.io.rx_in(2)           := UInt(0)

  // Transceiver
  val phy = Module(new XilinxPhy)
  phy.io.fast_clk := io.fast_clk
  phy.io.pad      <> io.pad
  phy.io.tx       <> serdes.io.tx_out
  phy.io.rx       <> serdes.io.rx_in(0)


}

class FPGACrc(implicit p: Parameters) extends HurricaneHbwifModule()(p) {
  val io = new Bundle {
    val atos = new AtosIO()
    val crc = new SerialChannel(8).flip
    val sync = Bool(OUTPUT)
  }

  // This just needs to be large enough to buffer the maximum number of AtosRequests
  // Since this isn't going on the chip, just make it arbitrarily large
  val buf = Module(new Queue(new AtosRequest, 16*hbwifBufferDepth, false, false))

  val tx = Module(new FPGACrcTx)
  val rx = Module(new FPGACrcRx)

  tx.io.sync := rx.io.sync
  io.sync := rx.io.sync

  io.atos.req <> buf.io.deq
  buf.io.enq  <> rx.io.out
  io.crc.rx   <> rx.io.in

  io.atos.resp <> tx.io.in
  io.crc.tx    <> tx.io.out

}

class FPGACrcTx(implicit p: Parameters) extends HurricaneHbwifModule()(p) {
  val io = new Bundle {
    val in  = Decoupled(new AtosResponse).flip
    val out = Decoupled(new SerialSymbol(8))
    val sync = Bool(INPUT)
  }

  val cnt = Reg(init = UInt(0, width = log2Up(atosResponseBytes)))
  val buf = Reg(init = UInt(0, width = atosResponseBits))
  val (s_sync :: s_ack :: s_idle :: s_busy :: s_last :: Nil) = Enum(Bits(), 5)

  val state = Reg(init = s_sync)

  when(state === s_sync) {
    // wait for SYNC and then send ACK
    when(io.sync) {
      state := s_ack
    }
  } .elsewhen(state === s_ack) {
    // send ACK
    state := s_idle
  } .elsewhen(state === s_idle) {
    when(io.in.fire()) {
      buf := io.in.bits.toBits
      state := s_busy
      cnt := UInt(0)
    }
  } .elsewhen(state === s_busy) {
    when(cnt === UInt(atosResponseBytes-2)) {
      state := s_last
    }
    cnt := cnt + UInt(1)
  } .elsewhen(state === s_last) {
    cnt := UInt(0)
    when(io.in.fire()) {
      buf := io.in.bits.toBits
      state := s_busy
    } .otherwise {
      state := s_idle
    }
  } .otherwise {
    // should never get here
    state := s_sync
  }

  // note we ignore io.out.ready since it's not "really" decoupled (ready is always 1)
  // FIXME we should use Valid instead of Decoupled here
  io.out.valid := (state === s_ack | state === s_busy | state === s_last)
  val lo = Cat(cnt, UInt(0, width=3))
  val extracted_buf = (buf >> lo) & UInt(0xff, width = 8)
  io.out.bits.bits := Mux(state === s_ack,Crc.ACK,extracted_buf)
  io.out.bits.control := (state === s_ack)

  io.in.ready := (state === s_idle | state === s_last)

}

class FPGACrcRx(implicit p: Parameters) extends HurricaneHbwifModule()(p) {
  val io = new Bundle {
    val in  = Decoupled(new SerialSymbol(8)).flip
    val out = Decoupled(new AtosRequest)
    val sync = Bool(OUTPUT)
  }

  val cnt = Reg(init = UInt(0, width = log2Up(atosRequestBytes)))
  val (s_align :: s_sync :: s_idle :: s_rcv :: s_resp :: Nil) = Enum(Bits(), 5)

  val state = Reg(init = s_align)
  val buf   = Reg(Vec(atosRequestBytes,UInt(width = 8)))

  when(state === s_align) {
    when(io.in.fire() & io.in.bits.isComma()) {
      state := s_sync
    }
  } .elsewhen(state === s_sync) {
    when(io.in.fire() & Crc.isSync(io.in.bits)) {
      state := s_idle
    }
    cnt := UInt(0)
  } .elsewhen(state === s_idle) {
    when(io.in.fire()) {
      when(io.in.bits.isComma()) {
        // keep waiting for a valid symbol
        state := s_idle
      } .elsewhen(io.in.bits.isControl()) {
        // we got some unknown control symbol, back to idle
        state := s_align
      } .otherwise {
        // got a valid data symbol
        state := s_rcv
        cnt := UInt(1)
      }
    }
  } .elsewhen(state === s_rcv) {
    when(io.in.fire() & io.in.bits.isData()) {
      // got a valid data byte
      when(cnt === UInt(atosRequestBytes-1)) {
        state := s_resp
      } .otherwise {
        state := s_rcv
        cnt := cnt + UInt(1)
      }
    } .otherwise {
      // something strange happened, reset
      // we only need this since the Serial interface isn't "really" decoupled
      state := s_align
    }

  } .elsewhen(state === s_resp) {
    when(io.out.fire()) {
      when(io.in.bits.isComma()) {
        state := s_idle
      } .elsewhen(io.in.bits.isControl()) {
        state := s_align
      } .elsewhen(io.in.bits.isData()) {
        // skip straight to receive with the next data
        state := s_rcv
        cnt := UInt(1)
      }
    } .elsewhen(io.in.valid & io.in.bits.isData()) {
      // this shouldn't happen, but it means something screwed up
      // if we implement error correction, this needs to be handled
      //assert(Bool(false), "Got a valid data packet without a ready queue")
      state := s_align
    }
  } .otherwise {
    // should never get here
    state := s_align
  }

  when(state === s_rcv | ((state === s_resp | state === s_idle) & io.in.valid & io.in.bits.isData())) {
    buf(atosRequestBytes-1) := io.in.bits.bits
    for(i <- 0 until atosRequestBytes-1) { buf(i) := buf(i+1) }
  }

  io.in.ready := UInt(1) // we cannot apply backpressure to the encoding engine

  io.out.bits := (new AtosRequest).fromBits(buf.toBits)
  io.out.valid := (state === s_resp)

  io.sync := (state === s_idle | state === s_rcv | state === s_resp)

}

class XilinxPhy extends BlackBox {
  val io = new Bundle {
    val fast_clk = Bool(INPUT)
    val pad = new FPGAQuadIO
    val tx = Bits(INPUT, width = 2)
    val rx = Bits(OUTPUT, width = 2)
  }

  moduleName = "xilinx_phy"
}

