// johnwright@eecs.berkeley.edu

package splash2_hbwif

import hbwif._

import Chisel._
import ChiselError._
import math._
import cde.{Parameters, Field}
import junctions.ParameterizedBundle

class Splash2Interface(implicit p: Parameters) extends Splash2HbwifBundle()(p) {
  val link = new Splash2LinkInterfaceIO
  val data = Vec.fill(splash2HbwifDataWords) { UInt(OUTPUT, width = splash2HbwifWordBits) }
}

class Splash2ADCConfig(implicit p: Parameters) extends Splash2HbwifBundle()(p) {
  val xor_select        = Bits(INPUT, width = 3)
  val inhibit           = Bool(INPUT)
  val xor_delay         = Bits(INPUT, width = log2Up(splash2HbwifMaxLatency))
  val lane_fine_skew    = Vec.fill(linkTopNumLanes) { Bits(INPUT, width = log2Up(linkTopDataBits)) }
  val lane_coarse_skew  = Vec.fill(linkTopNumLanes) { Bits(INPUT, width = splash2HbwifCoarseSkewBits) }
  val xor_divide        = UInt(INPUT, width = splash2HbwifXorCountWidth)
  val in_map            = Vec.fill(linkTopNumLanes) { Bits(INPUT, width = log2Up(linkTopNumLanes)) }
  val out_map           = Vec.fill(2) { Bits(INPUT, width = log2Up(linkTopNumLanes)) }
  val hittite_prbs_mode = UInt(INPUT, width = 2)
  val adc_xor_mode      = Bool(INPUT)
  val fine_counts       = Vec.fill(linkTopNumLanes) { Bits(OUTPUT, width = log2Up(linkTopDataBits+1)) }
  val fine_count_clear  = Bool(INPUT)
}

class Splash2ADCInterfaceIO(implicit p: Parameters) extends Splash2HbwifBundle()(p) {
  val interface = new Splash2Interface
  val config = new Splash2ADCConfig
  // reuse PRBS15
  val prbs_in = UInt(INPUT, width = linkTopDataBits)
}

class Splash2ADCInterface(implicit p: Parameters) extends Splash2HbwifModule()(p) {

  val io = new Splash2ADCInterfaceIO

  ///////////////////////////////////////////////////////
  // Define XOR signal generators
  ///////////////////////////////////////////////////////

  // define a programmable counter to divide the xor signal
  val counter = Reg(init = UInt(0, width = splash2HbwifXorCountWidth))

  when (counter === io.config.xor_divide) {
    counter := UInt(0)
  } .otherwise {
    counter := counter + UInt(1)
  }

  // instantiate a (low frequency) PRBS
  val prbs_slow = Module(new PRBS(prbsWidth = splash2HbwifPrbsWidth, parallelOutBits = 1, generatorPolynomial = splash2HbwifGenPoly))
  prbs_slow.io.seed_in := UInt(1)
  prbs_slow.io.load_in := UInt(1)
  prbs_slow.io.mode := io.config.hittite_prbs_mode

  val prbs_slow_div = Reg(init = UInt(0, width = 1))

  // square wave generator (high freq)
  // create bits ...01010101
  var fast_sq_data = 0
  for (i <- 0 until linkTopDataBits/2) {
    fast_sq_data = (fast_sq_data << 2) | 1
  }
  val square_fast = UInt(fast_sq_data)

  // square wave generator (low freq)
  val square_slow = Reg(init = UInt(0, width = 1))

  when (counter === UInt(0)) {
    square_slow := ~square_slow
    prbs_slow_div := prbs_slow.io.out(0)
  }

  ///////////////////////////////////////////////////////
  // Multiplex the signals into the xor signal
  ///////////////////////////////////////////////////////
  val slow_xor_signals = Vec.fill(2) { UInt(width = 1) }
  slow_xor_signals(0) := square_slow
  slow_xor_signals(1) := prbs_slow_div

  val fast_xor_signals = Vec.fill(2) { UInt(width = linkTopDataBits) }
  fast_xor_signals(0) := square_fast
  fast_xor_signals(1) := io.prbs_in

  val slow_xor_signal_out = slow_xor_signals(io.config.xor_select(0))
  val fast_xor_signal_out = fast_xor_signals(io.config.xor_select(0))

  // outputs to the tx
  val xor = Mux(io.config.xor_select(1),Fill(linkTopDataBits, slow_xor_signal_out),fast_xor_signal_out)
  val inhibit = Fill(linkTopDataBits,io.config.inhibit)

  // config.out_map(0) sets xor, config.out_map(1) sets inhibit
  for (i <- 0 until linkTopNumLanes) {
    when (io.config.out_map(0) === UInt(i)) {
      io.interface.link.tx(i) := xor
    } .elsewhen (io.config.out_map(1) === UInt(i)) {
      io.interface.link.tx(i) := inhibit
    } .otherwise {
      io.interface.link.tx(i) := UInt(0)
    }
  }

  ///////////////////////////////////////////////////////
  // Per-lane manual skew compensation
  ///////////////////////////////////////////////////////

  // alignment buffer allows aligning bits within a frame
  val alignment_buffer = Vec.fill(linkTopNumLanes) { Reg(init = UInt(0, width = linkTopDataBits-1)) }
  val alignment_wires = Vec.fill(linkTopNumLanes) { UInt(width = 2*linkTopDataBits-1) }

  // connect the alignment_buffer to the MSB-1 to 0 bits of the rx (i.e. delay by a cycle)
  alignment_buffer.zip(io.interface.link.rx).foreach { x =>
    x._1 := x._2(linkTopDataBits-1,1)
  }

  // create the alignment wires that get selected by the io.config.lane_fine_skew input
  alignment_wires.zipWithIndex.foreach { x =>
    x._1 := Cat(io.interface.link.rx(x._2),alignment_buffer(x._2))
  }

  // coarse buffer allows skew and latency compensation with aligned frames
  val coarse_buffer = Vec.fill(linkTopNumLanes) { Vec.fill(1 << splash2HbwifCoarseSkewBits) { Reg(init = UInt(0, width = linkTopDataBits)) } }

  // implement the muxing logic into the coarse shift register
  alignment_wires.zip(coarse_buffer).zip(io.config.lane_fine_skew).foreach { x =>
    var tmp = when (x._2 === UInt(0)) {
      x._1._2(0) := x._1._1(linkTopDataBits-1,0)
    }
    for (i <- 1 until linkTopDataBits) {
      tmp.elsewhen (x._2 === UInt(i)) {
        x._1._2(0) := x._1._1(linkTopDataBits+i-1,i)
      }
    }
  }

  // shift
  coarse_buffer.foreach { x =>
    x.slice(1,(1 << splash2HbwifCoarseSkewBits)).zipWithIndex.foreach { y =>
      y._1 := x(y._2)
    }
  }

  // select which reg out of the coarse buffer to output to the xor
  val modulated_words = Vec.fill(linkTopNumLanes) { UInt(width = linkTopDataBits) }

  coarse_buffer.zip(modulated_words).zip(io.config.lane_coarse_skew).foreach { x =>
    var tmp = when (x._2 === UInt(0)) {
      x._1._2 := x._1._1(0)
    }
    for (i <- 1 until (1 << splash2HbwifCoarseSkewBits)) {
      tmp.elsewhen (x._2 === UInt(i)) {
        x._1._2 := x._1._1(i)
      }
    }
    tmp.otherwise {
      // who cares
      x._1._2 := UInt(0)
    }
  }

  // add some counters to provide feedback about alignment
  val fine_counts = Vec.fill(linkTopNumLanes) { Reg(UInt(width = log2Up(linkTopDataBits+1))) }
  when (io.config.fine_count_clear) {
    fine_counts.foreach { x => x := UInt(0) }
  } .otherwise {
    fine_counts.zip(modulated_words).foreach { x =>
      when (x._1 === UInt(0)) {
        x._1 := PopCount(x._2)
      }
    }
  }
  io.config.fine_counts := fine_counts

  ///////////////////////////////////////////////////////
  // XOR and XOR delay (slow mode)
  ///////////////////////////////////////////////////////

  // xor delay buffer
  val slow_xor_delay_buffer = Vec.fill(splash2HbwifMaxLatency) { Reg(init = UInt(0, width = 1)) }
  val slow_xor_delay_out = slow_xor_delay_buffer(io.config.xor_delay)

  slow_xor_delay_buffer(0) := slow_xor_signal_out
  for (i <- 1 until splash2HbwifMaxLatency) { slow_xor_delay_buffer(i) := slow_xor_delay_buffer(i-1) }

  val slow_demodulated_out = Vec.fill(linkTopNumLanes) { UInt() }
  slow_demodulated_out.zip(modulated_words).foreach { x => x._1 := x._2 ^ Fill(linkTopDataBits, slow_xor_delay_out) }
  val demodulated_out = slow_demodulated_out

  ///////////////////////////////////////////////////////
  // Apply the lane map and output packaged words
  ///////////////////////////////////////////////////////
  // X = first sample (3:0)
  // Y = second sample (7:4)

  for (i <- 0 until linkTopDataBits) {
    var word0 = UInt(0, width = 4)
    var word1 = UInt(0, width = 4)
    for (j <- 0 until linkTopNumLanes) {
      var map = io.config.in_map(j)
      var bits = demodulated_out(j)
      word0 |= Cat(bits(i) & (map === UInt(3)), bits(i) & (map === UInt(2)), bits(i) & (map === UInt(1)), bits(i) & (map === UInt(0)))
      word1 |= Cat(bits(i) & (map === UInt(7)), bits(i) & (map === UInt(6)), bits(i) & (map === UInt(5)), bits(i) & (map === UInt(4)))
    }
    io.interface.data(2*i) := word0
    io.interface.data(2*i + 1) := word1
  }

}
