// johnwright@eecs.berkeley.edu

package hbwif

import Chisel._
import ChiselError._
import cde.{Parameters, Field}
import junctions.ParameterizedBundle
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

case object BertKey extends Field[BertParameters]

case class BertParameters(
  countWidth: Int = 45,
  snapshotWidth: Int = 32,
  shutoffPoints: Int = 3,
  dataWidth: Int = 16,
  shutoffSpacing: Int = 15,
  numWays: Int = 2)

trait HasBertParameters {
  implicit val p: Parameters
  val bertCountWidth = p(BertKey).countWidth
  val bertSnapshotWidth = p(BertKey).snapshotWidth
  val bertDataWidth = p(BertKey).dataWidth
  val bertShutoffPoints = p(BertKey).shutoffPoints
  val bertShutoffSpacing = p(BertKey).shutoffSpacing
  val bertNumWays  = p(BertKey).numWays
}

abstract class BertModule(implicit val p: Parameters) extends Module
  with HasBertParameters
abstract class BertBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasBertParameters

class BertIO(implicit p: Parameters) extends BertBundle()(p) {
    val enable             = Bool(INPUT)
    val clear              = Bool(INPUT)
    val snapshot_en        = Bool(INPUT)
    val data_in            = Bits(INPUT, width = bertDataWidth)
    val prbs_load_data     = Bits(INPUT, width = 31)
    val prbs_mode          = Bits(INPUT, width = 2)
    val shutoff_select     = Bits(INPUT, width = log2Up(bertShutoffPoints))
    val prbs_select        = Bits(INPUT, width = 2)
    val ber_mode           = Bool(INPUT)
    val seed_good          = Bool(OUTPUT)
    val error_counts       = Vec.fill(bertNumWays) { UInt(OUTPUT, width = bertCountWidth) }
    val bit_count          = UInt(OUTPUT, width = bertCountWidth)
    val snapshot           = Bits(OUTPUT, width = bertSnapshotWidth)
}

class Bert()(implicit p: Parameters) extends BertModule()(p) {

  val io = new BertIO

  // track the bit error count
  val error_counts = Vec.fill(bertNumWays) { Reg(UInt(width = bertCountWidth), init = UInt(0)) }
  io.error_counts := error_counts

  // track the bit count
  val bit_count = Reg(UInt(width = bertCountWidth), init = UInt(0))
  io.bit_count := bit_count
  val next_bit_count = bit_count + UInt(bertDataWidth/bertNumWays)

  // correct data
  val correct_bits = Vec.fill(bertNumWays) { UInt(width = bertDataWidth/bertNumWays) }

  // test data
  val test_bits = Vec.fill(bertNumWays) { Reg(UInt(width = bertDataWidth/bertNumWays)) }

  val data_in_vec = Vec.fill(bertNumWays) { UInt(width = bertDataWidth/bertNumWays) }
  for (i <- 0 until bertNumWays) {
    var tmpwire = UInt(io.data_in(i), width = 1)
    for (j <- bertNumWays until bertDataWidth) {
      if (j % bertNumWays == i) {
        tmpwire = Cat(UInt(io.data_in(j), width = 1),tmpwire)
      }
    }
    data_in_vec(i) := tmpwire
    test_bits(i) := data_in_vec(i)
  }

  // delay enable and clear
  val enable_d = Reg(init = Bool(false))
  val clear_d = Reg(next = io.clear, init = Bool(false))
  val shutoff_wires = Vec.fill(bertShutoffPoints) { Bool() }

  // shutoff selector
  for(i <- 0 until bertShutoffPoints) {
    shutoff_wires(i) := next_bit_count(bertCountWidth-i*bertShutoffSpacing-1) & (io.shutoff_select === UInt(i))
  }
  //enable_d := io.enable & shutoff_wires.foldLeft(Bool(false))( _ | _ )
  enable_d := io.enable & ~orR(shutoff_wires.toBits)

  // bitwise errors
  val bit_errors = Vec.fill(bertNumWays) { UInt(width = bertDataWidth/bertNumWays) }
  // total error count on this cycle
  var cycle_error_counts = Vec.fill(bertNumWays) { UInt(width = log2Up(bertDataWidth/bertNumWays+1)) }
  for (i <- 0 until bertNumWays) {
    bit_errors(i) := Mux(io.ber_mode, (correct_bits(i) ^ test_bits(i)), test_bits(i))
    cycle_error_counts(i) := PopCount(bit_errors(i))
  }


  // snapshot shift register
  val snapshot = Reg(UInt(width = bertSnapshotWidth), init = UInt(0))
  io.snapshot := snapshot

  // instantiate the PRBS31s
  val prbs31s = List.fill(bertNumWays) { Module(new PRBS(prbsWidth = 31, parallelOutBits = bertDataWidth/bertNumWays, generatorPolynomial = 0x09)) }
  prbs31s.zipWithIndex.foreach { x =>
    x._1.io.mode := io.prbs_mode
    x._1.io.load_in := io.prbs_load_data
    x._1.io.seed_in := data_in_vec(x._2)
  }

  // instantiate the PRBS15s
  val prbs15s = List.fill(bertNumWays) { Module(new PRBS(prbsWidth = 15, parallelOutBits = bertDataWidth/bertNumWays, generatorPolynomial = 0x03)) }
  prbs15s.zipWithIndex.foreach { x =>
    x._1.io.mode := io.prbs_mode
    x._1.io.load_in := io.prbs_load_data(14,0)
    x._1.io.seed_in := data_in_vec(x._2)
  }

  // instantiate the PRBS7s
  val prbs7s = List.fill(bertNumWays) { Module(new PRBS(prbsWidth = 7, parallelOutBits = bertDataWidth/bertNumWays, generatorPolynomial = 0x03)) }
  prbs7s.zipWithIndex.foreach { x =>
    x._1.io.mode := io.prbs_mode
    x._1.io.load_in := io.prbs_load_data(7,0)
    x._1.io.seed_in := data_in_vec(x._2)
  }

  // output Muxes
  io.seed_good := Mux(io.prbs_select(0),
                  Mux(io.prbs_select(1),prbs15s.map(_.io.seed_good).foldLeft(Bool(true))(_&_),prbs31s.map(_.io.seed_good).foldLeft(Bool(true))(_&_))
                  ,prbs7s.map(_.io.seed_good).foldLeft(Bool(true))(_&_))
  for(i <- 0 until bertNumWays) {
    correct_bits(i) := Mux(io.prbs_select(0),
                       Mux(io.prbs_select(1),prbs15s(i).io.out,prbs31s(i).io.out),
                       prbs7s(i).io.out)
  }

  // count errors
  when (clear_d) {
    error_counts := Vec.fill(bertNumWays) { UInt(0) }
    bit_count := UInt(0)
  } .elsewhen (enable_d) {
    error_counts.zip(cycle_error_counts).foreach { x => x._1 := x._1 + x._2 }
    bit_count := next_bit_count
  }

  // snapshot
  when (io.snapshot_en) {
    snapshot := Cat(snapshot(bertSnapshotWidth-1-bertDataWidth,0), io.data_in)
  }

}

/* TODO this is broken after the change to CDE-style Chisel
class BertTester(c: Bert, errors: Boolean, prbs7: Boolean, threshold: Double = 3.0, shutoff_test: Boolean = false) extends Tester(c) {
  def run_prbs(iterations: Int) {
    for (i <- 0 until iterations) {
      var error_mask = 0
      if (shutoff_test & total_bits >= shutoff_threshold) is_shutoff = true
      if (shutoff_test & total_bits >= shutoff_threshold) println(total_bits)
      for (j <- 0 until c.bertDataWidth/c.bertNumWays) {
        for (k <- 0 until c.bertNumWays) {
          var masked_bits = (lfsr_value & generator_polynomial)
          var new_bit = 0
          var error_mask = 0
          for (k <- 0 until prbs_width) {
            new_bit ^= (masked_bits & 1)
            masked_bits = masked_bits >>> 1
          }
          if (errors && (rand.nextGaussian > threshold)) {
            // introduce errors
            error_mask = 1
            if(!first_cycle & !is_shutoff) error_counts(k) = error_counts(k) + 1
          }
          test_bits = test_bits >>> 1
          test_bits |= ((lfsr_value ^ error_mask) & 1) << (c.bertDataWidth-1)
          lfsr_value = lfsr_value >>> 1
          lfsr_value |= (new_bit << (prbs_width-1))
          if(!first_cycle & !is_shutoff) total_bits += 1
        }
      }
      poke(c.io.data_in,test_bits)
      snapshot = snapshot << c.bertDataWidth
      snapshot |= (test_bits & ((1 << (c.bertSnapshotWidth - c.bertDataWidth))-1))
      var test_bits_tmp = test_bits
      for (j <- 0 until c.bertDataWidth/c.bertNumWays) {
        for (k <- 0 until c.bertNumWays) {
          if(!first_cycle & !is_shutoff) one_counts(k) = one_counts(k) + (test_bits_tmp & 1)
          test_bits_tmp = test_bits_tmp >>> 1
        }
      }
      step(1)
      if(errors) {
        for (i <- 0 until c.bertNumWays) { expect(c.io.error_counts(i), error_counts(i)) }
      } else {
        for (i <- 0 until c.bertNumWays) { expect(c.io.error_counts(i), one_counts(i)) }
      }
      expect(c.io.bit_count,total_bits)
      expect(c.io.snapshot,snapshot)
      first_cycle = false
    }
  }
  var prbs_width = 31
  var generator_polynomial = 0x09
  if(prbs7) {
    prbs_width = 7
    generator_polynomial = 0x03
  }
  var seed = 0xaff1 // some arbitrary bits that aren't zero
  var lfsr_value = seed
  if (c.bertDataWidth > prbs_width) {
    lfsr_value = (seed & ((1 << c.bertDataWidth) - 1)) >>> (c.bertDataWidth - prbs_width)
  } else {
    lfsr_value = (seed & ((1 << c.bertDataWidth) - 1)) << (prbs_width - c.bertDataWidth)
  }
  var test_bits = (seed & ((1 << c.bertDataWidth) - 1))
  var total_bits = 0
  var rand = Random
  var error_counts = ArrayBuffer[Int](c.bertNumWays)
  var one_counts = ArrayBuffer[Int](c.bertNumWays)
  var snapshot = 0
  var first_cycle = true
  var is_shutoff = false
  var shutoff_threshold = 1 << (c.bertCountWidth - c.bertShutoffSpacing*(c.bertShutoffPoints - 1) - 1)
  if(errors) {
    poke(c.io.ber_mode, 1)
  } else {
    poke(c.io.ber_mode, 0)
  }
  poke(c.io.prbs_mode, c.prbs7s(0).SEED)
  poke(c.io.data_in, seed)
  poke(c.io.prbs_select, if(prbs7) 1 else 0)
  poke(c.io.enable, 0)
  if(shutoff_test) {
    // pick the smallest number
    poke(c.io.shutoff_select, c.bertShutoffPoints-1)
  } else {
    poke(c.io.shutoff_select, 0)
  }
  for (j <- 0 until c.bertDataWidth) {
    var masked_bits = (lfsr_value & generator_polynomial)
    var new_bit = 0
    for (k <- 0 until prbs_width) {
      new_bit ^= (masked_bits & 1)
      masked_bits = masked_bits >>> 1
    }
    lfsr_value = lfsr_value >>> 1
    lfsr_value |= (new_bit << (prbs_width-1))
  }
  step(1)
  poke(c.io.snapshot_en, 1)
  poke(c.io.enable, 1)
  poke(c.io.prbs_mode, c.prbs7s(0).RUN)
  if(shutoff_test) {
    run_prbs(3000)
  } else {
    run_prbs(200)
  }
  poke(c.io.prbs_mode, c.prbs7s(0).LOAD)
  poke(c.io.enable, 0)
  step(1)
  poke(c.io.clear, 1)
  step(2)
  for (i <- 0 until c.bertNumWays) {
    expect(c.io.error_counts(i), 0)
  }
  expect(c.io.bit_count, 0)
  step(1)

}

object BertMain {
  def main(args: Array[String]): Unit = {

    println("Noise-Free 1-bit Test (PRBS7)")
    chiselMainTest(args, () =>
      Module(new Bert(1,41,32,3,15,1))) {
      c => new BertTester(c,false,true,1,false)
    }

    println("Noise-Free 16-bit Test (PRBS7)")
    chiselMainTest(args, () =>
      Module(new Bert(16,41,32))) {
      c => new BertTester(c,false,true,1,false)
    }

    println("Noisy 16-bit Test (PRBS7)")
    chiselMainTest(args, () =>
      Module(new Bert(16,41,32))) {
      c => new BertTester(c,true,true,1,false)
    }

    println("Noise-Free 1-bit Test (PRBS31)")
    chiselMainTest(args, () =>
      Module(new Bert(1,41,32,3,15,1))) {
      c => new BertTester(c,false,false,1,false)
    }

    println("Noise-Free 16-bit Test (PRBS31)")
    chiselMainTest(args, () =>
      Module(new Bert(16,41,32))) {
      c => new BertTester(c,false,false,1,false)
    }

    println("Noisy 16-bit Test (PRBS31)")
    chiselMainTest(args, () =>
      Module(new Bert(16,41,32))) {
      c => new BertTester(c,true,false,1,false)
    }

    println("Noisy 16-bit Test (PRBS31) with shutoff")
    chiselMainTest(args, () =>
      Module(new Bert(16,45,32,3,15))) {
      c => new BertTester(c,true,false,1,true)
    }

  }
}
*/
