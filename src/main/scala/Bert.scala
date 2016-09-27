package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle
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
  val error_counts = Reg(init = Vec.fill(bertNumWays) { UInt(0, width = bertCountWidth) })
  io.error_counts := error_counts

  // track the bit count
  val bit_count = Reg(UInt(width = bertCountWidth), init = UInt(0))
  io.bit_count := bit_count
  val next_bit_count = bit_count + UInt(bertDataWidth/bertNumWays)

  // correct data
  val correct_bits = Wire(Vec.fill(bertNumWays) { UInt(width = bertDataWidth/bertNumWays) })

  // test data
  val test_bits = Wire(Vec.fill(bertNumWays) { Reg(UInt(width = bertDataWidth/bertNumWays)) })

  val data_in_vec = Wire(Vec.fill(bertNumWays) { UInt(width = bertDataWidth/bertNumWays) })
  for (i <- 0 until bertNumWays) {
    val tmp = Cat((i until bertDataWidth by bertNumWays).map(j => io.data_in(j)).reverse)
    data_in_vec(i) := tmp
    test_bits(i) := tmp
  }

  // delay enable and clear
  val enable_d = Reg(init = Bool(false))
  val clear_d = Reg(next = io.clear, init = Bool(false))
  val shutoff_wires = ArrayBuffer.fill(bertShutoffPoints) { Wire(Bool()) }

  // shutoff selector
  for(i <- 0 until bertShutoffPoints) {
    shutoff_wires(i) := next_bit_count(bertCountWidth-i*bertShutoffSpacing-1) & (io.shutoff_select === UInt(i))
  }
  enable_d := io.enable & ~shutoff_wires.foldLeft(Bool(false))( _ | _ )

  // bitwise errors
  val bit_errors = Wire(Vec.fill(bertNumWays) { UInt(width = bertDataWidth/bertNumWays) })
  // total error count on this cycle
  var cycle_error_counts = Wire(Vec.fill(bertNumWays) { UInt(width = log2Up(bertDataWidth/bertNumWays+1)) })
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
