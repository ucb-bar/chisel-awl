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
  shutoffSpacing: Int = 15,
  numWays: Int = 2)

trait HasBertParameters extends HasTransceiverParameters{
  implicit val p: Parameters
  val bertCountWidth = p(BertKey).countWidth
  val bertSnapshotWidth = p(BertKey).snapshotWidth
  val bertShutoffPoints = p(BertKey).shutoffPoints
  val bertDataWidth = transceiverDataWidth
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
    val snapshotEn         = Bool(INPUT)
    val dataIn             = UInt(INPUT, width = bertDataWidth)
    val dataOut            = UInt(OUTPUT, width = bertDataWidth)
    val rxPRBSLoadData     = UInt(INPUT, width = 31)
    val rxPRBSMode         = UInt(INPUT, width = 2)
    val txPRBSLoadData     = UInt(INPUT, width = 31)
    val txPRBSMode         = UInt(INPUT, width = 2)
    val shutoffSelect      = UInt(INPUT, width = log2Up(bertShutoffPoints))
    val prbsSelect         = UInt(INPUT, width = 2)
    val berMode            = Bool(INPUT)
    val seedGood           = Bool(OUTPUT)
    val errorCounts        = Vec(bertNumWays, UInt(OUTPUT, width = bertCountWidth))
    val bitCount           = UInt(OUTPUT, width = bertCountWidth)
    val snapshot           = UInt(OUTPUT, width = bertSnapshotWidth)
}

class Bert()(implicit p: Parameters) extends BertModule()(p) {

  val io = new BertIO

  // track the bit error count
  val errorCounts = Reg(init = Vec.fill(bertNumWays) { UInt(0, width = bertCountWidth) } )
  io.errorCounts := errorCounts

  // track the bit count
  val bitCount = Reg(init = Wire(UInt(width = bertCountWidth), init = UInt(0)))
  io.bitCount := bitCount
  val nextBitCount = bitCount + UInt(bertDataWidth/bertNumWays)

  // correct data
  val correctBits = Wire(Vec(bertNumWays, UInt(width = bertDataWidth/bertNumWays)))

  // test data
  val testBits = Wire(Vec(bertNumWays, UInt(width = bertDataWidth/bertNumWays)))

  val dataInVec = Wire(Vec(bertNumWays, UInt(width = bertDataWidth/bertNumWays)))
  for (i <- 0 until bertNumWays) {
    val tmp = Cat((i until bertDataWidth by bertNumWays).map(j => io.dataIn(j)).reverse)
    dataInVec(i) := tmp
    testBits(i) := tmp
  }

  // delay enable and clear
  val enable_d = Reg(init = Bool(false))
  val clear_d = Reg(next = io.clear, init = Bool(false))
  val shutoffWires = ArrayBuffer.fill(bertShutoffPoints) { Wire(Bool()) }

  // shutoff selector
  for(i <- 0 until bertShutoffPoints) {
    shutoffWires(i) := nextBitCount(bertCountWidth-i*bertShutoffSpacing-1) & (io.shutoffSelect === UInt(i))
  }
  enable_d := io.enable & ~shutoffWires.foldLeft(Bool(false))( _ | _ )

  // bitwise errors
  val bitErrors = Wire(Vec(bertNumWays, UInt(width = bertDataWidth/bertNumWays)))
  // total error count on this cycle
  var cycleErrorCounts = Wire(Vec(bertNumWays, UInt(width = log2Up(bertDataWidth/bertNumWays+1))))
  for (i <- 0 until bertNumWays) {
    bitErrors(i) := Mux(io.berMode, (correctBits(i) ^ testBits(i)), testBits(i))
    cycleErrorCounts(i) := PopCount(bitErrors(i))
  }


  // snapshot shift register
  val snapshot = Reg(UInt(width = bertSnapshotWidth), init = UInt(0))
  io.snapshot := snapshot

  // instantiate the PRBS31s
  val prbs31s = List.fill(bertNumWays) { PRBS31(bertDataWidth/bertNumWays) }
  prbs31s.zipWithIndex.foreach { x =>
    x._1.io.mode := io.rxPRBSMode
    x._1.io.loadIn := io.rxPRBSLoadData
    x._1.io.seedIn := dataInVec(x._2)
  }
  val txPRBS31 = PRBS31(bertDataWidth)
  txPRBS31.io.mode   := io.txPRBSMode
  txPRBS31.io.loadIn := io.txPRBSLoadData
  txPRBS31.io.seedIn := UInt(1)

  // instantiate the PRBS15s
  val prbs15s = List.fill(bertNumWays) { PRBS15(bertDataWidth/bertNumWays) }
  prbs15s.zipWithIndex.foreach { x =>
    x._1.io.mode := io.rxPRBSMode
    x._1.io.loadIn := io.rxPRBSLoadData(14,0)
    x._1.io.seedIn := dataInVec(x._2)
  }
  val txPRBS15 = PRBS15(bertDataWidth)
  txPRBS15.io.mode   := io.txPRBSMode
  txPRBS15.io.loadIn := io.txPRBSLoadData(14,0)
  txPRBS15.io.seedIn := UInt(1)

  // instantiate the PRBS7s
  val prbs7s = List.fill(bertNumWays) { PRBS7(bertDataWidth/bertNumWays) }
  prbs7s.zipWithIndex.foreach { x =>
    x._1.io.mode := io.rxPRBSMode
    x._1.io.loadIn := io.rxPRBSLoadData(7,0)
    x._1.io.seedIn := dataInVec(x._2)
  }
  val txPRBS7 = PRBS7(bertDataWidth)
  txPRBS7.io.mode   := io.txPRBSMode
  txPRBS7.io.loadIn := io.txPRBSLoadData(7,0)
  txPRBS7.io.seedIn := UInt(1)

  io.dataOut := Mux(io.prbsSelect(0),
                Mux(io.prbsSelect(1), txPRBS15.io.out, txPRBS31.io.out),
                    txPRBS7.io.out)


  // output Muxes
  io.seedGood := Mux(io.prbsSelect(0),
                 Mux(io.prbsSelect(1),prbs15s.map(_.io.seedGood).foldLeft(Bool(true))(_&_),prbs31s.map(_.io.seedGood).foldLeft(Bool(true))(_&_))
                 ,prbs7s.map(_.io.seedGood).foldLeft(Bool(true))(_&_))
  for(i <- 0 until bertNumWays) {
    correctBits(i) := Mux(io.prbsSelect(0),
                      Mux(io.prbsSelect(1),prbs15s(i).io.out,prbs31s(i).io.out),
                      prbs7s(i).io.out)
  }

  // count errors
  when (clear_d) {
    errorCounts.foreach { _ := UInt(0) }
    bitCount := UInt(0)
  } .elsewhen (enable_d) {
    errorCounts.zip(cycleErrorCounts).foreach { x => x._1 := x._1 + x._2 }
    bitCount := nextBitCount
  }

  // snapshot
  when (io.snapshotEn) {
    snapshot := Cat(snapshot(bertSnapshotWidth-1-bertDataWidth,0), io.dataIn)
  }

}
