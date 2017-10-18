package hbwif2

import chisel3._
import chisel3.experimental._

class Differential extends Bundle {
  val p = Analog(1.W)
  val n = Analog(1.W)
}

class TransceiverIO(
  dataWidth: Int,
  transceiverNumIrefs: Int,
  cdrIWidth: Int,
  cdrPWidth: Int,
  dfeNumTaps: Int,
  dfeTapWidth: Int,
  dLevDACWidth: Int
) extends Bundle {

  // high speed clock input
  val fastClock = Input(Clock())

  // low speed clock output
  val dataClock = Output(Clock())

  // reset
  val resetIn = Input(Bool())
  val resetOut = Output(Bool())

  // RX pad inputs
  val rx = Flipped(new Differential)

  // TX pad outputs
  val tx = new Differential

  // internal data interface
  val dataDLev = Output(UInt(dataWidth.W))
  val dataRx = Output(UInt(dataWidth.W))
  val dataTx = Input(UInt(dataWidth.W))

  // CDR stuff
  val cdrI = Input(UInt(cdrIWidth.W))
  val cdrP = Input(UInt(cdrPWidth.W))

  // Clock dither for CDR
  val clockDither = Input(Bool())

  // DFE stuff
  val dfeTaps = Input(Vec(dfeNumTaps, UInt(dfeTapWidth.W)))
  val dLevDAC = Input(UInt(dLevDACWidth.W))

  //val config = TODO

  //val debug = TODO

  // reference current (if any)
  val bias = Analog(transceiverNumIrefs.W)

}

class Transceiver(
  dataWidth: Int,
  transceiverNumIrefs: Int,
  cdrIWidth: Int,
  cdrPWidth: Int,
  dfeNumTaps: Int,
  dfeTapWidth: Int,
  dLevDACWidth: Int
) extends BlackBox {

  val io = IO(new TransceiverIO(
    dataWidth: Int,
    transceiverNumIrefs: Int,
    cdrIWidth: Int,
    cdrPWidth: Int,
    dfeNumTaps: Int,
    dfeTapWidth: Int,
    dLevDACWidth: Int
  ))

  override def desiredName = transceiverName

}


