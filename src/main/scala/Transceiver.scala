package hbwif2

import chisel3._
import chisel3.experimental._

class Differential extends Bundle {
  val p = Analog(1.W)
  val n = Analog(1.W)
}

class TransceiverIO(
  transceiverDataWidth: Int,
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
  val slowClock = Output(Clock())

  // reset
  val resetIn = Input(Bool())
  val resetOut = Output(Bool())

  // RX pad inputs
  val rx = Flipped(new Differential)

  // TX pad outputs
  val tx = new Differential

  // internal data interface
  val dataDLev = Output(UInt(transceiverDataWidth.W))
  val dataRx = Output(UInt(transceiverDataWidth.W))
  val dataTx = Input(UInt(transceiverDataWidth.W))

  // reference current (if any)
  val iref = Analog(transceiverNumIrefs.W)

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

}

class Transceiver(
  transceiverDataWidth: Int,
  transceiverNumIrefs: Int,
  cdrIWidth: Int,
  cdrPWidth: Int,
  dfeNumTaps: Int,
  dfeTapWidth: Int,
  dLevDACWidth: Int
) extends BlackBox {

  val io = IO(new TransceiverIO(
    transceiverDataWidth: Int,
    transceiverNumIrefs: Int,
    cdrIWidth: Int,
    cdrPWidth: Int,
    dfeNumTaps: Int,
    dfeTapWidth: Int,
    dLevDACWidth: Int
  ))

  override def desiredName = transceiverName

}


