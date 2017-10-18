package hbwif2

import chisel3._
import chisel3.experimental._

class DFEIO(
  transceiverDataWidth: Int,
  dfeNumTaps: Int,
  dfeTapWidth: Int
) extends Bundle {

  val dfeTaps = Output(Vec(dfeNumTaps, UInt(dfeTapWidth.W)))
  val dataDLev = Input(UInt(transceiverDataWidth.W))
  val dataRx = Input(UInt(transceiverDataWidth.W))

}

class DFE(
  transceiverDataWidth: Int,
  dfeNumTaps: Int,
  dfeTapWidth: Int
) extends Module {

  val io = IO(new DFEIO(
    transceiverDataWidth,
    dfeNumTaps,
    dfeTapWidth
  ))

  //TODO

}
