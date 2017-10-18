package hbwif2

import chisel3._
import chisel3.experimental._

class CDRIO(
  transceiverDataWidth: Int,
  cdrIWidth: Int,
  cdrPwidth: Int
) extends Bundle {

  val cdrI = Output(UInt(cdrIWidth.W))
  val cdrP = Output(UInt(cdrPWidth.W))
  val clockDither = Output(Bool())
  val dataDLev = Input(UInt(transceiverDataWidth.W))
  val dataRx = Input(UInt(transceiverDataWidth.W))

}

class CDR(
  transceiverDataWidth: Int,
  cdrIWidth: Int,
  cdrPwidth: Int
) extends Module {

  val io = IO(new CDRIO(
    transceiverDataWidth,
    cdrIWidth,
    cdrPwidth
  ))

  // TODO

}
