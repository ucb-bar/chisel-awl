package hbwif2

import chisel3._
import chisel3.experimental._

class DLEVIO(
  transceiverDataWidth: Int,
  dLevDACWidth: Int
) extends Bundle {

  val dLevDAC = Output(UInt(dLevDACWidth.W))
  val dataRx = Input(UInt(transceiverDataWidth.W))
  val dataDLev = Input(UInt(transceiverDataWidth.W))

}

class DLEV(
  transceiverDataWidth: Int,
  dLevDACWidth: Int
) extends Module {

  val io = IO(new DLEVIO(
    transceiverDataWidth,
    dLevDACWidth
  ))

  //TODO

}
