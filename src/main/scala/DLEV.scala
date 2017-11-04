package hbwif2

import chisel3._
import chisel3.util.PopCount
import chisel3.experimental._

class DLEVIO()(implicit c: SerDesGeneratorConfig) extends Bundle {

  val dlevDAC = Output(UInt(c.dlevDACWidth.W))
  val dataRx = Input(UInt(c.transceiverDataWidth.W))
  val dataDLev = Input(UInt(c.transceiverDataWidth.W))

}

class DLEV()(implicit c: SerDesGeneratorConfig) extends Module {

  val io = IO(new DLEVIO)

  val code = InitReg(0.U(c.dlevDACWidth.W))

  when (((PopCount(io.data_dlev) << 1) >= PopCount(io.data_rx)) & code.orR) {
    code := code - 1.U
  } .elsewhen (!code.andR) {
    code := code + 1.U
  }

}
