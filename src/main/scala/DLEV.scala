package hbwif2

import chisel3._
import chisel3.util.PopCount
import chisel3.experimental._

class DLEVIO()(implicit c: SerDesGeneratorConfig) extends Bundle {

  val dlev_dac = Output(UInt(c.dlevDACWidth.W))
  val data_rx = Input(UInt(c.dataWidth.W))
  val data_dlev = Input(UInt(c.dataWidth.W))

}

class DLEV()(implicit c: SerDesGeneratorConfig) extends Module {

  val io = IO(new DLEVIO)

  val code = RegInit(0.U(c.dlevDACWidth.W))

  when (((PopCount(io.data_dlev) << 1) >= PopCount(io.data_rx)) & code.orR) {
    code := code - 1.U
  } .elsewhen (!code.andR) {
    code := code + 1.U
  }

}
