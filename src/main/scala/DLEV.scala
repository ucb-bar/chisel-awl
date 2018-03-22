package hbwif2

import chisel3._
import chisel3.util.PopCount
import chisel3.experimental._

class DLEVIO()(implicit val c: SerDesConfig) extends Bundle {

  val code = Output(UInt(c.dlevDACWidth.W))
  val data_rx = Input(UInt(c.dataWidth.W))
  val data_dlev = Input(UInt(c.dataWidth.W))

}

class DLEV()(implicit val c: SerDesConfig) extends Module {

  val io = IO(new DLEVIO)

  val code = RegInit(0.U(c.dlevDACWidth.W))
  io.code := code

  when (((PopCount(io.data_dlev) << 1) >= PopCount(io.data_rx)) & code.orR) {
    code := code - 1.U
  } .elsewhen (!code.andR) {
    code := code + 1.U
  }

}
