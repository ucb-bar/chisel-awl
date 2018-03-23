package hbwif

import chisel3._
import chisel3.experimental._

class CDRIO()(implicit c: SerDesConfig) extends Bundle {

  val i = Output(UInt(c.cdrIWidth.W))
  val p = Output(UInt(c.cdrPWidth.W))
  val dither_clock = Output(Bool())
  val data_dlev = Input(UInt(c.dataWidth.W))
  val data_rx = Input(UInt(c.dataWidth.W))

}

class CDR()(implicit val c: SerDesConfig) extends Module {

  val io = IO(new CDRIO)

  // TODO Placeholder
  io.i := 0.U(c.cdrIWidth.W)
  io.p := 0.U(c.cdrPWidth.W)
  io.dither_clock := false.B

}
