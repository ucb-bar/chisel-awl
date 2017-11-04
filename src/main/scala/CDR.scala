package hbwif2

import chisel3._
import chisel3.experimental._

class CDRIO()(implicit c: SerDesGeneratorConfig) extends Bundle {

  val cdri = Output(UInt(c.cdrIWidth.W))
  val cdrp = Output(UInt(c.cdrPWidth.W))
  val dither_clock = Output(Bool())
  val data_dlev = Input(UInt(c.transceiverDataWidth.W))
  val data_rx = Input(UInt(c.transceiverDataWidth.W))

}

class CDR()(implicit c: SerDesGeneratorConfig) extends Module {

  val io = IO(new CDRIO)

  // TODO Placeholder
  io.cdri := 0.U(c.cdrIWidth.W)
  io.cdrp := 0.U(c.cdrPWidth.W)
  io.dither_clock := false.B

}
