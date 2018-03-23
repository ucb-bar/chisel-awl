package hbwif

import chisel3._
import chisel3.experimental._

class DFEIO()(implicit val c: SerDesConfig) extends Bundle {

  val taps = Output(Vec(c.dfeNumTaps, UInt(c.dfeTapWidth.W)))
  val data_dlev = Input(UInt(c.dataWidth.W))
  val data_rx = Input(UInt(c.dataWidth.W))

}

class DFE()(implicit val c: SerDesConfig) extends Module {

  val io = IO(new DFEIO)

  // TODO Placeholder
  io.taps := Vec.fill(c.dfeNumTaps) { 0.U(c.dfeTapWidth.W) }

}
