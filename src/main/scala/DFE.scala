package hbwif2

import chisel3._
import chisel3.experimental._

class DFEIO()(implicit c: SerDesGeneratorConfig) extends Bundle {

  val dfe_taps = Output(Vec(c.dfeNumTaps, UInt(c.dfeTapWidth.W)))
  val data_dlev = Input(UInt(c.dataWidth.W))
  val data_rx = Input(UInt(c.dataWidth.W))

}

class DFE()(implicit c: SerDesGeneratorConfig) extends Module {

  val io = IO(new DFEIO)

  // TODO Placeholder
  io.dfe_taps := Vec.fill(c.dfeNumTaps) { 0.U(c.dfeTapWidth.W) }

}
