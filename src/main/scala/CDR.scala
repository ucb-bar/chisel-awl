package hbwif

import chisel3._
import chisel3.experimental._

class CDRIO()(implicit c: SerDesConfig) extends Bundle {

    val p = Output(UInt(c.cdrPWidth.W))
    val data_dlev = Input(UInt(c.dataWidth.W))
    val data_rx = Input(UInt(c.dataWidth.W))

}

class CDR()(implicit val c: SerDesConfig) extends Module {

    val io = IO(new CDRIO)

    // TODO Placeholder
    io.p := 0.U(c.cdrPWidth.W)

}
