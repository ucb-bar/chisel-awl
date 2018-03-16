package hbwif2

import chisel3._
import chisel3.util._

/*
class LoopbackDebugIO()(implicit c: SerDesConfig) extends DebugIO {
    val enable = Input(Bool())
}

class LoopbackDebug()(implicit c: SerDesConfig) extends Debug {

    val io = IO(new LoopbackDebugIO)

    // TODO deal with rx valid and tx ready

    io.txOut.bits := Mux(io.enable, io.rxIn, io.txIn.bits)
    io.txIn.ready := Mux(io.enable, false.B, io.txOut.ready)
    io.rxOut <> io.rxIn

    def connectController(builder: ControllerBuilder) {
        builder.w("loopback_enable", io.enable)
    }

}

trait HasLoopbackDebug extends HasDebug {
    implicit val c: SerDesConfig
    abstract override def genDebug() = Seq(new LoopbackDebug) ++ super.genDebug()
}
*/
