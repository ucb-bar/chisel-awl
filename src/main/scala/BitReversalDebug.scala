package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset

class BitReversalDebug()(implicit c: SerDesConfig) extends Debug()(c) {

    override val controlIO = Some(IO(new ControlBundle {
        val txReverse = input(Bool(), 0, "bit_reverse_tx")
        val rxReverse = input(Bool(), 0, "bit_reverse_rx")
    }))

    io.txOut.bits := Mux(controlIO.get.txReverse, Reverse(io.txIn.bits), io.txIn.bits)
    io.rxOut.bits := Mux(controlIO.get.rxReverse, Reverse(io.rxIn.bits), io.rxIn.bits)

    io.txIn.ready := io.txOut.ready
    io.rxOut.valid := io.rxIn.valid

}

trait HasBitReversalDebug extends HasDebug {
    this: Lane =>
    implicit val c: SerDesConfig
    abstract override def genDebug() = super.genDebug() ++ Seq(Module(new BitReversalDebug()(c)))
}
