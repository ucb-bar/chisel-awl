package hbwif2

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArraySeq


class DebugIO(val dataWidth: Int) extends Bundle {
    val txIn = Input(UInt(dataWidth.W))
    val txOut = Output(UInt(dataWidth.W))
    val rxIn = Input(UInt(dataWidth.W))
}

abstract class Debug(val dataWidth: Int) extends Module with HasControllerConnector {

    val io: DebugIO

}

trait HasDebug {

    def genDebug(): Seq[Debug] = Seq[Debug]()

}
