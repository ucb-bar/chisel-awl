package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental.withClock
import scala.collection.mutable.HashMap


class ScanChainPort extends Bundle {
    val scanIn = Input(Bool())
    val scanOut = Output(Bool())
    val scanEnable = Input(Bool())
    val scanCommit = Input(Bool())
    val scanClock = Input(Clock())
}

object ScanChainPort {
    def apply(): ScanChainPort = new ScanChainPort
}

object ScanChainController {
    def apply(spec: ControlSpec) = new ScanChainController(spec)
}

class ScanChainController(spec: ControlSpec) extends Controller(spec) {

    type P = ScanChainPort
    def portFactory = ScanChainPort.apply

    private val addressMap = new HashMap[String, (Int, Int)]
    private var index = 0

    private val wScanOut = spec.w.foldLeft(io.port.scanIn) { case (scanIn, (name, node, init)) =>
        val w = node.getWidth

        addressMap += (name -> (index + w - 1, index))
        index += w

        val shift = Wire(UInt(w.W))
        withClock (io.port.scanClock) {
            val shiftReg = Reg(UInt(w.W))
            when (io.port.scanEnable) {
                if (w == 1) {
                    shiftReg := scanIn
                } else {
                    shiftReg := Cat(shiftReg(w-2,0),scanIn)
                }
            }
            shift := shiftReg
        }

        val shadow = RegInit(init.getOrElse(0.U))

        io.w(name) := shadow

        when (io.port.scanCommit) {
            shadow := shift
        }

        shift(w-1)
    }

    val wLength = index

    io.port.scanOut := spec.r.foldLeft(wScanOut) { case (scanIn, (name, node)) =>
        val w = node.getWidth

        addressMap += (name -> (index + w - 1, index))
        index += w

        val shift = Wire(UInt(w.W))
        withClock (io.port.scanClock) {
            val shiftReg = Reg(UInt(w.W))
            when (io.port.scanEnable) {
                if (w == 1) {
                    shiftReg := scanIn
                } else {
                    shiftReg := Cat(shiftReg(w-2,0),scanIn)
                }
            } .otherwise {
                shiftReg := io.r(name)
            }
            shift := shiftReg
        }

        shift(w-1)
    }

    def getAddressMap(): Map[String, (Int, Int)] = addressMap.toMap

    val rLength = index - wLength
    def length = rLength + wLength

}

trait HasScanChainController {
    type C = ScanChainController
    def genBuilder() = new ControllerBuilder(ScanChainController.apply)
}
