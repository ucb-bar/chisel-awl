package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental.withClock


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


class ScanChainController(spec: ControlSpec) extends Controller(spec) {

    type PortType = ScanChainPort
    val portFactory = ScanChainPort.apply _

    val wScanOut = spec.w.foldLeft(io.port.scanIn) { case (scanIn, (name, node, init)) =>
        val w = node.getWidth

        val shift = Wire(UInt(w.W))
        withClock (io.port.scanClock) {
            val shiftReg = Reg(UInt(w.W))
            when (io.port.scanEnable) {
                shiftReg := Cat(shiftReg(w-2,0),scanIn)
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

    io.port.scanOut := spec.r.foldLeft(wScanOut) { case (scanIn, (name, node)) =>
        val w = node.getWidth

        val shift = Wire(UInt(w.W))
        withClock (io.port.scanClock) {
            val shiftReg = Reg(UInt(w.W))
            when (io.port.scanEnable) {
                shiftReg := Cat(shiftReg(w-2,0),scanIn)
            } .otherwise {
                shiftReg := io.r(name)
            }
            shift := shiftReg
        }

        shift(w-1)
    }


    // TODO output scan chain data in some custom format

}

trait HasScanChainController {
    type ControllerPortType = ScanChainPort
    type ControllerType = ScanChainController
    val portFactory = { () => new ScanChainPort }
    val controllerFactory = { (spec: ControlSpec) => new ScanChainController(spec) }
}
