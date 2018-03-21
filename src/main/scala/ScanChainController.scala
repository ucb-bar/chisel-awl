package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental.{withClock,withClockAndReset}
import scala.collection.mutable.HashMap


class ScanChainPort extends Bundle {
    val scanIn = Input(Bool())
    val scanOut = Output(Bool())
    val scanEnable = Input(Bool())
    val scanCommit = Input(Bool())
    val scanClock = Input(Clock())
}

class ScanChainControllerBuilder extends ControllerBuilder {

    type P = ScanChainPort
    def createPort = new ScanChainPort

    private var wLength = 0
    private var rLength = 0
    val addressMap = new HashMap[String, (Int, Int)]

    def generate(laneClock: Clock, laneReset: Bool): P = {
        var index = 0

        val port = Wire(createPort)

        require(wSeqMems.length == 0, "ScanChainController does not support Mems")
        require(rSeqMems.length == 0, "ScanChainController does not support Mems")

        val syncCommit = Synchronizer(port.scanCommit, laneClock)

        val wScanOut = ws.foldLeft(port.scanIn) { case (scanIn, (name, node, init)) =>
            val w = node.getWidth

            addressMap += (name -> (index + w - 1, index))
            index += w

            val shift = Wire(UInt(w.W))
            withClock (port.scanClock) {
                val shiftReg = Reg(UInt(w.W))
                shiftReg.suggestName(name + "_reg")
                when (port.scanEnable) {
                    if (w == 1) {
                        shiftReg := scanIn
                    } else {
                        shiftReg := Cat(shiftReg(w-2,0),scanIn)
                    }
                }
                shift := shiftReg
            }
            withClockAndReset (laneClock, laneReset) {
                val shadow = init.map(x => RegInit(x.U(w.W))).getOrElse(Reg(UInt(w.W)))
                shadow.suggestName(name + "_shadow")

                node := shadow

                when (syncCommit) {
                    shadow := shift
                }

            }
            shift(w-1)
        }

        wLength = index

        port.scanOut := rs.foldLeft(wScanOut) { case (scanIn, (name, node)) =>
            val w = node.getWidth

            addressMap += (name -> (index + w - 1, index))
            index += w

            val shift = Wire(UInt(w.W))
            withClock (port.scanClock) {
                val shiftReg = Reg(UInt(w.W))
                shiftReg.suggestName(name + "_reg")
                when (port.scanEnable) {
                    if (w == 1) {
                        shiftReg := scanIn
                    } else {
                        shiftReg := Cat(shiftReg(w-2,0),scanIn)
                    }
                } .otherwise {
                    shiftReg := node
                }
                shift := shiftReg
            }

            shift(w-1)
        }

        rLength = index - wLength

        port
    }

    def getAddressMap(): Map[String, (Int, Int)] = addressMap.toMap

    def readLength = rLength
    def writeLength = wLength
    def length = rLength + wLength

}

trait HasScanChainController {
    def genBuilder() = new ScanChainControllerBuilder
}
