package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset
import freechips.rocketchip.tilelink._
import scala.collection.mutable.HashMap

class TLControllerMapEntry(val address: BigInt, val size: Int, val writeable: Boolean) {
    def toWritePattern(data: BigInt) = {
        require(writeable, "Trying to write to unwriteable address")
        WritePattern(address, log2Ceil(size), data)
    }
    def toReadPattern = ReadPattern(address, log2Ceil(size))
    def toReadExpectPattern(data: BigInt) = ReadExpectPattern(address, log2Ceil(size), data)
}

class TLControllerMap extends HashMap[String, TLControllerMapEntry] {
    def add(name: String, address: BigInt, size: Int, writeable: Boolean = true) {
        this put (name, new TLControllerMapEntry(address, size, writeable))
    }

    def toPattern(seq: Seq[(String, Option[BigInt], Boolean)]): Seq[Pattern] = seq.map { case (name, data, write) =>
        if (write) {
            this(name).toWritePattern(data.getOrElse(0))
        } else {
            if (data.isDefined) {
                this(name).toReadExpectPattern(data.get)
            } else {
                this(name).toReadPattern
            }
        }
    }
}

class TLControllerBuilder(edge: TLEdgeIn) extends ControllerBuilder {

    type P = TLBundle
    def createPort = Flipped(TLBundle(edge.bundle))

    val map = new TLControllerMap

    private val maxSize = edge.bundle.dataBits
    private val beatBytes = edge.bundle.dataBits / 8
    private val maxAddress = (1 << edge.bundle.addressBits) - beatBytes // TODO is this safe

    def generate(laneClock: Clock, laneReset: Bool, port: TLBundle) {

        require(wSeqMems.length == 0, "TODO")
        require(rSeqMems.length == 0, "TODO")

        withClockAndReset (laneClock, laneReset) {

            // unused channels
            port.b.valid := false.B
            port.b.bits := port.b.bits.fromBits(0.U)
            port.c.ready := true.B
            port.e.ready := true.B

            val dvalid = RegInit(false.B)
            val dopcode = Reg(UInt(3.W))
            val ddata = Reg(UInt(maxSize.W))
            val dsize = Reg(UInt(edge.bundle.sizeBits.W))
            val dsource = Reg(UInt(edge.bundle.sourceBits.W))
            // always ready
            port.a.ready := !dvalid
            port.d.valid := dvalid
            port.d.bits.data := ddata
            port.d.bits.opcode := dopcode
            port.d.bits.param := 0.U
            port.d.bits.size := dsize
            port.d.bits.source := dsource
            port.d.bits.sink := 0.U
            port.d.bits.error := false.B

            when (port.d.fire()) {
                dvalid := false.B
            }


            val rOffset = ws.foldLeft(0) { case (address, (name, node, init)) =>
                val w = node.getWidth
                require(w <= maxSize, s"Port $name width ${node.getWidth} is too large (> $maxSize)")
                require(address + beatBytes - 1 <= maxAddress, s"Ran out of address space! Allocate more to TLController.")
                map.add(name, address, beatBytes, true)

                val shadow = init.map(x => RegInit(x.U(w.W))).getOrElse(Reg(UInt(w.W)))
                node := shadow
                when (port.a.fire() && (port.a.bits.address === address.U)) {
                    when (edge.hasData(port.a.bits)) {
                        // Put
                        shadow := port.a.bits.data
                        dopcode := TLMessages.AccessAck
                    } .otherwise {
                        // Get
                        dopcode := TLMessages.AccessAckData
                    }
                    dvalid := true.B
                    ddata := node
                    dsize := port.a.bits.size
                    dsource := port.a.bits.source
                }
                address + beatBytes
            }

            rs.foldLeft(rOffset) { case (address, (name, node)) =>
                require(node.getWidth <= maxSize, s"FIXME, Port $name width ${node.getWidth} is too large (> $maxSize)")
                map.add(name, address, beatBytes, false)

                when (port.a.fire() && (port.a.bits.address === address.U)) {
                    when (edge.hasData(port.a.bits)) {
                        // Ignore Puts
                        dopcode := TLMessages.AccessAck
                    } .otherwise {
                        // Get
                        dopcode := TLMessages.AccessAckData
                    }
                    dvalid := true.B
                    ddata := node
                    dsize := port.a.bits.size
                    dsource := port.a.bits.source
                }
                address + beatBytes
            }
        }
    }

}

trait HasTLController {
    val configEdge: TLEdgeIn
    def genBuilder() = new TLControllerBuilder(configEdge)
}
