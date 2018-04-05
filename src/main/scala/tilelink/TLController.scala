package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.AsyncQueue
import freechips.rocketchip._
import freechips.rocketchip.config._
import scala.collection.mutable.HashMap

// Fix a bug in the version of rocket-chip we're using (it's fixed on master)
case class WritePattern(address: BigInt, size: Int, data: BigInt) extends Pattern {
    def bits(edge: TLEdgeOut) = edge.Put(UInt(0), UInt(address), UInt(size), UInt(data))
}

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

class TLControllerBuilder(edge: TLEdgeIn)(implicit val p: Parameters) extends ControllerBuilder {

    type P = TLBundle
    def createPort = Flipped(TLBundle(edge.bundle))

    val map = new TLControllerMap

    private val maxSize = edge.bundle.dataBits
    private val beatBytes = edge.bundle.dataBits / 8
    private val maxAddress = (1 << edge.bundle.addressBits) - beatBytes

    // TODO output address map

    def generate(laneClock: Clock, laneReset: Bool, globalClock: Clock, globalReset: Bool, port: TLBundle) {

        val qDepth = p(HbwifTLKey).asyncQueueDepth
        val qSync = p(HbwifTLKey).asyncQueueSync
        val qSafe = p(HbwifTLKey).asyncQueueSafe
        val qNarrow = p(HbwifTLKey).asyncQueueNarrow

        val aq = Module(new AsyncQueue(port.a.bits, qDepth, qSync, qSafe, qNarrow))
        val dq = Module(new AsyncQueue(port.d.bits, qDepth, qSync, qSafe, qNarrow))

        aq.suggestName("AsyncQueueControllerA")
        dq.suggestName("AsyncQueueControllerD")

        // unused channels
        port.b.valid := false.B
        port.b.bits  := DontCare
        port.c.ready := true.B
        port.e.ready := true.B

        aq.io.enq <> port.a
        port.d <> dq.io.deq

        aq.io.enq_clock := globalClock
        aq.io.enq_reset := globalReset
        aq.io.deq_clock := laneClock
        aq.io.deq_reset := laneReset
        dq.io.enq_clock := laneClock
        dq.io.enq_reset := laneReset
        dq.io.deq_clock := globalClock
        dq.io.deq_reset := globalReset

        withClockAndReset (laneClock, laneReset) {


            val dvalid = RegInit(false.B)
            val dopcode = Reg(UInt(3.W))
            val ddata = Reg(UInt(maxSize.W))
            val dsize = Reg(UInt(edge.bundle.sizeBits.W))
            val dsource = Reg(UInt(edge.bundle.sourceBits.W))
            // always ready
            aq.io.deq.ready := !dvalid
            dq.io.enq.valid := dvalid
            dq.io.enq.bits.data := ddata
            dq.io.enq.bits.opcode := dopcode
            dq.io.enq.bits.param := 0.U
            dq.io.enq.bits.size := dsize
            dq.io.enq.bits.source := dsource
            dq.io.enq.bits.sink := 0.U
            dq.io.enq.bits.error := false.B

            when (dq.io.enq.fire()) {
                dvalid := false.B
            }


            val rOffset = ws.foldLeft(0) { case (address, (name, node, init)) =>
                val w = node.getWidth
                require(w <= maxSize, s"Port $name width ${node.getWidth} is too large (> $maxSize)")
                require(address + beatBytes - 1 <= maxAddress, s"Ran out of address space! Allocate more to TLController.")
                map.add(name, address, beatBytes, true)

                val shadow = init.map(x => RegInit(x.U(w.W))).getOrElse(Reg(UInt(w.W)))
                node := shadow
                when (aq.io.deq.fire() && (aq.io.deq.bits.address === address.U)) {
                    when (edge.hasData(aq.io.deq.bits)) {
                        // Put
                        shadow := aq.io.deq.bits.data
                        dopcode := TLMessages.AccessAck
                    } .otherwise {
                        // Get
                        dopcode := TLMessages.AccessAckData
                    }
                    dvalid := true.B
                    ddata := node
                    dsize := aq.io.deq.bits.size
                    dsource := aq.io.deq.bits.source
                }
                address + beatBytes
            }

            rs.foldLeft(rOffset) { case (address, (name, node)) =>
                require(node.getWidth <= maxSize, s"FIXME, Port $name width ${node.getWidth} is too large (> $maxSize)")
                map.add(name, address, beatBytes, false)

                when (aq.io.deq.fire() && (aq.io.deq.bits.address === address.U)) {
                    when (edge.hasData(aq.io.deq.bits)) {
                        // Ignore Puts
                        dopcode := TLMessages.AccessAck
                    } .otherwise {
                        // Get
                        dopcode := TLMessages.AccessAckData
                    }
                    dvalid := true.B
                    ddata := node
                    dsize := aq.io.deq.bits.size
                    dsource := aq.io.deq.bits.source
                }
                address + beatBytes
            }
        }
    }

}

trait HasTLController {
    val configEdge: TLEdgeIn
    implicit val p: Parameters
    def genBuilder() = new TLControllerBuilder(configEdge)
}
