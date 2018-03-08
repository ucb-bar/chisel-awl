package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

trait TLPacketizerLike {

    val tlTypeWidth = 3

    def headerWidth(x: TLChannel): Int = {
        x match {
            case a: TLBundleA => { tlTypeWidth + List(a.opcode,a.param,a.size,a.source,a.address).map( _.getWidth).reduce(_+_) }// mask is separate
            case b: TLBundleB => { tlTypeWidth + List(b.opcode,b.param,b.size,b.source,b.address).map(_.getWidth).reduce(_+_) } // mask is separate
            case c: TLBundleC => { tlTypeWidth + List(c.opcode,c.param,c.size,c.source,c.address).map(_.getWidth).reduce(_+_) } // ignore error
            case d: TLBundleD => { tlTypeWidth + List(d.opcode,d.param,d.size,d.source,d.sink).map(_.getWidth).reduce(_+_) }    // ignore error
            case e: TLBundleE => { tlTypeWidth + e.sink.getWidth }
        }
    }

    val typeA = 0.U(tlTypeWidth.W)
    val typeB = 1.U(tlTypeWidth.W)
    val typeC = 2.U(tlTypeWidth.W)
    val typeD = 3.U(tlTypeWidth.W)
    val typeE = 4.U(tlTypeWidth.W)

    def div8Ceil(x: Int): Int = (x + 7)/8

    def tlFromBuffer[T <: TLChannel](edge: TLEdge, x: T, buf: UInt, error: Bool): DecoupledIO[T] = {
        val w = buf.getWidth - tlTypeWidth
        (x match {
            case a: TLBundleA => {
                val out = Wire(Decoupled(new TLBundleA(edge.bundle)))
                val bits = out.bits
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, bits.mask, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out
            }
            case b: TLBundleB => {
                val out = Wire(Decoupled(new TLBundleB(edge.bundle)))
                val bits = out.bits
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, bits.mask, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out
            }
            case c: TLBundleC => {
                val out = Wire(Decoupled(new TLBundleC(edge.bundle)))
                val bits = out.bits
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                bits.error := error
                out
            }
            case d: TLBundleD => {
                val out = Wire(Decoupled(new TLBundleD(edge.bundle)))
                val bits = out.bits
                List(bits.opcode, bits.param, bits.size, bits.source, bits.sink, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                bits.error := error
                out
            }
            case e: TLBundleE => {
                val out = Wire(Decoupled(new TLBundleE(edge.bundle)))
                out.bits.sink := buf(w - 1, w - out.bits.sink.getWidth)
                out
            }
        }).asInstanceOf[DecoupledIO[T]]
    }

    def tlToBuffer[T <: TLChannel](edge: TLEdge, x: T, padTo: Int): UInt = {

        x match {
            case a: TLBundleA => {
                val padBits = padTo - headerWidth(a) - a.data.getWidth - a.mask.getWidth
                Cat(typeA, a.opcode, a.param, a.size, a.source, a.address, a.mask, a.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
            case b: TLBundleB => {
                val padBits = padTo - headerWidth(b) - b.data.getWidth - b.mask.getWidth
                Cat(typeB, b.opcode, b.param, b.size, b.source, b.address, b.mask, b.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
            case c: TLBundleC => {
                val padBits = padTo - headerWidth(c) - c.data.getWidth
                Cat(typeC, c.opcode, c.param, c.size, c.source, c.address, c.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
            case d: TLBundleD => {
                val padBits = padTo - headerWidth(d) - d.data.getWidth
                Cat(typeD, d.opcode, d.param, d.size, d.source, d.sink, d.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
            case e: TLBundleE => {
                val padBits = padTo - headerWidth(e)
                Cat(typeE, e.sink, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
        }
    }

    def typeFromBuffer(buf: UInt): (UInt, UInt) = (buf(buf.getWidth - 1, buf.getWidth - tlTypeWidth), buf(buf.getWidth - tlTypeWidth - 1, buf.getWidth - tlTypeWidth - 3))

    def getNumSymbolsFromType(aceEdge: TLEdge, bdEdge: TLEdge, tlType: UInt, opcode: UInt, first: Bool): UInt = {
        val ret = Wire(UInt())
        when (tlType === typeA) {
            val a = Wire(new TLBundleA(aceEdge.bundle))
            a.opcode := opcode
            ret := getNumSymbols(aceEdge, a, first)
        } .elsewhen (tlType === typeB) {
            val b = Wire(new TLBundleB(bdEdge.bundle))
            b.opcode := opcode
            ret := getNumSymbols(bdEdge, b, first)
        } .elsewhen (tlType === typeC) {
            val c = Wire(new TLBundleC(aceEdge.bundle))
            c.opcode := opcode
            ret := getNumSymbols(aceEdge, c, first)
        } .elsewhen (tlType === typeD) {
            val d = Wire(new TLBundleD(bdEdge.bundle))
            d.opcode := opcode
            ret := getNumSymbols(bdEdge, d, first)
        } .otherwise {
            val e = Wire(new TLBundleE(aceEdge.bundle))
            ret := getNumSymbols(aceEdge, e, first)
        }
        ret
    }

    // Ignore error signals here, we'll generate our own
    def getNumSymbols(edge: TLEdge, x: TLChannel, first: Bool): UInt = {
        Mux(first, div8Ceil(headerWidth(x)).U, 0.U) + Mux(edge.hasData(x), (x.data.getWidth/8).U + (x match {
            case a: TLBundleA => { div8Ceil(a.mask.getWidth).U }
            case b: TLBundleB => { div8Ceil(b.mask.getWidth).U }
            case _ => { 0.U }
        }), 0.U)
    }

    def tlResponseMap(x: TLChannel): UInt = {
        // TODO
        assert(false)
        x match {
            case a: TLBundleA => {
                0.U
            }
            case b: TLBundleB => {
                0.U
            }
            case c: TLBundleC => {
                0.U
            }
            case d: TLBundleD => {
                0.U
            }
            case e: TLBundleE => {
                0.U
            }
        }
    }
}

// pack all valid bytes into the lowest-indexed slots available
// e.g.
//
// symbol valid  => symbol
// A      0         C
// B      0         D
// C      1         F
// D      1         F
// E      0         F
// F      1         F
class Packer(entries: Int, width: Int = 8) extends Module {

    val io = IO(new Bundle {
        val in = Vec(entries, Valid(UInt(width.W)))
        val out = Vec(entries, Output(UInt(width.W)))
        val count = Output(UInt())
    })

    io.out := (1 until entries).foldLeft(io.in)({ (prev, stage) =>
        Vec((0 until entries).map({ i =>
            if (i >= entries - stage) {
                prev(i)
            } else {
                val next = Wire(Valid(UInt(width.W)))
                next.bits := Mux(prev(i).valid, prev(i).bits, prev(i+1).bits)
                next.valid := prev(i).valid || prev(i+1).valid
                next
            }
        }))
    }).map(_.bits)

    io.count := PopCount(io.in map {_.valid})

}

object Pack {

    def apply(vec: Vec[Valid[UInt]]): (Vec[UInt], UInt) = {
        val mod = Module(new Packer(vec.length, vec(0).bits.getWidth))
        mod.io.in := vec
        (mod.io.out, mod.io.count)
    }

    def apply(bits: UInt, mask: UInt): (Vec[UInt], UInt) = {
        val w = bits.getWidth / mask.getWidth
        require(bits.getWidth % mask.getWidth == 0)
        this.apply(Vec((0 until mask.getWidth).map({ i =>
            val v = Wire(Valid(UInt(w.W)))
            v.bits := bits(w * i + w - 1, w * i)
            v.valid := mask(i)
            v
        })))
    }

}

class MultiQueue[T <: Data](gen: T, depth: Int, numEnqPorts: Int) extends Module {

    private val genType = chiselTypeOf(gen)

    val io = IO(new Bundle {
        val enq = Vec(numEnqPorts, EnqIO(genType))
        val deq = DeqIO(genType)
    })

    private val entries = Mem(depth, genType)
    private val rptr = RegInit(0.U(log2Up(depth).W))
    private val wptr = RegInit(0.U(log2Up(depth).W))
    private val empty = RegInit(true.B)

    io.deq.valid := !empty
    io.deq.bits := entries(rptr)

    val rptrNext = Mux(io.deq.fire(), Mux(rptr === (depth-1).U, 0.U, rptr + 1.U), rptr)
    rptr := rptrNext

    val wptrNext = io.enq.foldLeft(0.U) { (count, enq) =>
        when (enq.fire()) {
            entries(count + wptr) := enq.bits
        }
        Mux(count === (depth-1).U, 0.U, count + enq.fire())
    }
    wptr := wptrNext

    require(depth > numEnqPorts, "Can't have depth < number of Enq ports because that makes no sense, and can't have it = because we can't tell the difference between empty/full")

    val enqueued = io.enq.map(_.fire()).reduce(_||_)

    when (io.deq.fire()) {
        empty := (rptrNext === wptrNext) && !enqueued
    } .elsewhen (enqueued) {
        empty := false.B
    }

    val full = (rptrNext === wptrNext) && enqueued
    val free = Mux(full, 0.U, Mux(rptrNext > wptrNext, rptr - wptr, depth.U - wptr + rptr))
    // This could be more performant, but I'm not even going to use these signals
    io.enq.zipWithIndex.foreach { case ((enq, i)) =>
        enq.ready := (free > i.U)
    }
}
