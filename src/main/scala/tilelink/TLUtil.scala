package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import chisel3.experimental.chiselTypeOf
import freechips.rocketchip.tilelink._

trait TLPacketizerLike {

    val tlTypeWidth = 3

    def headerWidth(a: TLBundleA): Int = tlTypeWidth + List(a.opcode,a.param,a.size,a.source,a.address).map( _.getWidth).reduce(_+_) // mask is separate
    def headerWidth(b: TLBundleB): Int = tlTypeWidth + List(b.opcode,b.param,b.size,b.source,b.address).map(_.getWidth).reduce(_+_)  // mask is separate
    def headerWidth(c: TLBundleC): Int = tlTypeWidth + List(c.opcode,c.param,c.size,c.source,c.address).map(_.getWidth).reduce(_+_)  // ignore error
    def headerWidth(d: TLBundleD): Int = tlTypeWidth + List(d.opcode,d.param,d.size,d.source,d.sink).map(_.getWidth).reduce(_+_)     // ignore error
    def headerWidth(e: TLBundleE): Int = tlTypeWidth + e.sink.getWidth

    val typeA = 0.U(tlTypeWidth.W)
    val typeB = 1.U(tlTypeWidth.W)
    val typeC = 2.U(tlTypeWidth.W)
    val typeD = 3.U(tlTypeWidth.W)
    val typeE = 4.U(tlTypeWidth.W)

    def div8Ceil(x: Int): Int = (x + 7)/8

    def tlFromBuffer[T <: TLChannel](edge: TLEdge, x: T, buf: UInt, error: Bool): T = {
        val w = buf.getWidth - tlTypeWidth
        x match {
            case a: TLBundleA => {
                val out = TLBundleA(edge.bundle)
                List(out.opcode, out.param, out.size, out.source, out.address, out.mask, out.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out
            }
            case b: TLBundleB => {
                val out = TLBundleB(edge.bundle)
                List(out.opcode, out.param, out.size, out.source, out.address, out.mask, out.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out
            }
            case c: TLBundleC => {
                val out = TLBundleC(edge.bundle)
                List(out.opcode, out.param, out.size, out.source, out.address, out.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out.error := error
                out
            }
            case d: TLBundleD => {
                val out = TLBundleD(edge.bundle)
                List(out.opcode, out.param, out.size, out.source, out.address, out.sink, out.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out.error := error
                out
            }
            case e: TLBundleE => {
                val out = TLBundleE(edge.bundle)
                out.sink := buf(w - 1, w - out.sink.getWidth)
                out
            }
        }.asInstanceOf[T]
    }

    def tlToBuffer[T <: TLChannel](edge: TLEdge, x: T): UInt = {
        x match {
            case a: TLBundleA => {
                Cat(typeA, tltx.a.bits.opcode, tltx.a.bits.param, tltx.a.bits.size, tltx.a.bits.source, tltx.a.bits.address, tltx.a.bits.mask, tltx.a.bits.data, if (aPadBits > 0) 0.U(aPadBits.W) else Wire(UInt(0.W)))
            }
            case b: TLBundleB => {
                Cat(typeB, tltx.b.bits.opcode, tltx.b.bits.param, tltx.b.bits.size, tltx.b.bits.source, tltx.b.bits.address, tltx.b.bits.mask, tltx.b.bits.data, if (bPadBits > 0) 0.U(bPadBits.W) else Wire(UInt(0.W)))
            }
            case c: TLBundleC => {
                Cat(typeC, tltx.c.bits.opcode, tltx.c.bits.param, tltx.c.bits.size, tltx.c.bits.source, tltx.c.bits.address, tltx.c.bits.data, if (cPadBits > 0) 0.U(cPadBits.W) else Wire(UInt(0.W)))
            }
            case d: TLBundleD => {
                Cat(typeD, tltx.d.bits.opcode, tltx.d.bits.param, tltx.d.bits.size, tltx.d.bits.source, tltx.d.bits.sink, tltx.d.bits.data, if (dPadBits > 0) 0.U(dPadBits.W) else Wire(UInt(0.W)))
            }
            case e: TLBundleE => {
                Cat(typeE, tltx.e.bits.sink, if (ePadBits > 0) 0.U(ePadBits.W) else Wire(UInt(0.W)))
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
        Mux(first, div8Ceil(headerWidth(x)), 0.U) + Mux(edge.hasData(x), (x.data.getWidth/8).U + x match {
            case a: TLBundleA => { div8Ceil(a.mask.getWidth) }
            case b: TLBundleB => { div8Ceil(b.mask.getWidth) }
            case _ => { 0.U }
        }, 0.U)
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
        val out = Vec(entries, Output((UInt(width.W)))
        val count = Output(UInt())
    })

    io.out := (1 until entries) foldLeft(io.in) { case (prev, stage) =>
        Vec((0 until entries) map { i =>
            if (i >= entries-stage) {
                prev(i)
            } else {
                val next = Wire(Valid(UInt(width.W)))
                next.bits := Mux(prev(i).valid, prev(i), prev(i+1))
                next.valid := prev(i) || prev(i+1)
                next
            }
        })
    } map { _.bits }

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
        this.apply(Vec((0 until mask.getWidth) map { i => 
            val v = Wire(Valid(UInt(w.W)))
            v.bits := bits(w * i + w - 1, w * i)
            v.valid := mask(i)
        }))
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
        empty := (rptrNext === wptrNext) && !enqueued)
    } .elsewhen (enqueued) {
        empty := false.B
    }

    val full = (rptrNext === wptrNext) && enqueued
    val free = Mux(full, 0.U, Mux(rptrNext > wptrNext, rptr - wptr, depth.U - wptr + rptr))
    // This could be more performant, but I'm not even going to use these signals
    io.enq.zipWithIndex.foreach { (enq, i) =>
        enq.ready := (free > i.U)
    }
}
