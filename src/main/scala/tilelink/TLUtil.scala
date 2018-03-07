package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import chisel3.experimental.chiselTypeOf
import freechips.rocketchip.tilelink._

trait TLPacketizerLike {

    val tlTypeWidth = 3

    def headerWidth(a: TLBundleA): Int = tlTypeWidth + List(a.opcode,a.param,a.size,a.source,a.address,a.mask).map( _.getWidth).reduce(_+_)
    def headerWidth(b: TLBundleB): Int = tlTypeWidth + List(b.opcode,b.param,b.size,b.source,b.address,b.mask).map(_.getWidth).reduce(_+_)
    def headerWidth(c: TLBundleC): Int = tlTypeWidth + 1 + List(c.opcode,c.param,c.size,c.source,c.address).map(_.getWidth).reduce(_+_)
    def headerWidth(d: TLBundleD): Int = tlTypeWidth + 1 + List(d.opcode,d.param,d.size,d.source,d.sink).map(_.getWidth).reduce(_+_)
    def headerWidth(e: TLBundleE): Int = tlTypeWidth + e.sink.getWidth

    val typeA = 0.U(tlTypeWidth.W)
    val typeB = 1.U(tlTypeWidth.W)
    val typeC = 2.U(tlTypeWidth.W)
    val typeD = 3.U(tlTypeWidth.W)
    val typeE = 4.U(tlTypeWidth.W)

    def div8Ceil(x: Int): Int = (x + 7)/8

    def tlSymbolMap(x: TLChannel): Map[UInt, UInt] = {
        x match {
            case _:TLBundleA => { Map(
                (TLMessages.PutFullData    -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.PutPartialData -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.ArithmeticData -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.LogicalData    -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.Get            -> div8Ceil(headerWidth(x)).U),
                (TLMessages.Hint           -> div8Ceil(headerWidth(x)).U),
                (TLMessages.AcquireBlock   -> div8Ceil(headerWidth(x)).U),
                (TLMessages.AcquirePerm    -> div8Ceil(headerWidth(x)).U)) }
            case _:TLBundleB => { Map(
                (TLMessages.PutFullData    -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.PutPartialData -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.ArithmeticData -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.LogicalData    -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.Get            -> div8Ceil(headerWidth(x)).U),
                (TLMessages.Hint           -> div8Ceil(headerWidth(x)).U),
                (TLMessages.Probe          -> div8Ceil(headerWidth(x)).U)) }
            case _:TLBundleC => { Map(
                (TLMessages.AccessAck      -> div8Ceil(headerWidth(x)).U),
                (TLMessages.AccessAckData  -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.HintAck        -> div8Ceil(headerWidth(x)).U),
                (TLMessages.ProbeAck       -> div8Ceil(headerWidth(x)).U),
                (TLMessages.ProbeAckData   -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.Release        -> div8Ceil(headerWidth(x)).U),
                (TLMessages.ReleaseData    -> div8Ceil(headerWidth(x) + x.data.getWidth).U)) }
            case _:TLBundleD => { Map(
                (TLMessages.AccessAck      -> div8Ceil(headerWidth(x)).U),
                (TLMessages.AccessAckData  -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.HintAck        -> div8Ceil(headerWidth(x)).U),
                (TLMessages.Grant          -> div8Ceil(headerWidth(x)).U),
                (TLMessages.GrantData      -> div8Ceil(headerWidth(x) + x.data.getWidth).U),
                (TLMessages.ReleaseAck     -> div8Ceil(headerWidth(x)).U)) }
            case _:TLBundleE => { Map(
                (TLMessages.GrantAck       -> div8Ceil(headerWidth(x)).U)) }
        }

/*
    def tlResponseMap(x: TLChannel): UInt = {
        x match {
            case _:TLBundleA => {}
            case _:TLBundleB => {}
            case _:TLBundleC => {}
            case _:TLBundleD => {}
            case _:TLBundleE => {}
        }
    }
*/

    def getNumSymbolsFromType(x: TLChannel, tlType: UInt, opcode: UInt): UInt = {
        require(tlSymbolMap(x.e).length === 1)
        Mux((tlType === typeA),
            MuxLookup(opcode, 1.U, tlSymbolMap(x.a)),
        Mux((tlType === typeB),
            MuxLookup(opcode, 1.U, tlSymbolMap(x.b)),
        Mux((tlType === typeC),
            MuxLookup(opcode, 1.U, tlSymbolMap(x.c)),
        Mux((tlType === typeD),
            MuxLookup(opcode, 1.U, tlSymbolMap(x.d)),
            tlSymbolMap(x.e)(TLMessages.GrantAck)))))
    }

    def getNumSymbols(x: TLBundleA): UInt = {
        require(tlSymbolMap(x.e).length === 1)
        x match {
            case _:TLBundleA => { MuxLookup(x.opcode, 1.U, tlSymbolMap(x).toSeq) }
            case _:TLBundleB => { MuxLookup(x.opcode, 1.U, tlSymbolMap(x).toSeq) }
            case _:TLBundleC => { MuxLookup(x.opcode, 1.U, tlSymbolMap(x).toSeq) }
            case _:TLBundleD => { MuxLookup(x.opcode, 1.U, tlSymbolMap(x).toSeq) }
            case _:TLBundleE => { tlSymbolMap(x)(TLMessages.GrantAck) }
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
