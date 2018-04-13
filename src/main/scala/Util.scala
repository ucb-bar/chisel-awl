package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.withClockAndReset

object Ready {
    def apply[T <: Data](gen: T): Ready[T] = new Ready(gen)
}

class Ready[+T <: Data](gen: T) extends Bundle {
    val ready = Input(Bool())
    val bits = Output(gen)
    def fire: Bool = ready
    override def cloneType(): this.type = new Ready(gen).asInstanceOf[this.type]
}


object GCD {
    def apply(a: Int, b: Int): Int = if (b == 0) a else GCD(b, a%b)
}

object LCM {
    def apply(a: Int, b: Int): Int = a*b / GCD(a, b)
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

class MultiQueue[T <: Data](gen: T, depth: Int, numEnqPorts: Int, numDeqPorts: Int) extends Module {

    val io = IO(new Bundle {
        val enq = Flipped(Vec(numEnqPorts, Decoupled(gen)))
        val deq = Vec(numDeqPorts, Decoupled(gen))
    })
    require(depth > numEnqPorts)
    require(depth > numDeqPorts)
    require(isPow2(depth), "Depth must be power of 2 for this queue")

    assert(io.deq.map(_.ready).reduce(_||_) === io.deq.map(_.ready).reduce(_&&_), "All deq signals must be ready, or none")

    val entries = Reg(Vec(depth, gen))
    val rptr = RegInit(0.U(log2Ceil(depth).W))
    val wptr = RegInit(0.U(log2Ceil(depth).W))
    val maybeFull = RegInit(false.B)
    val full = (rptr === wptr) && maybeFull
    val empty = (rptr === wptr) && !maybeFull
    val free = Mux(full, 0.U(log2Ceil(depth+1).W), Mux(rptr > wptr, rptr - wptr, depth.U - wptr + rptr))
    val valid = Mux(empty, 0.U(log2Ceil(depth+1).W), Mux(wptr > rptr, wptr - rptr, depth.U - rptr + wptr))

    io.deq.map(_.valid).zipWithIndex.foreach { case (v,i) => v := (valid > i.U) }
    io.deq.map(_.bits).zipWithIndex.foreach { case (b,i) => b := entries(rptr + i.U) }

    val rptrNext = rptr + PopCount(io.deq.map(_.fire()))
    rptr := rptrNext

    val wptrNext = wptr + PopCount(io.enq.map(_.fire()))
    wptr := wptrNext

    val enqueued = io.enq.map(_.fire()).reduce(_||_)
    val dequeued = io.deq.map(_.fire()).reduce(_||_)

    when (dequeued) {
        maybeFull := false.B
    } .elsewhen(enqueued && (rptrNext === wptrNext)) {
        maybeFull := true.B
    }

    io.enq.foldLeft(0.U(log2Ceil(depth+1).W)) { (cnt, enq) =>
        enq.ready := (free > cnt)
        when (enq.fire()) {
            entries(wptr + cnt) := enq.bits
        }
        cnt + enq.valid
    }
}
