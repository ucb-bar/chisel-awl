package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental.withClock

object Synchronizer {
    def apply(x: Bool, c: Clock, stages: Int = 3): Bool = {
        withClock(c) {
            (0 until stages).foldLeft(x) { (b,i) => RegNext(b) }
        }
    }
}

object AsyncResetSynchronizer {
    def apply(c: Clock, r: Bool, stages: Int = 3): Bool = {
        (0 until stages).foldLeft(r) { (b,i) => AsyncResetReg(b, c, r, true) }
    }
}

object AsyncResetReg {
    def apply(d: Bool, c: Clock, r: Bool, init: Boolean = true): Bool = {
        val x = Module(new AsyncResetReg(init))
        x.io.d := d
        x.io.clk := c
        x.io.rst := r
        x.io.q
    }
}

class AsyncResetReg(init: Boolean = true) extends BlackBox {
//with HasBlackBoxResource {
    val io = IO(new Bundle {
        val d = Input(Bool())
        val clk = Input(Clock())
        val q = Output(Bool())
        val rst = Input(Bool())
    })

    override def desiredName = if (init) "async_reset_reg1" else "async_reset_reg0"

    //setResource("/async_reset_reg.v")
}

/*
class AsyncCrossing[T <: Data](gen: T, depth: Int = 4) extends Module {
    val io = IO(new Bundle {
        val enqClock = Input(Clock())
        val enqReset = Input(Bool())
        val deqClock = Input(Clock())
        val deqReset = Input(Bool())
        val enq = Flipped(Decoupled(gen))
        val deq = Decoupled(gen)
    })


}
*/
