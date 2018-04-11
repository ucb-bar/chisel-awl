package hbwif

import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import chisel3.experimental.withClockAndReset
import freechips.rocketchip.unittest._


class ErrorInjector(per1k: Int = 0) extends BlackBox(Map("per1k" -> IntParam(per1k))) {

    val io = IO(new Bundle {
        val in = Flipped(new Differential)
        val out = new Differential
        val clock = Input(Clock())
        val reset = Input(Bool())
        val errors = Output(UInt(64.W))
        val stop = Input(Bool())
    })

}

class DifferentialDelayLine(delay: Int) extends BlackBox(Map("delay" -> IntParam(delay))) {

    val io = IO(new Bundle {
        val in = Flipped(new Differential)
        val out = new Differential
        val clock = Input(Clock())
    })

}

class DifferentialToBool extends BlackBox {

    val io = IO(new Bundle {
        val in = Flipped(new Differential)
        val outBool = Output(Bool())
        val outTee = new Differential
    })

}

object DifferentialToBool {

    def apply(d: Differential): (Bool, Differential) = {
        val x = Module(new DifferentialToBool)
        x.io.in <> d
        (x.io.outBool, x.io.outTee)
    }

}
