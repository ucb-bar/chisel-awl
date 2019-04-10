package awl.test.tilelink

import awl.serial._
import awl.tilelink._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.util._

// This is really just a de-diplomacyized version of TLPatternPusher
class TLControllerPusher(edge: TLEdgeOut, pattern: Seq[Pattern])(implicit p: Parameters) extends Module {

    val io = IO(new Bundle {
        val tl = TLBundle(edge.bundle)
        val start = Input(Bool())
        val finished = Output(Bool())
    })

    io.tl.b.ready := true.B
    io.tl.c.valid := false.B
    io.tl.c.bits := io.tl.c.bits.fromBits(0.U)
    io.tl.e.valid := false.B
    io.tl.e.bits := io.tl.e.bits.fromBits(0.U)

    val started = RegInit(false.B)
    started := io.start || started
    val step   = RegInit(0.U(log2Ceil(pattern.size+1).W))
    val flight = RegInit(false.B)
    val ready  = RegInit(false.B)
    ready := true.B

    val end = step === pattern.size.U
    io.finished := end && !flight

    val a = io.tl.a
    val d = io.tl.d

    val check = Vec(pattern.map(_.dataIn.isDefined.B))(step) holdUnless a.fire()
    val expect = Vec(pattern.map(_.dataIn.getOrElse(BigInt(0)).U))(step) holdUnless a.fire()
    assert(!check || !d.fire() || expect === d.bits.data)

    when (a.fire()) {
        flight := true.B
        step := step + 1.U
    }
    when (d.fire()) {
        flight := false.B
    }

    val (plegal, pbits) = pattern.map(_.bits(edge)).unzip
    assert(end || Vec(plegal)(step), "Illegal")

    a.valid := started && ready && !end && !flight
    a.bits := Vec(pbits)(step)
    d.ready := true.B

}

// TODO add tests for new style controller
