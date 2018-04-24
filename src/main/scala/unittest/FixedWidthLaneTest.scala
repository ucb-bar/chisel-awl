package hbwif

import chisel3._
import chisel3.util._

import freechips.rocketchip.unittest._

import scala.util.Random

class FixedWidthLane8b10bTest[F <: Data](delay: Int, dataWidth: Int, fwDataFactory: () => F, timeout: Int = 50000) extends UnitTest(timeout) {

    implicit val c = SerDesConfig(dataWidth = dataWidth)

    val lane = Module(new FixedWidthLane8b10bBasic(fwDataFactory))
    val delayLine = Module(new DifferentialDelayLine(delay))

    lane.io.rx <> delayLine.io.out
    delayLine.io.in <> lane.io.tx
    delayLine.io.clock := this.clock
    lane.io.clockRef := this.clock
    lane.io.asyncResetIn := this.reset

    val control = lane.io.control.asInstanceOf[ScanChainPort]
    control.scanIn := false.B
    control.scanCommit := false.B
    control.scanClock := false.B.asClock
    control.scanEnable := false.B

    val rnd = new Random(5)
    val len = 500
    val numBits = lane.io.data.tx.bits.toBits.getWidth
    val seq = Seq.fill(len) { BigInt(numBits, rnd) }

    val pusher = Module(new BigPusher(seq, fwDataFactory))
    val checker = Module(new BigChecker(seq, fwDataFactory))

    io.finished := checker.io.finished

    lane.io.data.tx <> pusher.io.out
    checker.io.in <> lane.io.data.rx

}

class BigPusher[F <: Data](seq: Seq[BigInt], fwDataFactory: () => F) extends Module {

    val io = IO(new Bundle {
        val out = Decoupled(fwDataFactory())
    })

    val count = Counter(seq.length)
    val finished = RegInit(false.B)

    io.out.valid := !finished

    when (io.out.fire()) {
        when (count.inc()) {
            finished := true.B
        }
    }

    io.out.bits := io.out.bits.fromBits(0.U)

    seq.zipWithIndex.foreach { case (d, i) =>
        when (count.value === i.U) {
            io.out.bits := io.out.bits.fromBits(d.U)
        }
    }

}

class BigChecker[F <: Data](seq: Seq[BigInt], fwDataFactory: () => F) extends Module {

    val io = IO(new Bundle {
        val in = Flipped(Decoupled(fwDataFactory()))
        val finished = Output(Bool())
    })

    val count = Counter(seq.length)
    val finished = RegInit(false.B)

    io.finished := finished
    io.in.ready := !finished

    assert(io.in.ready || !io.in.valid, "Something went wrong and we got extra stuff")

    when (io.in.fire()) {
        when (count.inc()) {
            finished := true.B
        }
    }

    seq.zipWithIndex.foreach { case (d, i) =>
        when (count.value === i.U && io.in.fire()) {
            assert(io.in.bits.asUInt === io.in.bits.fromBits(d.U).asUInt, "Got the wrong data")
        }
    }

}

class FixedWidthTestData extends Bundle {
    val x = UInt(20.W)
    val y = UInt(100.W)
    val z = UInt(1.W)
}

object FixedWidthLaneTests {

    val delays = List(1, 11)
    val dataWidths = List(10, 16, 32, 64)
    val dataFormats = List({() => UInt(8.W)}, {() => UInt(130.W)}, {() => new FixedWidthTestData})

    def apply(timeout: Int = 50000):Seq[UnitTest] = for (x <- delays; y <- dataWidths; z <- dataFormats) yield Module(new FixedWidthLane8b10bTest(x, y, z, timeout))

}
