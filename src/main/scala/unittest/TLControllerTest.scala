package hbwif2.tilelink

import hbwif2._
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

class TLControllerTestLazy()(implicit p: Parameters) extends LazyModule {

    val beatBytes = 8

    val managerNode = TLManagerNode(Seq(TLManagerPortParameters(
        Seq(TLManagerParameters(
            address            = List(AddressSet(0x0,0xffff)),
            resources          = new SimpleDevice("HbwifConfig0",Seq()).reg("mem"),
            executable         = false,
            supportsGet        = TransferSizes(1, beatBytes),
            supportsPutFull    = TransferSizes(1, beatBytes),
            supportsPutPartial = TransferSizes(1, beatBytes),
            fifoId             = None)),
        beatBytes = beatBytes,
        minLatency = 1
    )))
    val clientNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(name = "LaneConfig")))))

    managerNode := clientNode

    lazy val module = new LazyModuleImp(this) with UnitTestModule {
        val (in, edgeIn) = managerNode.in(0)
        val (out, edgeOut) = clientNode.out(0)

        val builder = new TLControllerBuilder(edgeIn)

        val a = Wire(UInt(64.W))
        val b = Wire(UInt(64.W))
        val c = Wire(UInt(64.W))
        val d = Wire(UInt(64.W))
        val e = Wire(UInt(64.W))
        val f = Wire(UInt(64.W))

        val sumab = a + b
        val sumde = d + e

        builder.w("a", a)
        builder.w("b", b)
        builder.w("c", c)
        builder.w("d", d)
        builder.w("e", e)
        builder.w("f", f)
        builder.r("sumab", sumab)
        builder.r("sumde", sumde)
        builder.generate(clock, reset.toBool, in)

        val pusher = Module(new TLControllerPusher(edgeOut, builder.map.toPattern(pattern)))

        out <> pusher.io.tl
        pusher.io.start := io.start
        io.finished := pusher.io.finished
    }

    def pattern: Seq[(String, Option[BigInt], Boolean)] = Seq(
        ("a",Some(0xf1),true),
        ("b",Some(0x12),true),
        ("a",Some(0xf1),false),
        ("c",Some(0x33),true),
        ("d",Some(0x4f),true),
        ("e",Some(0xcc),true),
        ("f",Some(0x11),true),
        ("f",Some(0x11),false),
        ("e",Some(0xcc),false),
        ("d",Some(0x4f),false),
        ("b",Some(0x12),false),
        ("sumab",Some(0xf1+0x12),false),
        ("sumde",Some(0xcc+0x4f),false)
    ).map { case (x,y,z) => (x, y.map(BigInt(_)), z) }

}

class TLControllerTest(timeout: Int = 50000)(implicit p: Parameters) extends UnitTest(timeout) {

    val dut = Module(LazyModule(new TLControllerTestLazy).module)

    io.finished := dut.io.finished
    dut.io.start := io.start

}

object TLControllerTests {

    def apply()(implicit p: Parameters):Seq[UnitTest] = Seq(Module(new TLControllerTest))

}
