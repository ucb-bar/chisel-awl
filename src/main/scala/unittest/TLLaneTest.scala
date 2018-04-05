package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.coreplex.CacheBlockBytes

class TLLaneTestLazy(delay: Int)(implicit p: Parameters) extends LazyModule {

    val txns = 100

    require(p(HbwifTLKey).numLanes == 1, "Only testing 1 lane right now")

    val fuzz = LazyModule(new TLFuzzer(txns))
    val model = LazyModule(new TLRAMModel("SRAMSimple"))
    val hbwif = LazyModule(new GenericHbwifModule)
    val ram = LazyModule(new TLRAM(p(HbwifTLKey).managerAddressSets(0), beatBytes=p(CacheBlockBytes)))
    val configNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(name = "LaneConfig")))))

    ram.node := TLDelayer(0.25) := hbwif.clientNodes(0)
    hbwif.managerNodes(0) := model.node := fuzz.node
    hbwif.configNodes(0) := configNode

    lazy val module = new LazyModuleImp(this) with UnitTestModule {
        val (config, edgeConfig) = configNode.out(0)

        val pusher = Module(new TLControllerPusher(edgeConfig, hbwif.module.laneModules(0).builder.asInstanceOf[TLControllerBuilder].map.toPattern(settings)))
        pusher.io.start := io.start

        val delayLine = Module(new DifferentialDelayLine(delay))

        // Check that HBWIF parameters are configured correctly, since there is a break in the Diplomacy graph here
        val m = hbwif.module.laneModules(0).io.data.manager
        val c = hbwif.module.laneModules(0).io.data.client
        val ma = m.a.bits
        val md = m.d.bits
        val ca = c.a.bits
        val cd = c.d.bits
        require(ma.opcode.getWidth == ca.opcode.getWidth, "HBWIF is misconfigured")
        require(ma.param.getWidth == ca.param.getWidth, "HBWIF is misconfigured")
        require(ma.size.getWidth == ca.size.getWidth, "HBWIF is misconfigured")
        require(ma.source.getWidth == ca.source.getWidth, "HBWIF is misconfigured")
        require(ma.address.getWidth == ca.address.getWidth, "HBWIF is misconfigured")
        require(ma.mask.getWidth == ca.mask.getWidth, "HBWIF is misconfigured")
        require(ma.data.getWidth == ca.data.getWidth, "HBWIF is misconfigured")
        require(md.opcode.getWidth == cd.opcode.getWidth, "HBWIF is misconfigured")
        require(md.param.getWidth == cd.param.getWidth, "HBWIF is misconfigured")
        require(md.size.getWidth == cd.size.getWidth, "HBWIF is misconfigured")
        require(md.source.getWidth == cd.source.getWidth, "HBWIF is misconfigured")
        require(md.sink.getWidth == cd.sink.getWidth, "HBWIF is misconfigured")
        require(md.data.getWidth == cd.data.getWidth, "HBWIF is misconfigured")

        hbwif.module.hbwifReset(0) := reset
        hbwif.module.hbwifRefClock(0) := clock
        hbwif.module.rx(0) <> delayLine.io.out
        delayLine.io.in <> hbwif.module.tx(0)
        delayLine.io.clock := clock
        config <> pusher.io.tl

        io.finished := fuzz.module.io.finished
    }

    def settings:Seq[(String, Option[BigInt], Boolean)] = Seq(
        ("bert_enable", Option(BigInt(0)), true),
        ("mem_mode_enable", Option(BigInt(1)), true)
    )
}

class TLLaneTest(delay: Int, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {

    val dut = Module(LazyModule(new TLLaneTestLazy(delay)).module)

    io.finished := dut.io.finished
    dut.io.start := io.start

}

object TLLaneTests {

    val delays = List(1, 11, 21)

    def apply(timeout: Int = 500000)(implicit p: Parameters):Seq[UnitTest] = for (d <- delays) yield Module(new TLLaneTest(d, timeout))

}

class DifferentialDelayLine(delay: Int) extends BlackBox(Map("delay" -> IntParam(delay))) {

    val io = IO(new Bundle {
        val in = Flipped(new Differential)
        val out = new Differential
        val clock = Input(Clock())
    })

}

