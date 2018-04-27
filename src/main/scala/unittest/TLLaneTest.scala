package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem.CacheBlockBytes

class TLLaneTestLazy(delay: Int)(implicit p: Parameters) extends LazyModule {

    val txns = 100

    require(p(HbwifTLKey).numLanes == 1, "Only testing 1 lane right now")

    val fuzz = LazyModule(new TLFuzzer(txns))
    val model = LazyModule(new TLRAMModel("SRAMSimple"))
    val hbwif = LazyModule(new GenericHbwifModule)
    val ram = LazyModule(new TLRAM(p(HbwifTLKey).managerAddressSet, beatBytes=p(CacheBlockBytes)))
    val configNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(name = "LaneConfig")))))

    ram.node := TLDelayer(0.25) := hbwif.clientNode
    hbwif.managerNode := model.node := fuzz.node
    hbwif.configNode := configNode

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
        require(ma.opcode.getWidth == ca.opcode.getWidth,   s"HBWIF is misconfigured in TLLaneTest: A opcode mismatch: ${ma.opcode.getWidth} != ${ca.opcode.getWidth}")
        require(ma.param.getWidth == ca.param.getWidth,     s"HBWIF is misconfigured in TLLaneTest: A param mismatch: ${ma.param.getWidth} != ${ca.param.getWidth}")
        require(ma.size.getWidth == ca.size.getWidth,       s"HBWIF is misconfigured in TLLaneTest: A size mismatch: ${ma.size.getWidth} != ${ca.size.getWidth}")
        require(ma.source.getWidth == ca.source.getWidth,   s"HBWIF is misconfigured in TLLaneTest: A source mismatch: ${ma.source.getWidth} != ${ca.source.getWidth}")
        require(ma.address.getWidth == ca.address.getWidth, s"HBWIF is misconfigured in TLLaneTest: A address mismatch: ${ma.address.getWidth} != ${ca.address.getWidth}")
        require(ma.mask.getWidth == ca.mask.getWidth,       s"HBWIF is misconfigured in TLLaneTest: A mask mismatch: ${ma.mask.getWidth} != ${ca.mask.getWidth}")
        require(ma.data.getWidth == ca.data.getWidth,       s"HBWIF is misconfigured in TLLaneTest: A data mismatch: ${ma.data.getWidth} != ${ca.data.getWidth}")
        require(md.opcode.getWidth == cd.opcode.getWidth,   s"HBWIF is misconfigured in TLLaneTest: D opcode mismatch: ${md.opcode.getWidth} != ${cd.opcode.getWidth}")
        require(md.param.getWidth == cd.param.getWidth,     s"HBWIF is misconfigured in TLLaneTest: D param mismatch: ${md.param.getWidth} != ${cd.param.getWidth}")
        require(md.size.getWidth == cd.size.getWidth,       s"HBWIF is misconfigured in TLLaneTest: D size mismatch: ${md.size.getWidth} != ${cd.size.getWidth}")
        require(md.source.getWidth == cd.source.getWidth,   s"HBWIF is misconfigured in TLLaneTest: D source mismatch: ${md.source.getWidth} != ${cd.source.getWidth}")
        require(md.sink.getWidth == cd.sink.getWidth,       s"HBWIF is misconfigured in TLLaneTest: D sink mismatch: ${md.sink.getWidth} != ${cd.sink.getWidth}")
        require(md.data.getWidth == cd.data.getWidth,       s"HBWIF is misconfigured in TLLaneTest: D data mismatch: ${md.data.getWidth} != ${cd.data.getWidth}")

        hbwif.module.hbwifResets(0) := reset
        hbwif.module.hbwifRefClocks(0) := clock
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
