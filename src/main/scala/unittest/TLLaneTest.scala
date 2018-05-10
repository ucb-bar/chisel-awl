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

class TLLaneTestLazy(delay: Int, loopback: Boolean)(implicit p: Parameters) extends LazyModule {

    val txns = 100

    require(p(HbwifTLKey).numLanes == 1, "Only testing 1 lane (per chip) right now")
    val nChips = if (loopback) 1 else 2

    val fuzz = LazyModule(new TLFuzzer(txns))
    val model = LazyModule(new TLRAMModel("SRAMSimple"))
    // Use two hbwifs rather than 2 lanes to emulate second chip
    val hbwif = Seq.fill(nChips) { LazyModule(new GenericHbwifModule) }
    val ram = LazyModule(new TLRAM(p(HbwifTLKey).managerAddressSet, beatBytes=p(CacheBlockBytes)))
    val configNodes = Seq.tabulate(nChips) { i => TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(name = s"LaneConfig$i"))))) }

    ram.node := TLDelayer(0.25) := hbwif(nChips - 1).clientNode
    hbwif(0).managerNode := model.node := fuzz.node
    hbwif.zip(configNodes).foreach { case (x, y) => x.configNodes(0) := y }
    // Dummy connections to make diplomacy happy; this does nothing since no transactions will go through
    if (!loopback) { hbwif(1).managerNode := hbwif(0).clientNode }

    lazy val module = new LazyModuleImp(this) with UnitTestModule {
        val (configs, edgeConfigs) = configNodes.map(_.out(0)).unzip

        val pushers = Seq.tabulate(nChips) { i => Module(new TLControllerPusher(edgeConfigs(i), TLController.toPattern(hbwif(i).module.addrmaps(0), p(HbwifTLKey).configAddressSets(0).base, settings))) }
        pushers.foreach(_.io.start := io.start)

        val delayLines = Seq.fill(nChips) { Module(new DifferentialDelayLine(delay)) }

        // Check that HBWIF parameters are configured correctly, since there is a break in the Diplomacy graph here
        val m = hbwif(0).module.laneModules(0).io.data.manager
        val c = hbwif(if (loopback) 0 else 1).module.laneModules(0).io.data.client
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

        hbwif.zip(delayLines).foreach { case (h, d) =>
            h.module.hbwifResets(0) := reset
            h.module.hbwifRefClocks(0) := clock
            h.module.rx(0) <> d.io.out
        }
        configs.zip(pushers).foreach { case (c, p) => c <> p.io.tl }
        if (loopback) {
            delayLines(0).io.in <> hbwif(0).module.tx(0)
            delayLines(0).io.clock := clock
        } else {
            delayLines(0).io.in <> hbwif(1).module.tx(0)
            delayLines(1).io.in <> hbwif(0).module.tx(0)
            delayLines(0).io.clock := clock
            delayLines(1).io.clock := clock
        }

        io.finished := fuzz.module.io.finished
    }

    def settings: Seq[TLControllerPattern] = Seq(
        TLControllerWritePattern("bert_enable", 0),
        TLControllerWritePattern("mem_mode_enable", 1)
    )
}

class TLLaneTest(delay: Int, loopback: Boolean, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {

    this.suggestName(s"TLLaneTest_delay_${delay}" + (if(loopback) "_loopback" else ""))

    val dut = Module(LazyModule(new TLLaneTestLazy(delay, loopback)).module)

    io.finished := dut.io.finished
    dut.io.start := io.start

}

object TLLaneTests {

    val delays = List(1, 11, 21)
    val loopbacks = List(true, false)

    def apply(timeout: Int = 500000)(implicit p: Parameters):Seq[UnitTest] = for (d <- delays; l <- loopbacks) yield Module(new TLLaneTest(d, l, timeout))

}
