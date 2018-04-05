package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._

class TLLaneTestLazy(delay: Int)(implicit p: Parameters) extends LazyModule {

    val txns = 100

    require(p(HbwifTLKey).numLanes == 1, "Only testing 1 lane right now")

    val fuzz = LazyModule(new TLFuzzer(txns))
    val model = LazyModule(new TLRAMModel("SRAMSimple"))
    val hbwif = LazyModule(new GenericHbwifModule)
    val ram = LazyModule(new TLRAM(p(HbwifTLKey).managerAddressSets(0), beatBytes = p(HbwifTLKey).beatBytes))
    val configNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(name = "LaneConfig")))))

    ram.node := TLDelayer(0.25) := hbwif.clientNodes(0)
    hbwif.managerNodes(0) := model.node := fuzz.node
    hbwif.configNodes(0) := configNode

    lazy val module = new LazyModuleImp(this) with UnitTestModule {
        val (config, edgeConfig) = configNode.out(0)

        val pusher = Module(new TLControllerPusher(edgeConfig, hbwif.module.laneModules(0).builder.asInstanceOf[TLControllerBuilder].map.toPattern(settings)))
        pusher.io.start := io.start

        val delayLine = Module(new DifferentialDelayLine(delay))

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

