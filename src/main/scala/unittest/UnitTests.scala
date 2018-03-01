package hbwif2

import chisel3.Module
import freechips.rocketchip.unittest._
import freechips.rocketchip.config.{Config, Parameters}

class TestHarness(implicit p: Parameters)
    extends freechips.rocketchip.unittest.TestHarness

class UnitTestConfig extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        Seq(
            Module(new ScanChainControllerTest)
        ) ++ Encoding8b10bTests()
    }
})
