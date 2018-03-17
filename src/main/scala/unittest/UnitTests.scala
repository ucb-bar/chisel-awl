package hbwif2

import chisel3.Module
import hbwif2.tilelink.TLPacketizerTests
import freechips.rocketchip.unittest._
import freechips.rocketchip.config.{Config, Parameters}

class TestHarness(implicit p: Parameters)
    extends freechips.rocketchip.unittest.TestHarness

class UnitTestConfig extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        //ScanChainTests() ++
        //Encoding8b10bTests() ++
        TLPacketizerTests()(p)
        //Seq(Module(new freechips.rocketchip.tilelink.TLRAMSimpleTest(16)))
    }
})
