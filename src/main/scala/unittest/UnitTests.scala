package hbwif2

import hbwif2.tilelink._
import chisel3.Module
import freechips.rocketchip.unittest._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.AddressSet

class TestHarness(implicit p: Parameters)
    extends freechips.rocketchip.unittest.TestHarness

class UnitTestConfig extends Config((site, here, up) => {
    case HbwifTLKey => HbwifTLConfig(
        managerAddressSets = Seq(AddressSet(0x00000, 0xffff)),
        configAddressSets = Seq(AddressSet(0x10000, 0xffff)),
        numLanes = 1,
        numBanks = 1,
        beatBytes = 16,
        numXact = 2)
    case HbwifSerDesKey => SerDesConfig()
    case HbwifBertKey => BertConfig()
    case HbwifPatternMemKey => PatternMemConfig()
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        //ScanChainTests() ++
        //Encoding8b10bTests() ++
        //TLPacketizerTests()(p) ++
        TLControllerTests()(p)
        //TLLaneTests()(p)
        //Seq(Module(new freechips.rocketchip.tilelink.TLRAMSimpleTest(16)))
    }
})
