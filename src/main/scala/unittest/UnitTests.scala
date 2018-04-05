package hbwif

import hbwif.tilelink._
import chisel3.Module
import freechips.rocketchip.unittest._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.coreplex.CacheBlockBytes
import freechips.rocketchip.diplomacy.AddressSet

class TestHarness(implicit p: Parameters)
    extends freechips.rocketchip.unittest.TestHarness

class UnitTestConfig extends Config((site, here, up) => {
    case HbwifTLKey => HbwifTLConfig(
        managerAddressSets = Seq(AddressSet(0x10000, 0xffff)),
        configAddressSets = Seq(AddressSet(0x00000, 0xffff)),
        numLanes = 1,
        numBanks = 1,
        beatBytes = 16,
        numXact = 32,
        tluh = false,
        tlc = false)
    case HbwifSerDesKey => SerDesConfig()
    case HbwifBertKey => BertConfig()
    case HbwifPatternMemKey => PatternMemConfig()
    case CacheBlockBytes => 16
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        ScanChainTests() ++
        Encoding8b10bTests() ++
        TLPacketizerTests()(p) ++
        TLControllerTests()(p) ++
        TLLaneTests()(p) ++
        BitStufferTests()
        TLLaneTests()(p)
    }
})
