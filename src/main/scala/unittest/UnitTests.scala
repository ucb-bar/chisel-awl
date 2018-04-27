package hbwif

import hbwif.tilelink._
import chisel3.Module
import freechips.rocketchip.unittest._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tile.XLen

class TestHarness(implicit p: Parameters)
    extends freechips.rocketchip.unittest.TestHarness

class UnitTestConfig extends Config((site, here, up) => {
    case XLen => 64
    case HbwifTLKey => HbwifTLConfig(
        managerAddressSet = AddressSet(0x10000, 0xffff),
        configAddressSets = Seq(AddressSet(0x00000, 0xffff)),
        numLanes = 1,
        numBanks = 1,
        maxOutstanding = 8,
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
        /*
        ScanChainTests() ++
        Encoding8b10bTests() ++
        TLPacketizerTests()(p) ++
        TLControllerTests()(p) ++
        BitStufferTests() ++
        TLLaneTests()(p) ++
        BertTests() ++
        */
        FixedWidthLaneTests()
    }
})
