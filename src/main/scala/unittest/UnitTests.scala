package hbwif

import hbwif.tilelink._
import chisel3.Module
import freechips.rocketchip.unittest._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.subsystem.{PeripheryBusKey, PeripheryBusParams, CacheBlockBytes}
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
        managerTLUH = false,
        clientTLUH = false,
        managerTLC = false,
        clientTLC = false)
    case HbwifSerDesKey => SerDesConfig()
    case HbwifBertKey => BertConfig()
    case HbwifPatternMemKey => PatternMemConfig()
    case CacheBlockBytes => 16
    case PeripheryBusKey => PeripheryBusParams(
        beatBytes = 8,
        blockBytes = 8
    )
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        //TODO TLControllerTests()(p) ++
        TLPacketizerTests()(p) ++
        BertTests() ++
        Encoding8b10bTests() ++
        BitStufferTests() ++
        TLLaneTests()(p)
       // FixedWidthLaneTests()
    }
})
