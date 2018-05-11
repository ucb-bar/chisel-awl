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
        beatBytes = 8,
        numXact = 32,
        managerTLUH = false,
        clientTLUH = false,
        managerTLC = false,
        clientTLC = false)
    case HbwifSerDesKey => SerDesConfig()
    case HbwifBertKey => BertConfig()
    case HbwifPatternMemKey => PatternMemConfig()
    case CacheBlockBytes => 32
    case PeripheryBusKey => PeripheryBusParams(
        beatBytes = 8,
        blockBytes = 16
    )
})

class TLPacketizerTestConfig extends Config(new UnitTestConfig ++ new WithTLPacketizerTests)
class WithTLPacketizerTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        TLPacketizerTests()(p)
    }
})

class BertTestConfig extends Config(new UnitTestConfig ++ new WithBertTests)
class WithBertTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        BertTests()
    }
})

class Encoding8b10bTestConfig extends Config(new UnitTestConfig ++ new WithEncoding8b10bTests)
class WithEncoding8b10bTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        Encoding8b10bTests()
    }
})

class BitStufferTestConfig extends Config(new UnitTestConfig ++ new WithBitStufferTests)
class WithBitStufferTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        BitStufferTests()
    }
})

class TLLaneTestConfig extends Config(new UnitTestConfig ++ new WithTLLaneTests)
class WithTLLaneTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        TLLaneTests()(p)
    }
})

class FixedWidthLaneTestConfig extends Config(new UnitTestConfig ++ new WithFixedWidthLaneTests)
class WithFixedWidthLaneTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        FixedWidthLaneTests()
    }
})
