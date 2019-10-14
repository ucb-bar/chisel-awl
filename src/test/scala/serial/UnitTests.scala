package awl.serial

import chisel3.Module
import freechips.rocketchip.unittest._
import freechips.rocketchip.config.{Config, Parameters}

class TestHarness(implicit p: Parameters)
    extends freechips.rocketchip.unittest.TestHarness

class FixedWidthLaneTestConfig extends Config(new WithFixedWidthLaneTests)
class WithFixedWidthLaneTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        FixedWidthLaneTests()
    }
})

class BertTestConfig extends Config(new WithBertTests)
class WithBertTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        BertTests()
    }
})

class Encoding8b10bTestConfig extends Config(new WithEncoding8b10bTests)
class WithEncoding8b10bTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        Encoding8b10bTests()
    }
})

class BitStufferTestConfig extends Config(new WithBitStufferTests)
class WithBitStufferTests extends Config((site, here, up) => {
    case UnitTests => (q: Parameters) => {
        implicit val p = q
        BitStufferTests()
    }
})
