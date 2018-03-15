package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

/*
class TLBidirectionalPacketizerTestLazy[S <: DecodedSymbol](decodedSymbolsPerCycle: Int, symbolFactory: () => S)(implicit val p: Parameters) extends LazyModule {

    val fuzz0 = LazyModule(new TLFuzzer(txns))
    val model0 = LazyModule(new TLRAMModel("SRAMSimple"))
    val ram0 = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes))

    val fuzz1 = LazyModule(new TLFuzzer(txns))
    val model1 = LazyModule(new TLRAMModel("SRAMSimple"))
    val ram1 = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes))

    ram0.node := TLDelayer(0.25) := lane0.clientNode
    lane1.managerNode := model0.node := fuzz0.node
    ram1.node := TLDelayer(0.25) := lane1.clientNode
    lane0.managerNode := model1.node := fuzz0.node


    lazy val module = new LazyModuleImp(this) with UnitTestModule {
        io.finished := fuzz.module.io.finished
    }
}

class TLBidirectionalPacketizerTest[S <: DecodedSymbol](decodedSymbolsPerCycle: Int, symbolFactory: () => S, timeout: Int = 50000)(implicit p: Parameters)
    extends UnitTest(timeout) {

    val dut = Module(LazyModule(new TLBidirectionalPacketizerTestLazy(decodedSymbolsPerCycle, symbolFactory)).module)

}
*/

