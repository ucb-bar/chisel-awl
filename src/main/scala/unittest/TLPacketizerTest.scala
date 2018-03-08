package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest._

class TLBidirectionalPacketizerTestLazy[S <: DecodedSymbol](decodedSymbolsPerCycle: Int, symbolFactory: () => S, )(implicit p: Parameters) extends LazyModule {

    val fuzz = LazyModule(new TLFuzzer(txns))
    val model = LazyModule(new TLRAMModel("SRAMSimple"))
    val ram = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff), beatBytes = ramBeatBytes))

    ram.node := TLDelayer(0.25) := model.node := fuzz.node

    lazy val module = new LazyModuleImp(this) with UnitTestModule {
        io.finished := fuzz.module.io.finished
    }
}

class TLBidirectionalPacketizerTest[S <: DecodedSymbol](decodedSymbolsPerCycle: Int, symbolFactory: () => S, timeout: Int = 50000)(implicit p: Parameters)
    extends UnitTest(timeout) {

    val dut = Module(LazyModule(new TLBidirectionalPacketizerTestLazy(decodedSymbolsPerCycle, symbolFactory)).module)

}

class TLBidirectionalPacketizerTest8b10b(decodedSymbolsPerCycle: Int, timeout: Int = 50000)
    extends TLBidirectionalPacketizerTest(Decoded8b10bSymbol.apply _, timeout)(implicit p: Parameters)
