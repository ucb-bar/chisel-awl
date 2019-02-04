package awl.test.tilelink

import awl.hbwif._
import awl.tilelink._
import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._


class TLBidirectionalPacketizerTestLazy[S <: DecodedSymbol](decodedSymbolsPerCycle: Int, symbolFactory: () => S)(implicit p: Parameters) extends LazyModule {

    val numXact = 32
    val beatBytes = 16
    val managerAddressSet = Seq(AddressSet(0x0, 0x3ff))

    val fuzz = LazyModule(new TLFuzzer(100))
    val model = LazyModule(new TLRAMModel("SRAMSimple"))
    val ram = LazyModule(new TLRAM(managerAddressSet(0), beatBytes = beatBytes))
    val adapter = TLAdapterNode()
    ram.node := adapter := TLDelayer(0.25) := model.node := fuzz.node

    lazy val module = new LazyModuleImp(this) with UnitTestModule {
        val (in, edgeIn) = adapter.in(0)
        val (out, edgeOut) = adapter.out(0)

        val packetizer = Module(new TLBidirectionalPacketizer(edgeOut, edgeIn, decodedSymbolsPerCycle, symbolFactory))

        packetizer.controlIO.get.enable := true.B

        out <> packetizer.io.data.client
        packetizer.io.data.manager <> in

        packetizer.io.data.client.b.valid := false.B
        packetizer.io.data.manager.c.valid := false.B
        packetizer.io.data.manager.e.valid := false.B
        in.b.valid := false.B
        out.c.valid := false.B
        out.e.valid := false.B

        packetizer.io.symbolsTx <> packetizer.io.symbolsRx
        packetizer.io.symbolsTxReady := true.B

        io.finished := fuzz.module.io.finished
    }
}

class TLBidirectionalPacketizerTest[S <: DecodedSymbol](decodedSymbolsPerCycle: Int, symbolFactory: () => S, timeout: Int = 500000)(implicit p: Parameters)
    extends UnitTest(timeout) {

    val dut = Module(LazyModule(new TLBidirectionalPacketizerTestLazy(decodedSymbolsPerCycle, symbolFactory)).module)

    io.finished := dut.io.finished
    dut.io.start := io.start

}

object TLPacketizerTests {

    val decodedSymbols = List(1,3,4,5)
    val factories = List(Decoded8b10bSymbol.apply _)

    def apply()(implicit p: Parameters):Seq[UnitTest] = for (x <- decodedSymbols; y <- factories) yield Module(new TLBidirectionalPacketizerTest(x, y))

}
