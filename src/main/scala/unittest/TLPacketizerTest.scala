package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest._

class TLPacketizerMasterSlaveTest[S <: DecodedSymbol](val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, timeout: Int = 50000) extends UnitTest(timeout) {

    val master = Module(new TLPacketizerMaster(params, decodedSymbolsPerCycle, symbolFactory))
    val slave = Module(new TLPacketizerSlave(params, decodedSymbolsPerCycle, symbolFactory))
    val fuzzer
    val sram

    master.io.symbolsTx <> slave.io.symbolsRx
    master.io.symbolsRx <> slave.io.symbolsTx
    master.io.symbolsTxReady := true.B
    slave.io.symbolsTxReady := true.B
    master.tl <>
    slave.tl <>

}

class TLPacketizerMasterSlave8b10bTest(decodedSymbolsPerCycle: Int, timeout: Int = 50000) extends TLPacketizerMasterSlaveTest(decodedSymbolsPerCycle, Decoded8b10bSymbol.apply _, timeout)
