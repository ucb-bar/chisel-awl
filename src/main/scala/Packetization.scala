package hbwif2

import chisel3._
import chisel3.util._

abstract class Packetizer[S <: DecodedSymbol, T <: Data, R <: Data](val symbolFactory: () => S, val dataTxFactory: () => T, val dataRxFactory: () => R) extends Module {

    val decodedSymbolsPerCycle: Int

    final val io = IO(new Bundle {
        val packetTx = Flipped(Decoupled(dataTxFactory()))
        val symbolsTx = Vec(decodedSymbolsPerCycle, Valid(symbolFactory()))
        val symbolsTxReady = Input(Bool())
        val packetRx = Decoupled(dataRxFactory())
        val symbolsRx = Vec(decodedSymbolsPerCycle, Flipped(Valid(symbolFactory())))
    })

}

