package hbwif2

import chisel3._
import chisel3.util._

abstract class Packetizer[T <: DecodedSymbol, U <: Data, V <: Data](val symbolFactory: () => T, val dataTxFactory: () => U, val dataRxFactory: () => V) extends Module {

    // TODO can we figure this out automatically from the Encoder?
    val decodedSymbolsPerCycle: Int

    final val io = IO(new Bundle {
        val packetTx = Flipped(Decoupled(dataTxFactory()))
        val symbolsTx = Vec(decodedSymbolsPerCycle, Decoupled(symbolFactory()))
        val packetRx = Decoupled(dataRxFactory())
        val symbolsRx = Vec(decodedSymbolsPerCycle, Flipped(Valid(symbolFactory())))
    })

}

