package hbwif2

import chisel3._
import chisel3.util._

abstract class Packetizer[S <: DecodedSymbol, T <: Data](val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, val dataFactory: () => T) extends Module with HasControllerConnector {

    val dataWidth = decodedSymbolsPerCycle * symbolFactory().decodedWidth

    final val io = IO(new Bundle {
        val symbolsTx = Vec(decodedSymbolsPerCycle, Valid(symbolFactory()))
        val symbolsTxReady = Input(Bool())
        val data = dataFactory()
        val symbolsRx = Vec(decodedSymbolsPerCycle, Flipped(Valid(symbolFactory())))
    })

    def connectData(data: T)

}

