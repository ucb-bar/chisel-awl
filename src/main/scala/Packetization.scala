package hbwif

import chisel3._
import chisel3.util._

class PacketizerIO[S <: DecodedSymbol, T <: Data](val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, val dataFactory: () => T) extends Bundle {
    val symbolsTx = Vec(decodedSymbolsPerCycle, Valid(symbolFactory()))
    val symbolsTxReady = Input(Bool())
    val data = dataFactory()
    val symbolsRx = Vec(decodedSymbolsPerCycle, Flipped(Valid(symbolFactory())))
}

abstract class Packetizer[S <: DecodedSymbol, T <: Data](val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, val dataFactory: () => T) extends Module with HasControllerConnector {

    val dataWidth = decodedSymbolsPerCycle * symbolFactory().decodedWidth

    val io: PacketizerIO[S, T]

    def connectData(data: T)

}

