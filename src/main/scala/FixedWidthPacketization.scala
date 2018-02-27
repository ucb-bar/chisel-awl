package hbwif2

import chisel3._
import chisel3.util._

// TODO DO NOT USE
class FixedWidthPacketizerMaster[T <: DecodedSymbol, U <: Data, V <: Data](val decodedSymbolsPerCycle: Int, val symbolFactory: () => T, val dataTxFactory: () => U, val dataRxFactory: () => V) extends Packetizer {

    type DecodedSymbolType = T
    type TxDataType = U
    type RxDataType = V

    val symbolWidth = symbolFactory().decodedWidth
    val totalTxSymbols = (dataTxFactory().getWidth + symbolWidth - 1) / symbolWidth
    val totalRxSymbols = (dataRxFactory().getWidth + symbolWidth - 1) / symbolWidth
    val totalTxCycles = (totalTxSymbols + decodedSymbolsPerCycle - 1) / decodedSymbolsPerCycle
    val totalRxCycles = (totalRxSymbols + decodedSymbolsPerCycle - 1) / decodedSymbolsPerCycle

    val sReset :: sSync :: sWack :: sIdle :: sBusy :: sLast :: Nil = Enum(6)

    val txState = RegInit(sReset)
    val rxState = RegInit(sReset)

    val txCount = Wire(UInt())
    val rxCount = Wire(UInt())
    if (totalTxCycles > 1) {
        txCount := Reg(totalTxSymbols.U)
    }
    if (totalRxCycles > 1) {
        rxCount := Reg(totalRxSymbols.U)
    }

    val txBuf = Reg(Vec(totalTxSymbols, UInt(symbolWidth.W)))
    val rxBuf = Reg(Vec(totalRxSymbols, UInt(symbolWidth.W)))

    // Ack goes high when it sees an ACK symbol from the RX
    // This will tell the packet interface that it's ready for use
    val ack = io.symbolsRx.map { x => x.valid && x.bits === x.bits.ack } reduce { _ || _ }
    // Nack says something bad happened; reset
    val nack = io.symbolsRx.map { x => x.valid && x.bits === x.bits.nack } reduce { _ || _ }

    // TX state machine
    switch (txState) {

        is (sReset) {
            txState := sSync
        }

        is (sSync) {
            when (nack) {
                txState := sSync
            } .elsewhen (ack) {
                txState := sIdle
            }
        }

        is (sIdle) {
            when (nack) {
                txState := sSync
            } .elsewhen (io.packetTx.fire()) {
                // TODO read in buffer
                if (totalTxCycles > 2) {
                    txState := sBusy
                } else if (totalTxCycles > 1) {
                    txState := sLast
                }
                // We only  need to count if we actually use the other states
                if (totalTxCycles > 1) {
                    txCount := (totalTxSymbols - decodedSymbolsPerCycle).U
                }
            }
        }

        is (sBusy) {
            when (nack) {
                txState := sSync
            } .otherwise {
                // TODO shift buffer
                txCount := txCount - decodedSymbolsPerCycle.U
                when (txCount <= (2*decodedSymbolsPerCycle).U) {
                    txState := sLast
                }
            }
        }

        is (sLast) {
            when (nack) {
                txState := sSync
            } .otherwise {
                when (io.packetTx.fire()) {
                    // TODO read in buffer
                    when ( totalTxSymbols.U - (decodedSymbolsPerCycle.U - txCount) > decodedSymbolsPerCycle.U) {
                        txState := sBusy
                    }
                    // otherwise stay in sLast
                } .otherwise {
                    txState := sIdle
                }
            }
        }
    }

    // TX channel outputs
    io.packetTx.ready := txState === sIdle || txState === sLast

    // RX state machine
    switch (rxState) {

        is (sReset) {
            rxState := sWack
        }

        is (sWack) {
            when (ack) {
                rxState := sIdle
            }
        }

        is (sIdle) {
            when (nack) {
                rxState := sWack
            } .otherwise {
                // TODO
            }
        }

        is (sBusy) {
            when (nack) {
                rxState := sWack
            } .otherwise {
            }
        }

        is (sLast) {
            when (nack) {
                rxState := sWack
            } .otherwise {
            }
        }
    }
}
