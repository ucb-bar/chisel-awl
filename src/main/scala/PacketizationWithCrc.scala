package hbwif2

import chisel3._
import chisel3.util._

abstract class PacketizerWithCrc[T <: DecodedSymbol](val packetWidth: Int, val crcWidth: Int, val numSymbols: Int, symbolFactory: () => T) extends Packetizer[T,UInt,UInt](symbolFactory, UInt.apply, UInt.apply) {

}

// TODO how to negotiate with multiple masters
class FixedWidthPacketizerMaster[T <: DecodedSymbol](val numSymbols: Int, symbolFactory: () => T) extends Packetizer[T,UInt,UInt](symbolFactory, UInt.apply, UInt.apply) {

    val symbolWidth = symbolFactory().decodedWidth
    val totalTxSymbols = (dataTxFactory().getWidth + symbolWidth - 1) / symbolWidth
    val totalRxSymbols = (dataRxFactory().getWidth + symbolWidth - 1) / symbolWidth
    val totalTxCycles = (totalTxSymbols + numSymbols - 1) / numSymbols
    val totalRxCycles = (totalRxSymbols + numSymbols - 1) / numSymbols

    val sReset :: sSync :: sWack :: sIdle :: sBusy :: sLast :: Nil = Enum(6)

    val txState = RegInit(sReset)
    val rxState = RegInit(sReset)

    val txCount = UInt()
    val rxCount = UInt()
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
    when (txState === sReset) {
        txState := sSync
    } .elsewhen (txState === sSync) {
        when (~nack) {
            txState := sWack
        }
    } .elsewhen (txState === sWack) {
        when (nack) {
            txState := sSync
        } .elsewhen (ack) {
            txState := sIdle
        }
    } .elsewhen (txState === sIdle) {
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
                txCount := (totalTxSymbols - numSymbols).U
            }
        }
    } .elsewhen (txState === sBusy) {
        when (nack) {
            txState := sSync
        } .otherwise {
            // TODO shift buffer
            txCount := txCount - numSymbols.U
            when (txCount <= (2*numSymbols).U) {
                txState := sLast
            }
        }
    } .elsewhen (txState === sLast) {
        when (nack) {
            txState := sSync
        } .otherwise {
            when (io.packetTx.fire()) {
                // TODO read in buffer
                when ( totalTxSymbols.U - (numSymbols.U - txCount) > numSymbols.U) {
                    txState := sBusy
                }
                // otherwise stay in sLast
            } .otherwise {
                txState := sIdle
            }
        }
    } .otherwise {
        // Should never get here
        txState := sReset
    }

    // TX channel outputs
    io.packetTx.ready := txState === sIdle || txState === sLast

    // RX STATE MACHINE GOES HERE
    when (rxState === sReset) {
        rxState := sWack
    } .elsewhen (rxState === sWack) {
        when (ack) {
            rxState := sIdle
        }
    } .elsewhen (rxState === sIdle) {
        when (nack) {
            rxState := sWack
        } .otherwise {
            // TODO
        }
    } .elsewhen (rxState === sBusy) {
        when (nack) {
            rxState := sWack
        } .otherwise {
            
        }
    } .elsewhen (rxState === sLast) {
        when (nack) {
            rxState := sWack
        } .otherwise {
            
        }
    } .otherwise {
        // Should never get here
        rxState := sReset
    }

}
