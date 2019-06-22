package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.MultiIOModule

class PacketizerIO[S <: DecodedSymbol, T <: Data](val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, val dataFactory: () => T) extends Bundle {
    val symbolsTx = Vec(decodedSymbolsPerCycle, Valid(symbolFactory()))
    val symbolsTxReady = Input(Bool())
    val data = dataFactory()
    val symbolsRx = Vec(decodedSymbolsPerCycle, Flipped(Valid(symbolFactory())))
}

abstract class Packetizer[S <: DecodedSymbol, T <: Data](val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, val dataFactory: () => T) extends MultiIOModule with HasControllerConnector {

    val dataWidth = decodedSymbolsPerCycle * symbolFactory().decodedWidth

    val io = IO(new PacketizerIO[S, T](decodedSymbolsPerCycle, symbolFactory, dataFactory))

    // This method must implement async crossings into the global "clock" domain
    def connectData(dataClock: Clock, dataReset: Bool, data: T)

}

trait NoHandshakePacketizerStateMachine[S <: DecodedSymbol, T <: Data] extends Packetizer[S, T] {

    val txEnable = Wire(Bool())
    val rxEnable = Wire(Bool())

    val sTxReset :: sTxReady :: Nil = Enum(2)
    val sRxReset :: sRxReady :: Nil = Enum(2)
    val txState = RegInit(sTxReset)
    val rxState = RegInit(sRxReset)

    when (txEnable) {
       txState := sTxReady
    } .otherwise {
       txState := sTxReset
    }

    when (rxEnable) {
       rxState := sRxReady
    } .otherwise {
       rxState := sRxReset
    }

    val txSymbolData = Wire(Vec(decodedSymbolsPerCycle, UInt(symbolFactory().decodedWidth.W)))
    val txSymbolValid = Wire(Vec(decodedSymbolsPerCycle, Bool()))

    io.symbolsTx.reverse.zipWithIndex.foreach { case (s,i) =>
        val doSync = (i.U === 0.U) && (txState === sTxReset)
        s.valid := (txSymbolValid(i) && (txState === sTxReady)) || doSync
        s.bits := Mux(doSync, symbolFactory().sync, symbolFactory().fromData(txSymbolData(i)))
    }

}

trait BasicPacketizerStateMachine[S <: DecodedSymbol, T <: Data] extends Packetizer[S, T] {

    val enable = Wire(Bool())

    // state machine states
    val sReset :: sSync :: sAckAcked :: sAckSynced :: sWaitForAck :: sReady :: Nil = Enum(6)
    val state = RegInit(sReset)

    // These come from the RX
    val ack = io.symbolsRx map { x => x.valid && (x.bits === symbolFactory().ack) } reduce (_||_)
    val sync = io.symbolsRx map { x => x.valid && (x.bits === symbolFactory().sync) } reduce (_||_)

    // This counter will send a sync every N cycles in an attempt to communicate with the receiver
    val txSyncCounter = Counter(16)

    when (enable) {
        // I am in reset
        when (state === sReset) {
            state := sSync
        // I am waiting for an ack or a sync to signify the remote link is up
        // I will periodically send syncs to the remote link while waiting
        } .elsewhen(state === sSync) {
            txSyncCounter.inc()
            // ack should have priority over sync
            when (ack) {
                state := sAckAcked
            } .elsewhen (sync) {
                state := sAckSynced
            }
        // I got an ack, so I can just ack back and then go straight to sReady
        } .elsewhen(state === sAckAcked) {
            // Wait until symbolsTxReady (i.e. symbolsTx fires)
            when (io.symbolsTxReady) {
                state := sReady
            }
        // I got a sync, so I need to wait for an ack after sending mine
        } .elsewhen(state === sAckSynced) {
            // If I happen to get an ack here, I still may need to ack back, so go to sAckAcked
            when (ack) {
                state := sAckAcked
            // Wait until symbolsTxReady (i.e. symbolsTx fires)
            } .elsewhen (io.symbolsTxReady) {
                state := sWaitForAck
            }
        // I am waiting for an ack to go to sReady; I came from sAckSynced
        } .elsewhen(state === sWaitForAck) {
            when (ack) {
                state := sReady
            }
        }
        // otherwise I am ready! I will not respond to acks or syncs
    } .otherwise {
        state := sReset
    }

    val txSymbolData = Wire(Vec(decodedSymbolsPerCycle, UInt(symbolFactory().decodedWidth.W)))
    val txSymbolValid = Wire(Vec(decodedSymbolsPerCycle, Bool()))

    io.symbolsTx.reverse.zipWithIndex.foreach { case (s,i) =>
        val doSync = (i.U === 0.U) && (state === sSync) && (txSyncCounter.value === 0.U)
        val doAck = (i.U === 0.U) && ((state === sAckAcked) || (state === sAckSynced))
        s.valid := (txSymbolValid(i) && (state === sReady)) || doSync || doAck
        s.bits := Mux(doSync, symbolFactory().sync, Mux(doAck, symbolFactory().ack, symbolFactory().fromData(txSymbolData(i))))
    }

}
