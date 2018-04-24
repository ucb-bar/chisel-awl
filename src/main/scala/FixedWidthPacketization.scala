package hbwif

import chisel3._
import chisel3.util._

import freechips.rocketchip.util.AsyncQueue

class FixedWidthData[F <: Data](factory: () => F) extends Bundle {
    val tx = Flipped(Decoupled(factory()))
    val rx = Decoupled(factory())
}

object FixedWidthData {
    def apply[F <: Data](factory: () => F)() = new FixedWidthData[F](factory)
}

class FixedWidthPacketizerIO[S <: DecodedSymbol, F <: Data](decodedSymbolsPerCycle: Int, symbolFactory: () => S, val fwDataFactory: () => F)
    extends PacketizerIO[S, FixedWidthData[F]](decodedSymbolsPerCycle, symbolFactory, FixedWidthData.apply(fwDataFactory) _) {
    val enable = Input(Bool())
}

class FixedWidthPacketizer[S <: DecodedSymbol, F <: Data](decodedSymbolsPerCycle: Int, symbolFactory: () => S, val fwDataFactory: () => F)
    extends Packetizer[S, FixedWidthData[F]](decodedSymbolsPerCycle, symbolFactory, FixedWidthData.apply(fwDataFactory) _) {

    val io = IO(new FixedWidthPacketizerIO(decodedSymbolsPerCycle, symbolFactory, fwDataFactory))

    val decodedWidth = symbolFactory().decodedWidth
    val packetWidth = io.data.tx.bits.toBits.getWidth
    val symbolsPerPacket = (packetWidth + decodedWidth - 1)/decodedWidth
    val cyclesPerPacket = (symbolsPerPacket + decodedSymbolsPerCycle - 1)/decodedSymbolsPerCycle
    val extendedWidth = (if (cyclesPerPacket > 1) packetWidth else decodedSymbolsPerCycle*decodedWidth)

    val txBuffer = Reg(UInt(packetWidth.W))
    val rxBuffer = Reg(Vec(symbolsPerPacket, UInt(8.W)))
    val txExtended = Wire(UInt(extendedWidth.W))

    if (extendedWidth > packetWidth) {
        txExtended := Cat(txBuffer, 0.U((extendedWidth-packetWidth).W))
    } else {
        txExtended := txBuffer
    }

    val sTxReset :: sTxSync :: sTxAck :: sTxReady :: Nil = Enum(4)
    val txState = RegInit(sTxReset)

    val txCount = RegInit(0.U(log2Ceil(symbolsPerPacket + 1).W))

    // These come from the RX
    val ack = io.symbolsRx map { x => x.valid && (x.bits === symbolFactory().ack) } reduce (_||_)
    val nack = io.symbolsRx map { x => x.valid && (x.bits === symbolFactory().nack) } reduce (_||_)

    when (io.enable) {
        when (txState === sTxReset) {
            txState := sTxSync
        } .elsewhen(txState === sTxSync) {
            when (ack) {
                txState := sTxReady
            } .otherwise {
                txState := sTxAck
            }
        } .elsewhen(txState === sTxAck) {
            when (nack) {
                txState := sTxSync
            } .elsewhen(ack) {
                txState := sTxReady
            }
        } .elsewhen(txState === sTxReady) {
            when (nack) {
                txState := sTxSync
            }
        } .otherwise {
            // shouldn't get here
            txState := sTxSync
        }
    } .otherwise {
        txState := sTxReset
    }

    io.symbolsTx.reverse.zipWithIndex.foreach { case (s,i) =>
        val doSync = ((i.U === 0.U) && (txState === sTxSync))
        s.valid := ((i.U < txCount) && (txState === sTxReady)) || doSync
        val sym = txExtended(extendedWidth - decodedWidth*i - 1, extendedWidth - decodedWidth*(i+1))
        s.bits := Mux(doSync, symbolFactory().ack, symbolFactory().fromData(sym))
    }

    io.data.tx.ready := io.enable && (txCount === 0.U) && (txState === sTxReady)

    when (io.data.tx.fire()) {
        txCount := symbolsPerPacket.U - txCount
        txBuffer := io.data.tx.bits.toBits
    } .elsewhen(txCount > decodedSymbolsPerCycle.U) {
        when (io.symbolsTxReady) {
            txCount := txCount - decodedSymbolsPerCycle.U
            txBuffer := txBuffer << (decodedSymbolsPerCycle*8)
        }
    } .otherwise {
        when (io.symbolsTxReady) {
            txCount := 0.U
        }
    }

    val rxSymCount = RegInit(0.U(log2Ceil(symbolsPerPacket + 1).W))

    assert(io.data.rx.ready || !io.data.rx.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")

    io.data.rx.bits := io.data.rx.bits.fromBits(rxBuffer.asUInt()(packetWidth - 1, 0))
    io.data.rx.valid := (rxSymCount >= symbolsPerPacket.U)

    rxSymCount := io.symbolsRx.foldRight(rxSymCount - Mux(io.data.rx.fire(), symbolsPerPacket.U, 0.U)) { (symbol, count) =>
        when (symbol.valid && symbol.bits.isData) {
            rxBuffer((symbolsPerPacket - 1).U - count) := symbol.bits.bits
        }
        count + (symbol.valid && symbol.bits.isData && (txState === sTxReady))
    }

    def connectData(dataClock: Clock, dataReset: Bool, data: FixedWidthData[F]) {
        val qDepth = 8
        val qSync = 3
        val qSafe = true
        val qNarrow = false

        val txq = Module(new AsyncQueue(io.data.tx.bits, qDepth, qSync, qSafe, qNarrow))
        val rxq = Module(new AsyncQueue(io.data.rx.bits, qDepth, qSync, qSafe, qNarrow))

        txq.suggestName("AsyncQueueTx")
        rxq.suggestName("AsyncQueueRx")

        txq.io.enq <> data.tx
        io.data.tx <> txq.io.deq
        data.rx <> rxq.io.deq
        rxq.io.enq <> io.data.rx

        txq.io.enq_clock := dataClock
        txq.io.deq_clock := this.clock

        rxq.io.enq_clock := this.clock
        rxq.io.deq_clock := dataClock

        txq.io.enq_reset := dataReset
        txq.io.deq_reset := this.reset.toBool

        rxq.io.enq_reset := this.reset.toBool
        rxq.io.deq_reset := dataReset

    }

    def connectController(builder: ControllerBuilder) {
        builder.w("packetizer_enable", io.enable, 0)
    }

}

trait HasFixedWidthPacketizer[F <: Data] {
    this: Lane =>

    type T = FixedWidthData[F]

    def decodedSymbolsPerCycle: Int
    val fwDataFactory: () => F

    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S) = new FixedWidthPacketizer[S, F](decodedSymbolsPerCycle, symbolFactory, fwDataFactory)

}
