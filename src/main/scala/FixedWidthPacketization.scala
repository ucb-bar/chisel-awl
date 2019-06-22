package hbwif

import chisel3._
import chisel3.util._

// TODO get rid of this rocketchip dependency
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}

class FixedWidthData[F <: Data](val factory: () => F) extends Bundle {
    val tx = Flipped(Decoupled(factory()))
    val rx = Decoupled(factory())
}

object FixedWidthData {
    def apply[F <: Data](factory: () => F)() = new FixedWidthData[F](factory)
}

class FixedWidthPacketizer[S <: DecodedSymbol, F <: Data](decodedSymbolsPerCycle: Int, symbolFactory: () => S, val fwDataFactory: () => F)
    extends Packetizer[S, FixedWidthData[F]](decodedSymbolsPerCycle, symbolFactory, FixedWidthData.apply(fwDataFactory) _)
    with BasicPacketizerStateMachine[S, FixedWidthData[F]] {

    override val controlIO = Some(IO(new ControlBundle {
        val enable = input(Bool(), 0, "packetizer_enable",
            "When high, enables traffic to flow over the interface. Unsafe to deassert while traffic is in flight", TxClock)
    }))

    enable := controlIO.get.enable

    val decodedWidth = symbolFactory().decodedWidth
    val packetWidth = io.data.tx.bits.toBits.getWidth
    val symbolsPerPacket = (packetWidth + decodedWidth*decodedSymbolsPerCycle - 1)/decodedWidth
    val cyclesPerPacket = (symbolsPerPacket + decodedSymbolsPerCycle - 1)/decodedSymbolsPerCycle
    val extendedWidth = symbolsPerPacket*decodedWidth

    val txBuffer = Reg(UInt(packetWidth.W))
    val rxBufferEntries = 2*symbolsPerPacket - 1
    val rxBuffer = Reg(Vec(rxBufferEntries, UInt(decodedWidth.W)))
    val txExtended = (if (extendedWidth > packetWidth) Cat(txBuffer, 0.U((extendedWidth-packetWidth).W)) else txBuffer)

    val txCount = RegInit(0.U(log2Ceil(symbolsPerPacket + 1).W))

    io.data.tx.ready := enable && (state === sReady) && ((txCount === 0.U) || ((txCount <= decodedSymbolsPerCycle.U) && io.symbolsTxReady))

    when (io.data.tx.fire()) {
        txCount := symbolsPerPacket.U
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

    // Feed symbols to the state machine stuff
    for (i <- 0 until decodedSymbolsPerCycle) {
        txSymbolData(i) := txExtended(extendedWidth - decodedWidth*i - 1, extendedWidth - decodedWidth*(i+1))
        txSymbolValid(i) := (i.U < txCount)
    }

    val rxSymCount = RegInit(0.U(log2Ceil(rxBufferEntries + 1).W))

    assert(io.data.rx.ready || !io.data.rx.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")

    io.data.rx.bits := io.data.rx.bits.fromBits(rxBuffer.reverse.take(symbolsPerPacket).reduce(Cat(_,_))(extendedWidth - 1, extendedWidth - packetWidth))
    io.data.rx.valid := (rxSymCount >= symbolsPerPacket.U)

    rxSymCount := io.symbolsRx.foldRight(rxSymCount - Mux(io.data.rx.fire(), symbolsPerPacket.U, 0.U)) { (symbol, count) =>
        val symbolFire = symbol.valid && symbol.bits.isData && (state === sReady)
        when (symbolFire) {
            rxBuffer((rxBufferEntries - 1).U - count) := symbol.bits.bits
        }
        count + symbolFire
    }
    for (i <- 0 until decodedSymbolsPerCycle) {
        when(io.data.rx.fire() && (rxSymCount > (symbolsPerPacket + i).U)) {
            rxBuffer((rxBufferEntries - 1).U - i.U) := rxBuffer((rxBufferEntries - 1).U - i.U - symbolsPerPacket.U)
        }
    }

    def connectData(dataClock: Clock, dataReset: Bool, data: FixedWidthData[F]) {
        val qParams = AsyncQueueParams(8, 3, true, false)

        val txq = Module(new AsyncQueue(chiselTypeOf(io.data.tx.bits), qParams))
        val rxq = Module(new AsyncQueue(chiselTypeOf(io.data.rx.bits), qParams))

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

}

class HydraPacketizer[S <: DecodedSymbol, F <: Data](decodedSymbolsPerCycle: Int, symbolFactory: () => S, fwDataFactory: () => F)
    extends Packetizer[S, FixedWidthData[F]](decodedSymbolsPerCycle, symbolFactory, FixedWidthData.apply(fwDataFactory) _)
    with NoHandshakePacketizerStateMachine[S, FixedWidthData[F]] {

    override val controlIO = Some(IO(new ControlBundle {
        val txEnable = input(Bool(), 0, "packetizer TX enable",
            "When high, enables data transmission over the interface. When low, causes the packetizer to send synchronization symbols.")
        val rxEnable = input(Bool(), 0, "packetizer RX enable",
            "When high, allows data to be received.")
    }))

    txEnable := controlIO.get.txEnable
    rxEnable := controlIO.get.rxEnable

    val decodedWidth = symbolFactory().decodedWidth
    val packetWidth = io.data.tx.bits.toBits.getWidth
    val symbolsPerPacket = (packetWidth - 1)/decodedWidth + 1 // This is just ceil(packetWidth/decodedWidth)

    require(decodedSymbolsPerCycle == symbolsPerPacket, "This implementation requires that decodedSymbolsPerCycle is the same as symbolsPerPacket")

    val rxBuffer = Reg(Vec(2*symbolsPerPacket - 1, UInt(symbolFactory().decodedWidth.W)))
    val rxValidBuffer = Reg(Vec(2*symbolsPerPacket - 1, Bool()))
    val rxMuxIns = VecInit((0 until symbolsPerPacket).map { i => fwDataFactory().fromBits(VecInit(rxBuffer.slice(i, symbolsPerPacket + i)).toBits) })
    val rxAlign = Reg(UInt(log2Ceil(2*symbolsPerPacket).W))

    // OK, this is a bit of a hack, but what we're doing here is asserting that the RX valid output is high when at least 2 of the first 3 symbols are high
    // This makes us robust to 1 word error (from a valid perspective), which is needed for Hydra's system-level synchronization.
    val rxValids = VecInit((0 until symbolsPerPacket).map { i => Majority(rxValidBuffer(i), rxValidBuffer(i+1), rxValidBuffer(i+2)) })

    // MSB is least recent byte, LSB is most recent byte
    (0 until (2*symbolsPerPacket-1)).foreach { i =>
        if (i < symbolsPerPacket) {
            rxBuffer(i) := io.symbolsRx(i).bits
            rxValidBuffer(i) := io.symbolsRx(i).valid
            when (io.symbolsRx(i).valid && io.symbolsRx(i).bits === symbolFactory().sync) {
                // Last connect will win
                rxAlign := i.U
            }
        } else {
            rxValidBuffer(i) := rxValidBuffer(i - symbolsPerPacket)
            rxBuffer(i) := rxBuffer(i - symbolsPerPacket)
        }
    }

    io.data.rx.bits := rxMuxIns(rxAlign)
    io.data.rx.valid := rxValids(rxAlign) && (rxState === sRxReady)

    txSymbolData := txSymbolData.fromBits(io.data.tx.bits.toBits)
    txSymbolValid.foreach { _ := io.data.tx.valid }

    def connectData(dataClock: Clock, dataReset: Bool, data: FixedWidthData[F]) {
        val qParams = AsyncQueueParams(8, 3, true, false)

        val txq = Module(new AsyncQueue(chiselTypeOf(io.data.tx.bits), qParams))
        val rxq = Module(new AsyncQueue(chiselTypeOf(io.data.rx.bits), qParams))

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

}

trait HasFixedWidthPacketizer[F <: Data] {
    this: Lane =>

    type T = FixedWidthData[F]

    def decodedSymbolsPerCycle: Int
    val fwDataFactory: () => F

    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S) = new FixedWidthPacketizer[S, F](decodedSymbolsPerCycle, symbolFactory, fwDataFactory)

}

trait HasHydraPacketizer[F <: Data] {
    this: Lane =>

    type T = FixedWidthData[F]

    val fwDataFactory: () => F

    // This is final because it needs to be this number for this packetizer to work correctly
    // This will require you to put this trait after the encoder trait when building the lane
    final override def decodedSymbolsPerCycle: Int = {
        val decodedWidth = 8 // This is a hack
        val packetWidth = fwDataFactory().toBits.getWidth
        val symbolsPerPacket = (packetWidth - 1)/decodedWidth + 1 // This is just ceil(packetWidth/decodedWidth)
        return symbolsPerPacket
    }

    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S) = new HydraPacketizer(decodedSymbolsPerCycle, symbolFactory, fwDataFactory)
}
