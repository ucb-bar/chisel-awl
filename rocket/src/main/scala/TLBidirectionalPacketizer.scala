package awl.rocket

import awl.serial._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}
import freechips.rocketchip.config._

class TLBidirectionalPacketizerIO(clientEdge: TLEdgeOut, managerEdge: TLEdgeIn) extends Bundle {
    val client = TLBundle(clientEdge.bundle)
    val manager = Flipped(TLBundle(managerEdge.bundle))
}

object TLBidirectionalPacketizerIO {
    def apply(clientEdge: TLEdgeOut, managerEdge: TLEdgeIn)() = new TLBidirectionalPacketizerIO(clientEdge, managerEdge)
}

class TLBidirectionalPacketizer[S <: DecodedSymbol](clientEdge: TLEdgeOut, managerEdge: TLEdgeIn, decodedSymbolsPerCycle: Int, symbolFactory: () => S)(implicit val p: Parameters)
    extends Packetizer(decodedSymbolsPerCycle, symbolFactory, TLBidirectionalPacketizerIO.apply(clientEdge, managerEdge) _) with TLPacketizerUtils with BasicPacketizerStateMachine[S, TLBidirectionalPacketizerIO] {
    override val controlIO = Some(IO(new ControlBundle {
        val enable = input(Bool(), 0, "mem_mode_enable",
            "When high, enables memory traffic over the tilelink interface. Unsafe to disable while traffic is in flight", TxClock)
    }))

    enable := controlIO.get.enable

    val tltx = new Object {
        val a = io.data.manager.a
        val b = io.data.client.b
        val c = io.data.manager.c
        val d = io.data.client.d
        val e = io.data.manager.e
        val aEdge = managerEdge
        val bEdge = clientEdge
        val cEdge = managerEdge
        val dEdge = clientEdge
        val eEdge = managerEdge
    }
    val tlrx = new Object {
        val a = io.data.client.a
        val b = io.data.manager.b
        val c = io.data.client.c
        val d = io.data.manager.d
        val e = io.data.client.e
        val aEdge = clientEdge
        val bEdge = managerEdge
        val cEdge = clientEdge
        val dEdge = managerEdge
        val eEdge = clientEdge
    }
    val aMaxOutstanding = p(HbwifTLKey).maxOutstanding
    val bMaxOutstanding = p(HbwifTLKey).maxOutstanding
    val cMaxOutstanding = p(HbwifTLKey).maxOutstanding
    val dMaxOutstanding = p(HbwifTLKey).maxOutstanding
    val eMaxOutstanding = p(HbwifTLKey).maxOutstanding
    val aMaxBeats = divCeil(tlrx.aEdge.maxTransfer, (tlrx.aEdge.bundle.dataBits / 8))
    val bMaxBeats = divCeil(tlrx.bEdge.maxTransfer, (tlrx.bEdge.bundle.dataBits / 8))
    val cMaxBeats = divCeil(tlrx.cEdge.maxTransfer, (tlrx.cEdge.bundle.dataBits / 8))
    val dMaxBeats = divCeil(tlrx.dEdge.maxTransfer, (tlrx.dEdge.bundle.dataBits / 8))
    val eMaxBeats = 1

    require(symbolFactory().decodedWidth == 8, "TLPacketizer* only supports 8-bit wide symbols")

    // Just some sanity checks
    require(tltx.a.bits.opcode.getWidth == 3)
    require(tltx.b.bits.opcode.getWidth == 3)
    require(tltx.c.bits.opcode.getWidth == 3)
    require(tltx.d.bits.opcode.getWidth == 3)
    require(tlrx.a.bits.opcode.getWidth == 3)
    require(tlrx.b.bits.opcode.getWidth == 3)
    require(tlrx.c.bits.opcode.getWidth == 3)
    require(tlrx.d.bits.opcode.getWidth == 3)
    // We want the opcode to fit in the first byte with the type
    require(tlTypeWidth == 3)
    require(isPow2(managerEdge.bundle.dataBits))
    require(managerEdge.bundle.dataBits/8 >= 8) // mask must be a multiple of bytes

    /************************ TX *************************/

    val txHeaderBits = List(headerWidth(tltx.a.bits), headerWidth(tltx.b.bits), headerWidth(tltx.c.bits), headerWidth(tltx.d.bits), headerWidth(tltx.e.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + List(managerEdge.bundle.dataBits, clientEdge.bundle.dataBits).max + List(managerEdge.bundle.dataBits/8, clientEdge.bundle.dataBits/8).max
    val txBuffer = Reg(UInt(txBufferBits.W))

    val (txAFirst, txALast, txADone, txACount) = tltx.aEdge.firstlastHelper(tltx.a.bits, tltx.a.fire())
    val (txBFirst, txBLast, txBDone, txBCount) = tltx.bEdge.firstlastHelper(tltx.b.bits, tltx.b.fire())
    val (txCFirst, txCLast, txCDone, txCCount) = tltx.cEdge.firstlastHelper(tltx.c.bits, tltx.c.fire())
    val (txDFirst, txDLast, txDDone, txDCount) = tltx.dEdge.firstlastHelper(tltx.d.bits, tltx.d.fire())

    val txA = tlToBuffer(tltx.aEdge, tltx.a.bits, txBufferBits, txAFirst)
    val txB = tlToBuffer(tltx.bEdge, tltx.b.bits, txBufferBits, txBFirst)
    val txC = tlToBuffer(tltx.cEdge, tltx.c.bits, txBufferBits, txCFirst)
    val txD = tlToBuffer(tltx.dEdge, tltx.d.bits, txBufferBits, txDFirst)
    val txE = tlToBuffer(tltx.eEdge, tltx.e.bits, txBufferBits, true.B)

    val txPayloadBytes = Wire(UInt())
    val txPacked = Wire(UInt(txBufferBits.W))

    when (tltx.e.fire()) {
        txPayloadBytes := getNumSymbols(tltx.eEdge, tltx.e.bits, true.B)
        txPacked := txE
    } .elsewhen (tltx.d.fire()) {
        txPayloadBytes := getNumSymbols(tltx.dEdge, tltx.d.bits, txDFirst)
        txPacked := txD
    } .elsewhen (tltx.c.fire()) {
        txPayloadBytes := getNumSymbols(tltx.cEdge, tltx.c.bits, txCFirst)
        txPacked := txC
    } .elsewhen (tltx.b.fire()) {
        txPayloadBytes := getNumSymbols(tltx.bEdge, tltx.b.bits, txBFirst)
        txPacked := txB
    } .otherwise { // a
        txPayloadBytes := getNumSymbols(tltx.aEdge, tltx.a.bits, txAFirst)
        txPacked := txA
    }

    val txCount = RegInit(0.U(log2Ceil(txBufferBits/8 + 1).W))

    val txADataInflight = RegInit(false.B)
    val txBDataInflight = RegInit(false.B)
    val txCDataInflight = RegInit(false.B)
    val txDDataInflight = RegInit(false.B)

    val cOutstanding = RegInit(0.U(log2Up(cMaxOutstanding+1).W))
    val dOutstanding = RegInit(0.U(log2Up(dMaxOutstanding+1).W))
    val eOutstanding = RegInit(0.U(log2Up(eMaxOutstanding+1).W))

    when (tltx.a.fire()) {
        txADataInflight := !txALast
    }
    when (tltx.b.fire()) {
        txBDataInflight := !txBLast
    }
    when (tltx.c.fire()) {
        txCDataInflight := !txCLast
    }
    when (tltx.d.fire()) {
        txDDataInflight := !txDLast
    }
    val dataInflight = txADataInflight || txBDataInflight || txCDataInflight || txDDataInflight

    // Don't decrement cOutstanding on a Release (which is unsolicited)
    def cRelease(c: TLBundleC) = (c.opcode === TLMessages.Release) || (c.opcode === TLMessages.ReleaseData)
    cOutstanding := cOutstanding + Mux(tltx.b.fire() && txBFirst, tlResponseMap(tltx.b.bits), 0.U) - Mux(tlrx.c.fire() && !cRelease(tlrx.c.bits), tlrx.cEdge.last(tlrx.c), 0.U)
    dOutstanding := dOutstanding + Mux(tltx.a.fire() && txAFirst, tlResponseMap(tltx.a.bits), 0.U) + Mux(tltx.c.fire() && txCFirst, tlResponseMap(tltx.c.bits), 0.U) - Mux(tlrx.d.fire(), tlrx.dEdge.last(tlrx.d), 0.U)
    eOutstanding := eOutstanding + Mux(tltx.d.fire() && txDFirst, tlResponseMap(tltx.d.bits), 0.U) - Mux(tlrx.e.fire(), tlrx.eEdge.last(tlrx.e), 0.U)

    // Assign priorities to the channels
    val txReady = enable && (state === sReady) && ((txCount === 0.U) || ((txCount <= decodedSymbolsPerCycle.U) && (io.symbolsTxReady)))
    val aReady = txReady && (dOutstanding < dMaxOutstanding.U)
    val bReady = txReady && (cOutstanding < cMaxOutstanding.U)
    val cReady = txReady && (dOutstanding < dMaxOutstanding.U)
    val dReady = txReady && (eOutstanding < eMaxOutstanding.U)
    val eReady = txReady

    tltx.a.ready := (aReady && !tltx.b.valid && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txADataInflight)
    tltx.b.ready := (bReady && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txBDataInflight)
    tltx.c.ready := (cReady && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txCDataInflight)
    tltx.d.ready := (dReady && !tltx.e.valid && !dataInflight) || (txReady && txDDataInflight)
    tltx.e.ready := (eReady && !dataInflight)

    // Feed symbols to the state machine stuff
    for (i <- 0 until decodedSymbolsPerCycle) {
        txSymbolData(i) := txBuffer(txBufferBits-8*i-1, txBufferBits-8*i-8)
        txSymbolValid(i) := (i.U < txCount)
    }

    val txFire = tltx.a.fire() || tltx.b.fire() || tltx.c.fire() || tltx.d.fire() || tltx.e.fire()

    when (state === sReset) {
        txCount := 0.U
    } .elsewhen (txFire) {
        txCount := txPayloadBytes
        txBuffer := txPacked
    } .elsewhen(txCount > decodedSymbolsPerCycle.U) {
        when (io.symbolsTxReady) {
            txCount := txCount - decodedSymbolsPerCycle.U
            txBuffer := Cat(txBuffer(txBufferBits-decodedSymbolsPerCycle*8-1,0), txBuffer(decodedSymbolsPerCycle*8-1,0))
        }
    } .otherwise {
        when (io.symbolsTxReady) {
            txCount := 0.U
        }
    }

    /************************ RX *************************/

    val rxHeaderBits = List(headerWidth(tlrx.a.bits), headerWidth(tlrx.b.bits), headerWidth(tlrx.c.bits), headerWidth(tlrx.d.bits), headerWidth(tlrx.e.bits)).max
    val rxMaxBytes = div8Ceil(rxHeaderBits) + List(managerEdge.bundle.dataBits/64, clientEdge.bundle.dataBits/64).max + List(managerEdge.bundle.dataBits, clientEdge.bundle.dataBits).max/8 + (decodedSymbolsPerCycle - 1)
    val rxBufferBytes = 2*rxMaxBytes - 1

    val rxTypeValid = RegInit(false.B)
    val rxBuffer = Reg(Vec(rxBufferBytes, UInt(8.W)))
    val rxBufferUInt = rxBuffer.asUInt
    val (rxType, rxOpcode) = typeFromBuffer(rxBufferUInt)

    val rxA = tlFromBuffer(tlrx.aEdge, tlrx.a.bits, rxBufferUInt)
    val rxB = tlFromBuffer(tlrx.bEdge, tlrx.b.bits, rxBufferUInt)
    val rxC = tlFromBuffer(tlrx.cEdge, tlrx.c.bits, rxBufferUInt)
    val rxD = tlFromBuffer(tlrx.dEdge, tlrx.d.bits, rxBufferUInt)
    val rxE = tlFromBuffer(tlrx.eEdge, tlrx.e.bits, rxBufferUInt)

    // Assume we can only handle one thing at a time, for now
    val aQueue = Module(new Queue(chiselTypeOf(rxA.bits), aMaxOutstanding * aMaxBeats, true, true))
    aQueue.io.enq <> rxA
    tlrx.a <> aQueue.io.deq
    val bQueue = Module(new Queue(chiselTypeOf(rxB.bits), bMaxOutstanding * bMaxBeats, true, true))
    bQueue.io.enq <> rxB
    tlrx.b <> bQueue.io.deq
    val cQueue = Module(new Queue(chiselTypeOf(rxC.bits), cMaxOutstanding * cMaxBeats, true, true))
    cQueue.io.enq <> rxC
    tlrx.c <> cQueue.io.deq
    val dQueue = Module(new Queue(chiselTypeOf(rxD.bits), dMaxOutstanding * dMaxBeats, true, true))
    dQueue.io.enq <> rxD
    tlrx.d <> dQueue.io.deq
    val eQueue = Module(new Queue(chiselTypeOf(rxE.bits), eMaxOutstanding * eMaxBeats, true, true))
    eQueue.io.enq <> rxE
    tlrx.e <> eQueue.io.deq

    val (rxAFirst, rxALast, rxADone, rxACount) = tlrx.aEdge.firstlastHelper(rxA.bits, rxA.fire())
    val (rxBFirst, rxBLast, rxBDone, rxBCount) = tlrx.bEdge.firstlastHelper(rxB.bits, rxB.fire())
    val (rxCFirst, rxCLast, rxCDone, rxCCount) = tlrx.cEdge.firstlastHelper(rxC.bits, rxC.fire())
    val (rxDFirst, rxDLast, rxDDone, rxDCount) = tlrx.dEdge.firstlastHelper(rxD.bits, rxD.fire())

    val rxADataInflight = RegInit(false.B)
    val rxBDataInflight = RegInit(false.B)
    val rxCDataInflight = RegInit(false.B)
    val rxDDataInflight = RegInit(false.B)

    when (rxA.fire()) {
        rxADataInflight := !rxALast
    }
    when (rxB.fire()) {
        rxBDataInflight := !rxBLast
    }
    when (rxC.fire()) {
        rxCDataInflight := !rxCLast
    }
    when (rxD.fire()) {
        rxDDataInflight := !rxDLast
    }

    val rxFirst = !rxADataInflight && !rxBDataInflight && !rxCDataInflight && !rxDDataInflight
    val rxFire = rxA.fire() || rxB.fire() || rxC.fire() || rxD.fire() || rxE.fire()

    val rxSymCount = RegInit(0.U(log2Ceil(rxBufferBytes + 1).W))

    val rxSymPopped = Wire(UInt(log2Ceil(rxBufferBytes + 1).W))

    val rxNumSymbols = Mux(rxTypeValid, getNumSymbolsFromType(tlrx.aEdge, tlrx.bEdge, rxType, rxOpcode, true.B), 0.U)
    val rxNumSymbolsNotLast = Mux(rxTypeValid, getNumSymbolsFromType(tlrx.aEdge, tlrx.bEdge, rxType, rxOpcode, false.B), 0.U)
    val rxValid = (rxSymCount >= rxNumSymbols) && rxTypeValid

    rxTypeValid := (rxSymCount =/= rxNumSymbols) || io.symbolsRx.map(x => x.valid && x.bits.isData).reduce(_||_)

    rxSymPopped := Mux(
        (rxALast && (rxType === typeA)) ||
        (rxBLast && (rxType === typeB)) ||
        (rxCLast && (rxType === typeC)) ||
        (rxDLast && (rxType === typeD)) ||
        (rxType === typeE), rxNumSymbols, rxNumSymbolsNotLast)

    rxA.valid := rxValid && (rxType === typeA)
    rxB.valid := rxValid && (rxType === typeB)
    rxC.valid := rxValid && (rxType === typeC)
    rxD.valid := rxValid && (rxType === typeD)
    rxE.valid := rxValid && (rxType === typeE)
    assert(rxA.ready || !rxA.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(rxB.ready || !rxB.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(rxC.ready || !rxC.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(rxD.ready || !rxE.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(rxE.ready || !rxD.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")

    rxSymCount := io.symbolsRx.foldRight(rxSymCount - Mux(rxFire, rxSymPopped, 0.U)) { (symbol, count) =>
        when (symbol.valid && symbol.bits.isData) {
            rxBuffer((rxBufferBytes - 1).U - count) := symbol.bits.bits
        }
        count + (symbol.valid && symbol.bits.isData && (state === sReady))
    }
    for (i <- 0 until decodedSymbolsPerCycle) {
        when(rxFire && (rxSymCount > (rxNumSymbols +& i.U))) {
            rxBuffer((rxBufferBytes - 1).U - i.U - (rxNumSymbols - rxSymPopped)) := rxBuffer((rxBufferBytes - 1).U - i.U - rxNumSymbols)
        }
    }

    // TODO can we add another symbol to NACK a transaction in progress (and set error)
    // TODO need to not assume that the sender interface looks like ours, it's possible we get multiple E messages per cycle

    def connectData(dataClock: Clock, dataReset: Bool, data: TLBidirectionalPacketizerIO) {
        val qParams = AsyncQueueParams(p(HbwifTLKey).asyncQueueDepth, p(HbwifTLKey).asyncQueueSync, p(HbwifTLKey).asyncQueueSafe, p(HbwifTLKey).asyncQueueNarrow)

        val maq = Module(new AsyncQueue(chiselTypeOf(io.data.manager.a.bits), qParams))
        val mbq = Module(new AsyncQueue(chiselTypeOf(io.data.manager.b.bits), qParams))
        val mcq = Module(new AsyncQueue(chiselTypeOf(io.data.manager.c.bits), qParams))
        val mdq = Module(new AsyncQueue(chiselTypeOf(io.data.manager.d.bits), qParams))
        val meq = Module(new AsyncQueue(chiselTypeOf(io.data.manager.e.bits), qParams))
        val caq = Module(new AsyncQueue(chiselTypeOf(io.data.client.a.bits), qParams))
        val cbq = Module(new AsyncQueue(chiselTypeOf(io.data.client.b.bits), qParams))
        val ccq = Module(new AsyncQueue(chiselTypeOf(io.data.client.c.bits), qParams))
        val cdq = Module(new AsyncQueue(chiselTypeOf(io.data.client.d.bits), qParams))
        val ceq = Module(new AsyncQueue(chiselTypeOf(io.data.client.e.bits), qParams))

        maq.suggestName("AsyncQueueManagerA")
        mbq.suggestName("AsyncQueueManagerB")
        mcq.suggestName("AsyncQueueManagerC")
        mdq.suggestName("AsyncQueueManagerD")
        meq.suggestName("AsyncQueueManagerE")
        caq.suggestName("AsyncQueueClientA")
        cbq.suggestName("AsyncQueueClientB")
        ccq.suggestName("AsyncQueueClientC")
        cdq.suggestName("AsyncQueueClientD")
        ceq.suggestName("AsyncQueueClientE")

        maq.io.enq <> data.manager.a
        data.manager.b <> mbq.io.deq
        mcq.io.enq <> data.manager.c
        data.manager.d <> mdq.io.deq
        meq.io.enq <> data.manager.e

        io.data.manager.a <> maq.io.deq
        mbq.io.enq <> io.data.manager.b
        io.data.manager.c <> mcq.io.deq
        mdq.io.enq <> io.data.manager.d
        io.data.manager.e <> meq.io.deq

        caq.io.enq <> io.data.client.a
        io.data.client.b <> cbq.io.deq
        ccq.io.enq <> io.data.client.c
        io.data.client.d <> cdq.io.deq
        ceq.io.enq <> io.data.client.e

        data.client.a <> caq.io.deq
        cbq.io.enq <> data.client.b
        data.client.c <> ccq.io.deq
        cdq.io.enq <> data.client.d
        data.client.e <> ceq.io.deq

        maq.io.enq_clock := dataClock
        mbq.io.deq_clock := dataClock
        mcq.io.enq_clock := dataClock
        mdq.io.deq_clock := dataClock
        meq.io.enq_clock := dataClock

        maq.io.deq_clock := this.clock
        mbq.io.enq_clock := this.clock
        mcq.io.deq_clock := this.clock
        mdq.io.enq_clock := this.clock
        meq.io.deq_clock := this.clock

        caq.io.enq_clock := this.clock
        cbq.io.deq_clock := this.clock
        ccq.io.enq_clock := this.clock
        cdq.io.deq_clock := this.clock
        ceq.io.enq_clock := this.clock

        caq.io.deq_clock := dataClock
        cbq.io.enq_clock := dataClock
        ccq.io.deq_clock := dataClock
        cdq.io.enq_clock := dataClock
        ceq.io.deq_clock := dataClock

        maq.io.enq_reset := dataReset
        mbq.io.deq_reset := dataReset
        mcq.io.enq_reset := dataReset
        mdq.io.deq_reset := dataReset
        meq.io.enq_reset := dataReset

        maq.io.deq_reset := this.reset.toBool
        mbq.io.enq_reset := this.reset.toBool
        mcq.io.deq_reset := this.reset.toBool
        mdq.io.enq_reset := this.reset.toBool
        meq.io.deq_reset := this.reset.toBool

        caq.io.enq_reset := this.reset.toBool
        cbq.io.deq_reset := this.reset.toBool
        ccq.io.enq_reset := this.reset.toBool
        cdq.io.deq_reset := this.reset.toBool
        ceq.io.enq_reset := this.reset.toBool

        caq.io.deq_reset := dataReset
        cbq.io.enq_reset := dataReset
        ccq.io.deq_reset := dataReset
        cdq.io.enq_reset := dataReset
        ceq.io.deq_reset := dataReset
    }
}

trait HasTLBidirectionalPacketizer {
    this: Lane =>

    type T = TLBidirectionalPacketizerIO

    def decodedSymbolsPerCycle: Int
    val clientEdge: TLEdgeOut
    val managerEdge: TLEdgeIn
    implicit val p: Parameters

    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S) = new TLBidirectionalPacketizer[S](clientEdge, managerEdge, decodedSymbolsPerCycle, symbolFactory)
}

