package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TLBidirectionalPacketizer[S <: DecodedSymbol](masterEdge: TLEdgeIn, slaveEdge: TLEdgeOut, decodedSymbolsPerCycle: Int, symbolFactory: () => S)
    extends Packetizer(decodedSymbolsPerCycle, symbolFactory, {() => new Bundle { val master = TLBundle(masterEdge); val slave = Flipped(TLBundle(slaveEdge))}) with TLPacketizerLike {

    val tltx = new Object {
        val a = io.data.master.a
        val b = io.data.slave.b
        val c = io.data.master.c
        val d = io.data.slave.d
        val e = io.data.master.e
        val aEdge = masterEdge
        val bEdge = slaveEdge
        val cEdge = masterEdge
        val dEdge = slaveEdge
        val eEdge = masterEdge
    }
    val tlrx = new Object {
        val a = io.data.slave.a
        val b = io.data.master.b
        val c = io.data.slave.c
        val d = io.data.master.d
        val e = io.data.slave.e
        val aEdge = slaveEdge
        val bEdge = masterEdge
        val cEdge = slaveEdge
        val dEdge = masterEdge
        val eEdge = slaveEdge
    }
    val aMaxOutstanding = 8
    val bMaxOutstanding = 8
    val cMaxOutstanding = 8
    val dMaxOutstanding = 8
    val eMaxOutstanding = 8

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
    require(isPow2(slaveEdge.bundle.dataBits))
    require(slaveEdge.bundle.dataBits/8 >= 8) // mask must be a multiple of bytes

    /************************ TX *************************/

    val txHeaderBits = List(headerWidth(tltx.a.bits), headerWidth(tltx.b.bits), headerWidth(tltx.c.bits), headerWidth(tltx.d.bits), headerWidth(tltx.e.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + List(slaveEdge.bundle.dataBits, masterEdge.bundle.dataBits).max + List(slaveEdge.bundle.dataBits/8, masterEdge.bundle.dataBits/8).max
    val txBuffer = Reg(UInt(txBufferBits.W))

    val aPadBits = txBufferBits - headerWidth(tltx.a.bits) - tltx.a.bits.data.getWidth - tltx.a.bits.mask.getWidth
    val bPadBits = txBufferBits - headerWidth(tltx.b.bits) - tltx.b.bits.data.getWidth - tltx.b.bits.mask.getWidth
    val cPadBits = txBufferBits - headerWidth(tltx.c.bits) - tltx.c.bits.data.getWidth
    val dPadBits = txBufferBits - headerWidth(tltx.d.bits) - tltx.d.bits.data.getWidth
    val ePadBits = txBufferBits - headerWidth(tltx.e.bits)

    val txA = tlToBuffer(tlrx.edgeA, tlrx.a, false.B)
    val txB = tlToBuffer(tlrx.edgeB, tlrx.b, false.B)
    val txC = tlToBuffer(tlrx.edgeC, tlrx.c, false.B)
    val txD = tlToBuffer(tlrx.edgeD, tlrx.d, false.B)
    val txE = tlToBuffer(tlrx.edgeE, tlrx.e, false.B)

    val txPayloadBytes = Wire(UInt())
    val txPacked = Wire(UInt(txBufferBits.W))

    when (tltx.e.fire()) {
        txPayloadBytes := getNumSymbols(tltx.e.bits)
        txPacked := txE
    } .elsewhen (tltx.d.fire()) {
        txPayloadBytes := getNumSymbols(tltx.c.bits)
        txPacked := txD
    } .elsewhen (tltx.c.fire()) {
        txPayloadBytes := getNumSymbols(tltx.c.bits)
        txPacked := txC
    } .elsewhen (tltx.b.fire()) {
        txPayloadBytes := getNumSymbols(tltx.b.bits)
        txPacked := txB
    } .otherwise { // a
        txPayloadBytes := getNumSymbols(tltx.a.bits)
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

    val (txAFirst, txALast, txADone, txACount) = tltx.aEdge.firstlastHelper(tltx.a, tltx.a.fire())
    val (txBFirst, txBLast, txBDone, txBCount) = tltx.bEdge.firstlastHelper(tltx.b, tltx.b.fire())
    val (txCFirst, txCLast, txCDone, txCCount) = tltx.cEdge.firstlastHelper(tltx.c, tltx.c.fire())
    val (txDFirst, txDLast, txDDone, txDCount) = tltx.dEdge.firstlastHelper(tltx.d, tltx.d.fire())

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

    // TODO how to handle a,b backpressure, and c needs something to handle Release/ReleaseData
    cOutstanding := cOutstanding + Mux(tltx.b.fire() && txBFirst, tlResponseMap(tltx.b), 0.U) - tlrx.c.fire()
    dOutstanding := aOutstanding + Mux(tltx.a.fire() && txAFirst, tlResponseMap(tltx.a), 0.U) + Mux(tltx.c.fire() && txCFirst, tlResponseMap(tltx.c), 0.U) - tlrx.d.fire()
    eOutstanding := eOutstanding + Mux(tltx.d.fire() && txDFirst, tlResponseMap(tltx.d), 0.U) - tlrx.e.fire()

    // TODO can we process more than one request at a time (e + other?)
    // Assign priorities to the channels
    val txReady = (((txCount <= decodedSymbolsPerCycle.U) && io.symbolsTxReady) || (txCount === 0.U)) && txState === sTxReady
    val aReady = txReady && (dOutstanding < (dMaxOutstanding.U - tlResponseMap(tltx.a)))
    val bReady = txReady && (cOutstanding < (cMaxOutstanding.U - tlResponseMap(tltx.b)))
    val cReady = txReady && (dOutstanding < (dMaxOutstanding.U - tlResponseMap(tltx.c)))
    val dReady = txReady && (eOutstanding < (eMaxOutstanding.U - tlResponseMap(tltx.d)))
    val eReady = txReady

    tltx.a.ready := (aReady && !tltx.b.valid && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txADataInflight)
    tltx.b.ready := (bReady && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txBDataInflight)
    tltx.c.ready := (cReady && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txCDataInflight)
    tltx.d.ready := (dReady && !tltx.e.valid && !dataInflight) || (txReady && txDDataInflight)
    tltx.e.ready := (eReady && !dataInflight)

    val sTxReset :: sTxSync :: sTxAck :: sTxReady :: Nil = Enum(4)
    val txState = RegInit(sTxReset)

    // These come from the RX
    val ack = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().ack } reduce (_||_)
    val nack = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().nack } reduce (_||_)

    when (txState === sTxReset) {
        txState := sTxSync
    } .elsewhen(txState === sTxSync) {
        txState := sTxAck
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

    io.symbolsTx.reverse.zipWithIndex.foreach { case (s,i) =>
        val doSync = ((i.U === 0.U) && (txState === sTxSync))
        s.valid := ((i.U < txCount) && (txState === sTxReady)) || doSync
        s.bits := Mux(doSync, symbolFactory().sync, symbolFactory().fromData(txBuffer(txBufferBits-8*i-1,txBufferBits-8*i-8)))
    }

    val txFire = tltx.a.fire() || tltx.b.fire() || tltx.c.fire() || tltx.d.fire() || tltx.e.fire()

    when (txFire) {
        txCount := txPayloadBytes - txCount
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

    val rxHeaderBitsMax = List(headerWidth(tlrx.a.bits), headerWidth(tlrx.b.bits), headerWidth(tlrx.c.bits), headerWidth(tlrx.d.bits), headerWidth(tlrx.e.bits)).max
    val rxBufferBytes = div8Ceil(rxHeaderBits) + List(clientParams.dataBits, managerParams.dataBits).max/8 + (decodedSymbolsPerCycle - 1)
    //val abcdeBundlesPerCycle = List(tlrx.a, tlrx.b, tlrx.c, tlrx.d, tlrx.e).map { case x => List(1, decodedSymbolsPerCycle/div8Ceil(headerWidth(x.bits))).max }
    //val maxBundlesPerCycle = abcdeBundlesPerCycle.max

    // Assume we can only handle one thing at a time, for now
    val aQueue = Queue(tlrx.a.bits, aMaxOutstanding)
    val bQueue = Queue(tlrx.b.bits, bMaxOutstanding)
    val cQueue = Queue(tlrx.c.bits, cMaxOutstanding)
    val dQueue = Queue(tlrx.d.bits, dMaxOutstanding)
    val eQueue = Queue(tlrx.e.bits, eMaxOutstanding)

    tlrx.a <> aQueue.io.deq
    tlrx.b <> bQueue.io.deq
    tlrx.c <> cQueue.io.deq
    tlrx.d <> dQueue.io.deq
    tlrx.e <> eQueue.io.deq

    aQueue.enq.bits := tlFromBuffer(tlrx.aAdge, tlrx.a, rxBuffer)
    bQueue.enq.bits := tlFromBuffer(tlrx.bEdge, tlrx.b, rxBuffer)
    cQueue.enq.bits := tlFromBuffer(tlrx.cEdge, tlrx.c, rxBuffer)
    dQueue.enq.bits := tlFromBuffer(tlrx.dEdge, tlrx.d, rxBuffer)
    eQueue.enq.bits := tlFromBuffer(tlrx.eEdge, tlrx.e, rxBuffer)

    val (rxAFirst, rxALast, rxADone, rxACount) = tlrx.aEdge.firstlastHelper(aQueue.enq.bits, aQueue.enq.fire())
    val (rxBFirst, rxBLast, rxBDone, rxBCount) = tlrx.bEdge.firstlastHelper(bQueue.enq.bits, bQueue.enq.fire())
    val (rxCFirst, rxCLast, rxCDone, rxCCount) = tlrx.cEdge.firstlastHelper(cQueue.enq.bits, cQueue.enq.fire())
    val (rxDFirst, rxDLast, rxDDone, rxDCount) = tlrx.dEdge.firstlastHelper(dQueue.enq.bits, dQueue.enq.fire())

    val rxADataInflight = RegInit(false.B)
    val rxBDataInflight = RegInit(false.B)
    val rxCDataInflight = RegInit(false.B)
    val rxDDataInflight = RegInit(false.B)

    when (aQueue.enq.fire()) {
        rxADataInflight := !rxALast
    }
    when (bQueue.enq.fire()) {
        rxBDataInflight := !rxBLast
    }
    when (cQueue.enq.fire()) {
        rxCDataInflight := !rxCLast
    }
    when (dQueue.enq.fire()) {
        rxDDataInflight := !rxDLast
    }

    val rxFirst = !rxADataInflight && !rxBDataInflight && !rxCDataInflight && !rxDDataInflight
    val rxFire = aQueue.enq.fire() || bQueue.enq.fire() || cQueue.enq.fire() || dQueue.enq.fire() || eQueue.enq.fire()

    val rxBuffer = Reg(Vec(rxBufferBytes, UInt(8.W)))
    val (rxType, rxOpcode) = typeFromBuffer(rxBuffer)

    val rxSymCount = RegInit(0.U(log2Ceil(rxBufferBytes + 1).W))

    val rxSymPopped = Wire(UInt(log2Ceil(rxBufferBytes + 1).W))

    val rxNumSymbols = getNumSymbolsFromType(tlrx.aEdge, tlrx.bEdge, rxType, rxOpcode, true.B)
    val rxNumSymbolsNotLast = getNumSymbolsFromType(tlrx.aEdge, tlrx.bEdge, rxType, rxOpcode, false.B)
    val rxValid = (rxSymCount >= rxNumSymbols)

    rxSymPopped := Mux(
        (rxALast && (rxType === typeA)) ||
        (rxBLast && (rxType === typeB)) ||
        (rxCLast && (rxType === typeC)) ||
        (rxDLast && (rxType === typeD)) ||
        (rxType === typeE), rxNumSymbols, rxNumSymbolsNotLast)

    aQueue.enq.valid := rxValid && (rxType === typeA)
    bQueue.enq.valid := rxValid && (rxType === typeB)
    cQueue.enq.valid := rxValid && (rxType === typeC)
    dQueue.enq.valid := rxValid && (rxType === typeD)
    eQueue.enq.valid := rxValid && (rxType === typeE)
    assert(aQueue.enq.ready || !aQueue.enq.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(bQueue.enq.ready || !bQueue.enq.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(cQueue.enq.ready || !cQueue.enq.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(dQueue.enq.ready || !dQueue.enq.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")
    assert(eQueue.enq.ready || !eQueue.enq.valid, "Something went wrong, we should never have a valid symbol and unready Queue- check your buffer depths")

    rxSymCount := rxSymCount + PopCount(io.symbolsRx.map(_.valid)) - Mux(rxFire, rxSymPopped, 0.U)

    val rxBufferReversed = Vec(rxBuffer.reverse)
    io.symbolsRx.foldRight(rxSymCount) { (symbol, count) =>
        when (symbol.valid) {
            rxBufferReversed(count) := symbol.bits
        }
        count + symbol.valid
    }

    // TODO can we add another symbol to NACK a transaction in progress (and set error)
    // TODO need to not assume that the sender interface looks like ours, it's possible we get multiple E messages per cycle
}
