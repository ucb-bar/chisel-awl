package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TLBidirectionalPacketizer[S <: DecodedSymbol](edgeMaster: TLEdgeIn, edgeSlave: TLEdgeOut, decodedSymbolsPerCycle: Int, symbolFactory: () => S)
    extends Packetizer(decodedSymbolsPerCycle, symbolFactory, {() => new Bundle { val master = TLBundle(edgeMaster); val slave = Flipped(TLBundle(edgeSlave))}) with TLPacketizerLike {

    val tltx = new Object {
        val a = io.data.master.a
        val b = io.data.slave.b
        val c = io.data.master.c
        val d = io.data.slave.d
        val e = io.data.master.e
        val aEdge = edgeMaster
        val bEdge = edgeSlave
        val cEdge = edgeMaster
        val dEdge = edgeSlave
        val eEdge = edgeMaster
    }
    val tlrx = new Object {
        val a = io.data.slave.a
        val b = io.data.master.b
        val c = io.data.slave.c
        val d = io.data.master.d
        val e = io.data.slave.e
        val aEdge = edgeSlave
        val bEdge = edgeMaster
        val cEdge = edgeSlave
        val dEdge = edgeMaster
        val eEdge = edgeSlave
    }
    val tlQueueDepth = 8 // TODO XXX

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

    /************************ TX *************************/

    val txHeaderBits = tlTypeWidth + List(headerWidth(tltx.a.bits), headerWidth(tltx.b.bits), headerWidth(tltx.c.bits), headerWidth(tltx.d.bits), headerWidth(tltx.e.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + List(managerParams.dataBits, clientParams.dataBits).max
    val txBuffer = Reg(UInt(txBufferBits.W))

    val txData = Mux(tl.a.fire(), tl.a.bits.data, tl.c.bits.data)

    val aPadBits = txBufferBits - headerWidth(tltx.a.bits) - tltx.a.bits.data.getWidth
    val bPadBits = txBufferBits - headerWidth(tltx.b.bits) - tltx.b.bits.data.getWidth
    val cPadBits = txBufferBits - headerWidth(tltx.c.bits) - tltx.c.bits.data.getWidth
    val dPadBits = txBufferBits - headerWidth(tltx.d.bits) - tltx.d.bits.data.getWidth
    val ePadBits = txBufferBits - headerWidth(tltx.e.bits)
    val aHeader = Cat(typeA, tltx.a.bits.opcode, tltx.a.bits.param, tltx.a.bits.size, tltx.a.bits.source, tltx.a.bits.address, tltx.a.bits.mask, if (aPadBits > 0) 0.U(aPadBits.W) else Wire(UInt(0.W)))
    val bHeader = Cat(typeB, tltx.b.bits.opcode, tltx.b.bits.param, tltx.b.bits.size, tltx.b.bits.source, tltx.b.bits.address, tltx.b.bits.mask, if (bPadBits > 0) 0.U(bPadBits.W) else Wire(UInt(0.W)))
    val cHeader = Cat(typeC, tltx.c.bits.opcode, tltx.c.bits.param, tltx.c.bits.size, tltx.c.bits.source, tltx.c.bits.address, tltx.c.bits.error, if (cPadBits > 0) 0.U(cPadBits.W) else Wire(UInt(0.W)))
    val dHeader = Cat(typeD, tltx.d.bits.opcode, tltx.d.bits.param, tltx.d.bits.size, tltx.d.bits.source, tltx.d.bits.sink, tltx.d.bits.error, if (dPadBits > 0) 0.U(dPadBits.W) else Wire(UInt(0.W)))
    val eHeader = Cat(typeE, tltx.e.bits.sink, if (ePadBits > 0) 0.U(ePadBits.W) else Wire(UInt(0.W)))

    val txPayloadBytes = Wire(UInt())
    val txPacked = Wire(UInt(txBufferBits.W))
    when (tltx.e.valid) {
        txPayloadBytes := getNumSymbols(tltx.e.bits)
        txPacked := Cat(eHeader, txData)
    } .elsewhen (tltx.d.valid) {
        txPayloadBytes := getNumSymbols(tltx.c.bits)
        txPacked := Cat(eHeader, txData)
    } .elsewhen (tltx.c.valid) {
        txPayloadBytes := getNumSymbols(tltx.c.bits)
        txPacked := Cat(cHeader, txData)
    } .elsewhen (tltx.b.valid) {
        txPayloadBytes := getNumSymbols(tltx.b.bits)
        txPacked := Cat(bHeader, txData)
    } .otherwise { // a
        txPayloadBytes := getNumSymbols(tltx.a.bits)
        txPacked := Cat(aHeader, txData)
    }

    val txCount = RegInit(0.U(log2Ceil(txBufferBits/8 + 1).W))

    val aDataInflight = RegInit(false.B)
    val bDataInflight = RegInit(false.B)
    val cDataInflight = RegInit(false.B)
    val dDataInflight = RegInit(false.B)

    val bRespAvail = RegInit(0.U(TODO.W))
    val cRespAvail = RegInit(0.U(TODO.W))
    val dRespAvail = RegInit(0.U(TODO.W))
    val eRespAvail = RegInit(0.U(TODO.W))

    val (aFirst, aLast, aDone, aCount) = tltx.aEdge.firstlastHelper(tltx.a, tltx.a.fire())._2
    val (bFirst, bLast, bDone, bCount) = tltx.bEdge.firstlastHelper(tltx.b, tltx.b.fire())._2
    val (cFirst, cLast, cDone, cCount) = tltx.cEdge.firstlastHelper(tltx.c, tltx.c.fire())._2
    val (dFirst, dLast, dDone, dCount) = tltx.dEdge.firstlastHelper(tltx.d, tltx.d.fire())._2

    when (tltx.a.fire()) {
        aDataInflight := !aLast
    }
    when (tltx.b.fire()) {
        bDataInflight := !bLast
    }
    when (tltx.c.fire()) {
        cDataInflight := !cLast
    }
    when (tltx.d.fire()) {
        dDataInflight := !dLast
    }
    val dataInflight = aDataInflight || bDataInflight || cDataInflight || dDataInflight

    dRespAvail := aRespAvail + Mux(tltx.a.fire(), tlResponseMap(tltx.a), 

    // TODO can we process more than one request at a time (e + other?)
    // Assign priorities to the channels
    val txReady = (((txCount <= decodedSymbolsPerCycle.U) && io.symbolsTxReady) || (txCount === 0.U)) && txState === sTxReady
    val aReady = txReady && 
    val bReady = txReady &&
    val cReady = txReady &&
    val dReady = txReady &&
    val eReady = txReady &&

    tltx.a.ready := (aReady && !tltx.b.valid && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && aDataInflight)
    tltx.b.ready := (bReady && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && bDataInflight)
    tltx.c.ready := (cReady && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && cDataInflight)
    tltx.d.ready := (dReady && !tltx.e.valid && !dataInflight) || (txReady && dDataInflight)
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

    val rxHeaderBitsMax = tlTypeWidth + List(headerWidth(tlrx.a.bits), headerWidth(tlrx.b.bits), headerWidth(tlrx.c.bits), headerWidth(tlrx.d.bits), headerWidth(tlrx.e.bits)).max
    val rxBufferBytes = div8Ceil(rxHeaderBits) + List(clientParams.dataBits, managerParams.dataBits).max/8
    val abcdeBundlesPerCycle = List(tlrx.a, tlrx.b, tlrx.c, tlrx.d, tlrx.e).map { case x => List(1, decodedSymbolsPerCycle/div8Ceil(tlTypeWidth + headerWidth(x.bits))).max }
    val maxBundlesPerCycle = abcdeBundlesPerCycle.max

    val aQueue = MultiQueue(tlrx.a.bits, tlQueueDepth, abcdeBundlesPerCycle(0))
    val bQueue = MultiQueue(tlrx.b.bits, tlQueueDepth, abcdeBundlesPerCycle(1))
    val cQueue = MultiQueue(tlrx.c.bits, tlQueueDepth, abcdeBundlesPerCycle(2))
    val dQueue = MultiQueue(tlrx.d.bits, tlQueueDepth, abcdeBundlesPerCycle(3))
    val eQueue = MultiQueue(tlrx.e.bits, tlQueueDepth, abcdeBundlesPerCycle(4))

    tlrx.a <> aQueue.io.deq
    tlrx.b <> bQueue.io.deq
    tlrx.c <> cQueue.io.deq
    tlrx.d <> dQueue.io.deq
    tlrx.e <> eQueue.io.deq

    val rxBuffer = Reg(Vec(rxBufferBytes, UInt(8.W)))
    val rxType = Reg(UInt(tlTypeWidth.W))
    val rxCount = RegInit(0.U(log2Ceil(rxBufferBytes + 1).W))

    val rxReversed = io.symbolsRx.reverse
    val (rxPacked, rxPackedCount) = Pack(rxReversed.map { x =>
        val v = Wire(Valid(UInt(8.W)))
        v.bits := x.bits.isData
        v.valid := x.bits.isData && x.valid
        v
    })

    val rxHeaderCounts = Wire(Vec(maxBundlesPerCycle, UInt(log2Ceil(2*rxBufferBytes + 1).W)))
    val rxTypesOpcodes = rxHeaderCounts.map { rxReversed(_) } map { x => (x(7,8-tlTypeWidth), x(7-tlTypeWidth,8-tlTypeWidth-3)) }
    val rxCountRem = rxHeaderCounts.zip(rxTypesOpcodes).foldLeft(rxCount) { case (prev, (count, (t, o))) => {
        count := prev
        prev + getNumSymbolsFromType(tl, t, o)
    }


}
