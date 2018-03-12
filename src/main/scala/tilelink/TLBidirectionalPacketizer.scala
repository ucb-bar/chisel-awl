package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TLBidirectionalPacketizerIO(clientEdge: TLEdgeOut, managerEdge: TLEdgeIn) extends Bundle {
    val client = TLBundle(clientEdge.bundle)
    val manager = Flipped(TLBundle(managerEdge.bundle))
}

object TLBidirectionalPacketizerIO {
    def apply(clientEdge: TLEdgeOut, managerEdge: TLEdgeIn)() =  new TLBidirectionalPacketizerIO(clientEdge, managerEdge)
}

class TLBidirectionalPacketizer[S <: DecodedSymbol](clientEdge: TLEdgeOut, managerEdge: TLEdgeIn, decodedSymbolsPerCycle: Int, symbolFactory: () => S)
    extends Packetizer(decodedSymbolsPerCycle, symbolFactory, TLBidirectionalPacketizerIO.apply(clientEdge, managerEdge) _) with TLPacketizerLike {

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
    require(isPow2(managerEdge.bundle.dataBits))
    require(managerEdge.bundle.dataBits/8 >= 8) // mask must be a multiple of bytes

    /************************ TX *************************/

    val txHeaderBits = List(headerWidth(tltx.a.bits), headerWidth(tltx.b.bits), headerWidth(tltx.c.bits), headerWidth(tltx.d.bits), headerWidth(tltx.e.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + List(managerEdge.bundle.dataBits, clientEdge.bundle.dataBits).max + List(managerEdge.bundle.dataBits/8, clientEdge.bundle.dataBits/8).max
    val txBuffer = Reg(UInt(txBufferBits.W))

    val txA = tlToBuffer(tlrx.aEdge, tlrx.a.bits, txBufferBits)
    val txB = tlToBuffer(tlrx.bEdge, tlrx.b.bits, txBufferBits)
    val txC = tlToBuffer(tlrx.cEdge, tlrx.c.bits, txBufferBits)
    val txD = tlToBuffer(tlrx.dEdge, tlrx.d.bits, txBufferBits)
    val txE = tlToBuffer(tlrx.eEdge, tlrx.e.bits, txBufferBits)

    val (txAFirst, txALast, txADone, txACount) = tltx.aEdge.firstlastHelper(tltx.a.bits, tltx.a.fire())
    val (txBFirst, txBLast, txBDone, txBCount) = tltx.bEdge.firstlastHelper(tltx.b.bits, tltx.b.fire())
    val (txCFirst, txCLast, txCDone, txCCount) = tltx.cEdge.firstlastHelper(tltx.c.bits, tltx.c.fire())
    val (txDFirst, txDLast, txDDone, txDCount) = tltx.dEdge.firstlastHelper(tltx.d.bits, tltx.d.fire())

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

    // TODO how to handle a,b backpressure, and c needs something to handle Release/ReleaseData
    cOutstanding := cOutstanding + Mux(tltx.b.fire() && txBFirst, tlResponseMap(tltx.b.bits), 0.U) - tlrx.c.fire()
    dOutstanding := dOutstanding + Mux(tltx.a.fire() && txAFirst, tlResponseMap(tltx.a.bits), 0.U) + Mux(tltx.c.fire() && txCFirst, tlResponseMap(tltx.c.bits), 0.U) - tlrx.d.fire()
    eOutstanding := eOutstanding + Mux(tltx.d.fire() && txDFirst, tlResponseMap(tltx.d.bits), 0.U) - tlrx.e.fire()

    val sTxReset :: sTxSync :: sTxAck :: sTxReady :: Nil = Enum(4)
    val txState = RegInit(sTxReset)

    // TODO can we process more than one request at a time (e + other?)
    // Assign priorities to the channels
    val txReady = (((txCount <= decodedSymbolsPerCycle.U) && io.symbolsTxReady) || (txCount === 0.U)) && txState === sTxReady
    val aReady = txReady && (dOutstanding < (dMaxOutstanding.U - tlResponseMap(tltx.a.bits)))
    val bReady = txReady && (cOutstanding < (cMaxOutstanding.U - tlResponseMap(tltx.b.bits)))
    val cReady = txReady && (dOutstanding < (dMaxOutstanding.U - tlResponseMap(tltx.c.bits)))
    val dReady = txReady && (eOutstanding < (eMaxOutstanding.U - tlResponseMap(tltx.d.bits)))
    val eReady = txReady

    tltx.a.ready := (aReady && !tltx.b.valid && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txADataInflight)
    tltx.b.ready := (bReady && !tltx.c.valid && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txBDataInflight)
    tltx.c.ready := (cReady && !tltx.d.valid && !tltx.e.valid && !dataInflight) || (txReady && txCDataInflight)
    tltx.d.ready := (dReady && !tltx.e.valid && !dataInflight) || (txReady && txDDataInflight)
    tltx.e.ready := (eReady && !dataInflight)

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

    val rxHeaderBits = List(headerWidth(tlrx.a.bits), headerWidth(tlrx.b.bits), headerWidth(tlrx.c.bits), headerWidth(tlrx.d.bits), headerWidth(tlrx.e.bits)).max
    val rxBufferBytes = div8Ceil(rxHeaderBits) + List(managerEdge.bundle.dataBits/64, clientEdge.bundle.dataBits/64).max + List(managerEdge.bundle.dataBits, clientEdge.bundle.dataBits).max/8 + (decodedSymbolsPerCycle - 1)

    val rxBuffer = Reg(Vec(rxBufferBytes, UInt(8.W)))
    val (rxType, rxOpcode) = typeFromBuffer(rxBuffer.asUInt)

    val rxA = tlFromBuffer(tlrx.aEdge, tlrx.a.bits, rxBuffer.asUInt, false.B)
    val rxB = tlFromBuffer(tlrx.bEdge, tlrx.b.bits, rxBuffer.asUInt, false.B)
    val rxC = tlFromBuffer(tlrx.cEdge, tlrx.c.bits, rxBuffer.asUInt, false.B)
    val rxD = tlFromBuffer(tlrx.dEdge, tlrx.d.bits, rxBuffer.asUInt, false.B)
    val rxE = tlFromBuffer(tlrx.eEdge, tlrx.e.bits, rxBuffer.asUInt, false.B)

    // Assume we can only handle one thing at a time, for now
    tlrx.a <> Queue(rxA, aMaxOutstanding)
    tlrx.b <> Queue(rxB, bMaxOutstanding)
    tlrx.c <> Queue(rxC, cMaxOutstanding)
    tlrx.d <> Queue(rxD, dMaxOutstanding)
    tlrx.e <> Queue(rxE, eMaxOutstanding)

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

    val rxNumSymbols = getNumSymbolsFromType(tlrx.aEdge, tlrx.bEdge, rxType, rxOpcode, true.B)
    val rxNumSymbolsNotLast = getNumSymbolsFromType(tlrx.aEdge, tlrx.bEdge, rxType, rxOpcode, false.B)
    val rxValid = (rxSymCount >= rxNumSymbols)

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

trait HasTLBidirectionalPacketizer {

    type T = TLBidirectionalPacketizerIO

    def decodedSymbolsPerCycle: Int
    val clientEdge: TLEdgeOut
    val managerEdge: TLEdgeIn

    def genPacketizer[S <: DecodedSymbol](symbolFactory: () => S) = new TLBidirectionalPacketizer[S](clientEdge, managerEdge, decodedSymbolsPerCycle, symbolFactory)
}

