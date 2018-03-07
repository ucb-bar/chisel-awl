package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

abstract class TLPacketizer[S <: DecodedSymbol](params: TLBundleParameters, decodedSymbolsPerCycle: Int, symbolFactory: () => S)
    extends Packetizer(decodedSymbolsPerCycle, symbolFactory, {() => TLBundle(params)}) with TLPacketizerLike {

    val tl = io.data
    val tlQueueDepth = 8 // TODO XXX

    require(symbolFactory().decodedWidth == 8, "TLPacketizer* only supports 8-bit wide symbols")

    // Just some sanity checks
    require(tl.a.bits.opcode.getWidth == 3)
    require(tl.b.bits.opcode.getWidth == 3)
    require(tl.c.bits.opcode.getWidth == 3)
    require(tl.d.bits.opcode.getWidth == 3)
    // We want the opcode to fit in the first byte with the type
    require(tlTypeWidth == 3)

}

class TLPacketizerMaster[S <: DecodedSymbol](params: TLBundleParameters, decodedSymbolsPerCycle: Int, symbolFactory: () => S)
    extends TLPacketizer(params, decodedSymbolsPerCycle, symbolFactory) {

    /************************ TX *************************/

    val txHeaderBits = tlTypeWidth + List(headerWidth(tl.a.bits), headerWidth(tl.c.bits), headerWidth(tl.e.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + params.dataBits
    val txBuffer = Reg(UInt(txBufferBits.W))

    val txData = Mux(tl.a.fire(), tl.a.bits.data, tl.c.bits.data)

    val aPadBits = txBufferBits - headerWidth(tl.a.bits) - params.dataBits
    val cPadBits = txBufferBits - headerWidth(tl.c.bits) - params.dataBits
    val ePadBits = txBufferBits - headerWidth(tl.e.bits)
    val aHeader = Cat(typeA, tl.a.bits.opcode, tl.a.bits.param, tl.a.bits.size, tl.a.bits.source, tl.a.bits.address, tl.a.bits.mask, if (aPadBits > 0) 0.U(aPadBits.W) else Wire(UInt(0.W)))
    val cHeader = Cat(typeC, tl.c.bits.opcode, tl.c.bits.param, tl.c.bits.size, tl.c.bits.source, tl.c.bits.address, tl.c.bits.error, if (cPadBits > 0) 0.U(cPadBits.W) else Wire(UInt(0.W)))
    val eHeader = Cat(typeE, tl.e.bits.sink, if (ePadBits > 0) 0.U(ePadBits.W) else Wire(UInt(0.W)))

    val txPayloadBytes = Wire(UInt())
    val txPacked = Wire(UInt(txBufferBits.W))
    when (tl.e.valid) {
        txPayloadBytes := getNumSymbols(tl.e.bits, 0.U, 0.U)
        txPacked := Cat(eHeader, txData)
    } .elsewhen (tl.c.valid) {
        txPayloadBytes := getNumSymbols(tl.c.bits, tl.c.bits.opcode, 0.U)
        txPacked := Cat(cHeader, txData)
    } .otherwise { // a
        txPayloadBytes := getNumSymbols(tl.a.bits, tl.a.bits.opcode, tl.a.bits.mask)
        txPacked := Cat(aHeader, txData)
    }

    val txFire = tl.a.fire() || tl.c.fire() || tl.e.fire()
    val txCount = RegInit(0.U(log2Ceil(txBufferBits/8 + 1).W))

    // TODO can we process more than one request at a time (e + other?)
    // Assign priorities to the downstream channels
    val txReady = ((txCount <= decodedSymbolsPerCycle.U) && io.symbolsTxReady) || (txCount === 0.U)
    io.data.a.ready := txReady && !io.data.c.valid && !io.data.e.valid
    io.data.c.ready := txReady && !io.data.e.valid
    io.data.e.ready := txReady

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

    when (txFire) {
        txCount := txPayloadBytes
        txBuffer := txPacked
    } .elsewhen(txCount > decodedSymbolsPerCycle.U) {
        when (io.symbolsTxReady) {
            txCount := txCount - decodedSymbolsPerCycle.U
            txBuffer := Cat(txBuffer(txBufferBits-decodedSymbolsPerCycle*8-1,0),txBuffer(decodedSymbolsPerCycle*8-1,0))
        }
    } .otherwise {
        when (io.symbolsTxReady) {
            txCount := 0.U
        }
    }

    /************************ RX *************************/

    val bBuf = Reg(new TLBundleB(params))
    val dBuf = Reg(new TLBundleD(params))

    val bQueue = Queue(new TLBundleB(params), tlQueueDepth)
    val dQueue = Queue(new TLBundleD(params), tlQueueDepth)

    val rxHeaderBits = tlTypeWidth + List(headerWidth(tl.b.bits), headerWidth(tl.d.bits)).max
    val rxBufferBytes = div8Ceil(rxHeaderBits) + params.dataBits/8
    val maxBundlesPerCycle = max(1,decodedSymbolsPerCycle/div8Ceil(tlTypeWidth + List(headerWidth(tl.b.bits), headerWidth(tl.d.bits)).min))

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

class TLPacketizerSlave[S <: DecodedSymbol](params: TLBundleParameters, decodedSymbolsPerCycle: Int, symbolFactory: () => S)
    extends TLPacketizer(params, decodedSymbolsPerCycle, symbolFactory) {

    /************************ TX *************************/

    val txHeaderBits = tlTypeWidth + List(headerWidth(tl.b.bits), headerWidth(tl.d.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + params.dataBits
    val txBuffer = Reg(UInt(txBufferBits.W))

    // TODO if we process more than one request at a time, this needs to change
    val txData = Mux(tl.b.fire(), tl.b.bits.data, tl.d.bits.data)

    val bPadBits = txBufferBits - headerWidth(tl.b.bits) - params.dataBits
    val dPadBits = txBufferBits - headerWidth(tl.d.bits) - params.dataBits
    val bHeader = Cat(typeB, tl.b.bits.opcode, tl.b.bits.param, tl.b.bits.size, tl.b.bits.source, tl.b.bits.address, tl.b.bits.mask, if (bPadBits > 0) 0.U(bPadBits.W) else Wire(UInt(0.W)))
    val dHeader = Cat(typeD, tl.d.bits.opcode, tl.d.bits.param, tl.d.bits.size, tl.d.bits.source, tl.d.bits.sink, tl.d.bits.error, if (dPadBits > 0) 0.U(dPadBits.W) else Wire(UInt(0.W)))

    val txPayloadBytes = Wire(UInt())
    val txPacked = Wire(UInt(txBufferBits.W))
    when (tl.d.valid) {
        txPayloadBytes := getNumSymbols(tl.d.bits, tl.d.bits.opcode, 0.U)
        txPacked := Cat(dHeader, txData)
    } .otherwise { // b
        txPayloadBytes := getNumSymbols(tl.b.bits, tl.b.bits.opcode, tl.b.bits.mask)
        txPacked := Cat(bHeader, txData)
    }

    val txFire = tl.b.fire() || tl.d.fire()
    val txCount = RegInit(0.U(log2Ceil(txBufferBits/8 + 1).W))

    // TODO can we process more than one request at a time (e + other?)
    // Assign priorities to the downstream channels
    val txReady = ((txCount <= decodedSymbolsPerCycle.U) && io.symbolsTxReady) || (txCount === 0.U)
    io.data.b.ready := txReady && !io.data.d.valid
    io.data.d.ready := txReady

    val sTxReset :: sTxSync :: sTxAck :: sTxReady :: Nil = Enum(4)
    val txState = RegInit(sTxReset)

    // These come from the RX
    val sync = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().sync } reduce (_||_)
    val nack = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().nack } reduce (_||_)

    when (txState === sTxReset) {
        txState := sTxSync
    } .elsewhen(txState === sTxSync) {
        when (sync) {
            txState := sTxAck
        }
    } .elsewhen(txState === sTxAck) {
        when (nack) {
            txState := sTxSync
        } .otherwise
            txState := sTxReady
        }
    } .elsewhen(txState === sTxReady) {
        when (nack) {
            txState := sTxSync
        } .elsewhen (sync) {
            txState := sTxAck
        }
    } .otherwise {
        // shouldn't get here
        txState := sTxSync
    }

    io.symbolsTx.reverse.zipWithIndex.foreach { case (s,i) =>
        val doAck = ((i.U === 0.U) && (txState === sTxAck))
        s.valid := ((i.U < txCount) && (txState === sTxReady)) || doAck
        s.bits := Mux(doAck, symbolFactory().ack, symbolFactory().fromData(txBuffer(txBufferBits-8*i-1,txBufferBits-8*i-8)))
    }

    when (txFire) {
        txCount := txPayloadBytes
        txBuffer := txPacked
    } .elsewhen(txCount > decodedSymbolsPerCycle.U) {
        when (io.symbolsTxReady) {
            txCount := txCount - decodedSymbolsPerCycle.U
            txBuffer := Cat(txBuffer(txBufferBits-decodedSymbolsPerCycle*8-1,0),txBuffer(decodedSymbolsPerCycle*8-1,0))
        }
    } .otherwise {
        when (io.symbolsTxReady) {
            txCount := 0.U
        }
    }

    /************************ RX *************************/

    val aBuf = Reg(new TLBundleA(params))
    val cBuf = Reg(new TLBundleC(params))
    val dBuf = Reg(new TLBundleE(params))

    val aQueue = Queue(new TLBundleA(params), tlQueueDepth)
    val cQueue = Queue(new TLBundleC(params), tlQueueDepth)
    val eQueue = Queue(new TLBundleE(params), tlQueueDepth)

}
