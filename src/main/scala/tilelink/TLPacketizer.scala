package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

abstract class TLPacketizer[S <: DecodedSymbol](val params: TLBundleParameters, val decodedSymbolsPerCycle: Int, val symbolFactory: () => S)
    extends Packetizer(decodedSymbolsPerCycle, symbolFactory, {() => TLBundle(params)}) with TLPacketizerLike {

    val tl = io.data

    require(symbolFactory().decodedWidth == 8, "TLPacketizer* only supports 8-bit wide symbols")

    // Just some sanity checks
    require(tl.a.bits.opcode.getWidth == 3)
    require(tl.b.bits.opcode.getWidth == 3)
    require(tl.c.bits.opcode.getWidth == 3)
    require(tl.d.bits.opcode.getWidth == 3)

}

class TLPacketizerMaster[S <: DecodedSymbol](val params: TLBundleParameters, val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, implicit val opt: Boolean = false)
    extends TLPacketizer(params, decodedSymbolsPerCycle, symbolFactory) {

    // TODO not implemented
    require(!opt, "Not implemented")

    /************************ TX *************************/

    val txHeaderBits = tlTypeWidth + List(headerWidth(tl.a.bits), headerWidth(tl.c.bits), headerWidth(tl.e.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + params.dataBits
    val txBuffer = Reg(UInt(txBufferBits.W))

    // TODO if we process more than one request at a time, this needs to change
    val txData = if (opt)
            Mux(tl.a.fire(), Mux(tl.a.bits.opcode === TLMessages.PutPartialData, packData(tl.a.bits.data, tl.a.bits.mask), tl.a.bits.data), tl.c.bits.data)
        else
            Mux(tl.a.fire(), tl.a.bits.data, tl.c.bits.data)

    val aPadBits = txBufferBits - aHeaderBits - params.dataBits
    val cPadBits = txBufferBits - cHeaderBits - params.dataBits
    val ePadBits = txBufferBits - eHeaderBits
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
    val count = RegInit(0.U(log2Ceil(txBufferBits/8 + 1).W))

    // TODO can we process more than one request at a time (e + other?)
    // Assign priorities to the downstream channels
    val txReady = ((count <= decodedSymbolsPerCycle.U) && io.symbolsTxReady) || (count === 0.U)
    io.data.a.ready := txReady && !io.data.c.valid && !io.data.e.valid
    io.data.c.ready := txReady && !io.data.e.valid
    io.data.e.ready := txReady

    val sReset :: sSync :: sAck :: sReady :: Nil = Enum(4)
    val txState = RegInit(sReset)

    // These come from the RX
    val ack = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().ack } reduce (_||_)
    val nack = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().nack } reduce (_||_)

    when (txState === sReset) {
        txState := sSync
    } .elsewhen(txState === sSync) {
        txState := sAck
    } .elsewhen(txState === sAck) {
        when (nack) {
            txState := sSync
        } .elsewhen(ack) {
            txState := sReady
        }
    } .elsewhen(txState === sReady) {
        when (nack) {
            txState := sSync
        }
    } .otherwise {
        // shouldn't get here
        txState := sSync
    }

    io.symbolsTx.reverse.zipWithIndex.foreach { case (s,i) =>
        val doSync = ((i.U === 0.U) && (txState === sSync))
        s.valid := ((i.U < count) && (txState === sReady)) || doSync
        s.bits := Mux(doSync, symbolFactory().sync, symbolFactory().fromData(txBuffer(txBufferBits-8*i-1,txBufferBits-8*i-8)))
    }

    when (txFire) {
        count := txPayloadBytes
        txBuffer := txPacked
    } .elsewhen(count > decodedSymbolsPerCycle.U) {
        when (io.symbolsTxReady) {
            count := count - decodedSymbolsPerCycle.U
            txBuffer := Cat(txBuffer(txBufferBits-decodedSymbolsPerCycle*8-1,0),txBuffer(decodedSymbolsPerCycle*8-1,0))
        }
    } .otherwise {
        when (io.symbolsTxReady) {
            count := 0.U
        }
    }

    /************************ RX *************************/

}

class TLPacketizerSlave[S <: DecodedSymbol](val params: TLBundleParameters, val decodedSymbolsPerCycle: Int, val symbolFactory: () => S, val opt: Boolean = false)
    extends TLPacketizer(params, decodedSymbolsPerCycle, symbolFactory) {

    // TODO not implemented
    require(!opt, "Not implemented")

    /************************ TX *************************/

    val txHeaderBits = tlTypeWidth + List(headerWidth(tl.b.bits), headerWidth(tl.d.bits)).max
    val txBufferBits = div8Ceil(txHeaderBits)*8 + params.dataBits
    val txBuffer = Reg(UInt(txBufferBits.W))

    // TODO if we process more than one request at a time, this needs to change
    val txData = Mux(tl.b.fire(), tl.b.bits.data, tl.d.bits.data)

    val bPadBits = txBufferBits - bHeaderBits - params.dataBits
    val dPadBits = txBufferBits - dHeaderBits - params.dataBits
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
    val count = RegInit(0.U(log2Ceil(txBufferBits/8 + 1).W))

    // TODO can we process more than one request at a time (e + other?)
    // Assign priorities to the downstream channels
    val txReady = ((count <= decodedSymbolsPerCycle.U) && io.symbolsTxReady) || (count === 0.U)
    io.data.b.ready := txReady && !io.data.d.valid
    io.data.d.ready := txReady

    val sReset :: sSync :: sAck :: sReady :: Nil = Enum(4)
    val txState = RegInit(sReset)

    // These come from the RX
    val sync = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().sync } reduce (_||_)
    val nack = io.symbolsRx map { x => x.valid && x.bits === symbolFactory().nack } reduce (_||_)

    when (txState === sReset) {
        txState := sSync
    } .elsewhen(txState === sSync) {
        when (sync) {
            txState := sAck
        }
    } .elsewhen(txState === sAck) {
        when (nack) {
            txState := sSync
        } .otherwise {
            txState := sReady
        }
    } .elsewhen(txState === sReady) {
        when (nack) {
            txState := sSync
        }
    } .otherwise {
        // shouldn't get here
        txState := sSync
    }

    io.symbolsTx.reverse.zipWithIndex.foreach { (s,i) =>
        val doAck = ((i.U === 0.U) && (txState === sAck))
        s.valid := ((i.U < count) && (txState === sReady)) || doAck
        s.bits := Mux(doAck, symbolFactory().ack, symbolFactory().fromData(txBuffer(txBufferBits-8*i-1,txBufferBits-8*i-8)))
    }

    when (txFire) {
        count := txPayloadBytes
        txBuffer := txPacked
    } .elsewhen(count > decodedSymbolsPerCycle.U) {
        when (io.symbolsTxReady) {
            count := count - decodedSymbolsPerCycle.U
            txBuffer := Cat(txBuffer(txBufferBits-decodedSymbolsPerCycle*8-1,0),txBuffer(decodedSymbolsPerCycle*8-1,0))
        }
    } .otherwise {
        when (io.symbolsTxReady) {
            count := 0.U
        }
    }

    /************************ RX *************************/

}

trait TLPacketizerLike {

    val opt: Boolean

    val tlTypeWidth = 3

    def headerWidth(a: TLBundleA): Int = tlTypeWidth + List(a.opcode,a.param,a.size,a.source,a.address,a.mask) map { _.getWidth } reduce (_+_)
    def headerWidth(b: TLBundleB): Int = tlTypeWidth + List(b.opcode,b.param,b.size,b.source,b.address,b.mask) map { _.getWidth } reduce (_+_)
    def headerWidth(c: TLBundleC): Int = tlTypeWidth + 1 + List(c.opcode,c.param,c.size,c.source,c.address) map { _.getWidth } reduce (_+_)
    def headerWidth(d: TLBundleD): Int = tlTypeWidth + 1 + List(d.opcode,d.param,d.size,d.source,d.sink) map { _.getWidth } reduce (_+_)
    def headerWidth(e: TLBundleE): Int = tlTypeWidth + e.sink.getWidth

    val typeA = 0.U(tlTypeWidth.W)
    val typeB = 1.U(tlTypeWidth.W)
    val typeC = 2.U(tlTypeWidth.W)
    val typeD = 3.U(tlTypeWidth.W)
    val typeE = 4.U(tlTypeWidth.W)

    def div8Ceil(x: Int) = (x + 7)/8

    def tlSymbolMap(a: TLBundleA, mask: UInt) = Map(
        (TLMessages.PutFullData    -> div8Ceil(headerWidth(a) + a.data.getWidth).U),
        (TLMessages.PutPartialData -> div8Ceil(headerWidth(a)).U + (if (opt) PopCount(mask) else b.data.getWidth.U)),
        (TLMessages.ArithmeticData -> div8Ceil(headerWidth(a) + a.data.getWidth).U),
        (TLMessages.LogicalData    -> div8Ceil(headerWidth(a) + a.data.getWidth).U),
        (TLMessages.Get            -> div8Ceil(headerWidth(a)).U),
        (TLMessages.Hint           -> div8Ceil(headerWidth(a)).U),
        (TLMessages.AcquireBlock   -> div8Ceil(headerWidth(a)).U),
        (TLMessages.AcquirePerm    -> div8Ceil(headerWidth(a)),U))

    def tlSymbolMap(b: TLBundleB, mask: UInt) = Map(
        (TLMessages.PutFullData    -> div8Ceil(headerWidth(b) + b.data.getWidth).U),
        (TLMessages.PutPartialData -> div8Ceil(headerWidth(b)).U + (if (opt) PopCount(mask) else b.data.getWidth.U)),
        (TLMessages.ArithmeticData -> div8Ceil(headerWidth(b) + b.data.getWidth).U),
        (TLMessages.LogicalData    -> div8Ceil(headerWidth(b) + b.data.getWidth).U),
        (TLMessages.Get            -> div8Ceil(headerWidth(b)).U),
        (TLMessages.Hint           -> div8Ceil(headerWidth(b)).U),
        (TLMessages.Probe          -> div8Ceil(headerWidth(b)).U))

    def tlSymbolMap(c: TLBundleC, mask: UInt) = Map(
        (TLMessages.AccessAck      -> div8Ceil(headerWidth(c)).U),
        (TLMessages.AccessAckData  -> div8Ceil(headerWidth(c) + c.data.getWidth).U),
        (TLMessages.HintAck        -> div8Ceil(headerWidth(c)).U),
        (TLMessages.ProbeAck       -> div8Ceil(headerWidth(c)).U),
        (TLMessages.ProbeAckData   -> div8Ceil(headerWidth(c) + c.data.getWidth).U),
        (TLMessages.Release        -> div8Ceil(headerWidth(c)).U),
        (TLMessages.ReleaseData    -> div8Ceil(headerWidth(c) + c.data.getWidth).U))

    def tlSymbolMap(d: TLBundleD, mask: UInt) = Map(
        (TLMessages.AccessAck      -> div8Ceil(headerWidth(d)).U),
        (TLMessages.AccessAckData  -> div8Ceil(headerWidth(d) + d.data.getWidth).U),
        (TLMessages.HintAck        -> div8Ceil(headerWidth(d)).U),
        (TLMessages.Grant          -> div8Ceil(headerWidth(d)).U),
        (TLMessages.GrantData      -> div8Ceil(headerWidth(d) + d.data.getWidth).U),
        (TLMessages.ReleaseAck     -> div8Ceil(headerWidth(d)).U))

    def tlSymbolMap(e: TLBundleE, mask: UInt) = Map(
        (TLMessages.GrantAck       -> div8Ceil(headerWidth(e)).U))

    def getNumSymbols[T <: TLChannel](chan: T, opcode: UInt, mask: UInt): UInt = {
        val ret = MuxLookup(
            opcode,
            0.U,
            tlSymbolMap(chan, mask)
        )
        assert(ret =/= 0.U, "Illegal TLMessage")
        ret
    }

    def packData(data: UInt, mask: UInt): UInt = {
        /* val out = Wire(Vec(mask.getWidth, UInt(8.W)))
        out.zipWithIndex foreach { (d,i) =>
            // This is broken
            //d := out.slice(mask.getWidth-1,i)(PriorityEncoder(mask(mask.getWidth-1,i)))
        }
        out.asUInt
        */
        ???
    }

}
