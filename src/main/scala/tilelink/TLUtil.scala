package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

trait TLPacketizerLike {

    val tlTypeWidth = 3

    def headerWidth(x: TLChannel): Int = {
        x match {
            case a: TLBundleA => { tlTypeWidth + List(a.opcode,a.param,a.size,a.source,a.address).map( _.getWidth).reduce(_+_) }// mask is separate
            case b: TLBundleB => { tlTypeWidth + List(b.opcode,b.param,b.size,b.source,b.address).map(_.getWidth).reduce(_+_) } // mask is separate
            case c: TLBundleC => { tlTypeWidth + List(c.opcode,c.param,c.size,c.source,c.address).map(_.getWidth).reduce(_+_) } // ignore error
            case d: TLBundleD => { tlTypeWidth + List(d.opcode,d.param,d.size,d.source,d.sink).map(_.getWidth).reduce(_+_) }    // ignore error
            case e: TLBundleE => { tlTypeWidth + e.sink.getWidth }
        }
    }

    val typeA = 0.U(tlTypeWidth.W)
    val typeB = 1.U(tlTypeWidth.W)
    val typeC = 2.U(tlTypeWidth.W)
    val typeD = 3.U(tlTypeWidth.W)
    val typeE = 4.U(tlTypeWidth.W)

    def div8Ceil(x: Int): Int = (x + 7)/8

    def tlFromBuffer[T <: TLChannel](edge: TLEdge, x: T, buf: UInt, error: Bool): DecoupledIO[T] = {
        val w = buf.getWidth - tlTypeWidth
        (x match {
            case a: TLBundleA => {
                val out = Wire(Decoupled(new TLBundleA(edge.bundle)))
                val bits = out.bits
                val pad = Wire(UInt((buf.getWidth - headerWidth(bits) - bits.mask.getWidth - bits.data.getWidth).W))
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, pad, bits.mask, bits.data).foldLeft(w) { (left, sig) =>
                    if (sig.getWidth > 0) {
                        sig := buf(left - 1, left - sig.getWidth)
                        left - sig.getWidth
                    } else {
                        left
                    }
                }
                out
            }
            case b: TLBundleB => {
                val out = Wire(Decoupled(new TLBundleB(edge.bundle)))
                val bits = out.bits
                val pad = Wire(UInt((buf.getWidth - headerWidth(bits) - bits.mask.getWidth - bits.data.getWidth).W))
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, pad, bits.mask, bits.data).foldLeft(w) { (left, sig) =>
                    if (sig.getWidth > 0) {
                        sig := buf(left - 1, left - sig.getWidth)
                        left - sig.getWidth
                    } else {
                        left
                    }
                }
                out
            }
            case c: TLBundleC => {
                val out = Wire(Decoupled(new TLBundleC(edge.bundle)))
                val bits = out.bits
                val pad = Wire(UInt((buf.getWidth - headerWidth(bits) - bits.data.getWidth).W))
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, pad, bits.data).foldLeft(w) { (left, sig) =>
                    if (sig.getWidth > 0) {
                        sig := buf(left - 1, left - sig.getWidth)
                        left - sig.getWidth
                    } else {
                        left
                    }
                }
                bits.error := error
                out
            }
            case d: TLBundleD => {
                val out = Wire(Decoupled(new TLBundleD(edge.bundle)))
                val bits = out.bits
                val pad = Wire(UInt((buf.getWidth - headerWidth(bits) - bits.data.getWidth).W))
                List(bits.opcode, bits.param, bits.size, bits.source, bits.sink, pad, bits.data).foldLeft(w) { (left, sig) =>
                    if (sig.getWidth > 0) {
                        sig := buf(left - 1, left - sig.getWidth)
                        left - sig.getWidth
                    } else {
                        left
                    }
                }
                bits.error := error
                out
            }
            case e: TLBundleE => {
                val out = Wire(Decoupled(new TLBundleE(edge.bundle)))
                out.bits.sink := buf(w - 1, w - out.bits.sink.getWidth)
                out
            }
        }).asInstanceOf[DecoupledIO[T]]
    }

    def tlToBuffer[T <: TLChannel](edge: TLEdge, x: T, padTo: Int): UInt = {

        x match {
            case a: TLBundleA => {
                val padBits = padTo - headerWidth(a) - edge.bundle.dataBits - edge.bundle.dataBits/8
                Cat(typeA, a.opcode, a.param, a.size, a.source, a.address, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)), a.mask, a.data)
            }
            case b: TLBundleB => {
                val padBits = padTo - headerWidth(b) - edge.bundle.dataBits - edge.bundle.dataBits/8
                Cat(typeB, b.opcode, b.param, b.size, b.source, b.address, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)), b.mask, b.data)
            }
            case c: TLBundleC => {
                val padBits = padTo - headerWidth(c) - edge.bundle.dataBits
                Cat(typeC, c.opcode, c.param, c.size, c.source, c.address, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)), c.data)
            }
            case d: TLBundleD => {
                val padBits = padTo - headerWidth(d) - edge.bundle.dataBits
                Cat(typeD, d.opcode, d.param, d.size, d.source, d.sink, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)), d.data)
            }
            case e: TLBundleE => {
                val padBits = padTo - headerWidth(e)
                Cat(typeE, e.sink, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
        }
    }

    def typeFromBuffer(buf: UInt): (UInt, UInt) = (buf(buf.getWidth - 1, buf.getWidth - tlTypeWidth), buf(buf.getWidth - tlTypeWidth - 1, buf.getWidth - tlTypeWidth - 3))

    def getNumSymbolsFromType(aceEdge: TLEdge, bdEdge: TLEdge, tlType: UInt, opcode: UInt, first: Bool): UInt = {
        val ret = Wire(UInt())
        when (tlType === typeA) {
            val a = Wire(new TLBundleA(aceEdge.bundle))
            a := a.fromBits(0.U)
            a.opcode := opcode
            ret := getNumSymbols(aceEdge, a, first)
        } .elsewhen (tlType === typeB) {
            val b = Wire(new TLBundleB(bdEdge.bundle))
            b := b.fromBits(0.U)
            b.opcode := opcode
            ret := getNumSymbols(bdEdge, b, first)
        } .elsewhen (tlType === typeC) {
            val c = Wire(new TLBundleC(aceEdge.bundle))
            c := c.fromBits(0.U)
            c.opcode := opcode
            ret := getNumSymbols(aceEdge, c, first)
        } .elsewhen (tlType === typeD) {
            val d = Wire(new TLBundleD(bdEdge.bundle))
            d := d.fromBits(0.U)
            d.opcode := opcode
            ret := getNumSymbols(bdEdge, d, first)
        } .otherwise {
            val e = Wire(new TLBundleE(aceEdge.bundle))
            e := e.fromBits(0.U)
            ret := getNumSymbols(aceEdge, e, first)
        }
        ret
    }

    // Ignore error signals here, we'll generate our own
    // TODO we can avoid sending mask in some cases
    def getNumSymbols(edge: TLEdge, x: TLChannel, first: Bool): UInt = {
        Mux(first, div8Ceil(headerWidth(x)).U, 0.U) + Mux(edge.hasData(x), (edge.bundle.dataBits/8).U, 0.U) + (x match {
            case a: TLBundleA => { div8Ceil(edge.bundle.dataBits/8).U }
            case b: TLBundleB => { div8Ceil(edge.bundle.dataBits/8).U }
            case _ => { 0.U }
        })
    }

    def tlResponseMap(x: TLChannel): UInt = 1.U

    /*
    def tlResponseMap(x: TLChannel): UInt = {
        // TODO
        assert(false)
        x match {
            case a: TLBundleA => {
                MuxLookup(a.opcode, 0.U, Seq(
                    (TLMessages.PutFullData,    1.U),
                    (TLMessages.PutPartialData, 1.U),
                    (TLMessages.ArithmeticData, X.U),
                    (TLMessages.LogicalData,    X.U),
                    (TLMessages.Get,            X.U),
                    (TLMessages.Hint,           X.U),
                    (TLMessages.AcquireBlock,   X.U),
                    (TLMessages.AcquirePerm,    X.U)
                ))
            }
            case b: TLBundleB => {
                MuxLookup(a.opcode, 0.U, Seq(
                    (TLMessages.PutFullData,    X.U),
                    (TLMessages.PutPartialData, X.U),
                    (TLMessages.ArithmeticData, X.U),
                    (TLMessages.LogicalData,    X.U),
                    (TLMessages.Get,            X.U),
                    (TLMessages.Hint,           X.U),
                    (TLMessages.Probe,          X.U)
                ))
            }
            case c: TLBundleC => {
                MuxLookup(a.opcode, 0.U, Seq(
                    (TLMessages.AccessAck,      X.U),
                    (TLMessages.AccessAckData,  X.U),
                    (TLMessages.HintAck,        X.U),
                    (TLMessages.ProbeAck,       X.U),
                    (TLMessages.ProbeAckData,   X.U),
                    (TLMessages.Hint,           X.U),
                    (TLMessages.Probe,          X.U)
                ))
            }
            case d: TLBundleD => {
                0.U
            }
            case e: TLBundleE => {
                0.U
            }
        }
    }
    */
}
