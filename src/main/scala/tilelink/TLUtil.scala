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
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, bits.mask, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out
            }
            case b: TLBundleB => {
                val out = Wire(Decoupled(new TLBundleB(edge.bundle)))
                val bits = out.bits
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, bits.mask, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                out
            }
            case c: TLBundleC => {
                val out = Wire(Decoupled(new TLBundleC(edge.bundle)))
                val bits = out.bits
                List(bits.opcode, bits.param, bits.size, bits.source, bits.address, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
                }
                bits.error := error
                out
            }
            case d: TLBundleD => {
                val out = Wire(Decoupled(new TLBundleD(edge.bundle)))
                val bits = out.bits
                List(bits.opcode, bits.param, bits.size, bits.source, bits.sink, bits.data).foldLeft(w) { (left, sig) =>
                    sig := buf(left - 1, left - sig.getWidth)
                    left - sig.getWidth
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
                val padBits = padTo - headerWidth(a) - a.data.getWidth - a.mask.getWidth
                Cat(typeA, a.opcode, a.param, a.size, a.source, a.address, a.mask, a.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
            case b: TLBundleB => {
                val padBits = padTo - headerWidth(b) - b.data.getWidth - b.mask.getWidth
                Cat(typeB, b.opcode, b.param, b.size, b.source, b.address, b.mask, b.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
            case c: TLBundleC => {
                val padBits = padTo - headerWidth(c) - c.data.getWidth
                Cat(typeC, c.opcode, c.param, c.size, c.source, c.address, c.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
            }
            case d: TLBundleD => {
                val padBits = padTo - headerWidth(d) - d.data.getWidth
                Cat(typeD, d.opcode, d.param, d.size, d.source, d.sink, d.data, if (padBits > 0) 0.U(padBits.W) else Wire(UInt(0.W)))
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
            a.opcode := opcode
            ret := getNumSymbols(aceEdge, a, first)
        } .elsewhen (tlType === typeB) {
            val b = Wire(new TLBundleB(bdEdge.bundle))
            b.opcode := opcode
            ret := getNumSymbols(bdEdge, b, first)
        } .elsewhen (tlType === typeC) {
            val c = Wire(new TLBundleC(aceEdge.bundle))
            c.opcode := opcode
            ret := getNumSymbols(aceEdge, c, first)
        } .elsewhen (tlType === typeD) {
            val d = Wire(new TLBundleD(bdEdge.bundle))
            d.opcode := opcode
            ret := getNumSymbols(bdEdge, d, first)
        } .otherwise {
            val e = Wire(new TLBundleE(aceEdge.bundle))
            ret := getNumSymbols(aceEdge, e, first)
        }
        ret
    }

    // Ignore error signals here, we'll generate our own
    def getNumSymbols(edge: TLEdge, x: TLChannel, first: Bool): UInt = {
        Mux(first, div8Ceil(headerWidth(x)).U, 0.U) + Mux(edge.hasData(x), (x.data.getWidth/8).U + (x match {
            case a: TLBundleA => { div8Ceil(a.mask.getWidth).U }
            case b: TLBundleB => { div8Ceil(b.mask.getWidth).U }
            case _ => { 0.U }
        }), 0.U)
    }

    def tlResponseMap(x: TLChannel): UInt = {
        // TODO
        assert(false)
        x match {
            case a: TLBundleA => {
                0.U
            }
            case b: TLBundleB => {
                0.U
            }
            case c: TLBundleC => {
                0.U
            }
            case d: TLBundleD => {
                0.U
            }
            case e: TLBundleE => {
                0.U
            }
        }
    }
}

