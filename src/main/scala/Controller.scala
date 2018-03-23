package hbwif

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, requireIsChiselType}
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap

abstract class ControllerBuilder {

    type P <: Bundle
    def createPort(): P

    protected val ws = new ArrayBuffer[(String,UInt,Option[BigInt])]
    protected val rs = new ArrayBuffer[(String,UInt)]
    protected val wSeqMems = new ArrayBuffer[(String,Int,UInt,UInt,Bool)]
    protected val rSeqMems = new ArrayBuffer[(String,Int,UInt,UInt,Bool)]

    def w(name: String, signal: UInt) { this.w(name, signal, None) }

    // TODO include some better error checking about trying to write to an unwriteable node
    def w(name: String, signal: UInt, init: Option[BigInt]) {
        if (signal.getWidth > 0) {
            ws.append((name, signal, init))
        }
    }

    def w(name: String, signal: Seq[UInt]) { this.w(name, signal, None) }

    def w(name: String, signal: Seq[UInt], init: Option[Seq[BigInt]]) {
        (0 until signal.length) foreach { i => this.w(name + s"_$i", signal(i), init.map(_(i))) }
    }

    def r(name: String, signal: UInt) {
        if (signal.getWidth > 0) {
            rs.append((name, signal))
        }
    }

    def r(name: String, signal: Seq[UInt]) {
        (0 until signal.length) foreach { i => this.r(name + s"_$i", signal(i)) }
    }


    def wSeqMem(name: String, depth: Int, signal: UInt, addr: UInt, en: Bool) {
        wSeqMems.append((name, depth, signal, addr, en))
    }

    def rSeqMem(name: String, depth: Int, signal: UInt, addr: UInt, en: Bool) {
        rSeqMems.append((name, depth, signal, addr, en))
    }

    // TODO deal with async crossings
    def generate(laneClock: Clock, laneReset: Bool, port: P)

}

trait HasControllerConnector {

    // Override this to connect your signals to the controller
    def connectController(builder: ControllerBuilder)

}
