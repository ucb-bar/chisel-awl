package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, requireIsChiselType}
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap
/*

final class CustomBundle(elts: (String, Data)*) extends Record {
  val elements = ListMap(elts map { case (field, elt) =>
    requireIsChiselType(elt)
    field -> elt
  }: _*)
  def apply(elt: String): Data = elements(elt)
  override def cloneType = {
    val cloned = elts.map { case (n, d) => n -> DataMirror.internal.chiselTypeClone(d) }
    (new CustomBundle(cloned: _*)).asInstanceOf[this.type]
  }
}

// TODO refactor this as an object
class ControllerIO[P <: Bundle](val portFactory: () => P, val spec: ControlSpec) extends Bundle {

    val w = Output(new CustomBundle(spec.w map {case (n,v,i) => (n,chiselTypeOf(v))}: _*))
    val r = Input(new CustomBundle(spec.r map {case (n,v) => (n,chiselTypeOf(v))}: _*))
    //val wSeqMem = Output(new CustomBundle())
    //val rSeqMem = Input(new CustomBundle())
    val control = portFactory()
}

sealed abstract class Controller(val spec: ControlSpec) extends Module {

    type P <: Bundle
    def portFactory(): P

    final val io = IO(new ControllerIO(portFactory, spec))

}
*/

abstract class ControllerBuilder {

    type P <: Bundle
    def createPort(): P

    protected val ws = new ArrayBuffer[(String,UInt,Option[UInt])]
    protected val rs = new ArrayBuffer[(String,UInt)]
    protected val wSeqMems = new ArrayBuffer[(String,Vec[UInt])]
    protected val rSeqMems = new ArrayBuffer[(String,Vec[UInt])]

    def w(name: String, signal: UInt, init: Option[UInt] = None) {
        if (init != None) require(init.get.isLit, s"Initial value for $signal must be a Chisel literal.")
        ws.append((name, signal, init))
    }

    def r(name: String, signal: UInt) {
        rs.append((name, signal))
    }

/*
    def wSeqMem(name: String, signal: SeqMem[UInt]) {
        wSeqMems.append((name, seqmem))
    }

    def rSeqMem(name: String, seqmem: SeqMem[UInt]) {
        rSeqMems.append((name, seqmem))
    }
*/
    // This function MUST implement any clock crossings into the implicit (clock, reset) domain from the (laneClock, laneReset) domain
    def generate(laneClock: Clock, laneReset: Bool): P

}

trait HasControllerConnector {

    // Override this to connect your signals to the controller
    def connectController(builder: ControllerBuilder)

}
