package hbwif2

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, requireIsChiselType}
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap

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

class ControllerIO[P <: Bundle](val portFactory: () => P, val spec: ControlSpec) extends Bundle {

    val w = Output(new CustomBundle(spec.w map {case (n,v,i) => (n,chiselTypeOf(v))}: _*))
    val r = Input(new CustomBundle(spec.r map {case (n,v) => (n,chiselTypeOf(v))}: _*))
    val port = portFactory()
}

abstract class Controller[P <: Bundle](val spec: ControlSpec, val portFactory: () => P) extends Module {

    final val io = IO(new ControllerIO(portFactory, spec))

}

class ControllerBuilder[P <: Bundle, C <: Controller[P]](val controllerFactory: (ControlSpec) => C) {

    private val wSeq = new ArrayBuffer[(String,UInt,Option[UInt])]
    private val rSeq = new ArrayBuffer[(String,UInt)]

    def w(name: String, signal: UInt, init: Option[UInt] = None) {
        if (init != None) require(init.get.isLit, s"Initial value for $signal must be a Chisel literal.")
        wSeq.append((name, signal, init))
    }

    def r(name: String, signal: UInt) {
        rSeq.append((name, signal))
    }

    def generate(): C = {
        val c = Module(controllerFactory(ControlSpec(wSeq,rSeq)))
        wSeq foreach { case (name, node, init) => node := c.io.w(name) }
        rSeq foreach { case (name, node) => c.io.r(name) := node }
        c
    }

}

case class ControlSpec(
    val w: Seq[(String, UInt, Option[UInt])],
    val r: Seq[(String, UInt)]
)
