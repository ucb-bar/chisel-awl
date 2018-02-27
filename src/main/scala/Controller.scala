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

class ControllerIO[T <: Bundle](val portFactory: () => T, val spec: ControlSpec) extends Bundle {

    val r = Input(new CustomBundle(spec.w map {case (x,y,z) => (x,y)} : _*))
    val w = Output(new CustomBundle(spec.r: _*))
    val port = portFactory()
}

abstract class Controller(val spec: ControlSpec) extends Module {

    type PortType <: Bundle
    val portFactory: () => PortType

    final val io = IO(new ControllerIO(portFactory, spec))

}

class ControllerBuilder[T <: Controller](val controlFactory: (ControlSpec) => T) {

    private val wSeq = new ArrayBuffer[(String,UInt,Option[UInt])]
    private val rSeq = new ArrayBuffer[(String,UInt)]

    def w(name: String, signal: UInt, init: Option[UInt] = None) {
        if (init != None) require(init.get.isLit, s"Initial value for $signal must be a Chisel literal.")
        wSeq.append((name, signal, init))
    }

    def r(name: String, signal: UInt) {
        rSeq.append((name, signal))
    }

    def generate(): T = {
        val c = Module(controlFactory(ControlSpec(wSeq,rSeq)))
        wSeq foreach { case (name, node, init) => node := c.io.w(name) }
        rSeq foreach { case (name, node) => c.io.r(name) := node }
        c
    }

}

case class ControlSpec(
    val w: Seq[(String, UInt, Option[UInt])],
    val r: Seq[(String, UInt)]
)
