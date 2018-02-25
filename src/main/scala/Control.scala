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

class ControlIO[T <: Bundle](val portFactory: () => T, val wSeq: Seq[(String, UInt, Option[UInt])], val rSeq: Seq[(String, UInt)]) extends Bundle {

    val r = Input(new CustomBundle(wSeq map {case (x,y,z) => (x,y)} : _*))
    val w = Output(new CustomBundle(rSeq: _*))
    val port = portFactory()
}

abstract class Control[T <: Bundle](val portFactory: () => T, val wSeq: Seq[(String, UInt, Option[UInt])], val rSeq: Seq[(String, UInt)]) extends Module {

    final val io = IO(new ControlIO(portFactory, wSeq, rSeq))

}

class ControlBuilder[T <: Bundle, U <: Control[T]](val controlFactory: (Seq[(String,UInt,Option[UInt])], Seq[(String,UInt)]) => U) {

    private val wSeq = new ArrayBuffer[(String,UInt,Option[UInt])]
    private val rSeq = new ArrayBuffer[(String,UInt)]

    def w(name: String, signal: UInt, init: Option[UInt] = None) {
        if (init != None) require(init.get.isLit, s"Initial value for $signal must be a Chisel literal.")
        wSeq.append((name, signal, init))
    }

    def r(name: String, signal: UInt) {
        rSeq.append((name, signal))
    }

    def generate(): U = {
        val c = Module(controlFactory(wSeq,rSeq))
        wSeq foreach { case (name, node, init) => node := c.io.w(name) }
        rSeq foreach { case (name, node) => c.io.r(name) := node }
        c
    }

}
