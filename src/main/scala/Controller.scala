package hbwif

import chisel3._
import chisel3.util._
import scala.collection.mutable.HashMap

case class ControlInput(
    name: String,
    signal: UInt,
    default: Option[BigInt],
    desc: Option[String]
)

case class ControlOutput(
    name: String,
    signal: UInt,
    desc: Option[String]
)

class ControlIO extends Bundle {
    final val inputMap = new HashMap[String, ControlInput]
    final val outputMap = new HashMap[String, ControlOutput]

    def attach[T <: ControlIO](other: T) {
        // connect by name
        inputMap.foreach { case (name, i) => i.signal := other.inputMap(name).signal }
        outputMap.foreach { case (name, o) => other.outputMap(name).signal := o.signal }
    }
    def attach[T <: ControlIO](other: Option[T]) { attach(other.get) }

    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: Option[String]): T = {
        val x = Input(in)
        inputMap.put(name, ControlInput(name, x, default, desc))
        x
    }
    def input[T <: UInt](in: T, default: Option[BigInt], name: String): T = input[T](in, default, name, None)
    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: String): T = input[T](in, default, name, Some(desc))
    def input[T <: UInt](in: T, default: BigInt, name: String, desc: String): T = input[T](in, Some(default), name, Some(desc))
    def input[T <: UInt](in: T, default: BigInt, name: String): T = input[T](in, Some(default), name, None)
    def input[T <: UInt](in: T, name: String, desc: String): T = input[T](in, None, name, Some(desc))
    def input[T <: UInt](in: T, name: String): T = input[T](in, None, name, None)

    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: Option[String]): Vec[T] = {
        val x = Input(in)
        x.zipWithIndex.foreach { case (signal, i) => inputMap.put(name + s"_$i", ControlInput(name + s"_$i", signal, default.map(_(i)), desc)) }
        x
    }
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String): Vec[T] = input[T](in, default, name, None)
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: String): Vec[T] = input[T](in, default, name, Some(desc))
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String, desc: String): Vec[T] = input[T](in, Some(default), name, Some(desc))
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String): Vec[T] = input[T](in, Some(default), name, None)
    def input[T <: UInt](in: Vec[T], name: String, desc: String): Vec[T] = input[T](in, None, name, Some(desc))
    def input[T <: UInt](in: Vec[T], name: String): Vec[T] = input[T](in: Vec[T], None, name, None)

    def output[T <: UInt](out: T, name: String, desc: Option[String]): T = {
        val x = Output(out)
        outputMap.put(name, ControlOutput(name, x, desc))
        x
    }
    def output[T <: UInt](out: T, name: String): T = output[T](out, name, None)

    def output[T <: UInt](out: Vec[T], name: String, desc: Option[String]): Vec[T] = {
        val x = Output(out)
        x.zipWithIndex.foreach { case (signal, i) => outputMap.put(name + s"_$i", ControlOutput(name + s"_$i", signal, desc)) }
        x
    }
    def output[T <: UInt](out: Vec[T], name: String): Vec[T] = output[T](out, name, None)
}

trait HasControllerConnector {

    val controlIO: Option[ControlIO] = None

}

