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

class ControlBundle extends Bundle {
    final val inputMap = new HashMap[String, ControlInput]
    final val outputMap = new HashMap[String, ControlOutput]

    def attach[T <: ControlBundle](other: T) {
        // connect by name
        inputMap.foreach { case (name, i) => i.signal := other.inputMap(name).signal }
        outputMap.foreach { case (name, o) => other.outputMap(name).signal := o.signal }
    }
    def attach[T <: ControlBundle](other: Option[T]) { attach(other.get) }

    def child[T <: ControlBundle](x: T, prefix: String): T = {
        x.inputMap.values.foreach { input =>
            addInput(input.signal, input.default, prefix + "_" + input.name, input.desc)
        }
        x.outputMap.values.foreach { output =>
            addOutput(output.signal, prefix + "_" + output.name, output.desc)
        }
        x
    }

    def child[T <: ControlBundle](x: Vec[T], prefix: String): Vec[T] = {
        x.zipWithIndex.foreach { case (c, i) =>
            c.inputMap.values.foreach { input =>
                addInput(input.signal, input.default, prefix + s"_${i}_" + input.name, input.desc)
            }
            c.outputMap.values.foreach { output =>
                addOutput(output.signal, prefix + s"_${i}_" + output.name, output.desc)
            }
        }
        x
    }

    def addInput[T <: UInt](in: T, default: Option[BigInt], name: String, desc: Option[String]) {
        inputMap.put(name, ControlInput(name, in, default, desc))
    }


    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: Option[String]): T = {
        val x = Input(in)
        addInput(x, default, name, desc)
        x
    }
    def input[T <: UInt](in: T, default: Option[BigInt], name: String): T = input[T](in, default, name, None)
    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: String): T = input[T](in, default, name, Some(desc))
    def input[T <: UInt](in: T, default: BigInt, name: String, desc: String): T = input[T](in, Some(default), name, Some(desc))
    def input[T <: UInt](in: T, default: BigInt, name: String): T = input[T](in, Some(default), name, None)
    def input[T <: UInt](in: T, name: String, desc: String): T = input[T](in, None, name, Some(desc))
    def input[T <: UInt](in: T, name: String): T = input[T](in, None, name, None)

    def addInput[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: Option[String]) {
        in.zipWithIndex.foreach { case (signal, i) => inputMap.put(name + s"_$i", ControlInput(name + s"_$i", signal, default.map(_(i)), desc)) }
    }

    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: Option[String]): Vec[T] = {
        val x = Input(in)
        addInput(x, default, name, desc)
        x
    }
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String): Vec[T] = input[T](in, default, name, None)
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: String): Vec[T] = input[T](in, default, name, Some(desc))
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String, desc: String): Vec[T] = input[T](in, Some(default), name, Some(desc))
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String): Vec[T] = input[T](in, Some(default), name, None)
    def input[T <: UInt](in: Vec[T], name: String, desc: String): Vec[T] = input[T](in, None, name, Some(desc))
    def input[T <: UInt](in: Vec[T], name: String): Vec[T] = input[T](in: Vec[T], None, name, None)

    def addOutput[T <: UInt](out: T, name: String, desc: Option[String]) {
        outputMap.put(name, ControlOutput(name, out, desc))
    }

    def output[T <: UInt](out: T, name: String, desc: Option[String]): T = {
        val x = Output(out)
        addOutput(x, name, desc)
        x
    }
    def output[T <: UInt](out: T, name: String): T = output[T](out, name, None)

    def addOutput[T <: UInt](out: Vec[T], name: String, desc: Option[String]) {
        out.zipWithIndex.foreach { case (signal, i) => outputMap.put(name + s"_$i", ControlOutput(name + s"_$i", signal, desc)) }
    }

    def output[T <: UInt](out: Vec[T], name: String, desc: Option[String]): Vec[T] = {
        val x = Output(out)
        addOutput(x, name, desc)
        x
    }
    def output[T <: UInt](out: Vec[T], name: String): Vec[T] = output[T](out, name, None)
}

trait HasControllerConnector {

    val controlIO: Option[ControlBundle] = None

}

