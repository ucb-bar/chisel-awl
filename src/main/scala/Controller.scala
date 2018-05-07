package hbwif

import chisel3._
import chisel3.util._
import scala.collection.mutable.HashMap

abstract class ControllerClock

case object TxClock extends ControllerClock
case object RxClock extends ControllerClock
case object OuterClock extends ControllerClock

case class ControlInput(
    name: String,
    signal: UInt,
    default: Option[BigInt],
    desc: Option[String],
    clock: ControllerClock
)

case class ControlOutput(
    name: String,
    signal: UInt,
    desc: Option[String],
    clock: ControllerClock
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
            addInput(input.signal, input.default, prefix + "_" + input.name, input.desc, input.clock)
        }
        x.outputMap.values.foreach { output =>
            addOutput(output.signal, prefix + "_" + output.name, output.desc, output.clock)
        }
        x
    }

    def child[T <: ControlBundle](x: Vec[T], prefix: String): Vec[T] = {
        x.zipWithIndex.foreach { case (c, i) =>
            c.inputMap.values.foreach { input =>
                addInput(input.signal, input.default, prefix + s"_${i}_" + input.name, input.desc, input.clock)
            }
            c.outputMap.values.foreach { output =>
                addOutput(output.signal, prefix + s"_${i}_" + output.name, output.desc, output.clock)
            }
        }
        x
    }

    def addInput[T <: UInt](in: T, default: Option[BigInt], name: String, desc: Option[String], clock: ControllerClock) {
        inputMap.put(name, ControlInput(name, in, default, desc, clock))
    }


    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: Option[String], clock: ControllerClock): T = {
        val x = Input(in)
        addInput(x, default, name, desc, clock)
        x
    }
    def input[T <: UInt](in: T, default: Option[BigInt], name: String): T = input[T](in, default, name, None, OuterClock)
    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: String): T = input[T](in, default, name, Some(desc), OuterClock)
    def input[T <: UInt](in: T, default: BigInt, name: String, desc: String): T = input[T](in, Some(default), name, Some(desc), OuterClock)
    def input[T <: UInt](in: T, default: BigInt, name: String): T = input[T](in, Some(default), name, None, OuterClock)
    def input[T <: UInt](in: T, name: String, desc: String): T = input[T](in, None, name, Some(desc), OuterClock)
    def input[T <: UInt](in: T, name: String): T = input[T](in, None, name, None, OuterClock)

    def input[T <: UInt](in: T, default: Option[BigInt], name: String, clock: ControllerClock): T = input[T](in, default, name, None, clock)
    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: String, clock: ControllerClock): T = input[T](in, default, name, Some(desc), clock)
    def input[T <: UInt](in: T, default: BigInt, name: String, desc: String, clock: ControllerClock): T = input[T](in, Some(default), name, Some(desc), clock)
    def input[T <: UInt](in: T, default: BigInt, name: String, clock: ControllerClock): T = input[T](in, Some(default), name, None, clock)
    def input[T <: UInt](in: T, name: String, desc: String, clock: ControllerClock): T = input[T](in, None, name, Some(desc), clock)
    def input[T <: UInt](in: T, name: String, clock: ControllerClock): T = input[T](in, None, name, None, clock)

    def addInput[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: Option[String], clock: ControllerClock) {
        in.zipWithIndex.foreach { case (signal, i) => inputMap.put(name + s"_$i", ControlInput(name + s"_$i", signal, default.map(_(i)), desc, clock)) }
    }

    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: Option[String], clock: ControllerClock): Vec[T] = {
        val x = Input(in)
        addInput(x, default, name, desc, clock)
        x
    }
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String): Vec[T] = input[T](in, default, name, None, OuterClock)
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: String): Vec[T] = input[T](in, default, name, Some(desc), OuterClock)
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String, desc: String): Vec[T] = input[T](in, Some(default), name, Some(desc), OuterClock)
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String): Vec[T] = input[T](in, Some(default), name, None, OuterClock)
    def input[T <: UInt](in: Vec[T], name: String, desc: String): Vec[T] = input[T](in, None, name, Some(desc), OuterClock)
    def input[T <: UInt](in: Vec[T], name: String): Vec[T] = input[T](in: Vec[T], None, name, None, OuterClock)

    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, clock: ControllerClock): Vec[T] = input[T](in, default, name, None, clock)
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: String, clock: ControllerClock): Vec[T] = input[T](in, default, name, Some(desc), clock)
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String, desc: String, clock: ControllerClock): Vec[T] = input[T](in, Some(default), name, Some(desc), clock)
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String, clock: ControllerClock): Vec[T] = input[T](in, Some(default), name, None, clock)
    def input[T <: UInt](in: Vec[T], name: String, desc: String, clock: ControllerClock): Vec[T] = input[T](in, None, name, Some(desc), clock)
    def input[T <: UInt](in: Vec[T], name: String, clock: ControllerClock): Vec[T] = input[T](in: Vec[T], None, name, None, clock)

    def addOutput[T <: UInt](out: T, name: String, desc: Option[String], clock: ControllerClock) {
        outputMap.put(name, ControlOutput(name, out, desc, clock))
    }

    def output[T <: UInt](out: T, name: String, desc: Option[String], clock: ControllerClock): T = {
        val x = Output(out)
        addOutput(x, name, desc, clock)
        x
    }
    def output[T <: UInt](out: T, name: String): T = output[T](out, name, None, OuterClock)
    def output[T <: UInt](out: T, name: String, clock: ControllerClock): T = output[T](out, name, None, clock)

    def addOutput[T <: UInt](out: Vec[T], name: String, desc: Option[String], clock: ControllerClock) {
        out.zipWithIndex.foreach { case (signal, i) => outputMap.put(name + s"_$i", ControlOutput(name + s"_$i", signal, desc, clock)) }
    }

    def output[T <: UInt](out: Vec[T], name: String, desc: Option[String], clock: ControllerClock): Vec[T] = {
        val x = Output(out)
        addOutput(x, name, desc, clock)
        x
    }
    def output[T <: UInt](out: Vec[T], name: String): Vec[T] = output[T](out, name, None, OuterClock)
    def output[T <: UInt](out: Vec[T], name: String, clock: ControllerClock): Vec[T] = output[T](out, name, None, clock)
}

trait HasControllerConnector {

    val controlIO: Option[ControlBundle] = None

}

