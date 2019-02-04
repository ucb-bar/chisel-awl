package awl.hbwif

import chisel3._
import chisel3.util._
import scala.collection.mutable.HashMap

abstract class ControlClock

case object TxClock extends ControlClock
case object RxClock extends ControlClock
case object OuterClock extends ControlClock

case class ControlInput(
    name: String,
    signal: UInt,
    default: Option[BigInt],
    desc: Option[String],
    clock: ControlClock
) { def width = signal.getWidth }

case class ControlOutput(
    name: String,
    signal: UInt,
    desc: Option[String],
    clock: ControlClock
) { def width = signal.getWidth }

class ControlBundle extends Bundle {
    final val inputMap = new HashMap[String, ControlInput]
    final val outputMap = new HashMap[String, ControlOutput]

    def attach[T <: ControlBundle](other: T) {
        // connect by name
        inputMap.foreach { case (name, i) => i.signal := other.inputMap(name).signal }
        outputMap.foreach { case (name, o) => other.outputMap(name).signal := o.signal }
    }
    def attach[T <: ControlBundle](other: Option[T]) { attach(other.get) }

    def child[T <: ControlBundle](x: T, prefix: String, clockOverride: Option[ControlClock]): T = {
        x.inputMap.values.foreach { input =>
            addInput(input.signal, input.default, prefix + "_" + input.name, input.desc, clockOverride.getOrElse(input.clock))
        }
        x.outputMap.values.foreach { output =>
            addOutput(output.signal, prefix + "_" + output.name, output.desc, clockOverride.getOrElse(output.clock))
        }
        x
    }
    def child[T <: ControlBundle](x: T, prefix: String, clockOverride: ControlClock): T = child(x, prefix, Some(clockOverride))
    def child[T <: ControlBundle](x: T, prefix: String): T = child(x, prefix, None)

    def child[T <: ControlBundle](x: Vec[T], prefix: String, clockOverride: Option[ControlClock] = None): Vec[T] = {
        x.zipWithIndex.foreach { case (c, i) =>
            c.inputMap.values.foreach { input =>
                addInput(input.signal, input.default, prefix + s"_${i}_" + input.name, input.desc, clockOverride.getOrElse(input.clock))
            }
            c.outputMap.values.foreach { output =>
                addOutput(output.signal, prefix + s"_${i}_" + output.name, output.desc, clockOverride.getOrElse(output.clock))
            }
        }
        x
    }
    def child[T <: ControlBundle](x: Vec[T], prefix: String, clockOverride: ControlClock): Vec[T] = child(x, prefix, Some(clockOverride))
    def child[T <: ControlBundle](x: Vec[T], prefix: String): Vec[T] = child(x, prefix, None)

    def addInput[T <: UInt](in: T, default: Option[BigInt], name: String, desc: Option[String], clock: ControlClock) {
        require(!inputMap.contains(name), s"Duplicate input control added: ${name}")
        require(!outputMap.contains(name), s"Input and output control added with same name: ${name}")
        inputMap.put(name, ControlInput(name, in, default, desc, clock))
    }


    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: Option[String], clock: ControlClock): T = {
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

    def input[T <: UInt](in: T, default: Option[BigInt], name: String, clock: ControlClock): T = input[T](in, default, name, None, clock)
    def input[T <: UInt](in: T, default: Option[BigInt], name: String, desc: String, clock: ControlClock): T = input[T](in, default, name, Some(desc), clock)
    def input[T <: UInt](in: T, default: BigInt, name: String, desc: String, clock: ControlClock): T = input[T](in, Some(default), name, Some(desc), clock)
    def input[T <: UInt](in: T, default: BigInt, name: String, clock: ControlClock): T = input[T](in, Some(default), name, None, clock)
    def input[T <: UInt](in: T, name: String, desc: String, clock: ControlClock): T = input[T](in, None, name, Some(desc), clock)
    def input[T <: UInt](in: T, name: String, clock: ControlClock): T = input[T](in, None, name, None, clock)

    def addInput[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: Option[String], clock: ControlClock) {
        in.zipWithIndex.foreach { case (signal, i) => addInput(signal, default.map(_(i)), name + s"_$i", desc, clock) }
    }

    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: Option[String], clock: ControlClock): Vec[T] = {
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

    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, clock: ControlClock): Vec[T] = input[T](in, default, name, None, clock)
    def input[T <: UInt](in: Vec[T], default: Option[Seq[BigInt]], name: String, desc: String, clock: ControlClock): Vec[T] = input[T](in, default, name, Some(desc), clock)
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String, desc: String, clock: ControlClock): Vec[T] = input[T](in, Some(default), name, Some(desc), clock)
    def input[T <: UInt](in: Vec[T], default: Seq[BigInt], name: String, clock: ControlClock): Vec[T] = input[T](in, Some(default), name, None, clock)
    def input[T <: UInt](in: Vec[T], name: String, desc: String, clock: ControlClock): Vec[T] = input[T](in, None, name, Some(desc), clock)
    def input[T <: UInt](in: Vec[T], name: String, clock: ControlClock): Vec[T] = input[T](in: Vec[T], None, name, None, clock)

    def addOutput[T <: UInt](out: T, name: String, desc: Option[String], clock: ControlClock) {
        require(!outputMap.contains(name), s"Duplicate output control added: ${name}")
        require(!inputMap.contains(name), s"Input and output control added with same name: ${name}")
        outputMap.put(name, ControlOutput(name, out, desc, clock))
    }

    def output[T <: UInt](out: T, name: String, desc: Option[String], clock: ControlClock): T = {
        val x = Output(out)
        addOutput(x, name, desc, clock)
        x
    }
    def output[T <: UInt](out: T, name: String): T = output[T](out, name, None, OuterClock)
    def output[T <: UInt](out: T, name: String, clock: ControlClock): T = output[T](out, name, None, clock)
    def output[T <: UInt](out: T, name: String, desc: String): T = output[T](out, name, Some(desc), OuterClock)
    def output[T <: UInt](out: T, name: String, desc: String, clock: ControlClock): T = output[T](out, name, Some(desc), clock)

    def addOutput[T <: UInt](out: Vec[T], name: String, desc: Option[String], clock: ControlClock) {
        out.zipWithIndex.foreach { case (signal, i) => addOutput(signal, name + s"_$i", desc, clock) }
    }

    def output[T <: UInt](out: Vec[T], name: String, desc: Option[String], clock: ControlClock): Vec[T] = {
        val x = Output(out)
        addOutput(x, name, desc, clock)
        x
    }
    def output[T <: UInt](out: Vec[T], name: String): Vec[T] = output[T](out, name, None, OuterClock)
    def output[T <: UInt](out: Vec[T], name: String, clock: ControlClock): Vec[T] = output[T](out, name, None, clock)
    def output[T <: UInt](out: Vec[T], name: String, desc: String): Vec[T] = output[T](out, name, Some(desc), OuterClock)
    def output[T <: UInt](out: Vec[T], name: String, desc: String, clock: ControlClock): Vec[T] = output[T](out, name, Some(desc), clock)
}

trait HasControllerConnector {

    val controlIO: Option[ControlBundle] = None

}

