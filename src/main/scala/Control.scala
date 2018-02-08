package hbwif2

import chisel3._
import chisel3.util._

abstract class Control[T <: Bundle](val portFactory: () => T) extends Module {

    val numStatus: Int
    val numControl: Int

    val io = IO(new Bundle {
        val status = Input(TODO)
        val control = Output(TODO)
        val port = portFactory()
    })


}

class ControlBuilder[T <: Control](val controlFactory: () => T) {

    private val controlMap = new HashMap[String,UInt]
    private val statusMap = new HashMap[String,UInt]

    def control(name: String, signal: UInt, init: UInt = null) = {
        if(init != null) require(init.isLit)
        controlMap += (name, signal)
        controlMap(name)
    }

    def status(name: String, signal: UInt) = {
        statusMap += (name, signal)
        statusMap(name)
    }

    def generate(): T = {
        Module(controlFactory(controlMap,statusMap))
        // Connect nets here
    }

}
