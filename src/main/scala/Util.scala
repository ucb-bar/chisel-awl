package hbwif2

import chisel3._
import chisel3.util._

object Ready {
    def apply[T <: Data](gen: T): Ready[T] = new Ready(gen)
}

class Ready[+T <: Data](gen: T) extends Bundle {
    val ready = Input(Bool())
    val bits = Output(gen)
    def fire: Bool = ready
}
