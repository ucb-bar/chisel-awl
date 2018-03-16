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


object GCD {
    def apply(a: Int, b: Int): Int = if (b == 0) a else GCD(b, a%b)
}

object LCM {
    def apply(a: Int, b: Int): Int = a*b / GCD(a, b)
}
