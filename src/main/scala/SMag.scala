package hbwif2

import chisel3._
import chisel3.experimental._

// TODO probably want more features here

class SMag extends Bundle {

  val s = Bool()
  val m = UInt()

}


object SMag {

  def apply(sint: SInt): SMag = {
    val x = new SMag()
    x.s := sint.head(1).asBool
    x.m := sint.abs
    x
  }

}

