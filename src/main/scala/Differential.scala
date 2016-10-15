package hbwif

import Chisel._

class Differential extends Bundle {

  val p = Bool(OUTPUT)
  val n = Bool(OUTPUT)

}
