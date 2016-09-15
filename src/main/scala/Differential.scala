package hbwif

import Chisel._

class Differential extends Bundle {

  val p = Bool(INPUT)
  val n = Bool(INPUT)

}
