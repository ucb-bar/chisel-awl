package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle
import scala.collection.mutable.ArrayBuffer
import testchipip.HeaderEnum


class PRBS(
  val prbsWidth: Int = 31,
  val parallelOutBits: Int = 16,
  val generatorPolynomial: Int = 0x09
) extends Module {
  val io = new Bundle {
    val mode           = Bits(INPUT, width = 2)
    val loadIn        = Bits(INPUT, width = prbsWidth)
    val seedIn        = Bits(INPUT, width = parallelOutBits)
    val seedGood      = Bool(OUTPUT)
    val out            = Bits(OUTPUT, width = parallelOutBits)
  }
  // modes
  val prbsModes = HeaderEnum("prbs_mode", "load", "seed", "run", "stop")

  // implement PRBS with an LFSR
  val lfsr = Reg(init = UInt(1, width = prbsWidth))
  val lfsrWires = Wire(Vec(parallelOutBits+1, UInt(width = prbsWidth)))

  lfsrWires(0) := lfsr

  for (i <- 1 until parallelOutBits+1) {
    val newBit = (lfsrWires(i-1) & UInt(generatorPolynomial)).xorR
    lfsrWires(i) := Cat(newBit,lfsrWires(i-1)(prbsWidth-1,1))
  }

  var wireIdx = if (parallelOutBits > prbsWidth) 0 else (prbsWidth - parallelOutBits)
  io.out := lfsrWires.slice(0,parallelOutBits).map { _(wireIdx) }.reverse.reduce[UInt] { Cat(_,_) }

  // seedGood is high whenever there is a nonzero value in the LFSR
  io.seedGood := lfsr.orR

  switch(io.mode) {
    is(prbsModes("load")) {
      // parallel load the state
      lfsr := io.loadIn
    }
    is(prbsModes("seed")) {
      if (parallelOutBits > prbsWidth) {
        lfsr := io.seedIn(prbsWidth-1,0)
      } else {
        lfsr := Cat(io.seedIn,lfsr(prbsWidth-1,parallelOutBits))
      }
    }
    is(prbsModes("run")) {
      lfsr := lfsrWires(parallelOutBits)
    }
  }
}
