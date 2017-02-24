package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle
import scala.collection.mutable.ArrayBuffer
import testchipip.HeaderEnum


class PRBS(
  val prbsWidth: Int = 31,
  val parallelOutBits: Int = 16,
  val generatorPolynomial: Int = 0x48000000
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
  val lfsrWires = Wire(Vec(parallelOutBits+prbsWidth+1, UInt(width = prbsWidth)))

  lfsrWires(0) := lfsr

  for (i <- 1 until parallelOutBits+prbsWidth+1) {
    val newBit = (lfsrWires(i-1) & UInt(generatorPolynomial)).xorR
    lfsrWires(i) := Cat(lfsrWires(i-1)(prbsWidth-2,0),newBit)
  }

  io.out := lfsrWires.slice(prbsWidth,prbsWidth+parallelOutBits).map { _(prbsWidth-1) }.reduce[UInt] { Cat(_,_) }

  // seedGood is high whenever there is a nonzero value in the LFSR
  io.seedGood := lfsr.orR

  switch(io.mode) {
    is(prbsModes("load")) {
      // parallel load the state
      lfsr := io.loadIn
    }
    is(prbsModes("seed")) {
      if (parallelOutBits > prbsWidth) {
        lfsr := io.seedIn(parallelOutBits-1,parallelOutBits-prbsWidth)
      } else {
        lfsr := Cat(lfsr(prbsWidth-parallelOutBits-1,0),io.seedIn)
      }
    }
    is(prbsModes("run")) {
      lfsr := lfsrWires(parallelOutBits)
    }
  }
}

object PRBS7 {
  def apply(bits: Int = 16): PRBS = {
    return Module(new PRBS(7, bits, 0x60))
  }
}

object PRBS15 {
  def apply(bits: Int = 16): PRBS = {
    return Module(new PRBS(15, bits, 0x6000))
  }
}

object PRBS31 {
  def apply(bits: Int = 16): PRBS = {
    return Module(new PRBS(31, bits, 0x48000000))
  }
}
