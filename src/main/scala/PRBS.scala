package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle
import scala.collection.mutable.ArrayBuffer


class PRBS(
  val prbsWidth: Int = 31,
  val parallelOutBits: Int = 16,
  val generatorPolynomial: Int = 0x09
) extends Module {
  val io = new Bundle {
    val mode           = Bits(INPUT, width = 2)
    val load_in        = Bits(INPUT, width = prbsWidth)
    val seed_in        = Bits(INPUT, width = parallelOutBits)
    val seed_good      = Bool(OUTPUT)
    val out            = Bits(OUTPUT, width = parallelOutBits)
  }
  // modes
  val LOAD = 0
  val SEED = 1
  val RUN  = 2
  val STOP = 3

  // implement PRBS with an LFSR
  val lfsr = Reg(init = UInt(1, width = prbsWidth))
  val lfsr_wires = Vec.fill(parallelOutBits+1) { UInt(width = prbsWidth) }

  lfsr_wires(0) := lfsr

  for (i <- 1 until parallelOutBits+1) {
    val new_bit = (lfsr_wires(i-1) & UInt(generatorPolynomial)).xorR
    lfsr_wires(i) := Cat(new_bit,lfsr_wires(i-1)(prbsWidth-1,1))
  }

  var wire_idx = if (parallelOutBits > prbsWidth) 0 else (prbsWidth - parallelOutBits)
  io.out := lfsr_wires.slice(0,parallelOutBits).map { _(wire_idx) }.reverse.reduce[UInt] { Cat(_,_) }

  // seed_good is high whenever there is a nonzero value in the LFSR
  io.seed_good := lfsr.orR

  switch(io.mode) {
    is(UInt(LOAD)) {
      // parallel load the state
      lfsr := io.load_in
    }
    is(UInt(SEED)) {
      if (parallelOutBits > prbsWidth) {
        lfsr := io.seed_in(prbsWidth-1,0)
      } else {
        lfsr := Cat(io.seed_in,lfsr(prbsWidth-1,parallelOutBits))
      }
    }
    is(UInt(RUN)) {
      lfsr := lfsr_wires(parallelOutBits)
    }
  }
}
