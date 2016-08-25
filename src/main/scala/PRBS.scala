// johnwright@eecs.berkeley.edu

package hbwif

import Chisel._
import ChiselError._
import math.max

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
    val new_bit = xorR(lfsr_wires(i-1) & UInt(generatorPolynomial))
    lfsr_wires(i) := Cat(new_bit,lfsr_wires(i-1)(prbsWidth-1,1))
  }

  var wire_idx = if (parallelOutBits > prbsWidth) 0 else (prbsWidth - parallelOutBits)
  io.out := lfsr_wires.slice(0,parallelOutBits).map { _(wire_idx) }.reverse.reduce[UInt] { Cat(_,_) }

  // seed_good is high whenever there is a nonzero value in the LFSR
  io.seed_good := orR(lfsr)

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

class PRBSTester(c: PRBS) extends Tester(c) {
  def run_prbs(iterations: Int) {
    for (i <- 0 until iterations) {
      out_val = 0
      for (j <- 0 until c.parallelOutBits) {
        var masked_bits = (lfsr_value & c.generatorPolynomial)
        printf("JCW LFSR VALUE: 0x%4h\n",lfsr_value)
        var new_bit = 0
        for (k <- 0 until c.prbsWidth) {
          new_bit = new_bit ^ (masked_bits & 1)
          masked_bits = masked_bits >> 1
        }
        out_val = out_val >> 1
        out_val |= (lfsr_value & 1) << (c.parallelOutBits-1)
        lfsr_value = lfsr_value >> 1
        lfsr_value |= (new_bit << (c.prbsWidth-1))
      }
      expect(c.io.out,out_val)
      expect(c.io.seed_good,1)
      step(1)
    }
  }
  println("Testing load mode...")
  var seed = 0x01
  var lfsr_value = seed
  var out_val = 0
  poke(c.io.mode, c.LOAD)
  poke(c.io.load_in, seed)
  poke(c.io.seed_in, 0)
  step(1)
  poke(c.io.mode, c.RUN)
  expect(c.io.seed_good, 1)
  run_prbs(200)

  println("Testing seed mode...")
  poke(c.io.load_in, 0)
  poke(c.io.mode, c.LOAD)
  step(1)
  expect(c.io.seed_good, 0)
  seed = 0xaff1 // some arbitrary bits that aren't zero
  if (c.parallelOutBits > c.prbsWidth) {
    lfsr_value = (seed & ((1 << c.parallelOutBits) - 1)) >> (c.parallelOutBits - c.prbsWidth)
  } else {
    lfsr_value = (seed & ((1 << c.parallelOutBits) - 1)) << (c.prbsWidth - c.parallelOutBits)
  }
  poke(c.io.mode, c.SEED)
  poke(c.io.seed_in,seed)
  step(1)
  poke(c.io.mode, c.RUN)
  step(1)
  expect(c.io.seed_good, 1)
  run_prbs(200)
}

object PRBSMain {
  def main(args: Array[String]): Unit = {

    println("PRBS7 1-bit test")
    chiselMainTest(args, () =>
      Module(new PRBS(7,1,0x03))) {
      c => new PRBSTester(c)
    }

    println("PRBS7 4-bit test")
    chiselMainTest(args, () =>
      Module(new PRBS(7,4,0x03))) {
      c => new PRBSTester(c)
    }

    println("PRBS31 1-bit test")
    chiselMainTest(args, () =>
      Module(new PRBS(31,1,0x09))) {
      c => new PRBSTester(c)
    }

    println("PRBS31 16-bit test")
    chiselMainTest(args, () =>
      Module(new PRBS(31,16,0x09))) {
      c => new PRBSTester(c)
    }

    println("PRBS7 16-bit test")
    chiselMainTest(args, () =>
      Module(new PRBS(7,16,0x03))) {
      c => new PRBSTester(c)
    }

  }
}
