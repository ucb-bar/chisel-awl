
package hbwif2

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester
import scala.util.Random

class TransceiverNearFarIF()(implicit val c: SerDesGeneratorConfig) extends Bundle {

  val overrides = new TransceiverOverrideIF
  val data = new TransceiverDataIF

}


class TransceiverPairIO()(implicit val c: SerDesGeneratorConfig) extends Bundle {

  val near = new TransceiverNearFarIF
  val far = new TransceiverNearFarIF

}

class TransceiverPair()(implicit val c: SerDesGeneratorConfig) extends Module {

  val io = IO(new TransceiverPairIO)

  val near = Module(new TransceiverSubsystem)
  val far  = Module(new TransceiverSubsystem)

  near.io.overrides <> io.near.overrides
  far.io.overrides <> io.far.overrides

  near.io.data <> io.near.data
  far.io.data <> io.far.data

  near.io.clock_ref := clock
  far.io.clock_ref  := clock

  near.io.async_reset_in := reset
  far.io.async_reset_in  := reset

  near.io.rx.p <> far.io.tx.p
  near.io.rx.n <> far.io.tx.n
  near.io.tx.p <> far.io.rx.p
  near.io.tx.n <> far.io.rx.n

}

class TransceiverUnitTester(dut: TransceiverPair)  extends PeekPokeTester(dut) {

  def bigStep(n: Int) = { step(n * dut.c.dataWidth / 2) }

  val r = Random
  val x = 20
  val data = for (i <- 0 until x) yield r.nextInt % (1 << dut.c.dataWidth)

  poke(dut.io.near.overrides.cdr, false)
  poke(dut.io.far.overrides.cdr, false)

  poke(dut.io.near.overrides.dfe, false)
  poke(dut.io.far.overrides.dfe, false)

  poke(dut.io.near.overrides.dlev, false)
  poke(dut.io.far.overrides.dlev, false)

  bigStep(5)

  var far_idx = -1
  var near_idx = -1

  for (i <- 0 until x) {
    poke(dut.io.near.data.tx, data(i))
    if (peek(dut.io.far.data.rx) == data(0) && far_idx == -1) far_idx = 0
    if (peek(dut.io.near.data.rx) == data(0) && near_idx == -1) near_idx = 0
    if (far_idx >= 0) {
      expect(dut.io.far.data.rx, data(far_idx))
      poke(dut.io.far.data.tx, peek(dut.io.far.data.rx))
      far_idx += 1
    }
    if (near_idx >= 0) {
      expect(dut.io.near.data.rx, data(near_idx))
      near_idx += 1
    }
    bigStep(1)
  }

  //TODO FIXME

}
