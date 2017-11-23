
package hbwif2

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester

class TransceiverNearFarIF()(implicit val c: SerDesGeneratorConfig) extends Bundle {

  val overrides = new TransceiverOverrideIF
  val data = new TransceiverDataIF

}


class TransceiverDUTIO()(implicit val c: SerDesGeneratorConfig) extends Bundle {

  val near = new TransceiverNearFarIF
  val far = new TransceiverNearFarIF

}

class TransceiverDUT()(implicit c: SerDesGeneratorConfig) extends Module {

  val io = IO(new TransceiverDUTIO)

  val near = Module(new TransceiverSubsystem)
  val far  = Module(new TransceiverSubsystem)

  val started = RegInit(false.B)

  near.io.overrides <> io.near.overrides
  far.io.overrides <> io.far.overrides

  near.io.clock_ref := clock
  far.io.clock_ref  := clock

  near.io.async_reset_in := reset
  far.io.async_reset_in  := reset

  near.io.rx.p <> far.io.tx.p
  near.io.rx.n <> far.io.tx.n
  near.io.tx.p <> far.io.rx.p
  near.io.tx.n <> far.io.rx.n

}

class TransceiverUnitTester(dut: TransceiverDUT)  extends PeekPokeTester(dut) {

  poke(dut.io.near.overrides.cdr, false)
  poke(dut.io.far.overrides.cdr, false)

  poke(dut.io.near.overrides.dfe, false)
  poke(dut.io.far.overrides.dfe, false)

  poke(dut.io.near.overrides.dlev, false)
  poke(dut.io.far.overrides.dlev, false)
  step(10)

}
