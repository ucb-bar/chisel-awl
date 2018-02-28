package hbwif2

import hbwif2._
import chisel3._
import freechips.rocketchip.unittest._

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

class TransceiverTest(timeout: Int = 50000) extends UnitTest(timeout) {

    io.finished := false.B

}
