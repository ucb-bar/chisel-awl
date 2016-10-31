package hbwif

import Chisel._
import unittest._
import testchipip._
import unittest._
import uncore.tilelink._
import cde._
import scala.util.Random

object HbwifUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new Encoding8b10bTest),
      Module(new HbwifMemTest),
      Module(new HbwifBertTest)
    )
}

class Encoding8b10bTest extends UnitTest {

  val encoder = Module(new Encoder8b10b)
  val decoder = Module(new Decoder8b10b)

  decoder.io.encoded <> encoder.io.encoded

  // make this big enough to do the requisite math
  val disparity = Reg(init = SInt(0, width = 3))
  val ones  = PopCount(encoder.io.encoded).zext
  val zeros = SInt(10) - ones
  disparity := disparity + ones - zeros
  // disparity check
  assert(disparity === SInt(0) || disparity === SInt(2) || disparity === SInt(-2), "Disparity must be within +/- 2")

  val r = new Random(6)
  val vectors = Vec(r.shuffle(0 to 255).map(UInt(_)))
  val syncCount = Reg(init = UInt(0, width=2))
  val decoderCount = Reg(init = UInt(0, width = log2Up(vectors.size+1)))
  val encoderCount = Reg(init = UInt(0, width = log2Up(vectors.size+1)))
  val ready = Reg(init = Bool(false))

  when (syncCount < UInt(3)) {
    syncCount := syncCount + UInt(1)
  } .otherwise {
    ready := Bool(true)
  }

  when (decoderCount < UInt(vectors.size)) {
    io.finished := Bool(false)
  } .otherwise {
    io.finished := Bool(true)
  }

  when (ready) {
    encoderCount := encoderCount + UInt(1)
    encoder.io.decoded.valid := (encoderCount < UInt(vectors.size))
    encoder.io.decoded.control := Bool(false)
    encoder.io.decoded.data := vectors(encoderCount)
  } .otherwise {
    encoder.io.decoded.valid := Bool(false)
    encoder.io.decoded.control := Bool(true)
    encoder.io.decoded.data := UInt(0)
  }

  // check the bits that come out
  when (decoder.io.decoded.valid && ~decoder.io.decoded.control && decoderCount < UInt(vectors.size)) {
    assert(decoder.io.decoded.data === vectors(decoderCount), "Got the wrong data")
    decoderCount := decoderCount + UInt(1)
  }

}

class HbwifMemTest extends UnitTest {

  io.finished := UInt(1)

}

class HbwifBertTest extends UnitTest {

  io.finished := UInt(1)

}


class TestHarness(implicit p: Parameters) extends unittest.TestHarness()(p)
