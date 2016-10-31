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
      Module(new EncodingDataTest),
      Module(new EncodingAlignmentTest),
      Module(new EncodingErrorTest),
      Module(new HbwifMemTest),
      Module(new HbwifBertTest)
    )
}

class DisparityChecker extends Module {
  val io = new Bundle {
    val data = UInt(INPUT, width = 10)
  }

  // make this big enough to do the requisite math
  val disparity = Reg(init = SInt(0, width = 3))
  val ones  = PopCount(io.data).zext
  val zeros = SInt(10) - ones
  disparity := disparity + ones - zeros

  assert(disparity === SInt(0) || disparity === SInt(2) || disparity === SInt(-2), "Disparity must be within +/- 2")
}

class EncodingDataTest extends UnitTest {

  val encoder = Module(new Encoder8b10b)
  val decoder = Module(new Decoder8b10b)

  decoder.io.encoded <> encoder.io.encoded

  Module(new DisparityChecker).io.data := encoder.io.encoded

  // randomize every single number and insert an arbitrary number to flip the disparity so we cover everything
  // we pick 3 since it is guaranteed to flip the disparity
  // we also pick an arbitrary seed of 6 so that the tests are random but repeatable
  val v = (new Random(6)).shuffle(0 to 255)
  val vectors = Vec((v ++ List(3) ++ v).map(UInt(_)))
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

class EncodingAlignmentTest extends UnitTest {

  io.finished := UInt(1)

}

class EncodingErrorTest extends UnitTest {

  io.finished := UInt(1)

}

class HbwifMemTest extends UnitTest {

  io.finished := UInt(1)

}

class HbwifBertTest extends UnitTest {

  io.finished := UInt(1)

}


class TestHarness(implicit p: Parameters) extends unittest.TestHarness()(p)
