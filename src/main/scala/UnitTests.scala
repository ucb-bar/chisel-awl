package hbwif

import Chisel._
import unittest._
import testchipip._
import unittest._
import uncore.tilelink._
import uncore.util._
import uncore.devices._
import diplomacy.{LazyModule,LazyModuleImp}
import cde._
import scala.util.Random
import rocketchip._
import junctions._
import util.ParameterizedBundle

object HbwifUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new EncodingDataTest),
      Module(new EncodingAlignmentTest),
      Module(new EncodingErrorTest),
      Module(new HbwifMemTest),
      Module(new HbwifMemBERTest),
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

object DisparityCheck {

  def apply(d: UInt) = Module(new DisparityChecker).io.data := d

}

class EncodingDataTest extends UnitTest {

  val encoder = Module(new Encoder8b10b)
  val decoder = Module(new Decoder8b10b)

  decoder.io.encoded <> encoder.io.encoded

  DisparityCheck(encoder.io.encoded)

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
  when (decoder.io.decoded.isData() && decoderCount < UInt(vectors.size)) {
    assert(decoder.io.decoded.data === vectors(decoderCount), "Got the wrong data")
    decoderCount := decoderCount + UInt(1)
  }

}

class EncodingAlignmentTest extends UnitTest {

  val encoder = Module(new Encoder8b10b)

  val buf = Reg(next = encoder.io.encoded)
  val cat = Cat(buf, encoder.io.encoded)

  DisparityCheck(encoder.io.encoded)

  // pick some arbitrary data to send
  val data = 23

  // add 10 decoders which are spaced a bit time apart and ensure we can align to all of them
  io.finished := (0 until 10).map { x =>
    val m = Module(new Decoder8b10b)
    m.io.encoded := cat(x+10,x)
    val done = Reg(init = Bool(false))
    when (m.io.decoded.isData()) {
      assert(m.io.decoded.data === UInt(data), s"Data must be $data")
      done := Bool(true)
    }
    done
  }.reduce(_&_)

  val syncCount = Reg(init = UInt(0, width=2))
  val ready = Reg(init = Bool(false))

  when (syncCount < UInt(3)) {
    syncCount := syncCount + UInt(1)
  } .otherwise {
    ready := Bool(true)
  }

  when (ready) {
    encoder.io.decoded.valid := Bool(true)
    encoder.io.decoded.control := Bool(false)
    encoder.io.decoded.data := UInt(data)
  } .otherwise {
    encoder.io.decoded.valid := Bool(false)
    encoder.io.decoded.control := Bool(true)
    encoder.io.decoded.data := UInt(0)
  }


}

class EncodingErrorTest extends UnitTest {

  io.finished := UInt(1)

}

object HbwifSCRUtil extends HasSCRParameters {

  def writeLane(l: Int, s: String, d: Int)(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] = {
    val addr = SCRAddressMap(s"hbwif_lane$l").get(s)
    Seq((addr, d))
  }

  def writeAll(s: String, d: Int)(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] = {
    (0 until p(HbwifKey).numLanes) map {i => writeLane(i, s, d)} reduce (_ ++ _)
  }

  def writeAll(s: Seq[Tuple2[String,Int]])(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] = {
    s map { case x => writeAll(x._1,x._2) } reduce {_ ++ _}
  }

  def bertMode()(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] = writeAll("bert_enable", 1)
  def hbwifMode()(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] = writeAll("bert_enable", 0)
  def retransmitMode()(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] = writeAll("retransmit_cycles", 250) ++ writeAll("retransmit_enable", 1)

  def bertInit()(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] =
    writeAll("bert_enable", 1) ++
    writeAll("bert_tx_prbs_load_data", 1) ++
    writeAll("bert_tx_prbs_mode", 0) ++
    writeAll("bert_tx_prbs_mode", 2) ++
    writeAll("bert_rx_prbs_mode", 1) ++
    writeAll("bert_rx_prbs_mode", 2) ++
    writeAll("bert_clear", 1) ++
    writeAll("bert_ber_mode", 1) ++
    writeAll("bert_clear", 0)

  // This is actually for gets, not puts, but writeAll also works
  def bertErrorCheck(numErrors: Int)(implicit p: Parameters): Seq[Tuple2[BigInt,Int]] =
    (0 until p(BertKey).numWays).map { i => writeAll(s"bert_error_count_$i", numErrors) } reduce {_ ++ _}

}

class HbwifUnitTestBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HbwifBundle {
  val mem = Vec(hbwifNumLanes, new ClientUncachedTileLinkIO()(p.alterPartial({ case TLId => "Switcher" }))).flip
  val fastClk = Clock(INPUT)
  val scr = new ClientUncachedTileLinkIO()(p.alterPartial({ case TLId => "MMIOtoSCR" })).flip
  val reset = Bool(INPUT)
}

class HbwifUnitTest(p: Parameters) extends LazyModule {

  override lazy val module = Module(new HbwifUnitTestModule(p, this))

}

trait HbwifUnitTestSCRModule extends HasHbwifParameters {

  val scrParams = p.alterPartial({
    case TLId => "MMIOtoSCR"
  })

  val scrBus: TileLinkRecursiveInterconnect = Module(new TileLinkRecursiveInterconnect(1, p(GlobalAddrMap).subMap("io:pbus:scrbus"))(scrParams))
  val io = new HbwifUnitTestBundle()(p)
  val hbwifIO: Vec[ClientUncachedTileLinkIO] = io.mem

  scrBus.io.in(0) := io.scr
}


trait HbwifUnitTestWireModule extends HasHbwifParameters {

  val io: HbwifUnitTestBundle

  val hbwifReset: Bool
  val hbwifResetOverride: Bool
  val hbwifFastClock: Clock

  hbwifResetOverride := Bool(false)
  hbwifReset := io.reset
  hbwifFastClock := io.fastClk

}

class HbwifUnitTestModule[+L <: HbwifUnitTest]
  (val p: Parameters, l: L) extends LazyModuleImp(l)
  with HbwifUnitTestSCRModule
  with HbwifModule
  with HbwifUnitTestWireModule

trait HasHbwifTestModule extends HasTransceiverParameters with HasUnitTestIO {
  implicit val p: Parameters
  val clock: Clock
  val reset: Bool
  val txError = Wire(Bool())
  val rxError = Wire(Bool())

  val q = p.alterPartial({
    case GlobalAddrMap => AddrMap(AddrMapEntry("io",
      AddrMap(AddrMapEntry("pbus",
        AddrMap(AddrMapEntry("scrbus",
          AddrMap(AddrMapEntry("hbwif_lane0", MemSize(4096, MemAttr(AddrMapProt.RW))))
        ))
      ))
    ))
  })

  val hbwif = LazyModule(new HbwifUnitTest(q)).module
  val fiwbh = Module(new Fiwbh()(q))

  hbwif.io.reset := reset

  hbwif.io.fastClk := clock
  fiwbh.io.fastClk := clock

  hbwif.io.hbwifRx.zip(fiwbh.io.tx).foreach { case (h,f) =>
    h.p := f.p ^ rxError
    h.n := f.n ^ rxError
  }
  hbwif.io.hbwifTx.zip(fiwbh.io.rx).foreach { case (h,f) =>
    f.p := h.p ^ txError
    f.n := h.n ^ txError
  }

  val memIn = hbwif.io.mem(0)
  val memOut = fiwbh.io.mem(0)

  val analogTestHarness = Module(new AnalogTestHarness()(p))
  if(transceiverHasIref && transceiverRefGenHasInput) hbwif.io.hbwifIref.get := analogTestHarness.io.hbwifIref.get
  if(transceiverHasVcm) hbwif.io.hbwifVcm.get := analogTestHarness.io.hbwifVcm.get

}

class HbwifMemTest(scrConfigs: Seq[Tuple2[String,Int]] = Seq())(implicit val p: Parameters) extends UnitTest(100000)
  with HasHbwifTestModule with HasTileLinkParameters {

  fiwbh.io.loopback := Bool(false)

  val scrDriver = Module(new PutSeqDriver(HbwifSCRUtil.writeAll(scrConfigs) ++ HbwifSCRUtil.hbwifMode()))

  scrDriver.io.start := io.start
  hbwif.io.scr <> scrDriver.io.mem

  rxError := Bool(false)
  txError := Bool(false)

  val depth = 2 * tlDataBeats
  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := scrDriver.io.finished
  io.finished := driver.io.finished

  memIn <> driver.io.mem
  val testram = Module(new TileLinkTestRAM(depth))
  testram.io <> memOut

}

class HbwifMemBERTest(scrConfigs: Seq[Tuple2[String,Int]] = Seq())(implicit val p: Parameters) extends UnitTest(300000)
  with HasHbwifTestModule with HasTileLinkParameters {

  fiwbh.io.loopback := Bool(false)

  val scrDriver = Module(new PutSeqDriver(HbwifSCRUtil.writeAll(scrConfigs) ++ HbwifSCRUtil.hbwifMode() ++ HbwifSCRUtil.retransmitMode()))

  scrDriver.io.start := io.start
  hbwif.io.scr <> scrDriver.io.mem

  val txErrorGen = Reg(init = UInt(0x101, width = 15))
  val rxErrorGen = Reg(init = UInt(0x214, width = 15))

  txErrorGen := Cat((txErrorGen & UInt(0x03)).toBools.reduce(_^_), txErrorGen(14,1)).asUInt
  rxErrorGen := Cat((rxErrorGen & UInt(0x03)).toBools.reduce(_^_), rxErrorGen(14,1)).asUInt

  val ber = 1e-2
  val thres = (ber * (1 << 15)).toInt
  txError := (txErrorGen < UInt(thres))
  rxError := (rxErrorGen < UInt(thres))

  val depth = 2 * tlDataBeats
  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := scrDriver.io.finished
  io.finished := driver.io.finished

  memIn <> driver.io.mem
  val testram = Module(new TileLinkTestRAM(depth))
  testram.io <> memOut

}

class HbwifBertTest(scrConfigs: Seq[Tuple2[String,Int]] = Seq())(implicit val p: Parameters) extends UnitTest(300000)
  with HasHbwifTestModule with HasTileLinkParameters {

  fiwbh.io.loopback := Bool(true)

  // Somewhat arbitrary, but errorPeriod should not be a multiple of the number of lanes, so pick a reasonable prime number
  val numErrors = 76
  val errorPeriod = 13

  val scrPutDriver = Module(new PutSeqDriver(HbwifSCRUtil.writeAll(scrConfigs) ++ HbwifSCRUtil.bertInit()))
  val scrGetChecker = Module(new GetSeqChecker(HbwifSCRUtil.bertErrorCheck(numErrors)))
  val scrArbiter = Module(new ClientUncachedTileLinkIOArbiter(2))

  scrPutDriver.io.start := io.start
  hbwif.io.scr <> scrArbiter.io.out

  scrArbiter.io.in(0) <> scrPutDriver.io.mem
  scrArbiter.io.in(1) <> scrGetChecker.io.mem

  memIn.acquire.valid := Bool(false)
  memIn.grant.ready := Bool(false)
  memOut.acquire.ready := Bool(false)
  memOut.grant.valid := Bool(false)

  val errCount = Reg(init = UInt(0, width = 10))
  val clkCount = Reg(init = UInt(0, width = 10))

  rxError := Bool(false)

  scrGetChecker.io.start := errCount === UInt(numErrors)

  io.finished := scrGetChecker.io.finished

  when(scrPutDriver.io.finished) {
    when (errCount < UInt(numErrors)) {
      when (clkCount < UInt(errorPeriod-1)) {
        txError := Bool(false)
        clkCount := clkCount + UInt(1)
      } .elsewhen (clkCount === UInt(errorPeriod-1)) {
        txError := Bool(true)
        clkCount := UInt(0)
        errCount := errCount + UInt(1)
      } .otherwise {
        txError := Bool(false)
        clkCount := UInt(0)
      }
    }
  } .otherwise {
    txError := Bool(false)
  }

}

class HbwifBertNoErrorTest(scrConfigs: Seq[Tuple2[String,Int]] = Seq())(implicit val p: Parameters) extends UnitTest(5000)
  with HasHbwifTestModule with HasTileLinkParameters {

  fiwbh.io.loopback := Bool(true)

  val cycles = 300

  val scrPutDriver = Module(new PutSeqDriver(HbwifSCRUtil.writeAll(scrConfigs) ++ HbwifSCRUtil.bertInit()))
  val scrGetChecker = Module(new GetSeqChecker(HbwifSCRUtil.bertErrorCheck(0)))
  val scrArbiter = Module(new ClientUncachedTileLinkIOArbiter(2))

  scrPutDriver.io.start := io.start
  hbwif.io.scr <> scrArbiter.io.out

  scrArbiter.io.in(0) <> scrPutDriver.io.mem
  scrArbiter.io.in(1) <> scrGetChecker.io.mem

  memIn.acquire.valid := Bool(false)
  memIn.grant.ready := Bool(false)
  memOut.acquire.ready := Bool(false)
  memOut.grant.valid := Bool(false)

  val cnt = Reg(init = UInt(0, width = 10))

  rxError := Bool(false)
  txError := Bool(false)

  scrGetChecker.io.start := cnt === UInt(cycles)

  io.finished := scrGetChecker.io.finished

  when(scrPutDriver.io.finished && cnt < UInt(cycles)) {
    cnt := cnt + UInt(1)
  }

}


class TestHarness(implicit p: Parameters) extends unittest.TestHarness()(p)
