package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle
import rocketchip._
import junctions._
import uncore.tilelink._
import diplomacy.{LazyModule, LazyModuleImp}
import coreplex._

case object HbwifKey extends Field[HbwifParameters]

case class HbwifParameters(
  val numLanes: Int = 8,
  val maxRetransmitCycles: Int = 20000,
  val bufferDepth: Int = 16)

trait HasHbwifParameters extends HasBertParameters with HasTransceiverParameters {
  val memParams = p.alterPartial({ case TLId => "Switcher" })
  val mmioParams = p.alterPartial({ case TLId => "MMIOtoSCR" })
  val hbwifNumLanes = p(HbwifKey).numLanes
  val hbwifMaxRetransmitCycles = p(HbwifKey).maxRetransmitCycles
  val hbwifBufferDepth = p(HbwifKey).bufferDepth
}

trait HasHbwifTileLinkParameters extends HasHbwifParameters
  with HasTileLinkParameters {
  val hbwifRawGrantBits = tlBeatAddrBits + tlClientXactIdBits + tlManagerIdBits + 1 + tlGrantTypeBits + tlDataBits
  val hbwifRawAcquireBits = tlBeatAddrBits + tlClientXactIdBits + tlManagerIdBits + 1 + tlGrantTypeBits + tlDataBits
  val hbwifGrantPadBits = (8 - (hbwifRawGrantBits % 8)) % 8
  val hbwifAcquirePadBits = (8 - (hbwifRawAcquireBits % 8)) % 8
  val hbwifChecksumBits = 8
  val hbwifGrantBits = hbwifRawGrantBits + hbwifGrantPadBits + hbwifChecksumBits
  val hbwifAcquireBits = hbwifRawAcquireBits + hbwifAcquirePadBits + hbwifChecksumBits
  val hbwifGrantBytes = hbwifGrantBits / 8
  val hbwifAcquireBytes = hbwifAcquireBits / 8
}

trait Hbwif extends LazyModule
  with HasHbwifParameters {
  val scrDevices: ResourceManager[AddrMapEntry]

  (0 until hbwifNumLanes).foreach { i =>
    scrDevices.add(AddrMapEntry(s"hbwif_lane$i", MemSize(4096, MemAttr(AddrMapProt.RW))))
  }
}

trait HbwifBundle extends HasHbwifParameters {
  val hbwifRx      = Vec(hbwifNumLanes, new Differential)
  val hbwifTx      = Vec(hbwifNumLanes, (new Differential)).flip
  val hbwifIref    = if(transceiverRefGenHasInput) Some(Bool(INPUT)) else None
}

trait HbwifModule extends HasHbwifParameters {
  implicit val p: Parameters
  val io: HbwifBundle
  val scrBus: TileLinkRecursiveInterconnect
  val hbwifIO: Vec[ClientUncachedTileLinkIO]
  val hbwifFastClock: Clock
  val system_clock: Clock
  val system_reset: Bool
  val hbwifReset = Wire(Bool())
  val hbwifResetOverride = Wire(Bool())

  val hbwifLanes = (0 until hbwifNumLanes).map(id => Module(new HbwifLane(id=id,c=system_clock,r=system_reset)))

  hbwifLanes.foreach { _.io.fastClk := hbwifFastClock }
  hbwifLanes.foreach { _.io.hbwifReset := hbwifReset }
  hbwifLanes.foreach { _.io.hbwifResetOverride := hbwifResetOverride }

  hbwifLanes.map(_.io.rx).zip(io.hbwifRx) map { case (lane, top) => top <> lane }
  hbwifLanes.map(_.io.tx).zip(io.hbwifTx) map { case (lane, top) => lane <> top }

  (0 until hbwifNumLanes).foreach { i =>
    hbwifLanes(i).io.scr <> scrBus.port(s"hbwif_lane$i")
  }
  hbwifLanes.zip(hbwifIO).foreach { x => x._1.io.mem <> x._2 }

  // Instantiate and connect the reference generator if needed
  if (transceiverHasIRef) {
    val hbwifRefGen = Module(new ReferenceGenerator)
    hbwifRefGen.suggestName("hbwifRefGenInst")
    hbwifLanes.zipWithIndex.foreach { x => x._1.io.iref.get := hbwifRefGen.io.irefOut(x._2) }
    if (transceiverRefGenHasInput) {
      hbwifRefGen.io.irefIn.get := io.hbwifIref.get
    }
  }
}


