package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle
import rocketchip._
import junctions._
import uncore.tilelink._
import uncore.tilelink2.{LazyModule, LazyModuleImp}
import coreplex._

case object HbwifKey extends Field[HbwifParameters]

case class HbwifParameters(
  val numLanes: Int = 8,
  val bufferDepth: Int = 10) // Calculated based on worst-case transmission line delay and codec, serdes, etc. latency

trait HasHbwifParameters extends HasBertParameters with HasTransceiverParameters {
  val hbwifNumLanes = p(HbwifKey).numLanes
  val memParams = p.alterPartial({ case TLId => "L2toMC" })
  val mmioParams = p.alterPartial({ case TLId => "L2toMMIO" })
}

trait Hbwif extends LazyModule
  with HasHbwifParameters {
  val pDevices: ResourceManager[AddrMapEntry]

  (0 until hbwifNumLanes).foreach { i =>
    // TODO: 1024 is arbitrary, calculate this number from the SCR size
    pDevices.add(AddrMapEntry(s"hbwif_lane$i", MemSize(4096, MemAttr(AddrMapProt.RW))))
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
  val pBus: TileLinkRecursiveInterconnect
  val hbwifIO: Vec[ClientUncachedTileLinkIO]
  val hbwifFastClock: Clock

  val hbwifLanes = Seq.fill(hbwifNumLanes) { Module(new HbwifLane) }

  hbwifLanes.foreach { _.io.fastClk := hbwifFastClock }

  hbwifLanes.map(_.io.rx).zip(io.hbwifRx) map { case (lane, top) => top <> lane }
  hbwifLanes.map(_.io.tx).zip(io.hbwifTx) map { case (lane, top) => lane <> top }

  (0 until hbwifNumLanes).foreach { i =>
    hbwifLanes(i).io.scr <> pBus.port(s"hbwif_lane$i")
  }
  hbwifLanes.zip(hbwifIO).foreach { x => x._1.io.mem <> x._2 }

  // Instantiate and connect the reference generator if needed
  if (transceiverHasIRef) {
    val hbwifRefGen = Module(new ReferenceGenerator)
    hbwifLanes.zipWithIndex.foreach { x => x._1.io.iref.get := hbwifRefGen.io.irefOut(x._2) }
    if (transceiverRefGenHasInput) {
      hbwifRefGen.io.irefIn.get := io.hbwifIref.get
    }
  }
}


