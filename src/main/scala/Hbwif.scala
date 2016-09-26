package hbwif

import Chisel._
import cde._
import junctions.ParameterizedBundle
import uncore.tilelink._
import uncore.tilelink2.{LazyModule, LazyModuleImp}

case object HbwifKey extends Field[HbwifParameters]

case class HbwifParameters(
  val numLanes: Int = 8,
  val bufferDepth: Int = 10) // Calculated based on worst-case transmission line delay and codec, serdes, etc. latency

trait HasHbwifParameters extends HasBertParameters with HasTransceiverParameters {
  val hbwifNumLanes = p(HbwifKey).numLanes
  val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
}

trait Hbwif extends LazyModule
  with HasHbwifParameters {
  val pDevices: ResourceManager[AddrMapEntry]

  (0 until hbwifNumLanes).foreach { i =>
    // TODO: 1024 is arbitrary, calculate this number from the SCR size
    pDevices.add(AddrMapEntry(s"hbwif_lane$i", MemSize(1024, MemAttr(AddrMapProt.RW))))
  }
  pDevices.add(AddrMapEntry("hbwif_", MemSize(1024, MemAttr(AddrMapProt.RW))))
}

trait HbwifBundle extends HasHbwifParameters {
  val hbwifRx      = Vec(hbwifNumLanes, new Differential)
  val hbwifTx      = Vec(hbwifNumLanes, (new Differential)).flip
  val hbwifIref    = if(transceiverRefGenHasInput) Some(Bool(INPUT)) else None
  val hbwifFastClk = Bool(INPUT)
}

trait HbwifModule {
  implicit val p: Parameters
  val outer: Periphery
  val io: HbwifBundle
  val mmioNetwork: Option[TileLinkRecursiveInterconnect]
  val coreplex: Coreplex
  val hbwifIO: Vec[ClientUncachedTileLinkIO]

  val hbwifLanes = Seq.fill(hbwifNumLanes) { Module(new Lane) }

  hbwifLanes.foreach { _.io.fastClk := io.hbwifFastClk }

  hbwifLanes.zip(io.rx).foreach { x => x._2 <> x._1.io.hbwifRx }
  hbwifLanes.zip(io.tx).foreach { x => x._1.io.hbwifTx <> x._2 }

  (0 until hbwifNumLanes).foreach { i =>
    hbwifLanes(i).io.scr <> mmioNetwork.get.port(s"hbwif_lane$i")
  }
  hbwifLanes.zip(hbwifIO).foreach { x => x._1.io.mem <> x._2 }

  // Instantiate and connect the reference generator if needed
  if (transceiverHasIRef) {
    val hbwifRefGen = Module(new ReferenceGenerator)
    hbwifLanes.zip(hbwifRefGen.io.irefOut).foreach { x => x._1.io.iref.get <> x._2 }
    if (transceiverRefGenHasInput) {
      refGen.io.irefIn.get := io.hbwifIref.get
    }
  }
}


