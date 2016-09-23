package hbwif

import Chisel._
import cde._
import junctions.ParameterizedBundle
import uncore.tilelink._

case object HbwifKey extends Field[HbwifParameters]

case class HbwifParameters(
  val numLanes: Int = 8,
  val bufferDepth: Int = 10) // Calculated based on worst-case transmission line delay and codec, serdes, etc. latency

trait HasHbwifParameters extends HasBertParameters with HasTransceiverParameters {
  val hbwifNumLanes = p(HbwifKey).numLanes
  val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
}

abstract class HbwifBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasHbwifParameters

class HbwifIO(implicit p: Parameters) extends HbwifBundle()(p) {

  val fastClk = Clock(INPUT)

  val rx   = Vec(hbwifNumLanes, new Differential)
  val tx   = Vec(hbwifNumLanes, (new Differential)).flip
  val mem  = Vec(hbwifNumLanes, (new ClientUncachedTileLinkIO()(outermostParams))).flip
  val scr  = Vec(hbwifNumLanes, (new ClientUncachedTileLinkIO()(outermostMMIOParams))).flip

  val iref = if(transceiverRefGenHasInput) Some(Bool(INPUT)) else None

}

class Hbwif(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new HbwifIO

  val lanes = Seq.fill(hbwifNumLanes) { Module(new Lane) }

  lanes.foreach { _.io.fastClk := io.fastClk }

  lanes.zip(io.rx).foreach { x => x._2 <> x._1.io.rx }
  lanes.zip(io.tx).foreach { x => x._1.io.tx <> x._2 }
  lanes.zip(io.mem).foreach { x => x._1.io.mem <> x._2 }
  lanes.zip(io.scr).foreach { x => x._1.io.scr <> x._2 }

  // Instantiate and connect the reference generator if needed
  if (transceiverHasIRef) {
    val refGen = Module(new ReferenceGenerator)
    lanes.zip(refGen.io.irefOut).foreach { x => x._1.io.iref.get <> x._2 }
    if (transceiverRefGenHasInput) {
      refGen.io.irefIn.get := io.iref.get
    }
  }


}


