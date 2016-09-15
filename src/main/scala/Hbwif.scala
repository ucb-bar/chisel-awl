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

  val lanes = Vec.fill(hbwifNumLanes) { new LaneIO }

}

class Hbwif(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new HbwifIO

  val lanes = Seq.fill(hbwifNumLanes) { Module(new Lane) }

  lanes.zip(io.lanes).map { i => i._1.io <> i._2 }

}


