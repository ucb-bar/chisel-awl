package hbwif

import Chisel._
import cde._
import junctions.ParameterizedBundle


class ReferenceGeneratorIO(implicit p: Parameters) extends HbwifBundle()(p) {

  // parameterizable configuration bundle
  val config = p(TransceiverKey).refGenConfig.map { _.cloneType.asInput }

  // reference outputs
  val irefOut = Vec.fill(hbwifNumLanes) { Bool(OUTPUT) }

  // optional reference input
  val irefIn = if (transceiverRefGenHasInput) Some(Bool(INPUT)) else None

}

class ReferenceGenerator(implicit val p: Parameters) extends BlackBox {

  val io = new ReferenceGeneratorIO

  override def desiredName = p(TransceiverKey).refGenName

}
