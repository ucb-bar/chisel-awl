package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle


class ReferenceGeneratorIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasHbwifParameters {

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
