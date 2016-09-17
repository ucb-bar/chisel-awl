package hbwif

import Chisel._
import cde._
import junctions.ParameterizedBundle

case object TransceiverKey extends Field[TransceiverParameters]

case class TransceiverParameters(
  name: String = "generic_transceiver",
  extraInputs: Option[Bundle] = None,
  extraOutputs: Option[Bundle] = None,
  hasIRef: Boolean = true,
  refGenHasInput: Boolean = true,
  refGenConfig: Option[Bundle] = None,
  divideBy: Int = 5,
  isDDR: Boolean = true)

trait HasTransceiverParameters {
  implicit val p: Parameters
  val transceiverDivideBy = p(TransceiverKey).divideBy
  val transceiverIsDDR = p(TransceiverKey).isDDR
  val transceiverHasIRef = p(TransceiverKey).hasIRef
  val transceiverRefGenHasInput = p(TransceiverKey).refGenHasInput
  val transceiverDataWidth = if (transceiverIsDDR) 2*transceiverDivideBy else transceiverDivideBy
}

class TransceiverData(implicit p: Parameters) extends HbwifBundle()(p) {
      val rx = UInt(INPUT, width = transceiverDataWidth)
      val tx = UInt(OUTPUT, width = transceiverDataWidth)
}

class TransceiverIO(implicit p: Parameters) extends HbwifBundle()(p) {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // low speed clock output
  val slowClk = Clock(OUTPUT)

  // RX pad inputs
  val rx = new Differential

  // TX pad outputs
  val tx = (new Differential).flip

  // internal data interface
  val data = new TransceiverData

  // reference current (if any)
  val iref = if (transceiverHasIRef) Some(Bool(INPUT)) else None

  // parameterizable configuration bundle
  val extraInputs = p(TransceiverKey).extraInputs.map { _.cloneType.asInput }

  // parameterizable configuration bundle
  val extraOutputs = p(TransceiverKey).extraOutputs.map { _.cloneType.asOutput }

}

class Transceiver(implicit val p: Parameters) extends BlackBox
  with HasTransceiverParameters {

  val io = new TransceiverIO()(p)

  override def desiredName = p(TransceiverKey).name

}


