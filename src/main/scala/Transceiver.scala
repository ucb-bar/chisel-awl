package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle

case object TransceiverKey extends Field[TransceiverParameters]

case class TransceiverParameters(
  name: String = "generic_transceiver",
  fpgaName: String = "fpga_transceiver",
  extraInputs: Option[Bundle] = None,
  extraOutputs: Option[Bundle] = None,
  hasIRef: Boolean = true,
  refGenHasInput: Boolean = true,
  refGenConfig: Option[Bundle] = None,
  refGenName: String = "generic_reference_generator",
  refGenNumOutputs: Int = 8,
  divideBy: Int = 5,
  isDDR: Boolean = true)

trait HasTransceiverParameters {
  implicit val p: Parameters
  val transceiverDivideBy = p(TransceiverKey).divideBy
  val transceiverIsDDR = p(TransceiverKey).isDDR
  val transceiverHasIRef = p(TransceiverKey).hasIRef
  val transceiverRefGenHasInput = p(TransceiverKey).refGenHasInput
  val transceiverRefGenNumOutputs = p(TransceiverKey).refGenNumOutputs
  val transceiverDataWidth = if (transceiverIsDDR) 2*transceiverDivideBy else transceiverDivideBy
}

class TransceiverData(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasTransceiverParameters {
  val rx = UInt(OUTPUT, width = transceiverDataWidth)
  val tx = UInt(INPUT, width = transceiverDataWidth)
}

class TransceiverIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasTransceiverParameters {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // low speed clock output
  val slowClk = Clock(OUTPUT)

  // reset
  val resetIn = Bool(INPUT)
  val resetOut = Bool(OUTPUT)

  // RX pad inputs
  val rx = (new Differential).flip

  // TX pad outputs
  val tx = new Differential

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


