package hbwif

import Chisel._
import cde._
import junctions.ParameterizedBundle

case object TransceiverKey extends Field[TransceiverParameters]

case class TransceiverParameters(
  name: String = "transceiver",
  extraInputs: Option[Bundle] = None,
  extraOutputs: Option[Bundle] = None,
  divideBy: Int = 5,
  isDDR: Boolean = true)

trait HasTransceiverParameters {
  implicit val p: Parameters
  val transceiverDivideBy = p(TransceiverKey).divideBy
  val transceiverIsDDR = p(TransceiverKey).isDDR
  val transceiverName = p(TransceiverKey).name
  val transceiverDataWidth = if (transceiverIsDDR) 2*transceiverDivideBy else transceiverDivideBy
}

class TransceiverIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasTransceiverParameters {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // low speed clock output
  val slowClk = Clock(OUTPUT)

  // RX pad inputs
  val rx = new Differential

  // TX pad outputs
  val tx = (new Differential).flip

  // internal data interface
  val data = new Bundle {
      val rx = UInt(OUTPUT, width = transceiverDataWidth)
      val tx = UInt(INPUT, width = transceiverDataWidth)
  }

  // parameterizable configuration bundle (punched up to top level of HBWIF)
  val extraInputs = p(TransceiverKey).extraInputs.getOrElse(new Bundle).cloneType.asInput

  // parameterizable configuration bundle (punched up to top level of HBWIF)
  val extraOutputs = p(TransceiverKey).extraOutputs.getOrElse(new Bundle).cloneType.asOutput

}

class Transceiver(implicit val p: Parameters) extends BlackBox
  with HasTransceiverParameters {

  val io = new TransceiverIO()(p)
  suggestName(transceiverName)
}


