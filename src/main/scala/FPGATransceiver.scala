package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle

class FPGATransceiverIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasTransceiverParameters {

  // high speed clock input
  val fastClk = Clock(INPUT)

  // low speed clock output
  val slowClk = Clock(OUTPUT)

  // reset
  val reset = Bool(INPUT)

  // RX pad inputs
  val rx = (new Differential).flip

  // TX pad outputs
  val tx = new Differential

  // internal data interface
  val data = new TransceiverData

}

class FPGATransceiver(implicit val p: Parameters) extends BlackBox
  with HasTransceiverParameters {

  val io = new FPGATransceiverIO()(p)

  override def desiredName = p(TransceiverKey).fpgaName
}
