package hbwif

import Chisel._
import cde._
import util.ParameterizedBundle

class AnalogTestHarnessIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasHbwifParameters {

  val hbwifIref    = if(transceiverHasIref && transceiverRefGenHasInput) Some(UInt(OUTPUT, width=hbwifNumBanks*transceiverNumIrefs)) else None
  val hbwifVcm     = if(transceiverHasVcm) Some(UInt(OUTPUT, width=hbwifNumBanks)) else None

}

class AnalogTestHarness(implicit val p: Parameters) extends BlackBox {

  val io = new AnalogTestHarnessIO

  override def desiredName = p(TransceiverKey).analogTestHarnessName

}
