package hbwif2

import chisel3._

class SerDesGeneratorConfig(
  val dataWidth: Int,
  val transceiverName: String,
  val transceiverResource: String,
  val transceiverNumIrefs: Int,
  val cdrHasOverride: Boolean,
  val cdrIWidth: Int,
  val cdrPWidth: Int,
  val dfeNumTaps: Int,
  val dfeTapWidth: Int,
  val dfeHasOverride: Boolean,
  val dlevDACWidth: Int,
  val dlevHasOverride: Boolean
) {

}

class DefaultConfig extends SerDesGeneratorConfig(
    dataWidth = 16,
    transceiverName = "generic_transceiver",
    transceiverResource = "/generic_transceiver.sv",
    transceiverNumIrefs = 1,
    cdrHasOverride = false,
    cdrIWidth = 8,
    cdrPWidth = 8,
    dfeNumTaps = 4,
    dfeTapWidth = 4,
    dfeHasOverride = false,
    dlevDACWidth = 4,
    dlevHasOverride = false
  )


object SerDesGeneratorConfigs {

def apply(): Seq[SerDesGeneratorConfig] = Seq(new DefaultConfig)

}
