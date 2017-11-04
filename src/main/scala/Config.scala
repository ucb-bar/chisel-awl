package hbwif2

import chisel3._

class SerDesGeneratorConfig(
  val dataWidth: Int,
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
