package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._

class HbwifTileLinkMemSerDesIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  val down = (new Decoded8b10bSymbol).asOutput
  val up = (new Decoded8b10bSymbol).asInput

  val mem = (new ClientUncachedTileLinkIO()(edgeMemParams)).flip

}

class HbwifTileLinkMemSerDes(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new HbwifTileLinkMemSerDesIO

}
