package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._

class HbwifTileLinkMemSerDesIO(implicit p: Parameters) extends HbwifBundle()(p) {

  val down = (new Decoded8b10bSymbol).asOutput
  val up = (new Decoded8b10bSymbol).asInput

  val mem = (new ClientUncachedTileLinkIO()(outermostParams)).flip

}

class HbwifTileLinkMemSerDes(implicit val p: Parameters) extends Module
  with HasHbwifParameters {

  val io = new HbwifTileLinkMemSerDesIO

}
