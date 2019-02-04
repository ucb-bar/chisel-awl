package awl.hbwif

import chisel3._
import chisel3.util._

class FixedWidthLane8b10bBasic[F <: Data](val fwDataFactory: () => F)(implicit val c: SerDesConfig) extends Lane
    with HasEncoding8b10b
    with HasFixedWidthPacketizer[F]
    with HasGenericTransceiverSubsystem

class FixedWidthLane8b10b[F <: Data](val fwDataFactory: () => F)
    (implicit val c: SerDesConfig, implicit val b: BertConfig, implicit val m: PatternMemConfig) extends Lane
    with HasEncoding8b10b
    with HasBertDebug
    with HasPatternMemDebug
    with HasBitStufferDebug4Modes
    with HasBitReversalDebug
    with HasFixedWidthPacketizer[F]
    with HasGenericTransceiverSubsystem

