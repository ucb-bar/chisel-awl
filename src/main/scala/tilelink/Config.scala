package hbwif.tilelink

import hbwif._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy.AddressSet

case class HbwifTLConfig(
    managerAddressSets: Seq[AddressSet],
    configAddressSets: Seq[AddressSet],
    numLanes: Int = 8,
    numBanks: Int = 2,
    beatBytes: Int = 16,
    numXact: Int = 32
)

case object HbwifSerDesKey extends Field[SerDesConfig]
case object HbwifBertKey extends Field[BertConfig]
case object HbwifTLKey extends Field[HbwifTLConfig]
case object HbwifPatternMemKey extends Field[PatternMemConfig]

