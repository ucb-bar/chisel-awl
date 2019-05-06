package hbwif.tilelink

import hbwif._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.subsystem.{ExtMem, BankedL2Key, MemoryBusKey}

case class HbwifTLConfig(
    managerAddressSet: Seq[AddressSet],
    configAddressSets: Seq[AddressSet],
    numBanks: Int = 2,
    beatBytes: Int = 16,
    numXact: Int = 32,
    sinkIds: Int = 32,
    clientPort: Boolean = false,
    clientTLUH: Boolean = true,
    clientTLC: Boolean = true,
    managerTLUH: Boolean = true,
    managerTLC: Boolean = true,
    maxOutstanding: Int = 8,
    asyncQueueDepth: Int = 8,
    asyncQueueSync: Int = 3,
    asyncQueueSafe: Boolean = true,
    asyncQueueNarrow: Boolean = true
) {
    require(managerTLUH || !managerTLC)
}

case object HbwifSerDesKey extends Field[SerDesConfig]
case object HbwifBertKey extends Field[BertConfig]
case object HbwifNumLanes extends Field[Int]
case object HbwifTLKey extends Field[HbwifTLConfig]
case object HbwifPatternMemKey extends Field[PatternMemConfig]
case object HbwifPipelineResetDepth extends Field[Int]
case object BuildHbwif extends Field[Parameters => HbwifModule]

class WithNHbwifLanes(nLanes: Int) extends Config((site, here, up) => {
  case HbwifNumLanes => nLanes
})

class WithGenericSerdes extends Config((site, here, up) => {
    case HbwifSerDesKey => SerDesConfig(
        dataWidth = 16,
        numWays = 2
    )
    case HbwifNumLanes => site(BankedL2Key).nBanks
    case HbwifTLKey => {
        HbwifTLConfig(
            managerAddressSet = AddressSet.misaligned(site(ExtMem).get.master.base, site(ExtMem).get.master.size),
            configAddressSets = Seq.tabulate(site(HbwifNumLanes)) { i =>
                AddressSet(0x4000000 + i*0x10000, 0xffff)
            },
            numBanks = 2,
            beatBytes = site(MemoryBusKey).beatBytes,
            numXact = 16,
            clientTLC = true,
            managerTLUH = true,
            managerTLC = true,
            asyncQueueDepth = 8,
            asyncQueueSync = 3,
            asyncQueueSafe = true,
            asyncQueueNarrow = true
        )
    }
    case HbwifBertKey => BertConfig.fromBER(site(HbwifSerDesKey), 1e-15)
    case HbwifPatternMemKey => PatternMemConfig(
        patternDepth = 16,
        snapshotDepth = 16
    )
    case BuildHbwif => (p: Parameters) => new GenericHbwifModule()(p)
})

