package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.XLen

abstract class TLLane8b10b(val clientEdge: TLEdgeOut, val managerEdge: TLEdgeIn, val configEdge: TLEdgeIn)
    (implicit val c: SerDesConfig, implicit val b: BertConfig, implicit val m: PatternMemConfig, implicit val p: Parameters) extends Lane
    with HasEncoding8b10b
    with HasBertDebug
    with HasPatternMemDebug
    with HasBitStufferDebug4Modes
    with HasTLBidirectionalPacketizer
    with HasTLController

class GenericTLLane8b10b(clientEdge: TLEdgeOut, managerEdge: TLEdgeIn, configEdge: TLEdgeIn)(implicit p: Parameters)
    extends TLLane8b10b(clientEdge, managerEdge, configEdge)(p(HbwifSerDesKey), p(HbwifBertKey), p(HbwifPatternMemKey), p)
    with HasGenericTransceiverSubsystem


abstract class HbwifModule()(implicit p: Parameters) extends LazyModule {

    val lanes = p(HbwifTLKey).numLanes
    val beatBytes = p(HbwifTLKey).beatBytes
    val cacheBlockBytes = p(CacheBlockBytes)
    val numXact = p(HbwifTLKey).numXact
    val managerAddressSet = p(HbwifTLKey).managerAddressSet
    val configAddressSets = p(HbwifTLKey).configAddressSets
    val tlc = p(HbwifTLKey).tlc
    val tluh = p(HbwifTLKey).tluh

    val clientNode = TLClientNode((0 until lanes).map { id => TLClientPortParameters(
        Seq(TLClientParameters(
            name               = s"HbwifClient$id",
            sourceId           = IdRange(0,numXact),
            supportsGet        = TransferSizes(1, cacheBlockBytes),
            supportsPutFull    = TransferSizes(1, cacheBlockBytes),
            supportsPutPartial = TransferSizes(1, cacheBlockBytes),
            supportsArithmetic = TransferSizes(1, cacheBlockBytes),
            supportsLogical    = TransferSizes(1, cacheBlockBytes),
            supportsHint       = TransferSizes(1, cacheBlockBytes),
            supportsProbe      = TransferSizes(1, cacheBlockBytes))),
        minLatency = 1)
        })
    val managerNode = TLManagerNode((0 until lanes).map { id =>
        val base = managerAddressSet
        val filter = AddressSet(id * cacheBlockBytes, ~((lanes-1) * cacheBlockBytes))
        TLManagerPortParameters(Seq(TLManagerParameters(
            address            = base.intersect(filter).toList,
            resources          = (new SimpleDevice(s"HbwifManager$id",Seq())).reg("mem"),
            regionType         = if (tlc) RegionType.CACHED else RegionType.UNCACHED,
            executable         = true,
            supportsGet        = TransferSizes(1, cacheBlockBytes),
            supportsPutFull    = TransferSizes(1, cacheBlockBytes),
            supportsPutPartial = TransferSizes(1, cacheBlockBytes),
            supportsArithmetic = if (tluh) TransferSizes(1, cacheBlockBytes) else TransferSizes.none,
            supportsLogical    = if (tluh) TransferSizes(1, cacheBlockBytes) else TransferSizes.none,
            supportsHint       = if (tluh) TransferSizes(1, cacheBlockBytes) else TransferSizes.none,
            supportsAcquireT   = if (tlc) TransferSizes(1, cacheBlockBytes) else TransferSizes.none,
            supportsAcquireB   = if (tlc) TransferSizes(1, cacheBlockBytes) else TransferSizes.none,
            fifoId             = Some(0))),
        beatBytes = beatBytes,
        endSinkId = 0,
        minLatency = 1) })
    val configNode = TLManagerNode((0 until lanes).map { id => TLManagerPortParameters(
        Seq(TLManagerParameters(
            address            = List(configAddressSets(id)),
            resources          = new SimpleDevice(s"HbwifConfig$id",Seq()).reg("control"),
            executable         = false,
            supportsGet        = TransferSizes(1, p(XLen)/8),
            supportsPutFull    = TransferSizes(1, p(XLen)/8),
            supportsPutPartial = TransferSizes(1, p(XLen)/8),
            fifoId             = Some(0))),
        beatBytes = p(XLen)/8,
        minLatency = 1) })

    lazy val module = new LazyModuleImp(this) {
        val banks = p(HbwifTLKey).numBanks
        require(lanes % banks == 0)

        // These go to clock receivers
        val hbwifRefClock = IO(Input(Vec(banks, Clock())))
        // These go to mmio registers
        val hbwifReset = IO(Input(Vec(lanes, Bool())))

        val tx = IO(Vec(lanes, new Differential()))
        val rx = IO(Vec(lanes, Flipped(new Differential())))

        val laneModules = (0 until lanes).map { id =>
            val (clientOut, clientEdge) = clientNode.out(id)
            val (managerIn, managerEdge) = managerNode.in(id)
            val (configIn, configEdge) = configNode.in(id)
            val lane = Module(genLane(clientEdge, managerEdge, configEdge))
            clientOut <> lane.io.data.client
            lane.io.data.manager <> managerIn
            lane.io.control <> configIn
            tx(id) <> lane.io.tx
            lane.io.rx <> rx(id)
            lane.io.clockRef <> hbwifRefClock(id/(lanes/banks))
            lane.io.asyncResetIn <> hbwifReset(id)
            lane
        }
    }

    def genLane(clientEdge: TLEdgeOut, managerEdge: TLEdgeIn, configEdge: TLEdgeIn)(implicit p: Parameters): TLLane8b10b
}

class GenericHbwifModule()(implicit p: Parameters) extends HbwifModule()(p) {

    def genLane(clientEdge: TLEdgeOut, managerEdge: TLEdgeIn, configEdge: TLEdgeIn)(implicit p: Parameters) = {
        new GenericTLLane8b10b(clientEdge, managerEdge, configEdge)(p)
    }

}
