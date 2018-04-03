package hbwif.tilelink

import hbwif._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.coreplex.CacheBlockBytes

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
    val numXact = p(HbwifTLKey).numXact
    val managerAddressSets = p(HbwifTLKey).managerAddressSets
    val configAddressSets = p(HbwifTLKey).configAddressSets
    require(managerAddressSets.length == lanes)

    val clientNodes = (0 until lanes).map { id => TLClientNode(Seq(TLClientPortParameters(
        Seq(TLClientParameters(
            name               = s"HbwifClient$id",
            sourceId           = IdRange(0,numXact),
            supportsProbe      = TransferSizes(1, beatBytes), //TODO I think this can be larger
            supportsArithmetic = TransferSizes(1, beatBytes),
            supportsLogical    = TransferSizes(1, beatBytes),
            supportsGet        = TransferSizes(1, beatBytes),
            supportsPutFull    = TransferSizes(1, beatBytes),
            supportsPutPartial = TransferSizes(1, beatBytes),
            supportsHint       = TransferSizes(1, beatBytes)
        )),
        minLatency = 1
    ))) }
    val managerNodes = (0 until lanes).map { id => TLManagerNode(Seq(TLManagerPortParameters(
        Seq(TLManagerParameters(
            address            = List(managerAddressSets(id)),
            resources          = (new SimpleDevice(s"HbwifManager$id",Seq())).reg("mem"),
            regionType         = RegionType.CACHED,
            executable         = true,
            supportsGet        = TransferSizes(1, p(CacheBlockBytes)),
            supportsPutFull    = TransferSizes(1, p(CacheBlockBytes)),
            supportsAcquireT   = TransferSizes(1, p(CacheBlockBytes)), //TODO I think this can be larger
            supportsAcquireB   = TransferSizes(1, p(CacheBlockBytes)),
            supportsArithmetic = TransferSizes(1, p(CacheBlockBytes)),
            supportsLogical    = TransferSizes(1, p(CacheBlockBytes)),
            supportsPutPartial = TransferSizes(1, p(CacheBlockBytes)),
            supportsHint       = TransferSizes(1, p(CacheBlockBytes)),
            fifoId             = Some(0))),
        beatBytes = beatBytes,
        minLatency = 1
    ))) }
    //val adapterNodes = (0 until lanes).map { id => TLAdapterNode() }
    val configNodes = (0 until lanes).map { id => TLManagerNode(Seq(TLManagerPortParameters(
        Seq(TLManagerParameters(
            address            = List(configAddressSets(id)),
            resources          = new SimpleDevice(s"HbwifConfig$id",Seq()).reg("mem"), //TODO ?
            executable         = false,
            supportsGet        = TransferSizes(1, beatBytes),
            supportsPutFull    = TransferSizes(1, beatBytes),
            supportsPutPartial = TransferSizes(1, beatBytes),
            fifoId             = Some(0))),
        beatBytes = beatBytes,
        minLatency = 1
    ))) }

    lazy val module = new LazyModuleImp(this) {
        val banks = p(HbwifTLKey).numBanks
        require(lanes % banks == 0)

        // These go to clock receivers
        val hbwifRefClock = IO(Input(Vec(lanes/banks, Clock())))
        // These go to mmio registers
        val hbwifReset = IO(Input(Vec(lanes, Bool())))

        val tx = IO(Vec(lanes, new Differential()))
        val rx = IO(Vec(lanes, Flipped(new Differential())))

        val laneModules = (0 until lanes).map { id =>
            val (clientOut, clientEdge) = clientNodes(id).out(0)
            val (managerIn, managerEdge) = managerNodes(id).in(0)
            val (configIn, configEdge) = configNodes(id).in(0)
            val lane = Module(genLane(clientEdge, managerEdge, configEdge))
            clientOut <> lane.io.data.client
            lane.io.data.manager <> managerIn
            lane.io.control <> configIn
            tx(id) <> lane.io.tx
            lane.io.rx <> rx(id)
            lane.io.clockRef <> hbwifRefClock(id/banks)
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
