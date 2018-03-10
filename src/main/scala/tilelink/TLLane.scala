package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

//case class SerDesGeneratorKey extends Field[SerDesGeneratorConfig]

/*
class TLLane(masterEdge: TLEdgeIn, slaveEdge: TLEdgeOut, configEdge: TLEdgeIn)(implicit p: Parameters)
    extends Lane(TLControllerPort.apply _, TLBidirectionalPacketizerIF.apply ) {

    implicit val c: SerDesGeneratorConfig = p(SerDesGeneratorKey)

}
*/
/*
class HBWIFModule(numLanes: Int, addresses: Seq[AddressSet], beatBytes: Int, devName: Option[String] = None)(implicit p: Parameters) extends LazyModule {

    val device = devName
        .map(new SimpleDevice(_, Seq("bar,hbwif")))
        .getOrElse(new MemoryDevice())

    val slaveNode = TLManagerNode(addresses.map { address => TLManagerPortParameters(
        Seq(TLManagerParameters(
            address            = List(address),
            resources          = device.reg("mem"),
            regionType         = RegionType.CACHED, // TODO is this right???
            executable         = true,
            supportsGet        = TransferSizes(1, beatBytes),
            supportsPutPartial = TransferSizes(1, beatBytes),
            supportsPutFull    = TransferSizes(1, beatBytes),
            fifoId             = Some(0))), // TODO
        beatBytes  = beatBytes,
        minLatency = 1)})

    val configs = Seq() // TODO

    val masterNode = TLClientNode(TLClientPortParameters(
        Seq(TLClientParameters(
            sourceId = IdRange(0, 1), // TODO
            name = "TODO"))))

    val configNode = TLManagerNode(configs.map { address => TLManagerPortParameters(
        Seq(TLManagerParameters(
            address            = List(address),
            resources          = device.reg("mem"),
            regionType         = RegionType.UNCACHEABLE,
            executable         = false,
            supportsGet        = TransferSizes(1, beatBytes),
            supportsPutPartial = TransferSizes(1, beatBytes),
            supportsPutFull    = TransferSizes(1, beatBytes),
            fifoId             = Some(0))), // TODO
        beatBytes  = beatBytes,
        minLatency = 1)})

    lazy val module = new LazyModuleImp(this) {
        val (in, edge) = node.in(0)
    }

}
*/
