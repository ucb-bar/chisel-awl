package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._

class TLLane8b10b(val clientEdge: TLEdgeOut, val managerEdge: TLEdgeIn, val configEdge: TLEdgeOut)(implicit val c: SerDesGeneratorConfig) extends Lane
    with HasEncoding8b10b
    with HasBertDebug
    with HasTLBidirectionalPacketizer
    with HasTLController


// cake stuff
class HbwifModule(id: Int, managerParams: Seq[TLManagerPortParameters])(implicit p: Parameters) extends LazyModule {

    val clientNode = TLClientNode(Seq(TLClientPortParameters(
        Seq(TLClientParameters(
            name = s"HbwifClient$id",
            sourceId = IdRange(0,32) // TODO
        ))
    )))
    val managerNode = TLManagerNode(managerParams) // TODO
    val configNode = TLClientNode(Seq(TLClientPortParameters(
        Seq(TLClientParameters(
            name = s"HbwifConfig$id",
            sourceId = IdRange(0,32) // TODO
        ))
    )))

    val c = new DefaultConfig() // TODO

    lazy val module = new LazyModuleImp(this) {
        val (clientOut, clientEdge) = clientNode.out(0)
        val (managerIn, managerEdge) = managerNode.in(0)
        val (configOut, configEdge) = configNode.out(0)
        val lane = Module(new TLLane8b10b(clientEdge, managerEdge, configEdge)(c))

        lane.io.data.client <> clientOut
        lane.io.data.manager <> managerIn
        lane.io.control <> configOut
        /*
        lane.io.clock_ref <> TODO
        lane.io.async_reset_in <> TODO
        lane.io.bias <> TODO
        lane.io.rx <> io.rx(id) TODO
        lane.io.tx <> io.tx(id) TODO
        */

    }
}

class HbwifBundle extends Bundle {
    val numLanes = 8 // TODO
    val tx = Vec(numLanes, new Differential())
    val rx = Vec(numLanes, Flipped(new Differential()))
}

