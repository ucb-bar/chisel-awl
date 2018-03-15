package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TLControllerBuilder(edge: TLEdgeOut) extends ControllerBuilder {

    type P = TLBundle
    def createPort = TLBundle(edge.bundle)

    // TODO implement register router

    def generate(laneClock: Clock, laneReset: Bool): P = { ??? }

}

trait HasTLController {
    val configEdge: TLEdgeOut
    def genBuilder() = new TLControllerBuilder(configEdge)
}
