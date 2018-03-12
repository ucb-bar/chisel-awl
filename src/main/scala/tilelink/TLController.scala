package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

object TLControllerPort {
    def apply(edge: TLEdgeOut)(): TLBundle = TLBundle(edge.bundle)
}

class TLController(spec: ControlSpec, edge: TLEdgeOut) extends Controller(spec) {

    type P = TLBundle
    def portFactory = TLControllerPort.apply(edge)

    // TODO implement register router

}

object TLController {
    def apply(edge: TLEdgeOut)(spec: ControlSpec): TLController = new TLController(spec, edge)
}

trait HasTLController {
    type C = TLController
    val configEdge: TLEdgeOut // TODO
    def genBuilder() = new ControllerBuilder(TLController.apply(configEdge))
}
