package hbwif2.tilelink

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

object TLControllerPort {
    def apply(edge: TLEdgeIn)(): TLBundle = TLBundle(edge.bundle)
}

class TLController(spec: ControlSpec, edge: TLEdgeIn) extends Controller(spec) {

    type P = TLBundle
    def portFactory = TLControllerPort.apply(edge)

    // TODO implement register router

}

object TLController {
    def apply(edge: TLEdgeIn)(spec: ControlSpec): TLController = new TLController(spec, edge)
}

trait HasScanChainController {
    type C = TLController
    val edge: TLEdgeIn // TODO
    def genBuilder() = new ControllerBuilder(TLController.apply(edge))
}
