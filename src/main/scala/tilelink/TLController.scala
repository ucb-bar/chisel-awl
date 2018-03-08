package hbwif2.tilelink

import chisel3._
import chisel3.util._

object TLControllerPort {
    def apply(edge: TLEdgeIn)(): TLBundle = TLBundle(edge.bundle)
}

class TLController(spec: ControlSpec, edge: TLEdgeIn) extends Controller(spec, TLControllerPort.apply(edge) _) {

    // TODO implement register router

}

object TLController {
    def apply(edge: TLEdgeIn)(spec: ControlSpec): TLController = new TLController(spec, edge)
}
