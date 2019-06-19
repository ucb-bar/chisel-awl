package hbwif.dsp

import chisel3._
import chisel3.util._
import chisel3.experimental.MultiIOModule

import hbwif._
import hbwif.tilelink._

class AXI4StreamPacketizer[S <: DecodedSymbol](axi4sParams: AXI4StreamBundleParameters, decodedSymbolsPerCycle: Int, symbolFactory: () => S)(implicit val p: Parameters)
  extends FixedWidthPacketizer[S, AXI4StreamBundlePayload](decodedSymbolsPerCycle, symbolFactory, () => new AXI4StreamBundlePayload(axi4sParams))

trait HasAXI4StreamPacketizer extends HasFixedWidthPacketizer[AXI4StreamBundlePayload]

abstract class AXI4StreamLane8b10b(val axi4sParams: AXI4StreamBundleParameters)
  (implicit val c: SerDesConfig, implicit val b: BertConfig, implicit val m: PatternMemConfig, implicit val p: Parameters) extends Lane
  with HasEncoding8b10b
  with HasAXI4StreamPacketizer
  with HasTLController

class GenericAXI4StreamLane8b10b(axi4sParams: AXI4StreamBundleParameters)(implicit p: Parameters)
  extends AXI4StreamLane8b10b(axi4sParams)(p(HbwifSerDesKey), p(HbwifBertKey), p(HbwifPatternMemKey), p)
  with HasGenericTransceiverSubsystem
  with HasBertDebug
  with HasPatternMemDebug
  with HasBitStufferDebug4Modes
  with HasBitReversalDebug

case class HbwifAXI4SParameters(
  numBanks: Int,
  masterParams: AXI4StreamMasterPortParameters,
  slaveParams: AXI4StreamSlavePortParameters
)

case object HbwifAXI4SKey extends Field[HbwifAXI4SParameters]

abstract class AXI4StreamHbwifModule()(implicit p: Parameters) extends LazyModule {

  val lanes = p(HbwifNumLanes)

  txNodes = (0 until lanes).map { id =>
    AXI4StreamToBundleBridge(p(HbwifAXI4SKey).slaveParams)
  }

  rxNodes = (0 until lanes).map { id =>
    BundleBridgeToAXI4Stream(p(HbwifAXI4SKey).masterParams)
  }

  val registerNodes = (0 until lanes).map { id => TLRegisterNode(
    address            = List(configAddressSets(id)),
    device             = new SimpleDevice(s"HbwifConfig$id", Seq(s"ucb-bar,hbwif$id")),
    beatBytes          = p(PeripheryBusKey).beatBytes
  )}
  val configBuffers = (0 until lanes).map { id =>
    LazyModule(new TLBuffer())
  }
  val configNodes = (0 until lanes).map { id =>
    registerNodes(id) := configBuffers(id).node
    configBuffers(id).node
  }

  lazy val module = new LazyModuleImp(this) {
    val banks = p(HbwifAXI4SKey).numBanks
    require(lanes % banks == 0)

    // These go to clock receivers
    val hbwifRefClocks = IO(Input(Vec(banks, Clock())))
    // These go to mmio registers
    val hbwifResets = IO(Input(Vec(lanes, Bool())))
    // This is top-level reset
    val resetAsync = IO(Input(Bool()))

    val tx = IO(Vec(lanes, new Differential()))
    val rx = IO(Vec(lanes, Flipped(new Differential())))

    val (laneModules, addrmaps) = (0 until lanes).map({ id =>
      val (registerIn, registerEdge) = registerNodes(id).in(0)
      configBuffers(id).module.suggestName(s"hbwif_config_port_$id")
      registerIn.suggestName(s"hbwif_register_port_$id")
      val pipelinedReset = withReset(resetAsync) {
         AsyncResetShiftReg(reset.toBool, depth = p(HbwifPipelineResetDepth), init = 1)
      }

      val axi4sParams = rxNodes(id).out.params
      require(rxNodes(id).out.params == txNodes(id).in.params, "RX and TX must be symmetrically configured")

      val lane = Module(genLane(axi4sParams))
      val regmap = withReset(pipelinedReset) { lane.regmap }
      val addrmap = TLController.toAddrmap(regmap)
      registerNodes(id).regmap(regmap:_*)

      lane.io.data.tx <> txNodes(id).in
      rxNodes(id).out <> lane.io.data.rx

      tx(id) <> lane.io.tx
      lane.io.rx <> rx(id)
      lane.io.clockRef <> hbwifRefClocks(id/(lanes/banks))
      lane.io.txAsyncResetIn <> hbwifResets(id)
      lane.io.rxAsyncResetIn <> hbwifResets(id)
      lane.reset := pipelinedReset
      configBuffers(id).module.reset := pipelinedReset
      (lane, addrmap)
    }).unzip
  }

  def genLane(axi4sParams: AXI4StreamBundleParameters)(implicit p: Parameters): AXI4StreamLane8b10b
}

class GenericAXI4StreamHbwifModule()(implicit p: Parameters) extends AXI4StreamHbwifModule()(p) {

  def genLane(axi4sParams: AXI4StreamBundleParameters)(implicit p: Parameters) = {
    new GenericAXI4StreamLane8b10b(axi4sParams)
  }

}
