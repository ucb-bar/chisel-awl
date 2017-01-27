package hbwif

import Chisel._
import cde._
import uncore.tilelink._
import uncore.agents._
import rocketchip.{BaseConfig,GenerateGlobalAddrMap}
import unittest._
import coreplex._
import scala.math.min

object HbwifKeyHelper {
  def apply(site: View, bufferDepth: Int): HbwifParameters = {
    val tlConfig = site(TLKey("Switcher"))
    val maxBufferDepth = tlConfig.maxClientsPerPort * tlConfig.maxClientXacts
    HbwifParameters(
      numLanes = site(NMemoryChannels),
      bufferDepth = min(bufferDepth, 1 << log2Ceil(maxBufferDepth)))
  }
}

class WithBufferDepth(bufferDepth: Int) extends Config(
  (pname,site,here) => pname match {
    case HbwifKey => HbwifKeyHelper(site, bufferDepth)
    case _ => throw new CDEMatchError
  })

class DefaultHbwifConfig extends Config(
  (pname,site,here) => pname match {
    case BertKey => BertParameters()
    case TransceiverKey => TransceiverParameters()
    case HbwifKey => HbwifKeyHelper(site, 16)
    case _ => throw new CDEMatchError
  }
)

class ExampleHbwifConfig extends Config(
  (pname,site,here) => pname match {
    case BertKey => BertParameters() // Default is in Bert.scala
    case TransceiverKey => TransceiverParameters(
      name = "example_transceiver",
      extraInputs = Some(new ExampleTransceiverExtraInputs),
      extraOutputs = None,
      numIrefs = 1,
      refGenHasInput = true,
      refGenConfig = Some(new ExampleRefGenConfig),
      refGenName = "example_reference_generator",
      refGenNumOutputs = 8,
      divideBy = 5,
      isDDR = true
    )
    case HbwifKey => HbwifKeyHelper(site, 16)
    case _ => throw new CDEMatchError
  }
)

class ExampleTransceiverExtraInputs extends Bundle {
  val txSwing = UInt(width = 4)
  val cdrMode = Bool()
}

class ExampleRefGenConfig extends Bundle {
  val mirrorMultiplier = UInt(width = 2)
}

class WithHbwifUnitTests extends Config(
  (pname, site, here) => pname match {
    case HbwifKey => HbwifParameters(
      numLanes = 1,
      bufferDepth = 16)
    case TLKey("Switcher") =>
      site(TLKey("L2toMC")).copy(
        maxClientXacts = site(NAcquireTransactors) + 2,
        maxClientsPerPort = site(NBanksPerMemoryChannel) * site(NMemoryChannels))
    case TLKey("MMIOtoSCR") => {
      val scrDataBits = 64
      val scrDataBeats = (8 * site(CacheBlockBytes)) / scrDataBits
      site(TLKey("L2toMMIO")).copy(
        maxClientsPerPort = 3,
        dataBeats = scrDataBeats)
    }
    case TransceiverKey => TransceiverParameters(
      name = "example_transceiver",
      extraInputs = Some(new ExampleTransceiverExtraInputs),
      extraOutputs = None,
      numIrefs = 1,
      refGenHasInput = true,
      refGenConfig = Some(new ExampleRefGenConfig),
      refGenName = "example_reference_generator",
      refGenNumOutputs = 8,
      divideBy = 5,
      isDDR = true
    )
    case BertKey => BertParameters()
    case UnitTests => (testParams: Parameters) =>
      HbwifUnitTests(testParams)
    case TLId => "Switcher"
    case _ => throw new CDEMatchError
  })

class HbwifUnitTestConfig extends Config(
  new BaseConfig ++ new WithHbwifUnitTests)
