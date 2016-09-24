package hbwif

import Chisel._
import cde._
//import rocketchip._
import coreplex._

class DefaultHbwifConfig extends Config(
  (pname,site,here) => pname match {
    case BertKey => BertParameters()
    case TransceiverKey => TransceiverParameters()
    case HbwifKey => HbwifParameters(
      numLanes = site(NMemoryChannels),
      bufferDepth = 10)
  }
)

class ExampleHbwifConfig extends Config(
  (pname,site,here) => pname match {
    case BertKey => BertParameters() // Default is in Bert.scala
    case TransceiverKey => TransceiverParameters(
      name = "example_transceiver",
      extraInputs = Some(new ExampleTransceiverExtraInputs),
      extraOutputs = None,
      hasIRef = true,
      refGenHasInput = true,
      refGenConfig = Some(new ExampleRefGenConfig),
      refGenName = "example_reference_generator",
      divideBy = 5,
      isDDR = true
    )
    case HbwifKey => HbwifParameters(
      numLanes = site(NMemoryChannels),
      bufferDepth = 10)
  }
)

class ExampleTransceiverExtraInputs extends Bundle {
  val txSwing = UInt(width = 4)
  val cdrMode = Bool()
}

class ExampleRefGenConfig extends Bundle {
  val mirrorMultiplier = UInt(width = 2)
}

