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
//    case TMemoryChannels => BusType.TL
  }
)
