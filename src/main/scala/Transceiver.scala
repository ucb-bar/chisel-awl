// johnwright@eecs.berkeley.edu

package hbwif

import Chisel._
import ChiselError._

class TransceiverRxConfig extends Bundle {
  // Global config bits
  val cvd     = Bits(INPUT, width = 9)
  val delay   = Bits(INPUT, width = 3)
  // Slice 1 config bits
  val ioff1   = Bits(INPUT, width = 8)
  val ioffen1 = Bits(INPUT, width = 3)
  val irefen1 = Bits(INPUT, width = 3)
  val it1     = Bits(INPUT, width = 10)
  val sw1     = Bits(INPUT, width = 4)
  val clken1  = Bool(INPUT)
  // Slice 2 config bits
  val ioff2   = Bits(INPUT, width = 8)
  val ioffen2 = Bits(INPUT, width = 3)
  val irefen2 = Bits(INPUT, width = 3)
  val it2     = Bits(INPUT, width = 10)
  val sw2     = Bits(INPUT, width = 4)
  val clken2  = Bool(INPUT)
  // Slice 3 config bits
  val ioff3   = Bits(INPUT, width = 8)
  val ioffen3 = Bits(INPUT, width = 3)
  val irefen3 = Bits(INPUT, width = 3)
  val it3     = Bits(INPUT, width = 10)
  val sw3     = Bits(INPUT, width = 4)
  val rx3del  = Bits(INPUT, width = 8)
  val clken3  = Bool(INPUT)
  // VCM on/off switch
  val vcmsw   = Bool(INPUT)
}

class TransceiverTxConfig extends Bundle {
  val en    = Bool(INPUT)
  val xor   = Bool(INPUT)
  val swing = Bits(INPUT, width = 4)
}

class TransceiverIO(num_clocks: Int = 4) extends Bundle {

  // high speed clock input(s)
  val clks = Vec.fill(num_clocks) { Bool(INPUT) }

  // rx pad inputs
  val rx_inp = Bool(INPUT)
  val rx_inn = Bool(INPUT)

  // tx pad inputs
  val tx_outp = Bool(OUTPUT)
  val tx_outn = Bool(OUTPUT)

  // rx internal outputs
  // TODO parameterize this
  val rx_out1 = Bits(OUTPUT, width = 2)
  val rx_out2 = Bits(OUTPUT, width = 2)
  val rx_out3 = Bits(OUTPUT, width = 2)

  // tx internal inputs
  val tx_in = Bits(INPUT, width = 2)

  // config stuff
  val rx_config = new TransceiverRxConfig
  val tx_config = new TransceiverTxConfig

  // analog stuff
  val rx_vcm  = Bool(INPUT)
  val tx_iref = Bool(INPUT)
  val rx_iref = Bool(INPUT)
  val rx_ioff = Bool(INPUT)
}

class Transceiver(num_clocks: Int = 4) extends BlackBox {
  val io = new TransceiverIO(num_clocks)

  moduleName = "hurricane_hbwif_top"
}
