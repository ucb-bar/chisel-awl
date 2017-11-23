// See LICENSE for license details.

package hbwif2

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class HBWIFTester extends ChiselFlatSpec {
  private val backendNames = Array("vcs")

  for ( backendName <- backendNames ) {
    for ( config <- SerDesGeneratorConfigs() ) {

      "CDR" should s"do something fancy with $backendName" in {
        Driver(() => new CDR()(config), backendName) {
          c => new CDRUnitTester(c)
        } should be (true)
      }

      "Transceiver" should s"send and receive bits with $backendName" in {
        Driver(() => new TransceiverPair()(config), backendName) {
          c => new TransceiverUnitTester(c)
        } should be (true)
      }
    }
  }
}
