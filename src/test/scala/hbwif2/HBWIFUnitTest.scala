// See LICENSE for license details.

package hbwif2

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

/*
class HBWIFUnitTester(c: HBWIF) extends PeekPokeTester(c) {

}
*/

class CDRUnitTester(c: CDR) extends PeekPokeTester(c) {

}

class TransceiverUnitTester(c: TransceiverSubsystem) extends PeekPokeTester(c) {


}

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
        Driver(() => new TransceiverSubsystem()(config), backendName) {
          c => new TransceiverUnitTester(c)
        } should be (true)
      }
    }
  }
}
