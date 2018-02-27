package hbwif2

import chisel3._
import chisel3.util._


class Decoded128b130bSymbol extends DecodedSymbol {

    val decodedWidth = 8
    val encodedWidth = 130
    val rate = 16

    val control = Bool()

    override def ack: Decoded128b130bSymbol = Decoded128b130bSymbol(4, true)
    override def nack: Decoded128b130bSymbol = Decoded128b130bSymbol(2, true)
    override def sync: Decoded128b130bSymbol = Decoded128b130bSymbol(1, true)
    override def comma: Decoded128b130bSymbol = Decoded128b130bSymbol(0, true)

    override def fromData(x: UInt): Decoded128b130bSymbol = Decoded128b130bSymbol(x, false.B)
    override def isData: Bool = ~control

}

object Decoded128b130bSymbol {

    def apply(): Decoded128b130bSymbol = new Decoded128b130bSymbol

    def apply(bits: UInt, control: Bool): Decoded128b130bSymbol = {
        val x = new Decoded128b130bSymbol
        x.control := control
        x.bits := bits
        x
    }

    def apply(bits: Int, control: Boolean): Decoded128b130bSymbol = Decoded128b130bSymbol(bits.U, control.B)

}

class Encoder128b130b(val decodedSymbolsPerCycle: Int, val performanceEffort: Int = 0) extends Encoder {

    type DecodedSymbolType = Decoded128b130bSymbol
    val symbolFactory = Decoded128b130bSymbol.apply _

    require(decodedSymbolsPerCycle <= 16, "FIXME, multiple encoded frames per cycle not implemented")

    // TODO

}

class Decoder128b130b(val decodedSymbolsPerCycle: Int, val performanceEffort: Int = 0) extends Decoder {

    type DecodedSymbolType = Decoded128b130bSymbol
    val symbolFactory = Decoded128b130bSymbol.apply _

    require(decodedSymbolsPerCycle <= 16, "FIXME, multiple encoded frames per cycle not implemented")

    // TODO
}



trait HasEncoding128b130b {
    implicit val c: SerDesGeneratorConfig
    // This basically treats the output bitwidth as if it has 0% overhead,
    // but compensates for the two extra bits per 128 with the c.dataWidth/65 term (2/130 = 1/65)
    val optimalSymbolsPerCycle = (c.dataWidth + 7 - c.dataWidth/65) / 8
    val encoder = Module(new Encoder128b130b(optimalSymbolsPerCycle, c.performanceEffort))
    val decoder = Module(new Decoder128b130b(optimalSymbolsPerCycle, c.performanceEffort))
}
