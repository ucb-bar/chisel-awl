package hbwif

import chisel3._
import chisel3.util._


class Decoded128b130bSymbol extends DecodedSymbol(8, 130, 16) {

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

class Encoder128b130b(decodedSymbolsPerCycle: Int) extends Encoder(decodedSymbolsPerCycle) {

    type S = Decoded128b130bSymbol
    def symbolFactory = Decoded128b130bSymbol.apply

    require(decodedSymbolsPerCycle <= 16, "FIXME, multiple encoded frames per cycle not implemented")

    val io = IO(new EncoderIO(symbolFactory, decodedSymbolsPerCycle))
    // TODO

    def connectController(builder: ControllerBuilder) { ??? }
}

class Decoder128b130b(decodedSymbolsPerCycle: Int) extends Decoder(decodedSymbolsPerCycle) {

    type S = Decoded128b130bSymbol
    def symbolFactory = Decoded128b130bSymbol.apply

    require(decodedSymbolsPerCycle <= 16, "FIXME, multiple encoded frames per cycle not implemented")

    val io = IO(new DecoderIO(symbolFactory, decodedSymbolsPerCycle))
    // TODO

    def connectController(builder: ControllerBuilder) { ??? }
}



trait HasEncoding128b130b {
    implicit val c: SerDesConfig
    // This basically treats the output bitwidth as if it has 0% overhead,
    // but compensates for the two extra bits per 128 with the c.dataWidth/65 term (2/130 = 1/65)
    def decodedSymbolsPerCycle = (c.dataWidth + 7 - c.dataWidth/65) / 8
    final def denEncoder() = new Encoder128b130b(decodedSymbolsPerCycle)
    final def genDecoder() = new Decoder128b130b(decodedSymbolsPerCycle)
}
