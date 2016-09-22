package hbwif

import Chisel._
import scala.collection.immutable.Seq

object Encoding8b10b {

  private val encodings5b6b = Seq(
    // In here the input is prepended with K and RD
    // data codes
    // K=0 RD=-1               K=0 RD=+1
    ("0 0 00000" -> "100111"), ("0 1 00000" -> "011000"),
    ("0 0 00001" -> "011101"), ("0 1 00001" -> "100010"),
    ("0 0 00010" -> "101101"), ("0 1 00010" -> "010010"),
    ("0 0 00011" -> "110001"), ("0 1 00011" -> "110001"),
    ("0 0 00100" -> "110101"), ("0 1 00100" -> "001010"),
    ("0 0 00101" -> "101001"), ("0 1 00101" -> "101001"),
    ("0 0 00110" -> "011001"), ("0 1 00110" -> "011001"),
    ("0 0 00111" -> "111000"), ("0 1 00111" -> "000111"),
    ("0 0 01000" -> "111001"), ("0 1 01000" -> "000110"),
    ("0 0 01001" -> "100101"), ("0 1 01001" -> "100101"),
    ("0 0 01010" -> "010101"), ("0 1 01010" -> "010101"),
    ("0 0 01011" -> "110100"), ("0 1 01011" -> "110100"),
    ("0 0 01100" -> "001101"), ("0 1 01100" -> "001101"),
    ("0 0 01101" -> "101100"), ("0 1 01101" -> "101100"),
    ("0 0 01110" -> "011100"), ("0 1 01110" -> "011100"),
    ("0 0 01111" -> "010111"), ("0 1 01111" -> "101000"),
    ("0 0 10000" -> "011011"), ("0 1 10000" -> "100100"),
    ("0 0 10001" -> "100011"), ("0 1 10001" -> "100011"),
    ("0 0 10010" -> "010011"), ("0 1 10010" -> "010011"),
    ("0 0 10011" -> "110010"), ("0 1 10011" -> "110010"),
    ("0 0 10100" -> "001011"), ("0 1 10100" -> "001011"),
    ("0 0 10101" -> "101010"), ("0 1 10101" -> "101010"),
    ("0 0 10110" -> "011010"), ("0 1 10110" -> "011010"),
    ("0 0 10111" -> "111010"), ("0 1 10111" -> "000101"),
    ("0 0 11000" -> "110011"), ("0 1 11000" -> "001100"),
    ("0 0 11001" -> "100110"), ("0 1 11001" -> "100110"),
    ("0 0 11010" -> "010110"), ("0 1 11010" -> "010110"),
    ("0 0 11011" -> "110110"), ("0 1 11011" -> "001001"),
    ("0 0 11100" -> "001110"), ("0 1 11100" -> "001110"),
    ("0 0 11101" -> "101110"), ("0 1 11101" -> "010001"),
    ("0 0 11110" -> "011110"), ("0 1 11110" -> "100001"),
    ("0 0 11111" -> "101011"), ("0 1 11111" -> "010100"),
    // Control codes
    // For simplicity, we omit K.*.7, so only K.28 is needed for the 5b/6b code
    // K=1 RD = -1            K=1 RD = +1
    ("1 0 11100" -> "001111"), ("1 1 11100" -> "110000") // K.28
  ).map({ x => (Integer.parseInt(x._1.replaceAll(" ","")) -> Integer.parseInt(x._2.replaceAll(" ",""))) }).toMap

  // magic bit count algorithm
  private def bitCount(j: Int): Int = {
    var i = j
    i = i - ((i >>> 1) & 0x55555555)
    i = (i & 0x33333333) + ((i >>> 2) & 0x33333333)
    return (((i + (i >>> 4)) & 0x0F0F0F0F) * 0x01010101) >>> 24
  }

  // extract the RD bit from the 5b6b LUT
  private def get5b6bRd(input: Int): Int = (input >> 5) & 0x1

  // calculate the input RD to the 3b4b block
  private def intermediate5b6bRd(rd: Int, code: Int): Int = {
    if (rd != 0 && rd != 1) { throw new Exception }
    bitCount(code) match {
      case 2 => if (rd == 1) { return 0 } else { throw new Exception }
      case 3 => return rd
      case 4 => if (rd == 0) { return 1 } else { throw new Exception }
      case _ => throw new Exception
    }
    return 0
  }

  private val encodings3b4b = Seq(
    // In here the input is prepended with K, RD, and "use alt" (for D.x.7, see below)
    // data codes
    // K=0 RD=-1 UA=0        K=0 RD=+1 UA=0
    ("0 0 0 000" -> "1011"), ("0 1 0 000" -> "0100"),
    ("0 0 0 001" -> "1001"), ("0 1 0 001" -> "1001"),
    ("0 0 0 010" -> "0101"), ("0 1 0 010" -> "0101"),
    ("0 0 0 011" -> "1100"), ("0 1 0 011" -> "0011"),
    ("0 0 0 100" -> "1101"), ("0 1 0 100" -> "0010"),
    ("0 0 0 101" -> "1010"), ("0 1 0 101" -> "1010"),
    ("0 0 0 110" -> "0110"), ("0 1 0 110" -> "0110"),
    ("0 0 0 111" -> "1110"), ("0 1 0 111" -> "0001"),
    // K=0 RD=-1 UA=1        K=0 RD=+1 UA=1
    ("0 0 1 000" -> "1011"), ("0 1 1 000" -> "0100"),
    ("0 0 1 001" -> "1001"), ("0 1 1 001" -> "1001"),
    ("0 0 1 010" -> "0101"), ("0 1 1 010" -> "0101"),
    ("0 0 1 011" -> "1100"), ("0 1 1 011" -> "0011"),
    ("0 0 1 100" -> "1101"), ("0 1 1 100" -> "0010"),
    ("0 0 1 101" -> "1010"), ("0 1 1 101" -> "1010"),
    ("0 0 1 110" -> "0110"), ("0 1 1 110" -> "0110"),
    ("0 0 1 111" -> "0111"), ("0 1 1 111" -> "1000"), // Note that only this one is different
    // Control codes
    // For simplicity, we omit K.*.7, so only K.28 is needed for the 5b/6b code
    // The tables are identical for UA=0 and UA=1
    // K=1 RD=-1 UA=0        K=1 RD=+1 UA=0
    ("1 0 0 000" -> "1011"), ("1 1 0 000" -> "0100"),
    ("1 0 0 001" -> "0110"), ("1 1 0 001" -> "1001"),
    ("1 0 0 010" -> "1010"), ("1 1 0 010" -> "0101"),
    ("1 0 0 011" -> "1100"), ("1 1 0 011" -> "0011"),
    ("1 0 0 100" -> "1101"), ("1 1 0 100" -> "0010"),
    ("1 0 0 101" -> "0101"), ("1 1 0 101" -> "1010"),
    ("1 0 0 110" -> "1001"), ("1 1 0 110" -> "0110"),
    // K.x.7 is omitted
    // K=1 RD=-1 UA=1        K=1 RD=+1 UA=1
    ("1 0 1 000" -> "1011"), ("1 1 1 000" -> "0100"),
    ("1 0 1 001" -> "0110"), ("1 1 1 001" -> "1001"),
    ("1 0 1 010" -> "1010"), ("1 1 1 010" -> "0101"),
    ("1 0 1 011" -> "1100"), ("1 1 1 011" -> "0011"),
    ("1 0 1 100" -> "1101"), ("1 1 1 100" -> "0010"),
    ("1 0 1 101" -> "0101"), ("1 1 1 101" -> "1010"),
    ("1 0 1 110" -> "1001"), ("1 1 1 110" -> "0110")
    // K.x.7 is omitted
  ).map({ x => (Integer.parseInt(x._1.replaceAll(" ","")) -> Integer.parseInt(x._2.replaceAll(" ",""))) }).toMap

  // construct the big LUT from the two intermediate LUTs
  val encodings =
    // k, rd, code (10 bit input)
    (0 to (1<<10)).map { i =>
      val k = (i >> 9) & 0x1
      val rd = (i >> 8) & 0x1
      val edcba = i & 0x1f
      val hgf = (i >> 5) & 0x7
      val lutIn = (k << 6) | (rd << 5) | edcba
      if (encodings5b6b.contains(lutIn)) {
        val abcdei = encodings5b6b(lutIn)
        val ua = if (((edcba == 17 || edcba == 18 || edcba == 20) && rd == 0) || ((edcba == 11 || edcba == 13 || edcba == 14) && rd == 1)) 1 else 0
        val fghj = encodings3b4b((k << 5) | (intermediate5b6bRd(rd, abcdei) << 4) | (ua << 3) | hgf)
        // k, rd, input, output
        Some( ( Decoded8b10bSymbol(1, k, rd, i & 0xff).asUInt, Encoded8b10bSymbol(1, rd, (abcdei << 4) | fghj).asUInt ) )
      } else {
        None
      }
    }.filter(!_.isEmpty).map(_.get)

  // input a 10-bit UInt and output an aligned (or invalid) Encoded8b10bSymbol
  def align(d: UInt): Encoded8b10bSymbol = {
    val buf = Reg(next = d(8,0))
    val idx = Reg(init = UInt(0, width=4))
    val lock = Reg(init = Bool(false))
    val rd = Reg(init = Bool(false))

    val wires = Vec( (0 to 8).map { i => Cat(buf, d)(9+i, i) } )
    val commas = wires.map { commaDetect(_) }
    val found = commas.reduce { _ || _ }
    val idxs = commas.zipWithIndex.map { case (b, i) => (b, UInt(i)) }
    val next_idx = MuxCase(UInt(0), idxs)

    when(found) {
      idx := next_idx
      lock := Bool(true)
      // Force RD to a value based on the comma sequence
      rd := ~wires(next_idx)(5)
    } .elsewhen(wires(idx).xorR) {
      // Assume we got a valid symbol, if there are an uneven number of 1s and 0s, rd should flip
      rd := ~rd
    }

    Encoded8b10bSymbol(lock, rd, wires(idx))
  }

  // Check that bits cdeif (7,3) are the same
  def commaDetect(d: UInt): Bool = d(7,3).andR || ~(d(7,3).orR)

  // Default comma sequence for the encoder (K.28.5)
  val defaultComma = UInt(Integer.parseInt("10111100",2))

}

class Encoded8b10bSymbol extends Bundle {
  val data = UInt(width=10)
  val valid = Bool()
  val rd = Bool()

  // Abuse the fact that the only valid combinations of 1s and 0s is 6,4 5,5 and 4,6
  // and that the decoder should handle bit flips
  def nextRd(): Bool = Mux(data.xorR, rd, ~rd)

  def decode(): Decoded8b10bSymbol = {
    (new Decoded8b10bSymbol).fromBits(MuxLookup(this.asUInt, Decoded8b10bSymbol(0, 0, 0, 0).asUInt, Encoding8b10b.encodings.map { case (enc,dec) => (dec,enc) } ))
  }
}

object Encoded8b10bSymbol {

  def apply(v: Bool, rd: Bool, d: UInt): Encoded8b10bSymbol = {
    val s = Wire(new Encoded8b10bSymbol)
    s.valid := v
    s.data := d
    s.rd := rd
    s
  }

  def apply(v: Int, rd: Int, d: Int): Encoded8b10bSymbol = {
    Encoded8b10bSymbol(Bool(v == 1), Bool(rd == 1), UInt(d))
  }

}

class Decoded8b10bSymbol extends Bundle {
  val data = UInt(width=8)
  val control = Bool()
  val valid = Bool()
  val rd = Bool()

  def encode(): Encoded8b10bSymbol = {
    (new Encoded8b10bSymbol).fromBits(MuxLookup(this.asUInt, Encoded8b10bSymbol(0, 0, 0).asUInt, Encoding8b10b.encodings))
  }

}

object Decoded8b10bSymbol {

  def apply(v: Bool, k: Bool, rd: Bool, d: UInt): Decoded8b10bSymbol = {
    val s = Wire(new Decoded8b10bSymbol)
    s.valid := v
    s.control := k
    s.rd := rd
    s.data := d
    s
  }

  def apply(v: Int, k: Int, rd: Int, d: Int): Decoded8b10bSymbol = {
    Decoded8b10bSymbol(Bool(v == 1), Bool(k == 1), Bool(rd == 1), UInt(d))
  }

}

class Channel8b10b extends Bundle {
  val decoded = UInt(INPUT, width=8)
  val control = Bool(INPUT)
  val valid = Bool(INPUT)
  val encoded = UInt(OUTPUT, width=10)
}

class Encoder8b10b extends Module {

  val io = new Channel8b10b

  val rd = Reg(init = Bool(false))

  val encoded = Decoded8b10bSymbol(Bool(true), io.control || ~io.valid, rd, Mux(io.valid, Encoding8b10b.defaultComma, io.decoded)).encode

  rd := encoded.nextRd
  io.encoded := encoded.data
}

class Decoder8b10b extends Bundle {

  val io = (new Channel8b10b).flip

  val encoded = Encoding8b10b.align(io.encoded)
  val decoded = encoded.decode

  io.valid := decoded.valid
  io.control := decoded.control
  io.decoded := decoded.data

}
