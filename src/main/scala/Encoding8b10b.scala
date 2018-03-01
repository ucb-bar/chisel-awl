package hbwif2

import chisel3._
import chisel3.util._

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
    // K=1 RD = -1            K=1 RD = +1
    ("1 0 10111" -> "111010"), ("1 1 10111" -> "000101"), // K.23 == D.23
    ("1 0 11011" -> "110110"), ("1 1 11011" -> "001001"), // K.27 == D.27
    ("1 0 11100" -> "001111"), ("1 1 11100" -> "110000"), // K.28
    ("1 0 11101" -> "101110"), ("1 1 11101" -> "010001"), // K.29 == D.29
    ("1 0 11110" -> "011110"), ("1 1 11110" -> "100001")  // K.30 == D.30
  ).map({ x => (Integer.parseInt(x._1.replaceAll(" ",""),2) -> Integer.parseInt(x._2.replaceAll(" ",""),2)) }).toMap

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
      case _ => throw new Exception(s"Got an invalid bit count for $code: expected 2, 3, or 4")
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
    // The tables are identical for UA=0 and UA=1
    // K=1 RD=-1 UA=0        K=1 RD=+1 UA=0
    ("1 0 0 000" -> "1011"), ("1 1 0 000" -> "0100"),
    ("1 0 0 001" -> "0110"), ("1 1 0 001" -> "1001"),
    ("1 0 0 010" -> "1010"), ("1 1 0 010" -> "0101"),
    ("1 0 0 011" -> "1100"), ("1 1 0 011" -> "0011"),
    ("1 0 0 100" -> "1101"), ("1 1 0 100" -> "0010"),
    ("1 0 0 101" -> "0101"), ("1 1 0 101" -> "1010"),
    ("1 0 0 110" -> "1001"), ("1 1 0 110" -> "0110"),
    ("1 0 0 111" -> "0111"), ("1 1 0 111" -> "1000"),
    // K=1 RD=-1 UA=1        K=1 RD=+1 UA=1
    ("1 0 1 000" -> "1011"), ("1 1 1 000" -> "0100"),
    ("1 0 1 001" -> "0110"), ("1 1 1 001" -> "1001"),
    ("1 0 1 010" -> "1010"), ("1 1 1 010" -> "0101"),
    ("1 0 1 011" -> "1100"), ("1 1 1 011" -> "0011"),
    ("1 0 1 100" -> "1101"), ("1 1 1 100" -> "0010"),
    ("1 0 1 101" -> "0101"), ("1 1 1 101" -> "1010"),
    ("1 0 1 110" -> "1001"), ("1 1 1 110" -> "0110"),
    ("1 0 1 111" -> "0111"), ("1 1 1 111" -> "1000")
  ).map({ x => (Integer.parseInt(x._1.replaceAll(" ",""),2) -> Integer.parseInt(x._2.replaceAll(" ",""),2)) }).toMap

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
        val lutIn2 = (k << 5) | (intermediate5b6bRd(rd, abcdei) << 4) | (ua << 3) | hgf
        if (encodings3b4b.contains(lutIn2)) {
          val fghj = encodings3b4b(lutIn2)
          // k, rd, input, output
          Some( ( (k == 1, rd == 1, i & 0xff), abcdei << 4 | fghj) )
        } else {
          None
        }
      } else {
        None
      }
    }.filter(!_.isEmpty).map(_.get)

}

class Decoded8b10bSymbol extends DecodedSymbol(8, 10, 1) {

    val control = Output(Bool())

    override def ack: Decoded8b10bSymbol = Decoded8b10bSymbol.k(28,2)
    override def nack: Decoded8b10bSymbol = Decoded8b10bSymbol.k(28,3)
    override def sync: Decoded8b10bSymbol = Decoded8b10bSymbol.k(28,4)
    override def comma: Decoded8b10bSymbol = Decoded8b10bSymbol.comma

    override def fromData(x: UInt): Decoded8b10bSymbol = Decoded8b10bSymbol(x, false.B)
    override def isData: Bool = ~control

    def encode(rd: Bool): UInt = {
        MuxLookup(Cat(control,rd,bits), 0.U, Encoding8b10b.encodings.map
            { case ((k,r,d),enc) => (Cat(k.B,r.B,d.U(8.W)), enc.U(10.W)) })
    }

}

object Decoded8b10bSymbol {

    def apply(): Decoded8b10bSymbol = new Decoded8b10bSymbol

    def apply(bits: UInt, control: Bool): Decoded8b10bSymbol = {
        val x = Wire(new Decoded8b10bSymbol)
        x.control := control
        x.bits := bits
        x
    }

    def apply(tuple: (Int, Boolean)): Decoded8b10bSymbol = Decoded8b10bSymbol.apply(tuple._1.U, tuple._2.B)


    // Helper method to easily recognize Kcodes
    def k(edcba: Int, hgf: Int): Decoded8b10bSymbol = {
        require(hgf >= 0 && hgf <= 7, "HGF must be 0-7")
        require(edcba == 28 || edcba == 23 || edcba == 27 || edcba == 29 || edcba == 30, "EDCBA must be 28, 23, 27, 29, or 30")
        require(edcba == 28 || (hgf == 7), "HGF must be 7 for K codes other than EDCBA=28")
        require(edcba != 28 || hgf != 7, "K.28.7 is explicitly disallowed in this implementation because it would complicate comma detection")
        Decoded8b10bSymbol((hgf << 5 | edcba).U(8.W), true.B)
    }

    def comma: Decoded8b10bSymbol = Decoded8b10bSymbol.k(28,5)

    def decode(encoded: UInt, rd: Bool, valid: Bool): Valid[Decoded8b10bSymbol] = {
        val x = Wire(Valid(Decoded8b10bSymbol()))
        x.bits := MuxLookup(encoded, Decoded8b10bSymbol(0.U, false.B), Encoding8b10b.encodings.map
            { case ((k,r,d),enc) => (enc.U(10.W), Decoded8b10bSymbol(d.U,k.B)) })
        x.valid := valid & MuxLookup(encoded, false.B, Encoding8b10b.encodings.map
            { case ((k,r,d),enc) => (enc.U(10.W), true.B) })
        x
    }

}

// Note that this only checks for a comma in the 0th symbol position (if decodedSymbolsPerCycle > 1),
// so at least decodedSymbolsPerCycle commas must be transmitted to lock
// TODO right now this ignores RD altogether
// TODO need a flag to alert the user when lock or idx changes
// See Encoder8b10b notes below for symbol and bit ordering
class Decoder8b10b(decodedSymbolsPerCycle: Int, val performanceEffort: Int = 0) extends Decoder(Decoded8b10bSymbol.apply _, decodedSymbolsPerCycle) {

    val idx = RegInit(0.U(4.W))
    val lock = RegInit(false.B)
    val rd = RegInit(false.B)

    val extended = Cat(RegNext(io.encoded.bits(8,0)),io.encoded.bits)
    val offsets = Vec( (0 to 9).map { i => extended(decodedSymbolsPerCycle*10+i-1, i) } )
    // Check that bits cdeif (7,3) are the same (this defines a comma)
    val commas = offsets.map { x => (x(9,3) === "b0011111".U) || (x(9,3) === "b1100000".U) }
    val found = commas.reduce(_||_)
    val nextIdx = MuxCase(0.U, commas.zipWithIndex.map { case (b, i) => (b, i.U) })

    when (found) {
        idx := nextIdx
        lock := true.B
    }

    (0 until decodedSymbolsPerCycle).foreach { i =>
        io.decoded(i) := Decoded8b10bSymbol.decode(offsets(Mux(found,nextIdx,idx))(i*10+9,i*10), rd, io.encoded.valid)
    }

}

// io.decoded(MSB) is the LEAST recent time symbol
// io.decoded(LSB) is the MOST recent time symbol
// e.g. if we send A B C D E F G with decodedSymbolsPerCycle = 3,
// io.decoded(2) = A
// io.decoded(1) = B
// io.decoded(0) = C
//
// Similarly, this expects the encoded interface to go MSB..LSB with MSB being sent over the line first
class Encoder8b10b(decodedSymbolsPerCycle: Int, val performanceEffort: Int = 0) extends Encoder(Decoded8b10bSymbol.apply _, decodedSymbolsPerCycle) {

    val rd = RegInit(false.B)

    // ready to encode when next is high
    io.decodedReady := io.next

    val (e,r) = io.decoded.foldRight((Option.empty[UInt],rd)) { case (decoded,(prevEncoded,prevRd)) =>
    //val (e,r) = io.decoded.foldLeft((Wire(UInt(0.W)),rd)) { case ((prevEncoded,prevRd),decoded) =>
        val nextEncoded = Wire(UInt())
        val nextRd = Wire(Bool())
        if (performanceEffort <= 0) {
            // Low setting, prioritize area/power
            // Use a ripple topology to calculate RD
            val encoded = Mux(decoded.valid,decoded.bits.encode(prevRd),Decoded8b10bSymbol.comma.encode(prevRd))
            // RD will stay the same when the number of 1s and 0s is the same (5,5)
            // The number of 1s and 0s can only be (4,6), (5,5), or (6,4)
            // Therefore we can just invert RD whenever there is an even number of 1s
            nextRd := prevRd ^ ~encoded.xorR
            nextEncoded := prevEncoded.foldRight(encoded) { Cat(_,_) }
            // nextEncoded := Cat(encoded, prevEncoded)
        } else {
            // High setting, prioritize speed
            // Use a lookahead topology to calculate RD
            val encoded0 = Mux(decoded.valid,decoded.bits.encode(false.B),Decoded8b10bSymbol.comma.encode(false.B))
            val encoded1 = Mux(decoded.valid,decoded.bits.encode(true.B),Decoded8b10bSymbol.comma.encode(true.B))
            // Use the same RD algorithm as above, but reduce the constants out
            val nextRd0 = ~encoded0.xorR
            val nextRd1 = encoded1.xorR
            nextRd := Mux(prevRd, nextRd1, nextRd0)
            nextEncoded := prevEncoded.foldRight(Mux(prevRd,encoded1,encoded0)) { Cat(_,_) }
            // nextEncoded := Cat(Mux(prevRd,encoded1,encoded0), prevEncoded)
        }
        (Some(nextEncoded),nextRd)
    }
    io.encoded := e.get
    when (io.next) {
        rd := r
    }
}

