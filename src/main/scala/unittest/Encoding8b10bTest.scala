package hbwif

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest._

class Encoding8b10bTest(val decodedSymbolsPerCycle: Int, val performanceEffort: Int, val offset: Int, timeout: Int = 50000) extends UnitTest(timeout) {

    val str = s"{decodedSymbolsPerCycle: $decodedSymbolsPerCycle, performanceEffort: $performanceEffort, offset: $offset}"
    val encoder = Module(new Encoder8b10b(decodedSymbolsPerCycle, performanceEffort))
    val decoder = Module(new Decoder8b10b(decodedSymbolsPerCycle))
    val builder = new SymbolGeneratorCheckerBuilder8b10b(decodedSymbolsPerCycle)
    val generator = builder.generator()
    val checker = builder.checker(str)

    val buf = RegInit(0.U((decodedSymbolsPerCycle*20 - 1).W))

    buf := Cat(buf(decodedSymbolsPerCycle*10-2,0),encoder.io.encoded.bits)

    encoder.io.encoded.ready := true.B
    encoder.io.decoded <> generator.io.decoded
    generator.io.decodedReady := encoder.io.decodedReady
    decoder.io.decoded <> checker.io.decoded
    decoder.io.clearError := false.B

    val counter = Counter(10)
    val finished = RegInit(false.B)
    io.finished := finished

    when (reset.toBool) {
        assert(decoder.io.decoded map { _.valid } reduce(_||_), s"We're getting zeros, so this should be invalid in check $str")
    }

    when (checker.io.finished) {
        // Test that if we deassert valid we still get valid commas out
        when (counter.inc()) {
            finished := true.B
        }
        assert(decoder.io.decoded map { _.valid } reduce(_&&_), s"Even when sending commas we should see a valid output here in check $str")
        decoder.io.decoded foreach { (x:Valid[Decoded8b10bSymbol]) =>
            assert(x.bits === Decoded8b10bSymbol.comma, s"Should be getting commas here in check $str")
        }
    }

    decoder.io.encoded.bits := buf(decodedSymbolsPerCycle*10 + (offset % (10*decodedSymbolsPerCycle)) - 1, offset % (10*decodedSymbolsPerCycle))
    decoder.io.encoded.valid := true.B

}

class Symbol8b10bGenerator(val decodedSymbolsPerCycle: Int, val symbols: Seq[(Int,Boolean)]) extends Module {

    val io = IO(new Bundle {
        val decoded = Vec(decodedSymbolsPerCycle, Valid(Decoded8b10bSymbol()))
        val decodedReady = Input(Bool())
    })

    val symbolVec = Vec(symbols map { case x => Decoded8b10bSymbol(x) })
    val counter = Counter(symbols.length / decodedSymbolsPerCycle + 1)
    val initCounter = Counter(5)
    val finished = RegInit(false.B)
    val started = RegInit(false.B)

    when (initCounter.inc()) {
        started := true.B
    }

    io.decoded.reverse.zipWithIndex.foreach { case (x,i) =>
        when (finished || !started) {
            x.valid := false.B
        } .otherwise {
            x.valid := (i.U + (counter.value * decodedSymbolsPerCycle.U)) < symbols.length.U
        }

        (0 until ((symbols.length / decodedSymbolsPerCycle) + 1)).foldLeft(when(false.B) { x.bits := Decoded8b10bSymbol((0, false))}) { (w,j) =>
            w.elsewhen (counter.value === j.U) {
                if (j * decodedSymbolsPerCycle + i < symbols.length) {
                    x.bits := Decoded8b10bSymbol(symbols(j * decodedSymbolsPerCycle + i))
                } else {
                    x.bits := Decoded8b10bSymbol((0, false))
                }
            }
        }.otherwise {
            x.bits := Decoded8b10bSymbol((0, false))
        }
    }

    when (started && io.decodedReady) {
        when (counter.inc()) {
            finished := true.B
        }
    }

}


class Symbol8b10bChecker(val decodedSymbolsPerCycle: Int, val symbols: Seq[(Int,Boolean)], val str: String) extends Module {

    val io = IO(new Bundle {
        val decoded = Vec(decodedSymbolsPerCycle, Flipped(Valid(Decoded8b10bSymbol())))
        val finished = Output(Bool())
    })

    val finished = RegInit(false.B)
    io.finished := finished

    val decodedRev = Vec(io.decoded.reverse)

    val started = RegInit(false.B)
    val startIdx = Reg(UInt(log2Ceil(decodedSymbolsPerCycle).W))
    val first = PriorityEncoder(decodedRev.map { x => x.valid && x.bits === Decoded8b10bSymbol((0, false)) })
    val counter = Counter(symbols.length / decodedSymbolsPerCycle + 2)

    val error = RegInit(false.B)

    val symbolVec = Vec(symbols map { case x => Decoded8b10bSymbol(x) })
    val idx = (counter.value * decodedSymbolsPerCycle.U) - startIdx

    when (started) {
        assert(decodedRev.map { _.valid } reduce(_&&_), s"Once the decoder is locked, we should always see valid data or control symbols in check $str")

        (0 until decodedSymbolsPerCycle) foreach { i =>
            when (((idx + i.U) < symbols.length.U) && !finished) {
                check(symbolVec(idx + i.U), decodedRev(i.U).bits, idx + i.U)
            }
        }

        when (idx + decodedSymbolsPerCycle.U >= symbols.length.U) {
            assert(!error, "Got an error in this test")
            finished := true.B
        } .otherwise {
            counter.inc()
        }


    } .otherwise {
        startIdx := first
        when (decodedRev.map { x => x.valid && x.bits === Decoded8b10bSymbol((0, false)) } reduce(_||_)) {
            started := true.B
            counter.inc()

            (0 until decodedSymbolsPerCycle) foreach { i =>
                when (decodedRev(i).valid && decodedRev(i).bits === Decoded8b10bSymbol((0, false))) {
                    check(symbolVec(i.U - first), decodedRev(i.U).bits, i.U - first)
                }
            }
        }
    }

    def check(e: Decoded8b10bSymbol, g: Decoded8b10bSymbol, i: UInt) {
        when (e =/= g) {
            error := true.B
            switch (Cat(e.control, g.control)) {
                is (0.U) {
                    printf(s"Error: got D.%x, expected D.%x, for symbol %d in check $str\n", g.bits, e.bits, i)
                }
                is (1.U) {
                    printf(s"Error: got K.%x, expected D.%x, for symbol %d in check $str\n", g.bits, e.bits, i)
                }
                is (2.U) {
                    printf(s"Error: got D.%x, expected K.%x, for symbol %d in check $str\n", g.bits, e.bits, i)
                }
                is (3.U) {
                    printf(s"Error: got K.%x, expected K.%x, for symbol %d in check $str\n", g.bits, e.bits, i)
                }
            }
        }
    }

}


class SymbolGeneratorCheckerBuilder8b10b(val decodedSymbolsPerCycle: Int) {

    val allData = (0 until (1<<8)) map { (x:Int) => (x, false) }
    // Note we explicitly omit K.28.7
    val allControls = List((28,0),(28,1),(28,2),(28,3),(28,4),(28,5),(28,6),(23,7),(27,7),(29,7),(30,7)) map { case (x,y) => (y << 5 | x, true) }

    // Use a 0 data to start checking
    val dataSeq = List.fill(decodedSymbolsPerCycle) { (0, false) } ++
        (new scala.util.Random(1234)).shuffle(allData ++ allData ++ allControls ++ allControls)

    def generator() = Module(new Symbol8b10bGenerator(decodedSymbolsPerCycle, dataSeq))
    def checker(str: String) = Module(new Symbol8b10bChecker(decodedSymbolsPerCycle, dataSeq, str))

}

object Encoding8b10bTests {

    val decodedSymbols = List(1,2,3,5)
    val performanceEffort = List(0,1)
    val offset = List(0,3,10,19)

    def apply(timeout: Int = 50000):Seq[UnitTest] = for (x <- decodedSymbols; y <- performanceEffort; z <- offset) yield Module(new Encoding8b10bTest(x,y,z,timeout))

}
