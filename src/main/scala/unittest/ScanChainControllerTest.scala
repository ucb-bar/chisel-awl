package hbwif2

import hbwif2._
import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest._


class ScanChainControllerTestWidget extends Module {

    val io = IO(new Bundle {
        val in = Input(UInt(4.W))
        val out = Output(UInt(4.W))
    })

    io.out := io.in + 1.U

}

class ScanChainControllerTest(timeout: Int = 50000) extends UnitTest(timeout) {

    val set = Map(
        ("A" -> 9),
        ("B" -> 1),
        ("Foo" -> 0),
        ("Bar" -> 63),
        ("WidgetIn" -> 5)
    )
    val get = set ++ Map(
        ("C" -> 3),
        ("Baz" -> 0),
        ("WidgetOut" -> (set("WidgetIn") + 1))
    )

    val ctrlA = Wire(UInt(10.W))
    val ctrlB = Wire(UInt(1.W))
    val ctrlFoo = Wire(UInt(1.W))
    val ctrlBar = Wire(UInt(7.W))

    val statC = get("C").U(7.W)
    val statBaz = get("Baz").U(1.W)

    val widget = Module(new ScanChainControllerTestWidget)

    val builder = new ScanChainControllerBuilder

    builder.w("A", ctrlA)
    builder.w("B", ctrlB)
    builder.w("Foo", ctrlFoo)
    builder.w("Bar", ctrlBar)
    builder.r("C", statC)
    builder.r("Baz", statBaz)
    builder.w("WidgetIn", widget.io.in)
    builder.r("WidgetOut", widget.io.out)

    val port = builder.generate(clock, reset.toBool)
    val addressMap = builder.getAddressMap()

    val readLength = statC.getWidth + statBaz.getWidth + widget.io.out.getWidth
    val writeLength = ctrlA.getWidth + ctrlB.getWidth + ctrlFoo.getWidth + ctrlBar.getWidth + widget.io.in.getWidth
    require(builder.readLength == readLength, s"Expected ${readLength} for scan chain read length, got ${builder.readLength}")
    require(builder.writeLength == writeLength, s"Expected ${writeLength} for scan chain write length, got ${builder.writeLength}")
    require(builder.length == readLength + writeLength, s"Expected ${readLength + writeLength} for scan chain total length, got ${builder.length}")

    val scanInData = Wire(Vec(builder.writeLength, Bool()))
    val scanOutData = Reg(UInt(builder.length.W))

    set foreach { case ((name, value)) =>
        val (hi,lo) = addressMap(name)
        (lo to hi) foreach { x => scanInData(x) := value.U(x-lo) }
    }

    val sScanIn :: sUpdate :: sWait :: sScanOut :: sCheck :: sDone :: Nil = Enum(6)
    val state = RegInit(sScanIn)
    io.finished := state === sDone
    port.scanEnable := state === sScanIn || state === sScanOut
    port.scanCommit := state === sUpdate
    val wCounter = Counter(builder.writeLength)
    val counter = Counter(builder.length)
    val uCounter = Counter(4) // for commit synchronization
    port.scanIn := scanInData((writeLength-1).U - wCounter.value)
    port.scanClock := clock

    when (port.scanEnable) {
        scanOutData := Cat(scanOutData(builder.length - 2, 0), port.scanOut)
    }

    switch (state) {

        is (sScanIn) {
            when (wCounter.inc()) {
                state := sUpdate
            }
        }

        is (sUpdate) {
            when (uCounter.inc()) {
                state := sWait
            }
        }

        is (sWait) {
            state := sScanOut
        }

        is (sScanOut) {
            when (counter.inc()) {
                state := sCheck
            }
        }

        is (sCheck) {
            get foreach { case ((name, value)) =>
                val (hi, lo) = addressMap(name)
                assert(value.U === scanOutData(hi, lo), s"Expected $value for $name")
            }
            state := sDone
        }
    }

}

object ScanChainTests {
    def apply(timeout: Int = 50000):Seq[UnitTest] = Seq(Module(new ScanChainControllerTest(timeout)))
}
