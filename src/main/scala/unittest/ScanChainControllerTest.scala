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

    val builder = new ControllerBuilder[ScanChainController]({ (spec:ControlSpec) => new ScanChainController(spec) })

    builder.w("A", ctrlA)
    builder.w("B", ctrlB)
    builder.w("Foo", ctrlFoo)
    builder.w("Bar", ctrlBar)
    builder.r("C", statC)
    builder.r("Baz", statBaz)
    builder.w("WidgetIn", widget.io.in)
    builder.r("WidgetOut", widget.io.out)

    val dut = builder.generate()
    val addressMap = dut.getAddressMap()

    val rLength = statC.getWidth + statBaz.getWidth + widget.io.out.getWidth
    val wLength = ctrlA.getWidth + ctrlB.getWidth + ctrlFoo.getWidth + ctrlBar.getWidth + widget.io.in.getWidth
    require(dut.rLength == rLength, s"Expected ${rLength} for scan chain read length, got ${dut.rLength}")
    require(dut.wLength == wLength, s"Expected ${wLength} for scan chain write length, got ${dut.wLength}")
    require(dut.length == rLength + wLength, s"Expected ${rLength + wLength} for scan chain total length, got ${dut.length}")

    val scanInData = Wire(Vec(dut.wLength, Bool()))
    val scanOutData = Reg(UInt(dut.length.W))

    set foreach { case ((name, value)) =>
        val (hi,lo) = addressMap(name)
        (lo to hi) foreach { x => scanInData(x) := value.U(x-lo) }
    }

    val sScanIn :: sUpdate :: sWait :: sScanOut :: sCheck :: sDone :: Nil = Enum(6)
    val state = RegInit(sScanIn)
    io.finished := state === sDone
    dut.io.port.scanEnable := state === sScanIn || state === sScanOut
    dut.io.port.scanCommit := state === sUpdate
    val wCounter = Counter(dut.wLength)
    val counter = Counter(dut.length)
    dut.io.port.scanIn := scanInData((wLength-1).U - wCounter.value)
    dut.io.port.scanClock := clock

    when (dut.io.port.scanEnable) {
        scanOutData := Cat(scanOutData(dut.length - 2, 0), dut.io.port.scanOut)
    }

    switch (state) {

        is (sScanIn) {
            when (wCounter.inc()) {
                state := sUpdate
            }
        }

        is (sUpdate) {
            state := sWait
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
