package hbwif

import chisel3._
import chisel3.util._


class TxBitStuffer()(implicit val c: SerDesConfig) extends Module {

    val io = IO(new Bundle {
        val enq = Flipped(Ready(UInt(c.dataWidth.W)))
        val raw = Output(UInt(c.dataWidth.W))
        val mode = if (c.bitStuffModes > 1) Some(Input(UInt(log2Ceil(c.bitStuffModes).W))) else None
    })

    require(c.bitStuffModes > 0)
    require(c.dataWidth % (1 << (c.bitStuffModes - 1)) == 0)

    io.raw := io.enq.bits
    io.enq.ready := true.B

    if (c.bitStuffModes > 1) {
        val buf = Reg(UInt(c.dataWidth.W))
        buf.suggestName("buf")
        val count = RegInit(0.U((c.bitStuffModes - 1).W))
        count.suggestName("count")
        (1 until c.bitStuffModes).foreach { i =>
            val divisor = 1 << i
            val divBitWidth = c.dataWidth/divisor
            when (io.mode.get === i.U) {
                io.raw := FillInterleaved(divisor, buf(c.dataWidth - 1, c.dataWidth - divBitWidth))
                when (count === (divisor - 1).U) {
                    count := 0.U
                    buf := io.enq.bits
                } .otherwise {
                    buf := buf << divBitWidth
                    count := count + 1.U
                    io.enq.ready := false.B
                }
            }
        }
    }
}

class RxBitStuffer()(implicit val c: SerDesConfig) extends Module {

    val io = IO(new Bundle {
        val raw = Input(UInt(c.dataWidth.W))
        val deq = Valid(UInt(c.dataWidth.W))
        val mode = if (c.bitStuffModes > 1) Some(Input(UInt(log2Ceil(c.bitStuffModes).W))) else None
    })

    require(c.bitStuffModes > 0)
    require(c.dataWidth % (1 << (c.bitStuffModes - 1)) == 0)

    io.deq.bits := io.raw
    io.deq.valid := true.B

    if (c.bitStuffModes > 1) {
        val buf = Reg(UInt(c.dataWidth.W))
        buf.suggestName("buf")
        val count = RegInit(0.U((c.bitStuffModes - 1).W))
        count.suggestName("count")
        (1 until c.bitStuffModes).foreach { i =>
            val divisor = 1 << i
            val divBitWidth = c.dataWidth/divisor
            when(io.mode.get === i.U) {
                io.deq.bits := buf
                buf := Cat(buf(c.dataWidth - divBitWidth - 1, 0),
                    io.raw.toBools.reverse.zipWithIndex.filter(_._2 % divisor == (divisor - 1)).map(_._1.asUInt).reduceLeft(Cat(_,_)))
                when (count === (divisor - 1).U) {
                    count := 0.U
                } .otherwise {
                    count := count + 1.U
                    io.deq.valid := false.B
                }
            }
        }
    }

}
