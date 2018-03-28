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

    if (c.bitStuffModes == 1) {
        io.raw := io.enq.bits
        io.enq.ready := true.B
    } else {
        val buf = Reg(UInt(c.dataWidth.W))
        val count = RegInit(0.U((c.bitStuffModes - 1).W))
        io.raw := buf
        (0 until c.bitStuffModes).foreach { i =>
            when (io.mode.get === i.U) {
                io.raw := FillInterleaved((1 << i), buf(c.dataWidth - 1, c.dataWidth - (c.dataWidth/(1 << i))))
            }
        }
        when ((count +& 1.U)(io.mode.get) === true.B) {
            count := 0.U
            io.enq.ready := true.B
            buf := io.enq.bits
        } .otherwise {
            count := count + 1.U
            io.enq.ready := false.B
            (0 until c.bitStuffModes).foreach { i =>
                when(io.mode.get === i.U) {
                    if (i > 0) {
                        buf := buf << (c.dataWidth/(1 << i))
                    } else {
                        // but we shouldn't get here
                        buf := io.enq.bits
                    }
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

    // TODO implement this so that we don't incur a register delay in mode 0
    require(c.bitStuffModes > 0)
    require(c.dataWidth % (1 << (c.bitStuffModes - 1)) == 0)

    if (c.bitStuffModes == 1) {
        io.deq.bits := io.raw
        io.deq.valid := true.B
    } else {
        val buf = Reg(UInt(c.dataWidth.W))
        val count = RegInit(0.U(log2Ceil(c.bitStuffModes).W))
        io.deq.bits := buf
        (0 until c.bitStuffModes).foreach { i =>
            when(io.mode.get === i.U) {
                val tmp = Wire(UInt((c.dataWidth / (1 << i)).W))
                tmp := io.raw.toBools.zipWithIndex.filter(_._2 % (1 << i) == 0).map(_._1.asUInt).reduceLeft(Cat(_,_))
                if (i > 0) {
                    buf := Cat(buf(c.dataWidth - 1 - (c.dataWidth/(1 << i)),0), tmp)
                } else {
                    buf := tmp
                }
            }
        }
        when ((count +& 1.U)(io.mode.get) === true.B) {
            count := 0.U
            io.deq.valid := true.B
        } .otherwise {
            count := count + 1.U
            io.deq.valid := false.B
        }
    }

}
