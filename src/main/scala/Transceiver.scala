package hbwif2

import chisel3._
import chisel3.experimental._

class Differential extends Bundle {
  val p = Output(Bool())
  val n = Output(Bool())
}

class TransceiverIO(
  transceiverDataWidth: Int,
  transceiverNumIrefs: Int,
  cdrIWidth: Int,
  cdrPWidth: Int
) extends Bundle {

  // high speed clock input
  val fastClk = Input(Clock())

  // low speed clock output
  val slowClk = Output(Clock())

  // reset
  val resetIn = Input(Bool())
  val resetOut = Output(Bool())

  // RX pad inputs
  val rx = Flipped(new Differential)

  // TX pad outputs
  val tx = new Differential

  // internal data interface
  val dlev = Output(UInt(transceiverDataWidth.W))
  val dataRx = Output(UInt(transceiverDataWidth.W))
  val dataTx = Input(UInt(transceiverDataWidth.W))

  // reference current (if any)
  val iref = Analog(transceiverNumIrefs.W)

  // CDR stuff
  val cdrI = Input(UInt(cdrIWidth.W))
  val cdrP = Input(UInt(cdrPWidth.W))

  //val config = TODO

  //val debug = TODO

}

class Transceiver(
  transceiverDataWidth: Int,
  transceiverNumIrefs: Int,
  cdrIWidth: Int,
  cdrPWidth: Int
  ) extends BlackBox {

  val io = IO(new TransceiverIO())

  override def desiredName = transceiverName

}


