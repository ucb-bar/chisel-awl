package hbwif

import Chisel._
import cde._
import junctions._
import uncore.tilelink._
import testchipip._
import rocketchip._

class HbwifLaneBackendIO(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasHbwifParameters {

  // data from/to the transceiver
  val transceiverData = (new TransceiverData).flip

  // tilelink port for memory
  val mem = (new ClientUncachedTileLinkIO()(memParams)).flip

  // Configuration TileLink port
  val scr = (new ClientUncachedTileLinkIO()(mmioParams)).flip

  // parameterizable configuration bundle
  val transceiverExtraInputs = p(TransceiverKey).extraInputs.map { _.cloneType.asOutput }

  // parameterizable configuration bundle
  val transceiverExtraOutputs = p(TransceiverKey).extraOutputs.map { _.cloneType.asInput }

}

class HbwifLaneBackend(val c: Clock, val r: Bool, val id: Int)(implicit val p: Parameters) extends Module(_clock = c, _reset = r)
  with HasHbwifParameters {

  val io = new HbwifLaneBackendIO

  require(transceiverDataWidth == 10)
  val encoder = Module(new Encoder8b10b)
  encoder.suggestName("encoderInst")
  val decoder = Module(new Decoder8b10b)
  decoder.suggestName("decoderInst")

  decoder.io.encoded := io.transceiverData.rx

  val memSerDes = Module(new HbwifTileLinkMemSerDes()(memParams))
  memSerDes.suggestName("memSerDesInst")
  encoder.io.decoded <> memSerDes.io.tx
  memSerDes.io.rx <> decoder.io.decoded
  memSerDes.io.mem <> io.mem

  val scrBuilder = new SCRBuilder(s"hbwif_lane$id")

  val bert = Module(new Bert())

  bert.io.dataIn := io.transceiverData.rx

  // MUX between BERT and the tilelink channel when BERT is on
  io.transceiverData.tx := Mux(bert.io.enable, bert.io.dataOut, encoder.io.encoded)

  scrBuilder.addControl("bert_enable", UInt(0))
  scrBuilder.addControl("bert_clear", UInt(0))
  scrBuilder.addControl("bert_snapshot_en", UInt(1))
  scrBuilder.addControl("bert_rx_prbs_load_data", UInt(1))
  scrBuilder.addControl("bert_rx_prbs_mode", UInt(0))
  scrBuilder.addControl("bert_tx_prbs_load_data", UInt(1))
  scrBuilder.addControl("bert_tx_prbs_mode", UInt(0))
  scrBuilder.addControl("bert_shutoff_select", UInt(0))
  scrBuilder.addControl("bert_prbs_select", UInt(0))
  scrBuilder.addControl("bert_ber_mode", UInt(0))
  scrBuilder.addStatus("bert_seed_good")
  (0 until bertNumWays).foreach { i => scrBuilder.addStatus(s"bert_error_count_$i") }
  scrBuilder.addStatus("bert_bit_count")
  scrBuilder.addStatus("bert_snapshot")

  scrBuilder.addControl("retransmit_enable", UInt(0))
  scrBuilder.addControl("retransmit_cycles", UInt(1))

  // TODO this needs to handle nested Bundles
  io.transceiverExtraInputs.map(_.elements.keys.foreach {
    name => scrBuilder.addControl(name)
  })

  // TODO this needs to handle nested Bundles
  io.transceiverExtraOutputs.map(_.elements.keys.foreach {
    name => scrBuilder.addStatus(name)
  })

  // generate the SCR File and attach it to our SCR TileLink port
  val scr = scrBuilder.generate(p(GlobalAddrMap)(s"io:pbus:scrbus:hbwif_lane$id").start)(mmioParams)
  scr.io.tl <> io.scr

  // TODO this needs to handle nested Bundles
  io.transceiverExtraInputs.map(_.elements.foreach {
    case (name: String, data: Data) => data := scr.control(name)
  })

  // TODO this needs to handle nested Bundles
  io.transceiverExtraOutputs.map(_.elements.foreach {
    case (name: String, data: Data) => scr.status(name) := data
  })

  // wire up the SCRs
  bert.io.enable         := scr.control("bert_enable")(0)
  bert.io.clear          := scr.control("bert_clear")(0)
  bert.io.snapshotEn     := scr.control("bert_snapshot_en")(0)
  bert.io.rxPRBSLoadData := scr.control("bert_rx_prbs_load_data")(30,0)
  bert.io.rxPRBSMode     := scr.control("bert_rx_prbs_mode")(1,0)
  bert.io.txPRBSLoadData := scr.control("bert_tx_prbs_load_data")(30,0)
  bert.io.txPRBSMode     := scr.control("bert_tx_prbs_mode")(1,0)
  bert.io.shutoffSelect  := scr.control("bert_shutoff_select")(log2Up(bertShutoffPoints)-1,0)
  bert.io.prbsSelect     := scr.control("bert_prbs_select")(1,0)
  bert.io.berMode        := scr.control("bert_ber_mode")(0)
  scr.status("bert_seed_good") := bert.io.seedGood
  (0 until bertNumWays).foreach { i => scr.status(s"bert_error_count_$i") := bert.io.errorCounts(i)}
  scr.status("bert_bit_count") := bert.io.bitCount
  scr.status("bert_snapshot") := bert.io.snapshot

  memSerDes.io.retransmitEnable := scr.control("retransmit_enable")(0)
  memSerDes.io.retransmitCycles := scr.control("retransmit_cycles")(log2Up(hbwifMaxRetransmitCycles)-1,0)

}
