// johnwright@eecs.berkeley.edu

package hurricane_hbwif
import hbwif._

import Chisel._
import ChiselError._
import cde.{Parameters, Field}
import junctions._
import uncore.{SCRFile, HasHtifParameters}
import Serial._

class HurricaneHbwifLanePadIF(implicit p: Parameters) extends HurricaneHbwifBundle()(p) {
  val rx_inp  = Bool(INPUT)
  val rx_inn  = Bool(INPUT)
  val tx_outp = Bool(OUTPUT)
  val tx_outn = Bool(OUTPUT)
}

class HurricaneHbwifLaneUncoreIF(implicit p: Parameters) extends HurricaneHbwifBundle()(p) {
  val tx = AsyncFifoInternal(new AtosRequest).flip
  val rx = AsyncFifoInternal(new AtosResponse)
}

class HurricaneHbwifLaneConfigIF(implicit p: Parameters) extends HurricaneHbwifBundle()(p) {
  val rx_config = new TransceiverRxConfig
  val tx_config = new TransceiverTxConfig
  val config_rx_edge_sel = Bool(INPUT)
  val config_rx_sel = Bits(INPUT, width = 2)
}

// analog connections
class HurricaneHbwifLaneBiasIF(implicit p: Parameters) extends HurricaneHbwifBundle()(p) {
  val rx_vcm  = Bool(INPUT)
  val tx_iref = Bool(INPUT)
  val rx_iref = Bool(INPUT)
  val rx_ioff = Bool(INPUT)
}

class HurricaneHbwifLane(scrPrefix: String, baseAddr: BigInt, isMemChannel: Boolean = true)(implicit p: Parameters)
  extends HurricaneHbwifModule()(p) with HasHtifParameters {

  val io = new HurricaneHbwifBundle {
    val pad  = new HurricaneHbwifLanePadIF
    val uncore = new HurricaneHbwifLaneUncoreIF
    val bias = new HurricaneHbwifLaneBiasIF
    val mem_en = Bool(INPUT)
    val alignment_lock = Bool(INPUT)
    val scr_req = AsyncFifoInternal(new SmiReq(scrDataBits, scrAddrBits)).flip
    val scr_resp = AsyncFifoInternal(Bits(scrDataBits))
    val clks = Bool(INPUT)
  }

  val scr_file = Module(new SCRFile(scrPrefix,baseAddr))

  // SCR Async FIFOs
  val scr_req_fifo  = AsyncFifoRX(new SmiReq(scrDataBits,scrAddrBits))
  val scr_resp_fifo = AsyncFifoTX(Bits(scrDataBits))
  scr_req_fifo.io.from_tx <> io.scr_req
  scr_resp_fifo.io.to_rx  <> io.scr_resp
  scr_req_fifo.io.deq     <> scr_file.io.smi.req
  scr_resp_fifo.io.enq    <> scr_file.io.smi.resp

  // BERT/8b10b Mux
  val mode_8b10b = Reg(init = Bool(true))
  scr_file.io.scr.attach(mode_8b10b,"mode_8b10b")

  // BERT
  val bert = Module(new Bert)
  bert.io.enable := !mode_8b10b
  scr_file.io.scr.attach(bert.io.clear, "bert_clear", true)
  scr_file.io.scr.attach(bert.io.snapshot_en, "bert_snapshot_en", true)
  scr_file.io.scr.attach(bert.io.prbs_load_data, "bert_prbs_load_data", true)
  scr_file.io.scr.attach(bert.io.prbs_mode, "bert_prbs_mode", true)
  scr_file.io.scr.attach(bert.io.shutoff_select, "bert_shutoff_select", true)
  scr_file.io.scr.attach(bert.io.prbs_select, "bert_prbs_select", true)
  scr_file.io.scr.attach(bert.io.ber_mode, "bert_ber_mode", true)
  scr_file.io.scr.attach(bert.io.seed_good, "bert_seed_good", false, true)
  for(i <- 0 until bertNumWays) {
    scr_file.io.scr.attach(bert.io.error_counts(i), "bert_error_counts_%d".format(i), false, true)
  }
  scr_file.io.scr.attach(bert.io.bit_count, "bert_bit_count", false, true)
  scr_file.io.scr.attach(bert.io.snapshot, "bert_snapshot", false, true)

  // PRBS generators for BERT
  val tx_prbs_mode = Bits(width = 2)
  scr_file.io.scr.attach(tx_prbs_mode, "bert_tx_prbs_mode", true)
  val tx_prbs_load_data = Bits(width = 31)
  scr_file.io.scr.attach(tx_prbs_load_data, "bert_tx_prbs_load_data", true)
  val tx_prbs_select = Bits(width = 2)
  scr_file.io.scr.attach(tx_prbs_select, "bert_tx_prbs_select", true)

  // PRBS31
  val tx_prbs31 = Module(new PRBS(prbsWidth = 31, parallelOutBits = hbwifBertDataBits, generatorPolynomial = 0x09))
  tx_prbs31.io.mode    := tx_prbs_mode
  tx_prbs31.io.load_in := tx_prbs_load_data
  tx_prbs31.io.seed_in := UInt(1)

  // PRBS15
  val tx_prbs15 = Module(new PRBS(prbsWidth = 15, parallelOutBits = hbwifBertDataBits, generatorPolynomial = 0x03))
  tx_prbs15.io.mode    := tx_prbs_mode
  tx_prbs15.io.load_in := tx_prbs_load_data(14,0)
  tx_prbs15.io.seed_in := UInt(1)

  // PRBS7
  val tx_prbs7 = Module(new PRBS(prbsWidth = 7, parallelOutBits = hbwifBertDataBits, generatorPolynomial = 0x03))
  tx_prbs7.io.mode    := tx_prbs_mode
  tx_prbs7.io.load_in := tx_prbs_load_data(6,0)
  tx_prbs7.io.seed_in := UInt(1)

  // PRBS Mux
  val tx_prbs = Mux(tx_prbs_select(0),Mux(tx_prbs_select(1),tx_prbs15.io.out,tx_prbs31.io.out),tx_prbs7.io.out)

  val config_rx_edge_sel = Reg(init = Bool(true))
  val config_rx_sel = Reg(init = Bool(true))
  scr_file.io.scr.attach(config_rx_edge_sel,"config_rx_edge_sel")
  scr_file.io.scr.attach(config_rx_sel,"config_rx_sel")

  // SerDes
  val serdes = Module(new SerDes(divideBy = 5))
  serdes.io.clks               <> io.clks
  serdes.io.reset              := reset
  serdes.io.config_rx_edge_sel := config_rx_edge_sel
  serdes.io.config_rx_sel      := config_rx_sel
  bert.io.data_in := serdes.io.rx_out

  if (isMemChannel) {

    // Atos Async FIFOs
    // Note: the module names are intentionally backwards--
    // they reflect the direction of transceiver data flow
    val tx_fifo = AsyncFifoRX(new AtosRequest)
    tx_fifo.io.from_tx <> io.uncore.tx
    // deq_clk is postprocessed
    val rx_fifo = AsyncFifoTX(new AtosResponse)
    rx_fifo.io.to_rx <> io.uncore.rx
    // enq_clk is postprocessed

    // mem_en synchronizer
    val mem_en_sync = Reg(next = Reg(next = io.mem_en))

    // CRC + Buffer
    val crc = Module(new Crc)
    crc.io.mem_en        := mem_en_sync
    tx_fifo.io.deq       <> crc.io.atos.req
    rx_fifo.io.enq       <> crc.io.atos.resp
    scr_file.io.scr.attach(crc.io.sync, "sync", false, true)
    scr_file.io.scr.attach(crc.io.err, "crc_rx_error", false, true)


    // 8b/10b
    val codec = Module(new Channel8b10bController)
    codec.io.on := UInt(1) // Do we want to make this an SCR?
    codec.io.lock := Reg(next = Reg(next = io.alignment_lock))
    // delay this by a cycle to improve QoR (since there are combinational paths coming through the encoder)
    serdes.io.tx_in              := Reg(next = Mux(mode_8b10b,codec.io.phy.tx_data,tx_prbs))
    codec.io.phy.rx_data         := Mux(mode_8b10b,serdes.io.rx_out,UInt(0))
    codec.io.ctl <> crc.io.crc

  } else {
    serdes.io.tx_in := Reg(next = tx_prbs)
  }

  // Transceiver
  val transceiver = Module(new Transceiver)
  transceiver.io.clks.foreach(_ := io.clks)
  transceiver.io.rx_inp    <> io.pad.rx_inp
  transceiver.io.rx_inn    <> io.pad.rx_inn
  transceiver.io.tx_outp   <> io.pad.tx_outp
  transceiver.io.tx_outn   <> io.pad.tx_outn
  transceiver.io.rx_out1   <> serdes.io.rx_in(0)
  transceiver.io.rx_out2   <> serdes.io.rx_in(1)
  transceiver.io.rx_out3   <> serdes.io.rx_in(2)
  transceiver.io.tx_in     <> serdes.io.tx_out
  transceiver.io.rx_vcm    <> io.bias.rx_vcm
  transceiver.io.tx_iref   <> io.bias.tx_iref
  transceiver.io.rx_iref   <> io.bias.rx_iref
  transceiver.io.rx_ioff   <> io.bias.rx_ioff

  scr_file.io.scr.attach(transceiver.io.tx_config.en,"tx_config_en",true)
  scr_file.io.scr.attach(transceiver.io.tx_config.xor,"tx_config_xor",true)
  scr_file.io.scr.attach(transceiver.io.tx_config.swing,"tx_config_swing",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.cvd,"rx_config_cvd",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.delay,"rx_config_delay",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.ioff1,"rx_config_ioff1",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.ioffen1,"rx_config_ioffen1",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.irefen1,"rx_config_irefen1",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.it1,"rx_config_it1",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.sw1,"rx_config_sw1",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.clken1,"rx_config_clken1",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.ioff2,"rx_config_ioff2",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.ioffen2,"rx_config_ioffen2",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.irefen2,"rx_config_irefen2",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.it2,"rx_config_it2",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.sw2,"rx_config_sw2",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.clken2,"rx_config_clken2",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.ioff3,"rx_config_ioff3",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.ioffen3,"rx_config_ioffen3",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.irefen3,"rx_config_irefen3",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.it3,"rx_config_it3",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.sw3,"rx_config_sw3",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.clken3,"rx_config_clken3",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.rx3del,"rx_config_rx3del",true)
  scr_file.io.scr.attach(transceiver.io.rx_config.vcmsw,"rx_config_vcmsw",true)


}

