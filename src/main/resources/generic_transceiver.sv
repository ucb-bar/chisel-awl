`timescale 1ns/1ps

`ifndef SERDES_BITS
`define SERDES_BITS 16
`endif
`ifndef SERDES_LOGBITS
`define SERDES_LOGBITS 4
`endif

module generic_transceiver (
  input [`SERDES_BITS-1:0] data_tx,
  output [`SERDES_BITS-1:0] data_rx,
  output [`SERDES_BITS-1:0] data_dlev,
  output tx_p,
  output tx_n,
  input rx_p,
  input rx_n,
  input async_reset_in,
  output clock_tx,
  output clock_rx,
  input clock_ref
);

  // For now, this model just ignores the DFE and CDR stuff

  //***********************************
  //               PLL
  //***********************************
  // TODO implement a PLL- this is just using clock_ref as clock_fast
  wire clock_fast;
  assign clock_fast = clock_ref;

  //***********************************
  //       Digital Clock Divider
  //***********************************

  reg [`SERDES_LOGBITS-1:0] count = 0;
  reg clock_tx_reg = 0;
  assign clock_tx = clock_tx_reg;
  assign clock_rx = clock_tx_reg;

  always @(posedge clock_fast or negedge clock_fast) begin
    count <= (count + 1) % `SERDES_BITS;
  end

  always @(posedge clock_fast or negedge clock_fast) begin
    if (count == `SERDES_BITS/2-1) clock_tx_reg <= 1'b0;
    else if (count == `SERDES_BITS-1) clock_tx_reg <= 1'b1;
  end

  //***********************************
  //            Serializer
  //***********************************

  reg [`SERDES_BITS-1:0] data_tx_buf = 0;
  assign tx_p = data_tx_buf[`SERDES_BITS-1];
  assign tx_n = ~tx_p;

  always @(posedge clock_fast or negedge clock_fast) begin
    if (count == `SERDES_BITS-1)
      data_tx_buf <= data_tx;
    else
      data_tx_buf <= {data_tx_buf[`SERDES_BITS-2:0],1'b0};
  end

  //***********************************
  //           Deserializer
  //***********************************

  reg [`SERDES_BITS-1:0] data_rx_buf = 0;
  reg [`SERDES_BITS-1:0] data_rx_reg = 0;
  assign data_rx = data_rx_reg;

  wire rx_p_val;
  assign rx_p_val = (rx_p ^ rx_n) ? rx_p : 1'bx;

  always @(posedge clock_fast or negedge clock_fast) begin
    if (count == `SERDES_BITS-1) data_rx_reg <= data_rx_buf;
    data_rx_buf <= {data_rx_buf[`SERDES_BITS-2:0],rx_p_val};
  end

endmodule
