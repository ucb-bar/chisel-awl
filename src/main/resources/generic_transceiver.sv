`timescale 1ns/1ps

`ifndef SERDES_BITS
`define SERDES_BITS 16
`endif
`ifndef SERDES_LOGBITS
`define SERDES_LOGBITS 4
`endif

module generic_transceiver (
  input [3:0] dlev_dac,
  input [3:0] dfe_taps_0,
  input [3:0] dfe_taps_1,
  input [3:0] dfe_taps_2,
  input [3:0] dfe_taps_3,
  input dither_clock,
  input [7:0] cdrp,
  input [7:0] cdri,
  inout bias,
  input [`SERDES_BITS-1:0] data_tx,
  output [`SERDES_BITS-1:0] data_rx,
  output [`SERDES_BITS-1:0] data_dlev,
  inout tx_p,
  inout tx_n,
  inout rx_p,
  inout rx_n,
  output reset_out,
  input async_reset_in,
  output clock_digital,
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

  reg [`SERDES_LOGBITS-1:0] count;
  reg clock_digital_reg;
  assign clock_digital = clock_digital_reg;

  always @(posedge clock_fast or negedge clock_fast or posedge async_reset_in) begin
    if (async_reset_in) count <= 0;
    else count <= count + 1;
  end

  always @(posedge clock_fast or negedge clock_fast or posedge async_reset_in) begin
    if (async_reset_in) clock_digital_reg <= 1'b1;
    else if (count == `SERDES_BITS/2-1) clock_digital_reg <= 1'b0;
    else if (count == `SERDES_BITS-1) clock_digital_reg <= 1'b1;
  end

  //***********************************
  //            Serializer
  //***********************************

  reg [`SERDES_BITS-1:0] data_tx_buf;
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

  reg [`SERDES_BITS-1:0] data_rx_buf;
  reg [`SERDES_BITS-1:0] data_rx_reg;
  assign data_rx = data_rx_reg;

  wire rx_p_val;
  assign rx_p_val = (rx_p ^ rx_n) ? rx_p : 1'bx;

  always @(posedge clock_fast or negedge clock_fast) begin
    if (count == `SERDES_BITS-1) data_rx_reg <= data_rx_buf;
    data_rx_buf <= {data_rx_buf[`SERDES_BITS-2:0],rx_p_val};
  end

  //***********************************
  //         Reset Synchronizer
  //***********************************

  reg [1:0] reset_out_sync;
  assign reset_out = reset_out_sync[1];

  always @(posedge async_reset_in or posedge clock_digital) begin
    if (async_reset_in) begin
      reset_out_sync[0] <= 1'b1;
      reset_out_sync[1] <= 1'b1;
    end else begin
      reset_out_sync[0] <= async_reset_in;
      reset_out_sync[1] <= reset_out_sync[0];
    end
  end


endmodule