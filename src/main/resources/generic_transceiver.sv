`timescale 1ns/1ps

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
  input [15:0] data_tx,
  output [15:0] data_rx,
  output [15:0] data_dlev,
  inout tx_p,
  inout tx_n,
  inout rx_p,
  inout rx_n,
  output reset_out,
  input async_reset_in,
  output clock_digital,
  input clock_ref
);

  //***********************************
  //               PLL
  //***********************************
  // This is just a fake little PLL for digital simulation with an ideal,
  // jitter-less reference clock

  //TODO


endmodule
