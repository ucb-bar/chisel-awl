// Example transceiver module compatible with HBWIF
// Use the ExampleHbwifConfig to instantiate this IP

module example_transceiver (
  input fastClk,
  output slowClk,
  input resetIn,
  output resetOut,
  input rx_p,
  input rx_n,
  output tx_p,
  output tx_n,
  input [9:0] data_tx,
  output [9:0] data_rx,
  input iref_0,
  input [3:0] extraInputs_txSwing,
  input extraInputs_cdrMode
  `ifdef HBWIF_USE_PG_PINS
  , inout avdd
  , inout dvdd
  , inout gnd
  `endif
);

  `ifndef HBWIF_USE_PG_PINS
  wire avdd, dvdd, gnd;
  assign avdd = 1'b1;
  assign dvdd = 1'b1;
  assign gnd = 1'b0;
  `endif

  wire corrupt;
  wire slowClk_pre, tx_p_pre, tx_n_pre;
  wire [9:0] data_rx_pre;

  assign corrupt = (iref_0 !== 1'b1) || (avdd !== 1'b1) || (dvdd !== 1'b1) || (gnd !== 1'b0);

  assign slowClk = corrupt ? 1'bx : slowClk_pre;
  assign tx_p = corrupt ? 1'bx : tx_p_pre;
  assign tx_n = corrupt ? 1'bx : tx_n_pre;
  assign data_rx = corrupt ? {10{1'bx}} : data_rx_pre;

  fpga_transceiver U0 (
    .fastClk(fastClk),
    .slowClk(slowClk_pre),
    .resetIn(resetIn),
    .resetOut(resetOut),
    .rx_p(rx_p),
    .rx_n(rx_n),
    .tx_p(tx_p_pre),
    .tx_n(tx_n_pre),
    .data_tx(data_tx),
    .data_rx(data_rx_pre)
  );

endmodule


module example_reference_generator (
  input [1:0] config_mirrorMultiplier,
  input irefIn,
  output [7:0] irefOut
  `ifdef HBWIF_USE_PG_PINS
  , inout avdd
  , inout gnd
  `endif
);

  `ifndef HBWIF_USE_PG_PINS
  supply1 vdd;
  supply0 gnd;
  `endif

  assign irefOut[0] = irefIn;
  assign irefOut[1] = irefIn;
  assign irefOut[2] = irefIn;
  assign irefOut[3] = irefIn;
  assign irefOut[4] = irefIn;
  assign irefOut[5] = irefIn;
  assign irefOut[6] = irefIn;
  assign irefOut[7] = irefIn;

endmodule


module fpga_transceiver (
  input fastClk,
  output reg slowClk,
  input resetIn,
  output resetOut,
  input rx_p,
  input rx_n,
  output tx_p,
  output tx_n,
  input [9:0] data_tx,
  output [9:0] data_rx
);

  // Internal signals
  wire rx, clk_d;
  reg [9:0] ser;
  reg [9:0] des;
  reg [9:0] des_buffer;

  // Differential checks
  assign tx_n = ~tx_p;
  assign tx_p = ser[9];
  assign rx = (rx_p == ~rx_n) ? rx_p : 1'bx;

  assign data_rx = des_buffer;

  // Need to add a slight delay here to simulate correctly
  assign #0.1 clk_d = fastClk;

  reg [1:0] resetSync;

  // Async reset out
  always @(posedge slowClk or posedge resetIn) begin
    if (resetIn) begin
      resetSync[1] <= 1'b1;
      resetSync[0] <= 1'b1;
    end else begin
      resetSync[1:0] <= {resetSync[0],1'b0};
    end
  end
  assign resetOut = resetSync[1];

  // TX Serializer and RX Deserializer
  reg [3:0] serdes_count;
  always @(posedge clk_d or negedge clk_d) begin
    if (resetIn) begin
      serdes_count <= 4'd0;
      ser <= 10'd0;
      des_buffer <= 10'd0;
      des <= 10'd0;
    end else begin
      if (serdes_count == 4'd9) begin
        serdes_count <= 4'd0;
        ser <= data_tx;
        des_buffer <= des;
      end else begin
        serdes_count <= serdes_count + 4'd1;
        ser <= {ser[8:0], 1'b0};
      end
      des <= {des[8:0], rx};
    end
  end

  // Divide-by-5 using asynchronous reset
  reg [2:0] div5_count;
  always @(posedge fastClk or negedge fastClk or posedge resetIn) begin
    if (resetIn) begin
      div5_count <= 3'd0;
      slowClk <= 1'b1;
    end else begin
      if (div5_count == 3'd4) begin
        div5_count <= 3'd0;
        slowClk <= ~slowClk;
      end else begin
        div5_count <= div5_count + 3'd1;
      end
    end
  end

endmodule
