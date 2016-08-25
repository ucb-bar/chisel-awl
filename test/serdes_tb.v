`timescale 1ns/1ps

module serdes_tb();

  wire [1:0] rx_in, tx_out;
  wire [15:0] rx_out;
  wire txp,txn,rxn;

  reg [15:0] tx_in = 16'b0;
  reg rxp = 1'b0;
  assign rxn = ~rxp;

  reg clks = 1'b1;
  reg clkd = 1'b0;
  reg reset = 1'b1;

  always #(`CLOCK_PERIOD/2) clkd = ~clkd;
  always #(`CLOCK_PERIOD/16) clks = ~clks;

  hurricane_hbwif_serdes #(8,3) dut (
    .io_rx_in_0(rx_in),
    .io_rx_in_1(rx_in),
    .io_rx_in_2(rx_in),
    .io_tx_in(tx_in),
    .io_clks(clks),
    .io_clkd(clkd),
    .io_reset(reset),
    .io_config_rx_sel(2'b0),
    .io_config_rx_edge_sel(1'b0),
    .io_rx_out(rx_out),
    .io_tx_out(tx_out)
  );

  hurricane_hbwif_top transceiver (
    .io_rx_inp(rxp),
    .io_rx_inn(rxn),
    .io_tx_outp(txp),
    .io_tx_outn(txn),
    .io_clks_0(clks),
    .io_clks_1(clks),
    .io_clks_2(clks),
    .io_clks_3(clks),
    .io_tx_in(tx_out),
    .io_rx_out1(rx_in)
  );

  // use a counter to stimulate the serializer
  initial begin
    #(`CLOCK_PERIOD/16);
    while(1) begin
      #(`CLOCK_PERIOD);
      tx_in <= tx_in + 1;
    end
  end

  integer i;
  initial begin
    #(`CLOCK_PERIOD/32);
    while(1) begin
      for(i = 15; i >= 0; i = i - 1) begin
        rxp <= tx_in[i];
        #(`CLOCK_PERIOD/16);
      end
    end
  end

  initial begin
  $vcdpluson;
    @(posedge clkd);
    #(`CLOCK_PERIOD/16);
    reset <= 1'b0;

    #(`CLOCK_PERIOD*100);

  $finish;
  end

endmodule
