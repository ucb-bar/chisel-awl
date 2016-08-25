`timescale 1ns/1ps

module ordering_tb();

  wire [1:0] rx_out;
  wire txp,txn,rxn;

  reg [1:0] tx_in = 2'b0;
  reg rxp = 1'b1;
  assign rxn = ~rxp;

  reg clk = 1'b0;
  reg sim_error = 1'b0;
  reg tx_done = 1'b0;
  reg rx_done = 1'b0;
  wire done;
  assign done = tx_done & rx_done;

  always #(`CLOCK_PERIOD/2) clk = ~clk;


  hurricane_hbwif_top dut (
    .io_rx_inp(rxp),
    .io_rx_inn(rxn),
    .io_tx_outp(txp),
    .io_tx_outn(txn),
    .io_clks_0(clk),
    .io_clks_1(clk),
    .io_clks_2(clk),
    .io_clks_3(clk),
    .io_tx_in(tx_in),
    .io_rx_out1(rx_out)
  );

  initial begin
    dut.io_rx_out1_reg <= 2'b01;
    dut.io_rx_out2_reg <= 2'b01;
    dut.io_rx_out3_reg <= 2'b01;
    dut.io_tx_in_d <= 2'b0;

    dut.io_rx_out1_retime <= 1'b0;
    dut.io_rx_out2_retime <= 1'b0;
    dut.io_rx_out3_retime <= 1'b0;
    dut.io_tx_outp_reg <= 1'b0;
  end

  initial begin
    $vcdpluson;
    @(posedge done);
    if(sim_error)
      $display("******************************** TESTS COMPLETED WITH ERRORS ********************************\n");
    else begin
      $display("                         #       ###################       _   _ ");
      $display("                        #        #                 #       *   * ");
      $display("                   #   #         #     CORRECT     #         |   ");
      $display("                    # #          #                 #       \\___/ ");
      $display("                     #           ###################             ");
    end
    $finish;
    $vcdplusoff;
  end

  // TX stimulus
  initial begin
    @(posedge clk);
    step(0.5);
    tx_in <= 2'b00;
    step(1);
    tx_in <= 2'b01;
    step(1);
    tx_in <= 2'b10;
    step(1);
    tx_in <= 2'b10;
    step(1);
    tx_in <= 2'b00;
    step(1);
    tx_in <= 2'b11;
  end

  // TX checker
  initial begin
    @(posedge clk);
    @(posedge txp);
    @(clk) #0 expect("tx",txp,1'b1);
    @(clk) #0 expect("tx",txp,1'b0);
    @(clk) #0 expect("tx",txp,1'b0);
    @(clk) #0 expect("tx",txp,1'b1);
    @(clk) #0 expect("tx",txp,1'b0);
    @(clk) #0 expect("tx",txp,1'b1);
    @(clk) #0 expect("tx",txp,1'b0);
    @(clk) #0 expect("tx",txp,1'b0);
    @(clk) #0 expect("tx",txp,1'b1);
    @(clk) #0 expect("tx",txp,1'b1);
    tx_done <= 1'b1;
  end

  // RX stimulus
  initial begin
    rxp <= 1'b1;
    @(posedge clk);
    @(posedge clk);
    bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b1; bit_step(0.5);
    rxp <= 1'b0; bit_step(0.5);
  end

  // Note: for RX
  // OutA = out[0]
  // OutB = out[1]

  // RX checker
  initial begin
    step(4);
    @(negedge rx_out[0]);
    @(posedge clk) #0 expect("rx",rx_out,2'b10);
    @(posedge clk) #0 expect("rx",rx_out,2'b10);
    @(posedge clk) #0 expect("rx",rx_out,2'b00);
    @(posedge clk) #0 expect("rx",rx_out,2'b11);
    @(posedge clk) #0 expect("rx",rx_out,2'b11);
    @(posedge clk) #0 expect("rx",rx_out,2'b11);
    @(posedge clk) #0 expect("rx",rx_out,2'b10);
    @(posedge clk) #0 expect("rx",rx_out,2'b01);
    @(posedge clk) #0 expect("rx",rx_out,2'b10);
    @(posedge clk) #0 expect("rx",rx_out,2'b10);
    @(posedge clk) #0 expect("rx",rx_out,2'b11);
    @(posedge clk) #0 expect("rx",rx_out,2'b10);
    rx_done <= 1'b1;
  end

  task step(input real x);
  begin
    #(`CLOCK_PERIOD*x);
  end
  endtask

  task bit_step(input real x);
  begin
    #(`CLOCK_PERIOD*x/2);
  end
  endtask

  task expect(input [127:0] s,input integer x, input integer y);
  begin
    // XXX This fails when x or y is > 32 bits (INT_MAX)
    if(x == y) begin
      $display("Info  at time %0t Expected %d for %s, got %d",$time,y,s,x);
    end else begin
      $display("Error at time %0t Expected %d for %s, got %d",$time,y,s,x);
      sim_error = 1;
    end
  end
  endtask

endmodule
