// johnwright@eecs.berkeley.edu
//
// RTL for the HBWIF Deserializer
//

module serdes #(
    parameter NDIVBY = 8,
    parameter WDIVBY = 3 //log2up(NDIVBY)
    ) (
    input [1:0] rx_in_0,
    input [1:0] rx_in_1,
    input [1:0] rx_in_2,
    input [2*NDIVBY-1:0] tx_in,
    input clks,
    input reset,
    input [1:0] config_rx_sel,
    input config_rx_edge_sel,
    output reg [2*NDIVBY-1:0] rx_out,
    output [1:0] tx_out);

    reg [2*NDIVBY-1:0] rx_buffer;
    reg [2*NDIVBY-1:0] tx_buffer;
    reg [WDIVBY-1:0] count;
    reg [1:0] rx_in_d;
    reg [1:0] rx_edge_sel_out;
    reg [1:0] rx_mux_out;
    reg [1:0] rx_in_negedge;
    reg [1:0] rx_in_negedge_d;
    reg [1:0] rx_mux_out_rev_d;
    reg [1:0] rx_edge_sel_d;
    reg [1:0] rx_sel_d;
    integer i;

    // delayed signals to help with clock crossing

    // sync all the clock crossing boundaries
    // [jcw]: removed, move to Chisel domain


    // select which of the three slices
    always @(*) begin
      if (rx_sel_d == 2'b00) begin
        rx_mux_out = rx_in_0;
      end else if (rx_sel_d == 2'b01) begin
        rx_mux_out = rx_in_1;
      end else begin
        rx_mux_out = rx_in_2;
      end
    end

    wire [1:0] rx_mux_out_rev;
    assign rx_mux_out_rev[1] = rx_mux_out[0];
    assign rx_mux_out_rev[0] = rx_mux_out[1];

    // edge retiming logic
    always @(negedge clks) begin
      rx_in_negedge <= rx_mux_out_rev;
    end

    // buffer a cycle for QOR
    // TODO remove this
    always @(posedge clks) begin
      rx_in_negedge_d <= rx_in_negedge;
      rx_mux_out_rev_d <= rx_mux_out_rev;
      rx_edge_sel_d <= config_rx_edge_sel;
      rx_sel_d <= config_rx_sel;
    end

    // select which edge
    always @(*) begin
      if (rx_edge_sel_d) begin
        rx_edge_sel_out = rx_in_negedge_d;
      end else begin
        rx_edge_sel_out = rx_mux_out_rev_d;
      end
    end

    // buffer
    always @(posedge clks) begin
      rx_in_d <= rx_edge_sel_out;
    end

    assign tx_out[1] = tx_buffer[1];
    assign tx_out[0] = tx_buffer[0];

    // counter
    always @(posedge clks) begin
      if (reset) begin
        rx_out <= 0;
        count <= 0;
      end else begin

        // rx shift register
        for(i = 1; i < NDIVBY; i = i + 1) begin
          rx_buffer[i*2-2] <= rx_buffer[i*2];
          rx_buffer[i*2-1] <= rx_buffer[i*2+1];
        end
        rx_buffer[2*NDIVBY-1] <= rx_in_d[1];
        rx_buffer[2*NDIVBY-2] <= rx_in_d[0];

        if (count == (NDIVBY-1)) begin
          // This will hold the outputs for an entire slow clock cycle
          rx_out <= rx_buffer;
          tx_buffer <= tx_in;
          count <= 0;
        end else begin
          count <= count + 1;

          // tx shift register
          for(i = 1; i < NDIVBY; i = i + 1) begin
            tx_buffer[i*2-2] <= tx_buffer[i*2];
            tx_buffer[i*2-1] <= tx_buffer[i*2+1];
          end
          tx_buffer[2*NDIVBY-1] <= 1'b0;
          tx_buffer[2*NDIVBY-2] <= 1'b0;
        end
      end
    end

endmodule
