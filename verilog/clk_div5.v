// johnwright@eecs.berkeley.edu
//
// RTL for the HBWIF clock divider
//
// This clock divider is partially hand-synthesized to ensure only clock network
// cells are used in the clock path.
//

module clk_div5 (
  input clk_in,
  output clk_out
  );

  wire [2:0] d;
  wire [2:0] q;
  wire clkb, duty_correction, duty_correction_q;

  //////////////////////////////////////////// 40% duty cycle divide-by-5
  //
  // We use a custom pseudo-Gray-coded increment scheme that eliminates clock
  // glitches. 111 -> 101 -> 100 -> 000 -> 010 (-> 111 ...)
  // Q0 is a 40% duty cycle output clock
  // These equations come from solving the K map from the previously described
  // counter, i.e.:
  // rst Q2 Q1 Q0 | D2 D1 D0
  //   0  0  0  0 |  0  1  0
  //   0  0  0  1 |  x  x  x
  //   0  0  1  0 |  1  1  1
  //   0  0  1  1 |  x  x  x
  //   0  1  0  0 |  0  0  0
  //   0  1  0  1 |  1  0  0
  //   0  1  1  0 |  x  x  x
  //   0  1  1  1 |  1  0  1
  //   1  x  x  x |  1  1  1
  //   NOTE we got rid of reset, but this implementation will self-reset to
  //   a valid state
  assign d[0] = q[1];
  assign d[1] = ~q[2];
  assign d[2] = q[1] || q[0];

  // WARNING: Be sure to only use clock network cells in the clock path
  reg [2:0] q_reg;
  assign q = q_reg;

  always @(posedge clk_in) begin
    q_reg <= d;
  end

  //////////////////////////////////////////// Duty cycle correction

  // hold duty correction high after q = 101
  // (note that 001 isn't can't happen, so q2 is a don't care)
  assign duty_correction = ~q[1] && q[0];

  // WARNING: Be sure to only use clock network cells in the clock path
  reg duty_correction_q_reg;
  assign duty_correction_q = duty_correction_q_reg;
  always @(negedge clk_in) begin
    duty_correction_q_reg <= duty_correction;
  end

  assign clk_out = q[0] | duty_correction_q;

endmodule
