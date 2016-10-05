// Example reference generator module compatible with HBWIF
// Use the ExampleHbwifConfig to instantiate this IP

module example_reference_generator (
  input [1:0] config_mirrorMultiplier,
  input irefIn,
  output [7:0] irefOut
  `ifdef HBWIF_USE_PG_PINS
  , inout avdd
  , inout gnd
  `endif
);


endmodule