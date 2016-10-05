// Example transceiver module compatible with HBWIF
// Use the ExampleHbwifConfig to instantiate this IP

module example_transceiver (
  input fastClk,
  output slowClk,
  input reset,
  input rx_p,
  input rx_n,
  output tx_p,
  output tx_n,
  input [9:0] data_tx,
  output [9:0] data_rx,
  input iref,
  input [3:0] extraInputs_txSwing,
  input extraInputs_cdrMode
);


endmodule
