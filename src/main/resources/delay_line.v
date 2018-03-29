
module DifferentialDelayLine #(
    parameter delay = 5
) (
    input clock,
    input in_p,
    input in_n,
    output out_p,
    output out_n
);

    reg [delay-1:0] delayline_p;
    reg [delay-1:0] delayline_n;

    genvar i;

    always @(posedge clock or negedge clock) begin
        delayline_p[0] <= in_p;
        delayline_n[0] <= in_n;
    end

    assign out_p = delayline_p[delay-1];
    assign out_n = delayline_n[delay-1];

    generate
    for (i = 1; i < delay; i = i + 1) begin:loop
        always @(posedge clock or negedge clock) begin
            delayline_p[i] <= delayline_p[i-1];
            delayline_n[i] <= delayline_n[i-1];
        end
    end
    endgenerate

endmodule

module DifferentialToBool (
    input in_p,
    input in_n,
    output outTee_p,
    output outTee_n,
    output outBool
);

    assign outBool = (in_p ^ in_n) ? in_p : 1'bx;
    assign outTee_p = in_p;
    assign outTee_n = in_n;

endmodule
