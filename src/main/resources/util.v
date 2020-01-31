
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

module ClockToDifferential (
    input clock,
    inout out_p,
    inout out_n
);

    assign out_p = !clock;
    assign out_n = clock;

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

module BoolToDifferential (
    input in,
    inout out_p,
    inout out_n
);

    assign out_p = in;
    assign out_n = !in;

endmodule

module ErrorInjector #(
    parameter per1k = 0
) (
    input in_p,
    input in_n,
    output out_p,
    output out_n,
    input reset,
    input clock,
    input stop,
    output reg [63:0] errors
);

    reg inject;

    assign out_p = in_p ^ inject;
    assign out_n = in_n ^ inject;

    always @(posedge clock or negedge clock) begin
        if (reset) begin
            errors <= 0;
            inject <= 0;
        end else begin
            if (stop) begin
                inject <= 0;
            end else begin
                if ($urandom % 1000 < per1k) begin
                    inject <= 1;
                    errors <= errors + 1;
                end else begin
                    inject <= 0;
                end
            end
        end
    end


endmodule
