
module async_reset_reg0 (
    input d,
    input clk,
    input rst,
    output reg q
);

    always @(posedge clk or posedge rst) begin
        if (rst)
            q <= 1'b0;
        else
            q <= d;
    end

endmodule

module async_reset_reg1 (
    input d,
    input clk,
    input rst,
    output reg q
);

    always @(posedge clk or posedge rst) begin
        if (rst)
            q <= 1'b1;
        else
            q <= d;
    end

endmodule
