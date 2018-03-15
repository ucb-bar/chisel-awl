
module async_reset_reg0 (
    input d,
    input clk,
    input rst,
    output q
);

    reg d;

    always @(posedge clk or posedge rst) begin
        if (rst)
            d <= 1'b0;
        else
            d <= q;
    end

endmodule

module async_reset_reg1 (
    input d,
    input clk,
    input rst,
    output q
);

    reg d;

    always @(posedge clk or posedge rst) begin
        if (rst)
            d <= 1'b1;
        else
            d <= q;
    end

endmodule
