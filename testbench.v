`timescale 1ns / 1ps
`default_nettype none

module testbench;

    wire write;
    wire read;
    wire [1:0] size;
    reg clk;
    reg rst;

    wire [19:0] addr;
    wire [31:0] data;
    wire oe_l;
    wire we_l;
    wire ce1_l, ce2_l;
    wire ub1_l, lb1_l, ub2_l, lb2_l;

    always begin
        #10 clk = ~clk;
    end

    initial begin
        clk <= 1'b0;
        rst <= 1'b0;
        #5;
        rst <= 1'b1;
        #5;
        rst <= 1'b0;
        
        repeat (100) @(posedge clk);
        $finish;
    end

    assign addr[19:18] = 2'b0;

    sram mem(.addr(addr[17:0]), .data(data),  .oe_l(oe_l), .we_l(we_l));

    assign oe_l = ~read;
    assign we_l = ~write;
    assign ce1_l = 1'b0;
    assign ce2_l = 1'b0;
    assign ub1_l = 1'b0;
    assign lb1_l = 1'b0;
    assign ub2_l = 1'b0;
    assign lb2_l = 1'b0;

    rv_core rvc(
        .data(data),
        .addr(addr),
        .write(write),
        .read(read),
        .size(size),
        .clk(clk),
        .rst(rst)
    );

endmodule
