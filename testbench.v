`timescale 1ns / 1ps
`default_nettype none

`define INST_EBREAK     6'd41
`define S_FETCH     1'd0

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
        
        repeat (90) begin
            @(posedge clk);
            $display("fetch        %h: %h", addr, data);
            @(posedge clk);
            if (!oe_l) begin
                $display("update read  %h: %h", addr, data);
            end
            if (!we_l) begin
                $display("update write %h: %h", addr, data);
            end
        end

        $finish;
    end

    always @(*) begin
        if ((rvc.state_q == `S_FETCH) && (rvc.decode_inst.inst == `INST_EBREAK)) begin
            $display("EBREAK %t", $time);
            $finish;
        end
    end

    assign addr[19:18] = 2'b0;

    //sram sram1(.addr(addr[17:0]), .data(data[31:16]), .oe_l(oe_l), .we_l(we_l), .ce_l(ce1_l), .ub_l(ub1_l), .lb_l(lb1_l));
    //sram sram2(.addr(addr[17:0]), .data(data[15:0]),  .oe_l(oe_l), .we_l(we_l), .ce_l(ce2_l), .ub_l(ub2_l), .lb_l(lb2_l));
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
