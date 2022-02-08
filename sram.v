`timescale 1ns / 1ps
`default_nettype none

module sram(
    input wire [17:0] addr,
    inout wire [31:0] data,
    input wire oe_l, we_l
);

    reg [31:0] data_out;
    assign data = ~oe_l ? data_out : 'bz;

    reg [31:0] mem [0:65536];

    always @(*) begin
        if (~we_l) begin
            mem[addr[17:2]] <= {data[31:0], mem[addr[17:2]][31:0]};
        end else if (~oe_l) begin
            data_out[31:0] <= mem[addr[17:2]][31:0];
        end
    end

    initial begin
        mem[0]  = 32'h00450693;
        mem[1]  = 32'h00100713;
        mem[2]  = 32'h00b76463;
        mem[3]  = 32'h00008067;
        mem[4]  = 32'h0006a803;
        mem[5]  = 32'h00068613;
        mem[6]  = 32'h00070793;
        mem[7]  = 32'hffc62883;
        mem[8]  = 32'h01185a63;
        mem[9]  = 32'h01162023;
        mem[10] = 32'hfff78793;
        mem[11] = 32'hffc60613;
        mem[12] = 32'hfe0796e3;
        mem[13] = 32'h00279793;
        mem[14] = 32'h00f507b3;
        mem[15] = 32'h0107a023;
        mem[16] = 32'h00170713;
        mem[17] = 32'h00468693;
        mem[18] = 32'hfc1ff06f;
        mem[19] = 32'h00100073;
        mem[20] = 32'h00000000;
        mem[21] = 32'h00000004;
        mem[22] = 32'h00000003;
        mem[23] = 32'h00000007;
        mem[24] = 32'h00000002;
        mem[25] = 32'h00000005;
    end

endmodule
