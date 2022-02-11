`timescale 1ns / 1ps
`default_nettype none

`define OPCODE_LIST \
    `X(32'h0000007f, 32'h00000037, `FMT_U,   `INST_LUI,        "lui") \
    `X(32'h0000007f, 32'h00000017, `FMT_U,   `INST_AUIPC,      "auipc") \
    `X(32'h0000007f, 32'h0000006f, `FMT_J,   `INST_JAL,        "jal") \
    `X(32'h0000707f, 32'h00000067, `FMT_I,   `INST_JALR,       "jalr") \
    `X(32'h0000707f, 32'h00000063, `FMT_B,   `INST_BEQ,        "beq") \
    `X(32'h0000707f, 32'h00001063, `FMT_B,   `INST_BNE,        "bne") \
    `X(32'h0000707f, 32'h00004063, `FMT_B,   `INST_BLT,        "blt") \
    `X(32'h0000707f, 32'h00005063, `FMT_B,   `INST_BGE,        "bge") \
    `X(32'h0000707f, 32'h00006063, `FMT_B,   `INST_BLTU,       "bltu") \
    `X(32'h0000707f, 32'h00007063, `FMT_B,   `INST_BGEU,       "bgeu") \
    `X(32'h0000707f, 32'h00000003, `FMT_I,   `INST_LB,         "lb") \
    `X(32'h0000707f, 32'h00001003, `FMT_I,   `INST_LH,         "lh") \
    `X(32'h0000707f, 32'h00002003, `FMT_I,   `INST_LW,         "lw") \
    `X(32'h0000707f, 32'h00004003, `FMT_I,   `INST_LBU,        "lbu") \
    `X(32'h0000707f, 32'h00005003, `FMT_I,   `INST_LHU,        "lhu") \
    `X(32'h0000707f, 32'h00000023, `FMT_S,   `INST_SB,         "sb") \
    `X(32'h0000707f, 32'h00001023, `FMT_S,   `INST_SH,         "sh") \
    `X(32'h0000707f, 32'h00002023, `FMT_S,   `INST_SW,         "sw") \
    `X(32'h0000707f, 32'h00000013, `FMT_I,   `INST_ADDI,       "addi") \
    `X(32'h0000707f, 32'h00002013, `FMT_I,   `INST_SLTI,       "slti") \
    `X(32'h0000707f, 32'h00003013, `FMT_I,   `INST_SLTIU,      "sltiu") \
    `X(32'h0000707f, 32'h00004013, `FMT_I,   `INST_XORI,       "xori") \
    `X(32'h0000707f, 32'h00006013, `FMT_I,   `INST_ORI,        "ori") \
    `X(32'h0000707f, 32'h00007013, `FMT_I,   `INST_ANDI,       "andi") \
    `X(32'hfe00707f, 32'h00001013, `FMT_I,   `INST_SLLI,       "slli") \
    `X(32'hfe00707f, 32'h00005013, `FMT_I,   `INST_SRLI,       "srli") \
    `X(32'hfe00707f, 32'h40005013, `FMT_I,   `INST_SRAI,       "srai") \
    `X(32'hfe00707f, 32'h00000033, `FMT_R,   `INST_ADD,        "add") \
    `X(32'hfe00707f, 32'h40000033, `FMT_R,   `INST_SUB,        "sub") \
    `X(32'hfe00707f, 32'h00001033, `FMT_R,   `INST_SLL,        "sll") \
    `X(32'hfe00707f, 32'h00002033, `FMT_R,   `INST_SLT,        "slt") \
    `X(32'hfe00707f, 32'h00003033, `FMT_R,   `INST_SLTU,       "sltu") \
    `X(32'hfe00707f, 32'h00004033, `FMT_R,   `INST_XOR,        "xor") \
    `X(32'hfe00707f, 32'h00005033, `FMT_R,   `INST_SRL,        "srl") \
    `X(32'hfe00707f, 32'h40005033, `FMT_R,   `INST_SRA,        "sra") \
    `X(32'hfe00707f, 32'h00006033, `FMT_R,   `INST_OR,         "or") \
    `X(32'hfe00707f, 32'h00007033, `FMT_R,   `INST_AND,        "and") \
    `X(32'hf00fffff, 32'h0000000f, `FMT_I,   `INST_FENCE,      "fence") \
    `X(32'hffffffff, 32'h0000100f, `FMT_I,   `INST_FENCE_I,    "fence.i") \
    `X(32'hffffffff, 32'h00000073, `FMT_I,   `INST_ECALL,      "ecall") \
    `X(32'hffffffff, 32'h00100073, `FMT_I,   `INST_EBREAK,     "ebreak") \
    `X(32'h0000707f, 32'h00001073, `FMT_I,   `INST_CSRRW,      "csrrw") \
    `X(32'h0000707f, 32'h00002073, `FMT_I,   `INST_CSRRS,      "csrrs") \
    `X(32'h0000707f, 32'h00003073, `FMT_I,   `INST_CSRRC,      "csrrc") \
    `X(32'h0000707f, 32'h00005073, `FMT_I,   `INST_CSRRWI,     "csrrwi") \
    `X(32'h0000707f, 32'h00006073, `FMT_I,   `INST_CSRRSI,     "csrrsi") \
    `X(32'h0000707f, 32'h00007073, `FMT_I,   `INST_CSRRCI,     "csrrci")


`define SZ_1B 2'b00
`define SZ_2B 2'b01
`define SZ_4B 2'b11

`define REG_RA      1
`define REG_SP      2
`define REG_GP      3
`define REG_TP      4
`define REG_T0      5
`define REG_T1      6
`define REG_T2      7
`define REG_S0      8
`define REG_FP      8
`define REG_S1      9
`define REG_A0      10
`define REG_A1      11
`define REG_A2      12
`define REG_A3      13
`define REG_A4      14
`define REG_A5      15
`define REG_A6      16
`define REG_A7      17
`define REG_S2      18
`define REG_S3      19
`define REG_S4      20
`define REG_S5      21
`define REG_S6      22
`define REG_S7      23
`define REG_S8      24
`define REG_S9      25
`define REG_S10     26
`define REG_S11     27
`define REG_T3      28
`define REG_T4      29
`define REG_T5      30
`define REG_T6      31

`define S_FETCH     1'd0
`define S_UPDATE    1'd1

`define ALU_IN1     4'd0
`define ALU_IN2     4'd1
`define ALU_ADD     4'd2
`define ALU_EQ      4'd3
`define ALU_SUB     4'd4
`define ALU_LT      4'd5
`define ALU_XOR     4'd6
`define ALU_OR      4'd7
`define ALU_AND     4'd8
`define ALU_SLL     4'd9
`define ALU_SRL     4'd10
`define ALU_SRA     4'd11

`define SEL1_RS1    2'd0
`define SEL1_PC     2'd1

`define SEL2_RS2    2'd0
`define SEL2_IMM    2'd1
`define SEL2_SH     2'd2

`define RF_SEL_ALU  2'd0
`define RF_SEL_MEM  2'd1
`define RF_SEL_PC4  2'd2

`define BR_NO       2'd0
`define BR_YES      2'd1
`define BR_IF0      2'd2
`define BR_IF1      2'd3

`define INST_INVALID    6'd0
`define INST_LUI        6'd1
`define INST_AUIPC      6'd2
`define INST_JAL        6'd3
`define INST_JALR       6'd4
`define INST_BEQ        6'd5
`define INST_BNE        6'd6
`define INST_BLT        6'd7
`define INST_BGE        6'd8
`define INST_BLTU       6'd9
`define INST_BGEU       6'd10
`define INST_LB         6'd11
`define INST_LH         6'd12
`define INST_LW         6'd13
`define INST_LBU        6'd14
`define INST_LHU        6'd15
`define INST_SB         6'd16
`define INST_SH         6'd17
`define INST_SW         6'd18
`define INST_ADDI       6'd19
`define INST_SLTI       6'd20
`define INST_SLTIU      6'd21
`define INST_XORI       6'd22
`define INST_ORI        6'd23
`define INST_ANDI       6'd24
`define INST_SLLI       6'd25
`define INST_SRLI       6'd26
`define INST_SRAI       6'd27
`define INST_ADD        6'd28
`define INST_SUB        6'd29
`define INST_SLL        6'd30
`define INST_SLT        6'd31
`define INST_SLTU       6'd32
`define INST_XOR        6'd33
`define INST_SRL        6'd34
`define INST_SRA        6'd35
`define INST_OR         6'd36
`define INST_AND        6'd37
`define INST_FENCE      6'd38
`define INST_FENCE_I    6'd39
`define INST_ECALL      6'd40
`define INST_EBREAK     6'd41
`define INST_CSRRW      6'd42
`define INST_CSRRS      6'd43
`define INST_CSRRC      6'd44
`define INST_CSRRWI     6'd45
`define INST_CSRRSI     6'd46
`define INST_CSRRCI     6'd47

`define FMT_R           3'd0
`define FMT_I           3'd1
`define FMT_S           3'd2
`define FMT_B           3'd3
`define FMT_U           3'd4
`define FMT_J           3'd5

module rv_core(
    inout wire [31:0] data,
    output wire [19:0] addr,
    output wire write,
    output wire read,
    output wire [1:0] size,
    input wire clk,
    input wire rst
);

    wire [1:0] branch;
    wire [4:0] shamt;
    wire [31:0] imm;
    wire [1:0] alu_in1_sel, alu_in2_sel;

    wire state_n, state_q;
    assign state_n = (state_q == `S_FETCH) ? `S_UPDATE : `S_FETCH;
    ff_ar #(.RESET_VAL(`S_FETCH)) state_ff(.clk(clk), .rst(rst), .d(state_n), .q(state_q));

    wire [31:0] pc_q;
    reg [31:0] pc_n;
    ff_ar #(.W(32)) pc(.clk(clk), .rst(rst), .d(pc_n), .q(pc_q));

    wire rf_write_n, rf_write_q;
    ff_ar rf_write_ff(.clk(clk), .rst(rst), .d(rf_write_n && (state_q == `S_FETCH)), .q(rf_write_q));

    wire [1:0] rf_wdata_sel_n, rf_wdata_sel_q;
    ff_ar #(.W(2)) rf_wdata_sel_ff(.clk(clk), .rst(rst), .d(rf_wdata_sel_n), .q(rf_wdata_sel_q));

    wire [4:0] rd_n, rd_q, rs1, rs2;
    wire [31:0] rs1_val, rs2_val;
    ff_ar #(.W(5)) rd_ff(.clk(clk), .rst(rst), .d(rd_n), .q(rd_q));

    wire [3:0] alu_op;
    reg [31:0] alu_in1, alu_in2, alu_out;
    wire [31:0] alu_out_q;
    ff_ar #(.W(32)) alu_reg(.clk(clk), .rst(rst), .d(alu_out), .q(alu_out_q));

    wire [31:0] rs2_val_q;
    ff_ar #(.W(32)) rs2_val_ff(.clk(clk), .rst(rst), .d(rs2_val), .q(rs2_val_q));

    reg [31:0] reg_file [0:31];

    wire [19:0] addr_n, addr_q;
    assign addr_n = (state_n == `S_FETCH) ? pc_n : alu_out;
    ff_ar #(.W(20)) addr_reg(.clk(clk), .rst(rst), .d(addr_n), .q(addr_q));

    wire write_n, write_q;
    ff_ar write_ff(.clk(clk), .rst(rst), .d(write_n), .q(write_q));

    wire read_n, read_q;
    ff_ar #(.RESET_VAL(1)) read_ff(.clk(clk), .rst(rst), .d(read_n), .q(read_q));

    assign write = write_q && (state_q == `S_UPDATE);
    assign read = read_q || (state_q == `S_FETCH);
    assign size = `SZ_4B;
    assign data = write_q ? rs2_val_q : 'bz; // TODO: could we latch rs2 instead?
    assign addr = addr_q;

    always @(*) begin
        if (state_q == `S_FETCH) begin
            case (branch)
                `BR_NO:     pc_n = pc_q + 4;
                `BR_YES:    pc_n = alu_out;
                `BR_IF0:    pc_n = alu_out[0] == 1'b0 ? pc_q + imm : pc_q + 4;
                `BR_IF1:    pc_n = alu_out[0] == 1'b1 ? pc_q + imm : pc_q + 4;
            endcase
        end else begin
            pc_n = pc_q;
        end
    end

    assign shamt = {27'b0, data[24:20]};

    // decode
    decode decode_inst(
        .op(data),
        .alu_op(alu_op),
        .alu_in1_sel(alu_in1_sel),
        .alu_in2_sel(alu_in2_sel),
        .imm(imm),
        .rd(rd_n), .rs1(rs1), .rs2(rs2),
        .rf_write(rf_write_n),
        .rf_wdata_sel(rf_wdata_sel_n),
        .branch(branch),
        .read(read_n),
        .write(write_n)
    );

    // alu inputs
    always @(*) begin
        alu_in1 = 'bx;
        case (alu_in1_sel)
            `SEL1_RS1: alu_in1 = rs1_val;
            `SEL1_PC:  alu_in1 = pc_q;
        endcase
        alu_in2 = 'bx;
        case (alu_in2_sel)
            `SEL2_IMM: alu_in2 = imm;
            `SEL2_RS2: alu_in2 = rs2_val;
            `SEL2_SH:  alu_in2 = shamt;
        endcase
    end

    // alu output
    always @(*) begin
        alu_out = 'bx;
        case (alu_op)
            `ALU_IN1: alu_out = alu_in1;
            `ALU_IN2: alu_out = alu_in2;
            `ALU_ADD: alu_out = alu_in1 + alu_in2;
            `ALU_SUB: alu_out = alu_in1 - alu_in2;
            `ALU_EQ:  alu_out = alu_in1 == alu_in2;
            `ALU_LT : alu_out = alu_in1 < alu_in2;
            `ALU_XOR: alu_out = alu_in1 ^ alu_in2;
            `ALU_OR : alu_out = alu_in1 | alu_in2;
            `ALU_AND: alu_out = alu_in1 & alu_in2;
            `ALU_SLL: alu_out = alu_in1 << alu_in2;
            `ALU_SRL: alu_out = alu_in1 >> alu_in2;
            `ALU_SRA: alu_out = alu_in1 >> alu_in2;
        endcase
    end

    // regfile
    assign rs1_val = (rs1 == 5'b0) ? 32'b0 : reg_file[rs1];
    assign rs2_val = (rs2 == 5'b0) ? 32'b0 : reg_file[rs2];
    integer i;
    always @(posedge clk, rst) begin
        if (rst) begin
            for (i = 0; i < 32; i = i + 1) begin
                reg_file[i] <= 32'b0;
            end
            // TODO: remove the following -- for test program only
            reg_file[`REG_A1] <= 32'd5;
            reg_file[`REG_A0] <= 32'h54; // &a[0]
            reg_file[`REG_RA] <= 32'h4c; // return address
        end else begin
            if (rf_write_q) begin
                case (rf_wdata_sel_q)
                    `RF_SEL_ALU: reg_file[rd_q] <= alu_out_q;
                    `RF_SEL_MEM: reg_file[rd_q] <= data;
                    `RF_SEL_PC4: reg_file[rd_q] <= pc_q + 4;
                endcase
            end
        end
    end

endmodule

module decode(
    input wire [31:0] op,
    output wire [3:0] alu_op,
    output wire [1:0] alu_in1_sel,
    output wire [1:0] alu_in2_sel,
    output reg [31:0] imm,
    output wire [4:0] rd, rs1, rs2,
    output wire rf_write,
    output wire [1:0] rf_wdata_sel,
    output wire [1:0] branch,
    output wire read,
    output wire write
);

    reg [5:0] inst;
    reg [3:0] fmt;

    wire [ 2:0] func3;
    wire [ 6:0] func7;
    wire [11:0] imm_11_0;
    wire [ 4:0] imm_4_0;
    wire [ 6:0] imm_11_5;
    wire        imm_11_a7;
    wire [ 3:0] imm_4_1;
    wire [ 5:0] imm_10_5;
    wire        imm_12;
    wire [ 9:0] imm_31_12;
    wire [ 7:0] imm_19_12;
    wire        imm_11_a20;
    wire [ 9:0] imm_10_1;
    wire        imm_20;

    assign rd          = op[11:7];
    assign rs1         = op[19:15];
    assign rs2         = op[24:20];
    assign func3       = op[14:12];
    assign func7       = op[31:25];
    assign imm_11_0    = op[31:20];
    assign imm_4_0     = op[11:7];
    assign imm_11_5    = op[31:25];
    assign imm_11_a7   = op[7];
    assign imm_4_1     = op[11:8];
    assign imm_10_5    = op[30:25];
    assign imm_12      = op[31];
    assign imm_31_12   = op[31:12];
    assign imm_19_12   = op[19:12];
    assign imm_11_a20  = op[20];
    assign imm_10_1    = op[30:21];
    assign imm_20      = op[31];

    always @(*) begin

        inst = `INST_INVALID;
        if (0) begin
        `define X(MASK, VALUE, FMT, MNEM, STR) \
            end else if ((op & MASK) == VALUE) begin inst = MNEM;
        `OPCODE_LIST
        end
        `undef X
    end

    always @(*) begin
        fmt = 'bx;
        case (inst)
        `define X(MASK, VALUE, FMT, MNEM, STR) \
            MNEM:  fmt = FMT;
            `OPCODE_LIST
        `undef X
        endcase
    end

    // TODO: confirm this
    always @(*) begin
        imm = 'bx;
        case (fmt)
            `FMT_I: imm = {{20{imm_11_0[11]}}, imm_11_0};
            `FMT_S: imm = {{20{imm_11_5[6]}}, imm_11_5, imm_4_0};
            `FMT_B: imm = {{19{imm_12}}, imm_12, imm_11_a7, imm_10_5, imm_4_1, 1'b0};
            `FMT_U: imm = {imm_31_12, 12'b0};
            `FMT_J: imm = {{19{imm_20}}, imm_20, imm_19_12, imm_11_a20, imm_10_1, 1'b0};
        endcase
    end

    reg [14:0] decode_out;

    assign alu_op       = decode_out[14:11];
    assign alu_in1_sel  = decode_out[10:9];
    assign alu_in2_sel  = decode_out[8:7];
    assign rf_write     = decode_out[6];
    assign rf_wdata_sel = decode_out[5:4];
    assign branch       = decode_out[3:2];
    assign read         = decode_out[1];
    assign write        = decode_out[0];

    // TODO: make sure signed/unsigned is handled correctly
    always @(*) begin
        decode_out = 15'bx;
        case (inst)
            `INST_LUI:      decode_out = {`ALU_IN2, 2'bx,      `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_AUIPC:    decode_out = {`ALU_ADD, `SEL1_PC,  `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_JAL:      decode_out = {`ALU_ADD, `SEL1_PC,  `SEL2_IMM, 1'b1, `RF_SEL_PC4, `BR_YES, 1'b0, 1'b0};
            `INST_JALR:     decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_PC4, `BR_YES, 1'b0, 1'b0};
            `INST_BEQ:      decode_out = {`ALU_EQ,  `SEL1_RS1, `SEL2_RS2, 1'b0, 2'bx,        `BR_IF1, 1'b0, 1'b0};
            `INST_BNE:      decode_out = {`ALU_EQ,  `SEL1_RS1, `SEL2_RS2, 1'b0, 2'bx,        `BR_IF0, 1'b0, 1'b0};
            `INST_BLT:      decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_RS2, 1'b0, 2'bx,        `BR_IF1, 1'b0, 1'b0};
            `INST_BGE:      decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_RS2, 1'b0, 2'bx,        `BR_IF0, 1'b0, 1'b0};
            `INST_BLTU:     decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_RS2, 1'b0, 2'bx,        `BR_IF1, 1'b0, 1'b0};
            `INST_BGEU:     decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_RS2, 1'b0, 2'bx,        `BR_IF0, 1'b0, 1'b0};
            `INST_LB:       decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_MEM, `BR_NO,  1'b1, 1'b0};
            `INST_LH:       decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_MEM, `BR_NO,  1'b1, 1'b0};
            `INST_LW:       decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_MEM, `BR_NO,  1'b1, 1'b0};
            `INST_LBU:      decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_MEM, `BR_NO,  1'b1, 1'b0};
            `INST_LHU:      decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_MEM, `BR_NO,  1'b1, 1'b0};
            `INST_SB:       decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b0, 2'bx,        `BR_NO,  1'b0, 1'b1};
            `INST_SH:       decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b0, 2'bx,        `BR_NO,  1'b0, 1'b1};
            `INST_SW:       decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b0, 2'bx,        `BR_NO,  1'b0, 1'b1};
            `INST_ADDI:     decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SLTI:     decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SLTIU:    decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_XORI:     decode_out = {`ALU_XOR, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_ORI:      decode_out = {`ALU_OR,  `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_ANDI:     decode_out = {`ALU_AND, `SEL1_RS1, `SEL2_IMM, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SLLI:     decode_out = {`ALU_SLL, `SEL1_RS1, `SEL2_SH,  1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SRLI:     decode_out = {`ALU_SRL, `SEL1_RS1, `SEL2_SH,  1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SRAI:     decode_out = {`ALU_SRA, `SEL1_RS1, `SEL2_SH,  1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_ADD:      decode_out = {`ALU_ADD, `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SUB:      decode_out = {`ALU_SUB, `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SLL:      decode_out = {`ALU_SLL, `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SLT:      decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SLTU:     decode_out = {`ALU_LT,  `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_XOR:      decode_out = {`ALU_XOR, `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SRL:      decode_out = {`ALU_SRL, `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_SRA:      decode_out = {`ALU_SRA, `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_OR:       decode_out = {`ALU_OR,  `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_AND:      decode_out = {`ALU_AND, `SEL1_RS1, `SEL2_RS2, 1'b1, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0};
            `INST_FENCE:    decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_FENCE_I:  decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_ECALL:    decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_EBREAK:   decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_CSRRW:    decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_CSRRS:    decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_CSRRC:    decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_CSRRWI:   decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_CSRRSI:   decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            `INST_CSRRCI:   decode_out = {`ALU_IN1, `SEL1_RS1, `SEL2_RS2, 1'b0, `RF_SEL_ALU, `BR_NO,  1'b0, 1'b0}; // TODO
            default:        decode_out = {5'bx,     2'bx,      2'bx,      1'b0, 2'bx,        `BR_NO,  1'b0, 1'b0};
        endcase
    end

endmodule
