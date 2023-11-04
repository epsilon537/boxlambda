`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

module picorv_burst_fsm #(
    parameter BURST_REG_BASE_ADDR = 32'h10002020
    ) (
    input logic clk,
    input logic rst,

    //picorv interface
    input logic        picorv_valid_i,
	output logic       picorv_rdy_o,

	input logic [31:2] picorv_addr_i,
	input logic [31:0] picorv_wdata_i,
	input logic [ 3:0] picorv_wstrb_i,
	output logic [31:0] picorv_rdata_o,

    //32-bit pipelined Wishbone master interface.
    output logic [31:2] wbm_adr_o,
	output logic [31:0] wbm_dat_o,
	input logic [31:0] wbm_dat_i,
	output logic wbm_we_o,
	output logic [3:0] wbm_sel_o,
	output logic wbm_stb_o,
	input logic wbm_ack_i,
    input logic wbm_stall_i,
	output logic wbm_cyc_o,
    input logic wbm_err_i
);

localparam BURST_REG_BASE_WORD_ADDR = (BURST_REG_BASE_ADDR/4);
localparam NUM_BURST_REGS = 6;
localparam OFFSET_BURST_REG = 5;

logic unused = &{wbm_err_i};

typedef enum {IDLE, SINGLE, BURST, LOCAL } Single_Burst_State_Type;
Single_Burst_State_Type sb_state;

logic picorv_rdy;
logic [31:0] picorv_rdata;
logic [31:2] wbm_adr;
logic [31:0] wbm_dat;
logic wbm_we;
logic [3:0] wbm_sel;
logic wbm_stb;
logic wbm_cyc;

logic [31:0] burst_reg [0:5];
logic [1:0] burst_phase;
logic [2:0] burst_phase_ext;
logic addr_in_burst_reg_range;
logic [2:0] burst_reg_selector;

logic [1:0] offset;
logic [31:2] picorv_addr_reg;
logic [ 3:0] picorv_wstrb_reg;
logic picorv_rdy_reg;
logic wbm_stb_reg, wbm_cyc_reg;

assign picorv_rdy_o = picorv_rdy;
assign picorv_rdata_o = picorv_rdata;
assign wbm_adr_o = wbm_adr;
assign wbm_dat_o = wbm_dat;
assign wbm_we_o = wbm_we;
assign wbm_sel_o = wbm_sel;
assign wbm_stb_o = wbm_stb;
assign wbm_cyc_o = wbm_cyc;

assign burst_phase_ext = {1'b0, burst_phase};
assign offset = burst_reg[OFFSET_BURST_REG][1:0];
assign addr_in_burst_reg_range = (picorv_addr_i >= 30'(BURST_REG_BASE_WORD_ADDR)) && (picorv_addr_i < 30'(NUM_BURST_REGS+BURST_REG_BASE_WORD_ADDR));
assign burst_reg_selector = picorv_addr_i[4:2]; 

always_comb begin
    case (sb_state)
        IDLE: begin
            picorv_rdy=1'b0;
            picorv_rdata = 32'b0;
            wbm_adr = {1'b0, picorv_addr_i[30:2]};
            wbm_dat = picorv_wdata_i;
            wbm_we = |picorv_wstrb_i;
            wbm_sel = wbm_we ? picorv_wstrb_i : 4'b1111;
            wbm_stb = picorv_valid_i && ~addr_in_burst_reg_range && ~picorv_addr_i[31];
            wbm_cyc = picorv_valid_i && ~addr_in_burst_reg_range && ~picorv_addr_i[31];
        end
        SINGLE: begin
            picorv_rdy = wbm_ack_i;
            picorv_rdata = wbm_dat_i;
            wbm_adr = {1'b0, picorv_addr_i[30:2]};
            wbm_dat = picorv_wdata_i;
            wbm_we = |picorv_wstrb_i;
            wbm_sel = wbm_we ? picorv_wstrb_i : 4'b1111;
            wbm_stb = wbm_stb_reg;
            wbm_cyc = 1'b1;
        end
        BURST: begin
            picorv_rdy = picorv_rdy_reg;
            picorv_rdata = wbm_dat_i;
            wbm_adr = picorv_addr_reg + {28'b0, burst_phase};
            wbm_dat = burst_reg[burst_phase_ext];
            wbm_we = |picorv_wstrb_reg;
            wbm_sel = wbm_we ? picorv_wstrb_reg : 4'b1111;
            wbm_stb = wbm_stb_reg;
            wbm_cyc = wbm_cyc_reg;
        end
        default: begin /*LOCAL:*/
            picorv_rdy = 1'b1;
            picorv_rdata = burst_reg[burst_reg_selector];
            wbm_adr = 30'b0;
            wbm_dat = 32'b0;
            wbm_we = 1'b0;
            wbm_sel = 4'b0;
            wbm_stb = 1'b0;
            wbm_cyc = 1'b0;
        end
    endcase
end

always_ff @(posedge clk) begin
    if (rst) begin
        sb_state <= IDLE;
        burst_phase <= 2'd0;
        picorv_rdy_reg <= 1'b0;
        wbm_stb_reg <= 1'b0;
        wbm_cyc_reg <= 1'b0;

        for (int ii=0; ii<NUM_BURST_REGS; ii = ii+1) begin
            burst_reg[ii] <= 32'b0;
        end
    end
    else begin
        case (sb_state)
            IDLE: begin
                if (picorv_valid_i) begin
                    if (addr_in_burst_reg_range) begin
                        /*Write?*/
                        if (picorv_wstrb_i != 4'b0) begin
                            burst_reg[burst_reg_selector] <= picorv_wdata_i;
                        end
                        sb_state <= LOCAL;
                    end
                    else begin
                        if (~picorv_addr_i[31]) begin
                            sb_state <= SINGLE;
                        end
                        else begin
                            /*Read? Copy burst reg 4 to 0*/
                            if (picorv_wstrb_i == 4'b0) begin
                                burst_reg[0] <= burst_reg[4];
                            end
                            sb_state <= BURST;
                            picorv_rdy_reg <= 1'b1;
                            burst_phase <= 2'd0;
                        end
                        
                        picorv_addr_reg <= {1'b0, picorv_addr_i[30:2]};
                        picorv_wstrb_reg <= picorv_wstrb_i;
                        wbm_cyc_reg <= 1'b1;
                        wbm_stb_reg <= 1'b1;
                    end
                end
            end
            SINGLE: begin
                if (~wbm_stall_i) begin
                    wbm_stb_reg <= 1'b0;
                    if (wbm_ack_i) begin
                        wbm_cyc_reg <= 1'b0;
                        sb_state <= IDLE;
                    end
                end
            end
            BURST: begin
                picorv_rdy_reg <= 1'b0;
                if (~wbm_stall_i) begin
                    wbm_stb_reg <= 1'b0;
                    if (wbm_ack_i) begin
                        /*Read?*/
                        if (picorv_wstrb_reg == 4'b0) begin
                            case (offset)
                                2'd0: begin
                                    burst_reg[burst_phase_ext] <= wbm_dat_i;
                                end
                                2'd1: begin
                                    burst_reg[burst_phase_ext][31:8] <= wbm_dat_i[23:0];
                                    burst_reg[burst_phase_ext+1][7:0] <= wbm_dat_i[31:24];
                                end
                                2'd2: begin
                                    burst_reg[burst_phase_ext][31:16] <= wbm_dat_i[15:0];
                                    burst_reg[burst_phase_ext+1][15:0] <= wbm_dat_i[31:16];
                                end
                                2'd3: begin
                                    burst_reg[burst_phase_ext][31:24] <= wbm_dat_i[7:0];
                                    burst_reg[burst_phase_ext+1][23:0] <= wbm_dat_i[31:8];
                                end
                            endcase
                        end
                        if (burst_phase == 2'd3) begin
                            burst_phase <= 2'd0;
                            sb_state <= IDLE;
                            wbm_cyc_reg <= 1'b0;
                        end
                        else begin
                            burst_phase <= burst_phase + 2'd1;
                            wbm_stb_reg <= 1'b1;
                        end    
                    end
                end
            end
            LOCAL: begin /*End phase to allow picorv_valid to go to 0.*/
                sb_state <= IDLE;
            end
        endcase
    end
end

endmodule
