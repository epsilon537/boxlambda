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

typedef enum {IDLE, SINGLE, BURST, END_PHASE } Single_Burst_State_Type;
Single_Burst_State_Type sb_state;
typedef enum {WAIT_START, WAIT_ACK} Stall_State_Type;
Stall_State_Type ss_state;

logic [31:0] burst_reg [0:5];

logic wr_req;
logic [1:0] burst_phase;
logic [2:0] burst_phase_ext;
logic addr_in_burst_reg_range;
logic [2:0] burst_reg_sel;

logic [1:0] offset;
logic [31:2] picorv_addr_reg;
logic [ 3:0] picorv_wstrb_reg;

assign burst_phase_ext = {1'b0, burst_phase};
assign offset = burst_reg[OFFSET_BURST_REG][1:0];
assign addr_in_burst_reg_range = (picorv_addr_i >= 30'(BURST_REG_BASE_WORD_ADDR)) && (picorv_addr_i < 30'(NUM_BURST_REGS+BURST_REG_BASE_WORD_ADDR));
assign burst_reg_sel = picorv_addr_i[4:2]; 
assign wr_req = |picorv_wstrb_reg;
assign wbm_we_o = wr_req;
assign wbm_adr_o = picorv_addr_reg + {28'b0, burst_phase};
assign wbm_dat_o = (sb_state == BURST) ? burst_reg[burst_phase_ext] : picorv_wdata_i;
assign wbm_sel_o = wr_req ? picorv_wstrb_reg : 4'b1111;
assign picorv_rdata_o = addr_in_burst_reg_range ? burst_reg[burst_reg_sel] : wbm_dat_i;

always_ff @(posedge clk) begin
    if (rst) begin
        sb_state <= IDLE;
        ss_state <= WAIT_START;
        burst_phase <= 2'd0;
        picorv_rdy_o <= 1'b0;
        wbm_stb_o <= 1'b0;
        wbm_cyc_o <= 1'b0;

        for (int ii=0; ii<NUM_BURST_REGS; ii = ii+1) begin
            burst_reg[ii] <= 32'b0;
        end
    end
    else begin
        case (sb_state)
            IDLE: begin
                if (picorv_valid_i) begin
                    picorv_addr_reg <= {1'b0, picorv_addr_i[30:2]};
                    picorv_wstrb_reg <= picorv_wstrb_i;

                    if (addr_in_burst_reg_range) begin
                        if (picorv_wstrb_i != 4'b0) begin
                            burst_reg[burst_reg_sel] <= picorv_wdata_i;
                        end
                        picorv_rdy_o <= 1'b1;
                        sb_state <= END_PHASE;
                    end
                    else begin
                        if (~picorv_addr_i[31]) begin
                            sb_state <= SINGLE;
                        end
                        else begin
                            if (picorv_wstrb_i == 4'b0) begin
                                burst_reg[0] <= burst_reg[4];
                            end
                            sb_state <= BURST;
                            picorv_rdy_o <= 1'b1;
                            burst_phase <= 2'd0;
                        end
                        if (wbm_stall_i) begin
                            ss_state <= WAIT_START;    
                        end
                        else begin
                            ss_state <= WAIT_ACK;
                            wbm_stb_o <= 1'b1;
                        end

                        wbm_cyc_o <= 1'b1;
                    end
                end
            end
            SINGLE: begin
                case (ss_state)
                    WAIT_START: begin
                        if (~wbm_stall_i) begin
                            wbm_stb_o <= 1'b1;
                            ss_state <= WAIT_ACK;
                        end    
                    end
                    WAIT_ACK: begin
                        wbm_stb_o <= 1'b0;
                        if (wbm_ack_i) begin
                            wbm_cyc_o <= 1'b0;
                            picorv_rdy_o <= 1'b1;
                            sb_state <= END_PHASE;
                        end
                    end
                endcase
            end
            BURST: begin
                picorv_rdy_o <= 1'b0;
                case (ss_state)
                    WAIT_START: begin
                        if (~wbm_stall_i) begin
                            wbm_stb_o <= 1'b1;
                            ss_state <= WAIT_ACK;
                        end    
                    end
                    WAIT_ACK: begin
                        wbm_stb_o <= 1'b0;
                        if (wbm_ack_i) begin
                            if (~wr_req) begin
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
                                wbm_cyc_o <= 1'b0;
                            end
                            else begin
                                burst_phase <= burst_phase + 2'd1;

                                if (wbm_stall_i) begin
                                    ss_state <= WAIT_START;
                                end
                                else begin
                                    wbm_stb_o <= 1'b1;
                                    ss_state <= WAIT_ACK;
                                end
                            end    
                        end
                    end
                endcase
            end
            END_PHASE: begin /*End phase to allow picorv_valid to go to 0.*/
                picorv_rdy_o <= 1'b0;
                sb_state <= IDLE;
            end
        endcase
    end
end

endmodule
