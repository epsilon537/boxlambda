`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

/*This module turns PicoRV requests into individual or 4-word-burst Wishbone transactions (with offset handling), 
 *based on the address MSB.*/
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

//Turn byte address into word address:
localparam BURST_REG_BASE_WORD_ADDR = (BURST_REG_BASE_ADDR/4);
localparam NUM_BURST_REGS = 6;

//Index of the offset register
localparam OFFSET_BURST_REG_IDX = 5;

logic unused = &{wbm_err_i};

//FSM states
typedef enum {IDLE, SINGLE, BURST, LOCAL } Single_Burst_State_Type;
Single_Burst_State_Type sb_state;

logic [31:0] burst_reg [0:5]; /*Array of 6 Burst Registers. The 6th register is the offset register.*/
logic [1:0] burst_phase; /*A burst of 4 wishbone transactions proceeds in 4 phases.*/
logic [2:0] burst_phase_ext;
logic addr_in_burst_reg_range; /*Set if one of the burst registers is being addressed.*/
logic [2:0] burst_reg_idx; /*Index of selected burst register.*/

/*Offset is the byte offset of the destination pointer/address relative to a word-aligned source address/pointer.
 *This allows us to do handle unaligned DMA copies using word transactions instead of byte transactions.*/ 
logic [1:0] offset; /*Value of the offset burst register.*/

/*A burst read or write request is 'posted', i.e. the transaction completes immediately towards the PicoRV.
 *(In case of a burst read transaction the data returned to the PicorV is bogus).
 *This means that the picorv signals must be registered, because they won't hold their value after the transaction
 *has completed.
 *Note: Because a burst request transaction completes immediately towards the PicoRV, the PicoRV can immediately initiate
 *a new transaction. This 2nd transaction will stall until the FSM has returned to the idle state.*/
logic [31:2] picorv_addr_reg;
logic [ 3:0] picorv_wstrb_reg;
logic picorv_rdy_reg;
logic wbm_stb_reg, wbm_cyc_reg;

assign burst_phase_ext = {1'b0, burst_phase};
assign offset = burst_reg[OFFSET_BURST_REG_IDX][1:0];
assign addr_in_burst_reg_range = (picorv_addr_i >= 30'(BURST_REG_BASE_WORD_ADDR)) && (picorv_addr_i < 30'(NUM_BURST_REGS+BURST_REG_BASE_WORD_ADDR));
assign burst_reg_idx = picorv_addr_i[4:2]; 

/*The module's output signals are a function of the FSM state*/
always_comb begin
    case (sb_state)
        IDLE: begin
            picorv_rdy_o=1'b0;
            picorv_rdata_o = 32'b0;
            wbm_adr_o = {1'b0, picorv_addr_i[30:2]};
            wbm_dat_o = picorv_wdata_i;
            wbm_we_o = |picorv_wstrb_i;
            wbm_sel_o = wbm_we_o ? picorv_wstrb_i : 4'b1111;
            /*A PicoRV non-burst transaction instantly results in a Wishbone transaction, without register delay.*/
            wbm_stb_o = picorv_valid_i && ~addr_in_burst_reg_range && ~picorv_addr_i[31];
            wbm_cyc_o = picorv_valid_i && ~addr_in_burst_reg_range && ~picorv_addr_i[31];
        end
        SINGLE: begin
            picorv_rdy_o = wbm_ack_i;
            picorv_rdata_o = wbm_dat_i;
            wbm_adr_o = {1'b0, picorv_addr_i[30:2]};
            wbm_dat_o = picorv_wdata_i;
            wbm_we_o = |picorv_wstrb_i;
            wbm_sel_o = wbm_we_o ? picorv_wstrb_i : 4'b1111;
            wbm_stb_o = wbm_stall_i ? wbm_stb_reg : 1'b0; /*wbm_stb_reg will go to 0 in sequential logic when slave no longer stalls.*/
            wbm_cyc_o = 1'b1;
        end
        BURST: begin
            picorv_rdy_o = picorv_rdy_reg;
            picorv_rdata_o = wbm_dat_i;
            wbm_adr_o = picorv_addr_reg;
            wbm_dat_o = burst_reg[burst_phase_ext];
            wbm_we_o = |picorv_wstrb_reg;
            wbm_sel_o = wbm_we_o ? picorv_wstrb_reg : 4'b1111;
            wbm_stb_o = wbm_stb_reg;
            wbm_cyc_o = wbm_cyc_reg;
        end
        default: begin /*LOCAL register access:*/
            picorv_rdy_o = 1'b1;
            picorv_rdata_o = burst_reg[burst_reg_idx];
            wbm_adr_o = 30'b0;
            wbm_dat_o = 32'b0;
            wbm_we_o = 1'b0;
            wbm_sel_o = 4'b0;
            wbm_stb_o = 1'b0;
            wbm_cyc_o = 1'b0;
        end
    endcase
end

/*The FSM*/
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
                /*PicoRV read or write request. Classify into SINGLE transaction, BURST transaction, or LOCAL register access.*/
                if (picorv_valid_i) begin
                    if (addr_in_burst_reg_range) begin
                        /*Write?*/
                        if (picorv_wstrb_i != 4'b0) begin
                            burst_reg[burst_reg_idx] <= picorv_wdata_i;
                        end
                        sb_state <= LOCAL;
                    end
                    else begin
                        /*Address MSB determines if it's a SINGLE or a BURST transaction.*/
                        if (~picorv_addr_i[31]) begin
                            sb_state <= SINGLE;
                        end
                        else begin
                            /*In case of a copy with offset, burst reg 4 contains the bytes carried over from
                             *the previous burst transaction (or the initial bytes as set up by SW).*/
                            /*Read? Copy burst reg 4 to 0*/
                            if (picorv_wstrb_i == 4'b0) begin
                                burst_reg[0] <= burst_reg[4];
                            end
                            sb_state <= BURST;
                            picorv_rdy_reg <= 1'b1; /*Complete transaction towards the PicoRV right away. Don't wait until the burst transaction itself has completed.*/
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
                    wbm_stb_reg <= 1'b0; /*Deassert STB when no longer stalling.*/
                    if (wbm_ack_i) begin
                        wbm_cyc_reg <= 1'b0;
                        sb_state <= IDLE;
                    end
                end
            end
            BURST: begin
                picorv_rdy_reg <= 1'b0;
                if (~wbm_stall_i) begin
                    wbm_stb_reg <= 1'b0; /*Deassert STB when no longer stalling.*/
                    if (wbm_ack_i) begin
                        /*Read?*/
                        if (picorv_wstrb_reg == 4'b0) begin
                            /*Read into Burst Registers, with offset.*/
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
                            picorv_addr_reg <= picorv_addr_reg + 30'd1;
                            burst_phase <= burst_phase + 2'd1;
                            wbm_stb_reg <= 1'b1;
                        end    
                    end
                end
            end
            LOCAL: begin /*End phase to allow picorv_valid to go to 0 in response to ready signal.*/
                sb_state <= IDLE;
            end
        endcase
    end
end

endmodule
