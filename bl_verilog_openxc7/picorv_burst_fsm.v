`default_nettype none
module picorv_burst_fsm (
	clk,
	rst,
	picorv_valid_i,
	picorv_rdy_o,
	picorv_addr_i,
	picorv_wdata_i,
	picorv_wstrb_i,
	picorv_rdata_o,
	wbm_adr_o,
	wbm_dat_o,
	wbm_dat_i,
	wbm_we_o,
	wbm_sel_o,
	wbm_stb_o,
	wbm_ack_i,
	wbm_stall_i,
	wbm_cyc_o,
	wbm_err_i
);
	reg _sv2v_0;
	parameter BURST_REG_BASE_ADDR = 32'h10002020;
	input wire clk;
	input wire rst;
	input wire picorv_valid_i;
	output reg picorv_rdy_o;
	input wire [31:2] picorv_addr_i;
	input wire [31:0] picorv_wdata_i;
	input wire [3:0] picorv_wstrb_i;
	output reg [31:0] picorv_rdata_o;
	output reg [29:2] wbm_adr_o;
	output reg [31:0] wbm_dat_o;
	input wire [31:0] wbm_dat_i;
	output reg wbm_we_o;
	output reg [3:0] wbm_sel_o;
	output reg wbm_stb_o;
	input wire wbm_ack_i;
	input wire wbm_stall_i;
	output reg wbm_cyc_o;
	input wire wbm_err_i;
	localparam BURST_REG_BASE_WORD_ADDR = BURST_REG_BASE_ADDR / 4;
	localparam NUM_BURST_REGS = 6;
	localparam OFFSET_BURST_REG_IDX = 5;
	reg unused = &{wbm_err_i};
	reg [31:0] sb_state;
	reg [31:0] burst_reg [0:5];
	reg [1:0] burst_phase;
	wire [2:0] burst_phase_ext;
	wire addr_in_burst_reg_range;
	wire [2:0] burst_reg_idx;
	wire [1:0] offset;
	reg [29:2] picorv_addr_reg;
	reg [3:0] picorv_wstrb_reg;
	reg picorv_rdy_reg;
	reg wbm_stb_reg;
	reg wbm_cyc_reg;
	assign burst_phase_ext = {1'b0, burst_phase};
	assign offset = burst_reg[OFFSET_BURST_REG_IDX][1:0];
	function automatic [29:0] sv2v_cast_30;
		input reg [29:0] inp;
		sv2v_cast_30 = inp;
	endfunction
	assign addr_in_burst_reg_range = (picorv_addr_i >= sv2v_cast_30(BURST_REG_BASE_WORD_ADDR)) && (picorv_addr_i < sv2v_cast_30(NUM_BURST_REGS + BURST_REG_BASE_WORD_ADDR));
	assign burst_reg_idx = picorv_addr_i[4:2];
	always @(*) begin
		if (_sv2v_0)
			;
		case (sb_state)
			32'd0: begin
				picorv_rdy_o = 1'b0;
				picorv_rdata_o = 32'b00000000000000000000000000000000;
				wbm_adr_o = picorv_addr_i[29:2];
				wbm_dat_o = picorv_wdata_i;
				wbm_we_o = |picorv_wstrb_i;
				wbm_sel_o = (wbm_we_o ? picorv_wstrb_i : 4'b1111);
				wbm_stb_o = (picorv_valid_i && ~addr_in_burst_reg_range) && ~picorv_addr_i[31];
				wbm_cyc_o = (picorv_valid_i && ~addr_in_burst_reg_range) && ~picorv_addr_i[31];
			end
			32'd1: begin
				picorv_rdy_o = wbm_ack_i;
				picorv_rdata_o = wbm_dat_i;
				wbm_adr_o = picorv_addr_i[29:2];
				wbm_dat_o = picorv_wdata_i;
				wbm_we_o = |picorv_wstrb_i;
				wbm_sel_o = (wbm_we_o ? picorv_wstrb_i : 4'b1111);
				wbm_stb_o = (wbm_stall_i ? wbm_stb_reg : 1'b0);
				wbm_cyc_o = 1'b1;
			end
			32'd2: begin
				picorv_rdy_o = picorv_rdy_reg;
				picorv_rdata_o = wbm_dat_i;
				wbm_adr_o = picorv_addr_reg;
				wbm_dat_o = burst_reg[burst_phase_ext];
				wbm_we_o = |picorv_wstrb_reg;
				wbm_sel_o = (wbm_we_o ? picorv_wstrb_reg : 4'b1111);
				wbm_stb_o = wbm_stb_reg;
				wbm_cyc_o = wbm_cyc_reg;
			end
			default: begin
				picorv_rdy_o = 1'b1;
				picorv_rdata_o = burst_reg[burst_reg_idx];
				wbm_adr_o = 28'b0000000000000000000000000000;
				wbm_dat_o = 32'b00000000000000000000000000000000;
				wbm_we_o = 1'b0;
				wbm_sel_o = 4'b0000;
				wbm_stb_o = 1'b0;
				wbm_cyc_o = 1'b0;
			end
		endcase
	end
	always @(posedge clk)
		if (rst) begin
			sb_state <= 32'd0;
			burst_phase <= 2'd0;
			picorv_rdy_reg <= 1'b0;
			wbm_stb_reg <= 1'b0;
			wbm_cyc_reg <= 1'b0;
			begin : sv2v_autoblock_1
				reg signed [31:0] ii;
				for (ii = 0; ii < NUM_BURST_REGS; ii = ii + 1)
					burst_reg[ii] <= 32'b00000000000000000000000000000000;
			end
		end
		else
			case (sb_state)
				32'd0:
					if (picorv_valid_i) begin
						if (addr_in_burst_reg_range) begin
							if (picorv_wstrb_i != 4'b0000)
								burst_reg[burst_reg_idx] <= picorv_wdata_i;
							sb_state <= 32'd3;
						end
						else begin
							if (~picorv_addr_i[31])
								sb_state <= 32'd1;
							else begin
								if (picorv_wstrb_i == 4'b0000)
									burst_reg[0] <= burst_reg[4];
								sb_state <= 32'd2;
								picorv_rdy_reg <= 1'b1;
								burst_phase <= 2'd0;
							end
							picorv_addr_reg <= picorv_addr_i[29:2];
							picorv_wstrb_reg <= picorv_wstrb_i;
							wbm_cyc_reg <= 1'b1;
							wbm_stb_reg <= 1'b1;
						end
					end
				32'd1:
					if (~wbm_stall_i) begin
						wbm_stb_reg <= 1'b0;
						if (wbm_ack_i) begin
							wbm_cyc_reg <= 1'b0;
							sb_state <= 32'd0;
						end
					end
				32'd2: begin
					picorv_rdy_reg <= 1'b0;
					if (~wbm_stall_i) begin
						wbm_stb_reg <= 1'b0;
						if (wbm_ack_i) begin
							if (picorv_wstrb_reg == 4'b0000)
								case (offset)
									2'd0: burst_reg[burst_phase_ext] <= wbm_dat_i;
									2'd1: begin
										burst_reg[burst_phase_ext][31:8] <= wbm_dat_i[23:0];
										burst_reg[burst_phase_ext + 1][7:0] <= wbm_dat_i[31:24];
									end
									2'd2: begin
										burst_reg[burst_phase_ext][31:16] <= wbm_dat_i[15:0];
										burst_reg[burst_phase_ext + 1][15:0] <= wbm_dat_i[31:16];
									end
									2'd3: begin
										burst_reg[burst_phase_ext][31:24] <= wbm_dat_i[7:0];
										burst_reg[burst_phase_ext + 1][23:0] <= wbm_dat_i[31:8];
									end
								endcase
							if (burst_phase == 2'd3) begin
								burst_phase <= 2'd0;
								sb_state <= 32'd0;
								wbm_cyc_reg <= 1'b0;
							end
							else begin
								picorv_addr_reg <= picorv_addr_reg + 28'd1;
								burst_phase <= burst_phase + 2'd1;
								wbm_stb_reg <= 1'b1;
							end
						end
					end
				end
				32'd3: sb_state <= 32'd0;
			endcase
	initial _sv2v_0 = 0;
endmodule
