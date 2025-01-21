`default_nettype wire
module vs0 (
	sys_clk,
	rst,
	wbm_0_adr_o,
	wbm_0_dat_o,
	wbm_0_dat_i,
	wbm_0_we_o,
	wbm_0_sel_o,
	wbm_0_stb_o,
	wbm_0_ack_i,
	wbm_0_stall_i,
	wbm_0_cyc_o,
	wbm_0_err_i,
	wbm_1_adr_o,
	wbm_1_dat_o,
	wbm_1_dat_i,
	wbm_1_we_o,
	wbm_1_sel_o,
	wbm_1_stb_o,
	wbm_1_ack_i,
	wbm_1_stall_i,
	wbm_1_cyc_o,
	wbm_1_err_i,
	wbs_adr,
	wbs_dat_w,
	wbs_dat_r,
	wbs_sel,
	wbs_stall,
	wbs_cyc,
	wbs_stb,
	wbs_ack,
	wbs_we,
	wbs_err,
	irq_in,
	irq_out
);
	input wire sys_clk;
	input wire rst;
	output wire [27:0] wbm_0_adr_o;
	output wire [31:0] wbm_0_dat_o;
	input wire [31:0] wbm_0_dat_i;
	output wire wbm_0_we_o;
	output wire [3:0] wbm_0_sel_o;
	output wire wbm_0_stb_o;
	input wire wbm_0_ack_i;
	input wire wbm_0_stall_i;
	output wire wbm_0_cyc_o;
	input wire wbm_0_err_i;
	output wire [27:0] wbm_1_adr_o;
	output wire [31:0] wbm_1_dat_o;
	input wire [31:0] wbm_1_dat_i;
	output wire wbm_1_we_o;
	output wire [3:0] wbm_1_sel_o;
	output wire wbm_1_stb_o;
	input wire wbm_1_ack_i;
	input wire wbm_1_stall_i;
	output wire wbm_1_cyc_o;
	input wire wbm_1_err_i;
	input wire [17:0] wbs_adr;
	input wire [31:0] wbs_dat_w;
	output wire [31:0] wbs_dat_r;
	input wire [3:0] wbs_sel;
	output wire wbs_stall;
	input wire wbs_cyc;
	input wire wbs_stb;
	output reg wbs_ack;
	input wire wbs_we;
	output wire wbs_err;
	input wire [31:0] irq_in;
	output wire irq_out;
	localparam [31:0] STUB_SIGNATURE = 32'h0000510b;
	reg unused = &{rst, irq_in, wbm_0_dat_i, wbm_0_ack_i, wbm_0_stall_i, wbm_0_err_i, wbm_1_dat_i, wbm_1_ack_i, wbm_1_stall_i, wbm_1_err_i, wbs_adr, wbs_dat_w, wbs_sel, wbs_we};
	always @(posedge sys_clk) wbs_ack <= wbs_stb & wbs_cyc;
	assign wbm_0_adr_o = 28'b0000000000000000000000000000;
	assign wbm_0_dat_o = 32'b00000000000000000000000000000000;
	assign wbm_0_we_o = 1'b0;
	assign wbm_0_sel_o = 4'b0000;
	assign wbm_0_cyc_o = 1'b0;
	assign wbm_0_stb_o = 1'b0;
	assign wbm_1_adr_o = 28'b0000000000000000000000000000;
	assign wbm_1_dat_o = 32'b00000000000000000000000000000000;
	assign wbm_1_we_o = 1'b0;
	assign wbm_1_sel_o = 4'b0000;
	assign wbm_1_cyc_o = 1'b0;
	assign wbm_1_stb_o = 1'b0;
	assign wbs_stall = 1'b0;
	assign wbs_err = 1'b0;
	assign wbs_dat_r = STUB_SIGNATURE;
	assign irq_out = 1'b0;
endmodule
