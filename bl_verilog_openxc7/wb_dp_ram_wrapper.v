module wb_dp_ram_wrapper (
	clk,
	rst,
	a_adr_i,
	a_dat_i,
	a_dat_o,
	a_we_i,
	a_sel_i,
	a_stb_i,
	a_stall_o,
	a_ack_o,
	a_err_o,
	a_cyc_i,
	b_adr_i,
	b_dat_i,
	b_dat_o,
	b_we_i,
	b_sel_i,
	b_stb_i,
	b_stall_o,
	b_ack_o,
	b_err_o,
	b_cyc_i
);
	parameter ADDR_WIDTH = 14;
	parameter INIT_FILE = "";
	input wire clk;
	input wire rst;
	input wire [ADDR_WIDTH - 1:0] a_adr_i;
	input wire [31:0] a_dat_i;
	output wire [31:0] a_dat_o;
	input wire a_we_i;
	input wire [3:0] a_sel_i;
	input wire a_stb_i;
	output wire a_stall_o;
	output wire a_ack_o;
	output wire a_err_o;
	input wire a_cyc_i;
	input wire [ADDR_WIDTH - 1:0] b_adr_i;
	input wire [31:0] b_dat_i;
	output wire [31:0] b_dat_o;
	input wire b_we_i;
	input wire [3:0] b_sel_i;
	input wire b_stb_i;
	output wire b_stall_o;
	output wire b_ack_o;
	output wire b_err_o;
	input wire b_cyc_i;
	wire [ADDR_WIDTH + 1:0] a_adr_i_byte;
	wire [ADDR_WIDTH + 1:0] b_adr_i_byte;
	reg unused = rst;
	assign a_adr_i_byte = {a_adr_i, 2'b00};
	assign b_adr_i_byte = {b_adr_i, 2'b00};
	wb_dp_ram #(
		.DATA_WIDTH(32),
		.ADDR_WIDTH(ADDR_WIDTH + 2),
		.SELECT_WIDTH(4),
		.INIT_FILE(INIT_FILE)
	) wb_dp_ram_inst(
		.a_clk(clk),
		.b_clk(clk),
		.a_adr_i(a_adr_i_byte),
		.b_adr_i(b_adr_i_byte),
		.a_dat_i(a_dat_i),
		.a_dat_o(a_dat_o),
		.a_we_i(a_we_i),
		.a_sel_i(a_sel_i),
		.a_stb_i(a_stb_i),
		.a_ack_o(a_ack_o),
		.a_cyc_i(a_cyc_i),
		.b_dat_i(b_dat_i),
		.b_dat_o(b_dat_o),
		.b_we_i(b_we_i),
		.b_sel_i(b_sel_i),
		.b_stb_i(b_stb_i),
		.b_ack_o(b_ack_o),
		.b_cyc_i(b_cyc_i)
	);
	assign a_stall_o = (!a_cyc_i ? 1'b0 : !a_ack_o);
	assign a_err_o = 1'b0;
	assign b_stall_o = (!b_cyc_i ? 1'b0 : !b_ack_o);
	assign b_err_o = 1'b0;
endmodule
