`default_nettype none
module fifo_v2_EBA1F (
	clk_i,
	rst_ni,
	flush_i,
	testmode_i,
	full_o,
	empty_o,
	alm_full_o,
	alm_empty_o,
	data_i,
	push_i,
	data_o,
	pop_i
);
	parameter [0:0] FALL_THROUGH = 1'b0;
	parameter [31:0] DATA_WIDTH = 32;
	parameter [31:0] DEPTH = 8;
	parameter [31:0] ALM_EMPTY_TH = 1;
	parameter [31:0] ALM_FULL_TH = 1;
	parameter [31:0] ADDR_DEPTH = (DEPTH > 1 ? $clog2(DEPTH) : 1);
	input wire clk_i;
	input wire rst_ni;
	input wire flush_i;
	input wire testmode_i;
	output wire full_o;
	output wire empty_o;
	output wire alm_full_o;
	output wire alm_empty_o;
	input wire [33:0] data_i;
	input wire push_i;
	output wire [33:0] data_o;
	input wire pop_i;
	wire [ADDR_DEPTH - 1:0] usage;
	generate
		if (DEPTH == 0) begin : genblk1
			assign alm_full_o = 1'b0;
			assign alm_empty_o = 1'b0;
		end
		else begin : genblk1
			assign alm_full_o = usage >= ALM_FULL_TH[ADDR_DEPTH - 1:0];
			assign alm_empty_o = usage <= ALM_EMPTY_TH[ADDR_DEPTH - 1:0];
		end
	endgenerate
	fifo_v3_5B991 #(
		.FALL_THROUGH(FALL_THROUGH),
		.DATA_WIDTH(DATA_WIDTH),
		.DEPTH(DEPTH)
	) i_fifo_v3(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.flush_i(flush_i),
		.testmode_i(testmode_i),
		.full_o(full_o),
		.empty_o(empty_o),
		.usage_o(usage),
		.data_i(data_i),
		.push_i(push_i),
		.data_o(data_o),
		.pop_i(pop_i)
	);
endmodule
