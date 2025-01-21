module prim_onehot_mux (
	clk_i,
	rst_ni,
	in_i,
	sel_i,
	out_o
);
	parameter signed [31:0] Width = 32;
	parameter signed [31:0] Inputs = 8;
	input clk_i;
	input rst_ni;
	input wire [(Inputs * Width) - 1:0] in_i;
	input wire [Inputs - 1:0] sel_i;
	output wire [Width - 1:0] out_o;
	wire [Inputs - 1:0] in_mux [0:Width - 1];
	genvar _gv_b_2;
	generate
		for (_gv_b_2 = 0; _gv_b_2 < Width; _gv_b_2 = _gv_b_2 + 1) begin : g_in_mux_outer
			localparam b = _gv_b_2;
			wire [Inputs - 1:0] out_mux_bits;
			genvar _gv_i_2;
			for (_gv_i_2 = 0; _gv_i_2 < Inputs; _gv_i_2 = _gv_i_2 + 1) begin : g_in_mux_inner
				localparam i = _gv_i_2;
				assign in_mux[b][i] = in_i[(((Inputs - 1) - i) * Width) + b];
			end
			prim_and2 #(.Width(Inputs)) u_mux_bit_and(
				.in0_i(in_mux[b]),
				.in1_i(sel_i),
				.out_o(out_mux_bits)
			);
			assign out_o[b] = |out_mux_bits;
		end
	endgenerate
	wire unused_clk;
	wire unused_rst_n;
	assign unused_clk = clk_i;
	assign unused_rst_n = rst_ni;
endmodule
