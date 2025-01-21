module prim_onehot_enc (
	in_i,
	en_i,
	out_o
);
	parameter [31:0] OneHotWidth = 32;
	localparam [31:0] InputWidth = $clog2(OneHotWidth);
	input wire [InputWidth - 1:0] in_i;
	input wire en_i;
	output wire [OneHotWidth - 1:0] out_o;
	genvar _gv_i_1;
	generate
		for (_gv_i_1 = 0; _gv_i_1 < OneHotWidth; _gv_i_1 = _gv_i_1 + 1) begin : g_out
			localparam i = _gv_i_1;
			assign out_o[i] = (in_i == i) & en_i;
		end
	endgenerate
endmodule
