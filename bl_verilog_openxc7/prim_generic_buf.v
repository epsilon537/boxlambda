module prim_generic_buf (
	in_i,
	out_o
);
	parameter signed [31:0] Width = 1;
	input [Width - 1:0] in_i;
	output wire [Width - 1:0] out_o;
	wire [Width - 1:0] inv;
	assign inv = ~in_i;
	assign out_o = ~inv;
endmodule
