(* DONT_TOUCH = "yes" *) module prim_xilinx_buf (
	in_i,
	out_o
);
	parameter signed [31:0] Width = 1;
	input [Width - 1:0] in_i;
	output wire [Width - 1:0] out_o;
	assign out_o = in_i;
endmodule
