(* DONT_TOUCH = "yes" *) module prim_xilinx_and2 (
	in0_i,
	in1_i,
	out_o
);
	parameter signed [31:0] Width = 1;
	input [Width - 1:0] in0_i;
	input [Width - 1:0] in1_i;
	output wire [Width - 1:0] out_o;
	assign out_o = in0_i & in1_i;
endmodule
