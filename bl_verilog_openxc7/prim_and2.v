module prim_and2 (
	in0_i,
	in1_i,
	out_o
);
	parameter signed [31:0] Width = 1;
	input [Width - 1:0] in0_i;
	input [Width - 1:0] in1_i;
	output wire [Width - 1:0] out_o;
	localparam integer Impl = 32'sd1;
	generate
		if (Impl == 32'sd1) begin : gen_xilinx
			prim_xilinx_and2 #(.Width(Width)) u_impl_xilinx(
				.in0_i(in0_i),
				.in1_i(in1_i),
				.out_o(out_o)
			);
		end
		else begin : gen_generic
			prim_generic_and2 #(.Width(Width)) u_impl_generic(
				.in0_i(in0_i),
				.in1_i(in1_i),
				.out_o(out_o)
			);
		end
	endgenerate
endmodule
