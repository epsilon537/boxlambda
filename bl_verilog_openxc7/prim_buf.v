module prim_buf (
	in_i,
	out_o
);
	parameter signed [31:0] Width = 1;
	input [Width - 1:0] in_i;
	output wire [Width - 1:0] out_o;
	localparam integer Impl = 32'sd1;
	generate
		if (Impl == 32'sd1) begin : gen_xilinx
			prim_xilinx_buf #(.Width(Width)) u_impl_xilinx(
				.in_i(in_i),
				.out_o(out_o)
			);
		end
		else begin : gen_generic
			prim_generic_buf #(.Width(Width)) u_impl_generic(
				.in_i(in_i),
				.out_o(out_o)
			);
		end
	endgenerate
endmodule
