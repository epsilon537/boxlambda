module prim_clock_gating (
	clk_i,
	en_i,
	test_en_i,
	clk_o
);
	parameter [0:0] NoFpgaGate = 1'b0;
	parameter [0:0] FpgaBufGlobal = 1'b1;
	input clk_i;
	input en_i;
	input test_en_i;
	output wire clk_o;
	localparam integer Impl = 32'sd1;
	generate
		if (Impl == 32'sd1) begin : gen_xilinx
			prim_xilinx_clock_gating #(
				.FpgaBufGlobal(FpgaBufGlobal),
				.NoFpgaGate(NoFpgaGate)
			) u_impl_xilinx(
				.clk_i(clk_i),
				.en_i(en_i),
				.test_en_i(test_en_i),
				.clk_o(clk_o)
			);
		end
		else begin : gen_generic
			prim_generic_clock_gating #(
				.FpgaBufGlobal(FpgaBufGlobal),
				.NoFpgaGate(NoFpgaGate)
			) u_impl_generic(
				.clk_i(clk_i),
				.en_i(en_i),
				.test_en_i(test_en_i),
				.clk_o(clk_o)
			);
		end
	endgenerate
endmodule
