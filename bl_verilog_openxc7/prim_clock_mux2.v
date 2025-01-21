module prim_clock_mux2 (
	clk0_i,
	clk1_i,
	sel_i,
	clk_o
);
	parameter [0:0] NoFpgaBufG = 1'b0;
	input clk0_i;
	input clk1_i;
	input sel_i;
	output wire clk_o;
	localparam integer Impl = 32'sd1;
	generate
		if (Impl == 32'sd1) begin : gen_xilinx
			prim_xilinx_clock_mux2 #(.NoFpgaBufG(NoFpgaBufG)) u_impl_xilinx(
				.clk0_i(clk0_i),
				.clk1_i(clk1_i),
				.sel_i(sel_i),
				.clk_o(clk_o)
			);
		end
		else begin : gen_generic
			prim_generic_clock_mux2 #(.NoFpgaBufG(NoFpgaBufG)) u_impl_generic(
				.clk0_i(clk0_i),
				.clk1_i(clk1_i),
				.sel_i(sel_i),
				.clk_o(clk_o)
			);
		end
	endgenerate
endmodule
