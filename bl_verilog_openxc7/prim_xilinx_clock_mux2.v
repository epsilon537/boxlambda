module prim_xilinx_clock_mux2 (
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
	generate
		if (NoFpgaBufG) begin : gen_no_bufg
			assign clk_o = (sel_i ? clk1_i : clk0_i);
		end
		else begin : gen_bufg
			BUFGMUX bufgmux_i(
				.S(sel_i),
				.I0(clk0_i),
				.I1(clk1_i),
				.O(clk_o)
			);
		end
	endgenerate
endmodule
