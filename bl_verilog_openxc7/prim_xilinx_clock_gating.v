module prim_xilinx_clock_gating (
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
	generate
		if (NoFpgaGate) begin : gen_no_gate
			assign clk_o = clk_i;
		end
		else begin : gen_gate
			if (FpgaBufGlobal) begin : gen_bufgce
				BUFGCE #(.SIM_DEVICE("7SERIES")) u_bufgce(
					.I(clk_i),
					.CE(en_i | test_en_i),
					.O(clk_o)
				);
			end
			else begin : gen_bufhce
				BUFHCE u_bufhce(
					.I(clk_i),
					.CE(en_i | test_en_i),
					.O(clk_o)
				);
			end
		end
	endgenerate
endmodule
