module prim_generic_clock_gating (
	clk_i,
	en_i,
	test_en_i,
	clk_o
);
	reg _sv2v_0;
	parameter [0:0] NoFpgaGate = 1'b0;
	parameter [0:0] FpgaBufGlobal = 1'b1;
	input clk_i;
	input en_i;
	input test_en_i;
	output wire clk_o;
	reg en_latch;
	always @(*) begin
		if (_sv2v_0)
			;
		if (!clk_i)
			en_latch = en_i | test_en_i;
	end
	assign clk_o = en_latch & clk_i;
	initial _sv2v_0 = 0;
endmodule
