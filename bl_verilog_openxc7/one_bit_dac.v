module one_bit_dac (
	clk,
	clk_en,
	in,
	out
);
	parameter W = 16;
	input wire clk;
	input wire clk_en;
	input wire signed [W - 1:0] in;
	output wire out;
	reg signed [3:-W] acc1_r;
	reg signed [5:-W] acc2_r;
	initial begin
		acc1_r = 0;
		acc2_r = 0;
	end
	reg signed [5:0] feedback = (out ? -1 : 1);
	wire signed [3:-W] x1 = {{4 {in[W - 1]}}, in};
	wire signed [3:-W] s1 = {x1[3:0] + feedback[3:0], x1[-1:-W]};
	wire signed [3:-W] acc1 = acc1_r + s1;
	wire signed [5:-W] x2 = {{2 {acc1[3]}}, acc1};
	wire signed [5:-W] s2 = {x2[5:0] + feedback[5:0], x2[-1:-W]};
	wire signed [5:-W] acc2 = acc2_r + s2;
	always @(posedge clk)
		if (clk_en) begin
			acc1_r <= acc1;
			acc2_r <= acc2;
		end
	assign out = acc2_r[4];
endmodule
