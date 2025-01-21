module tgl2pls (
	pulse,
	q,
	d,
	clk,
	rst_n
);
	output wire pulse;
	output reg q;
	input wire d;
	input wire clk;
	input wire rst_n;
	initial q = 1'b0;
	always @(posedge clk)
		if (!rst_n)
			q <= 1'sb0;
		else
			q <= d;
	assign pulse = q ^ d;
endmodule
