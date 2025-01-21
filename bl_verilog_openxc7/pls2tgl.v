module pls2tgl (
	tgl,
	pulse,
	clk,
	rst_n
);
	output wire tgl;
	input wire pulse;
	input wire clk;
	input wire rst_n;
	reg q;
	initial q = 1'b0;
	always @(posedge clk)
		if (!rst_n)
			q <= 1'sb0;
		else
			q <= pulse ^ q;
	assign tgl = q;
endmodule
