`default_nettype wire
module jt49_div (
	cen,
	clk,
	rst_n,
	period,
	div
);
	parameter W = 12;
	(* direct_enable *) input wire cen;
	input wire clk;
	input wire rst_n;
	input wire [W - 1:0] period;
	output reg div;
	reg [W - 1:0] count;
	initial count = {W {1'b0}};
	wire [W - 1:0] one;
	assign one = {{W - 1 {1'b0}}, 1'b1};
	always @(posedge clk or negedge rst_n)
		if (!rst_n) begin
			count <= one;
			div <= 1'b0;
		end
		else if (cen) begin
			if (count >= period) begin
				count <= one;
				div <= ~div;
			end
			else
				count <= count + one;
		end
endmodule
