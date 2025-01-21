`default_nettype wire
module jt49_noise (
	cen,
	clk,
	rst_n,
	period,
	noise
);
	(* direct_enable *) input wire cen;
	input wire clk;
	input wire rst_n;
	input wire [4:0] period;
	output reg noise;
	reg [5:0] count;
	reg [16:0] poly17;
	wire poly17_zero = poly17 == 17'b00000000000000000;
	wire noise_en;
	reg last_en;
	initial last_en = 1'b0;
	wire noise_up;
	assign noise_up = noise_en && !last_en;
	always @(posedge clk)
		if (cen)
			noise <= ~poly17[0];
	always @(posedge clk or negedge rst_n)
		if (!rst_n) begin
			poly17 <= 17'd0;
			last_en <= 1'b0;
		end
		else if (cen) begin
			last_en <= noise_en;
			if (noise_up)
				poly17 <= {(poly17[0] ^ poly17[3]) ^ poly17_zero, poly17[16:1]};
		end
	jt49_div #(.W(5)) u_div(
		.clk(clk),
		.cen(cen),
		.rst_n(rst_n),
		.period(period),
		.div(noise_en)
	);
endmodule
