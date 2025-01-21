`default_nettype wire
module jt49_cen (
	clk,
	rst_n,
	cen,
	sel,
	cen16,
	cen256
);
	input wire clk;
	input wire rst_n;
	input wire cen;
	input wire sel;
	output reg cen16;
	output reg cen256;
	reg [9:0] cencnt;
	parameter CLKDIV = 3;
	localparam eg = CLKDIV;
	wire toggle16 = (sel ? ~|cencnt[CLKDIV - 1:0] : ~|cencnt[CLKDIV:0]);
	wire toggle256 = (sel ? ~|cencnt[eg - 2:0] : ~|cencnt[eg - 1:0]);
	always @(posedge clk or negedge rst_n)
		if (!rst_n)
			cencnt <= 10'd0;
		else if (cen)
			cencnt <= cencnt + 10'd1;
	always @(posedge clk) begin
		cen16 <= cen & toggle16;
		cen256 <= cen & toggle256;
	end
endmodule
