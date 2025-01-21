`default_nettype none
module sync (
	clk_i,
	rst_ni,
	serial_i,
	serial_o
);
	parameter [31:0] STAGES = 2;
	parameter [0:0] ResetValue = 1'b0;
	input wire clk_i;
	input wire rst_ni;
	input wire serial_i;
	output wire serial_o;
	(* dont_touch = "true" *) (* async_reg = "true" *) reg [STAGES - 1:0] reg_q;
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			reg_q <= {STAGES {ResetValue}};
		else
			reg_q <= {reg_q[STAGES - 2:0], serial_i};
	assign serial_o = reg_q[STAGES - 1];
endmodule
