`default_nettype none
module button_conditioner (
	clk,
	btn,
	out
);
	reg _sv2v_0;
	input wire clk;
	input wire btn;
	output wire out;
	localparam CTR_SZ = 19;
	reg [18:0] ctr_d;
	reg [18:0] ctr_q;
	wire [3:0] sync_d;
	(* ASYNC_REG = "TRUE" *) reg [3:0] sync_q;
	wire sync_q_3;
	assign out = ctr_q == {CTR_SZ {1'b1}};
	assign sync_d = {sync_q[2:0], btn};
	assign sync_q_3 = sync_q[3];
	always @(*) begin
		if (_sv2v_0)
			;
		ctr_d = ctr_q + 1;
		if (ctr_q == {CTR_SZ {1'b1}})
			ctr_d = ctr_q;
		if (!sync_q_3)
			ctr_d = 0;
	end
	initial begin
		ctr_q = {CTR_SZ {1'b0}};
		sync_q = 4'b0000;
	end
	always @(posedge clk) begin
		ctr_q <= ctr_d;
		sync_q <= sync_d;
	end
	initial _sv2v_0 = 0;
endmodule
