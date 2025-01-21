`default_nettype wire
module jt49_eg (
	cen,
	clk,
	step,
	null_period,
	rst_n,
	restart,
	ctrl,
	env
);
	(* direct_enable *) input wire cen;
	input wire clk;
	input wire step;
	input wire null_period;
	input wire rst_n;
	input wire restart;
	input wire [3:0] ctrl;
	output reg [4:0] env;
	reg inv = 1'b0;
	reg stop = 1'b0;
	reg [4:0] gain = 5'd0;
	wire CONT = ctrl[3];
	wire ATT = ctrl[2];
	wire ALT = ctrl[1];
	wire HOLD = ctrl[0];
	wire will_hold = !CONT || HOLD;
	always @(posedge clk)
		if (cen)
			env <= (inv ? ~gain : gain);
	reg last_step;
	initial last_step = 1'b0;
	wire step_edge;
	assign step_edge = (step && !last_step) || null_period;
	wire will_invert = (!CONT && ATT) || (CONT && ALT);
	reg rst_latch = 1'b0;
	reg rst_clr = 1'b0;
	always @(posedge clk)
		if (restart)
			rst_latch <= 1;
		else if (rst_clr)
			rst_latch <= 0;
	always @(posedge clk or negedge rst_n)
		if (!rst_n) begin
			gain <= 5'h1f;
			inv <= 0;
			stop <= 0;
			rst_clr <= 0;
			last_step <= 1'b0;
		end
		else if (cen) begin
			last_step <= step;
			if (rst_latch) begin
				gain <= 5'h1f;
				inv <= ATT;
				stop <= 1'b0;
				rst_clr <= 1;
			end
			else begin
				rst_clr <= 0;
				if (step_edge && !stop) begin
					if (gain == 5'h00) begin
						if (will_hold)
							stop <= 1'b1;
						else
							gain <= gain - 5'b00001;
						if (will_invert)
							inv <= ~inv;
					end
					else
						gain <= gain - 5'b00001;
				end
			end
		end
endmodule
