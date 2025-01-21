module sync3 (
	q,
	d,
	clk,
	rst_n
);
	output reg q;
	input wire d;
	input wire clk;
	input wire rst_n;
	(* ASYNC_REG = "TRUE" *) reg [1:0] xfer_pipe;
	initial begin
		xfer_pipe = 2'b00;
		q = 1'b0;
	end
	always @(posedge clk)
		if (!rst_n)
			{q, xfer_pipe} <= 1'sb0;
		else
			{q, xfer_pipe} <= {xfer_pipe, d};
endmodule
