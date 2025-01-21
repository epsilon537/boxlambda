`default_nettype none
module skidbuffer (
	i_clk,
	i_reset,
	i_valid,
	o_ready,
	i_data,
	o_valid,
	i_ready,
	o_data
);
	parameter [0:0] OPT_LOWPOWER = 0;
	parameter [0:0] OPT_OUTREG = 1;
	parameter [0:0] OPT_PASSTHROUGH = 0;
	parameter DW = 8;
	parameter [0:0] OPT_INITIAL = 1'b1;
	input wire i_clk;
	input wire i_reset;
	input wire i_valid;
	output wire o_ready;
	input wire [DW - 1:0] i_data;
	output wire o_valid;
	input wire i_ready;
	output reg [DW - 1:0] o_data;
	wire [DW - 1:0] w_data;
	generate
		if (OPT_PASSTHROUGH) begin : PASSTHROUGH
			assign {o_valid, o_ready} = {i_valid, i_ready};
			always @(*)
				if (!i_valid && OPT_LOWPOWER)
					o_data = 0;
				else
					o_data = i_data;
			assign w_data = 0;
			wire unused_passthrough;
			assign unused_passthrough = &{1'b0, i_clk, i_reset};
		end
		else begin : LOGIC
			reg r_valid;
			reg [DW - 1:0] r_data;
			initial if (OPT_INITIAL)
				r_valid = 0;
			always @(posedge i_clk)
				if (i_reset)
					r_valid <= 0;
				else if ((i_valid && o_ready) && (o_valid && !i_ready))
					r_valid <= 1;
				else if (i_ready)
					r_valid <= 0;
			initial if (OPT_INITIAL)
				r_data = 0;
			always @(posedge i_clk)
				if (OPT_LOWPOWER && i_reset)
					r_data <= 0;
				else if (OPT_LOWPOWER && (!o_valid || i_ready))
					r_data <= 0;
				else if (((!OPT_LOWPOWER || !OPT_OUTREG) || i_valid) && o_ready)
					r_data <= i_data;
			assign w_data = r_data;
			assign o_ready = !r_valid;
			if (!OPT_OUTREG) begin : NET_OUTPUT
				assign o_valid = !i_reset && (i_valid || r_valid);
				always @(*)
					if (r_valid)
						o_data = r_data;
					else if (!OPT_LOWPOWER || i_valid)
						o_data = i_data;
					else
						o_data = 0;
			end
			else begin : REG_OUTPUT
				reg ro_valid;
				initial if (OPT_INITIAL)
					ro_valid = 0;
				always @(posedge i_clk)
					if (i_reset)
						ro_valid <= 0;
					else if (!o_valid || i_ready)
						ro_valid <= i_valid || r_valid;
				assign o_valid = ro_valid;
				initial if (OPT_INITIAL)
					o_data = 0;
				always @(posedge i_clk)
					if (OPT_LOWPOWER && i_reset)
						o_data <= 0;
					else if (!o_valid || i_ready) begin
						if (r_valid)
							o_data <= r_data;
						else if (!OPT_LOWPOWER || i_valid)
							o_data <= i_data;
						else
							o_data <= 0;
					end
			end
		end
	endgenerate
	wire unused;
	assign unused = &{1'b0, w_data};
endmodule
