`default_nettype none
module wbarbiter (
	i_clk,
	i_reset,
	i_a_cyc,
	i_a_stb,
	i_a_we,
	i_a_adr,
	i_a_dat,
	i_a_sel,
	o_a_ack,
	o_a_stall,
	o_a_err,
	i_b_cyc,
	i_b_stb,
	i_b_we,
	i_b_adr,
	i_b_dat,
	i_b_sel,
	o_b_ack,
	o_b_stall,
	o_b_err,
	o_cyc,
	o_stb,
	o_we,
	o_adr,
	o_dat,
	o_sel,
	i_ack,
	i_stall,
	i_err
);
	parameter DW = 32;
	parameter AW = 32;
	parameter SCHEME = "ALTERNATING";
	parameter [0:0] OPT_ZERO_ON_IDLE = 1'b0;
	parameter [31:0] F_MAX_STALL = 3;
	parameter [31:0] F_MAX_ACK_DELAY = 3;
	parameter [31:0] F_LGDEPTH = 3;
	input wire i_clk;
	input wire i_reset;
	input wire i_a_cyc;
	input wire i_a_stb;
	input wire i_a_we;
	input wire [AW - 1:0] i_a_adr;
	input wire [DW - 1:0] i_a_dat;
	input wire [(DW / 8) - 1:0] i_a_sel;
	output wire o_a_ack;
	output wire o_a_stall;
	output wire o_a_err;
	input wire i_b_cyc;
	input wire i_b_stb;
	input wire i_b_we;
	input wire [AW - 1:0] i_b_adr;
	input wire [DW - 1:0] i_b_dat;
	input wire [(DW / 8) - 1:0] i_b_sel;
	output wire o_b_ack;
	output wire o_b_stall;
	output wire o_b_err;
	output wire o_cyc;
	output wire o_stb;
	output wire o_we;
	output wire [AW - 1:0] o_adr;
	output wire [DW - 1:0] o_dat;
	output wire [(DW / 8) - 1:0] o_sel;
	input wire i_ack;
	input wire i_stall;
	input wire i_err;
	reg r_a_owner;
	assign o_cyc = (r_a_owner ? i_a_cyc : i_b_cyc);
	initial r_a_owner = 1'b1;
	generate
		if (SCHEME == "PRIORITY") begin : PRI
			always @(posedge i_clk)
				if (!i_b_cyc)
					r_a_owner <= 1'b1;
				else if (i_b_stb && !i_a_cyc)
					r_a_owner <= 1'b0;
		end
		else if (SCHEME == "ALTERNATING") begin : ALT
			reg last_owner;
			initial last_owner = 1'b0;
			always @(posedge i_clk)
				if (i_a_cyc && r_a_owner)
					last_owner <= 1'b1;
				else if (i_b_cyc && !r_a_owner)
					last_owner <= 1'b0;
			always @(posedge i_clk)
				if (!i_a_cyc && !i_b_cyc)
					r_a_owner <= !last_owner;
				else if (r_a_owner && !i_a_cyc) begin
					if (i_b_stb)
						r_a_owner <= 1'b0;
				end
				else if (!r_a_owner && !i_b_cyc) begin
					if (i_a_stb)
						r_a_owner <= 1'b1;
				end
		end
		else begin : LST
			always @(posedge i_clk)
				if (!i_a_cyc && i_b_stb)
					r_a_owner <= 1'b0;
				else if (!i_b_cyc && i_a_stb)
					r_a_owner <= 1'b1;
		end
	endgenerate
	assign o_we = (r_a_owner ? i_a_we : i_b_we);
	generate
		if (OPT_ZERO_ON_IDLE) begin : ZERO_IDLE
			assign o_stb = (o_cyc ? (r_a_owner ? i_a_stb : i_b_stb) : 0);
			assign o_adr = (o_stb ? (r_a_owner ? i_a_adr : i_b_adr) : 0);
			assign o_dat = (o_stb ? (r_a_owner ? i_a_dat : i_b_dat) : 0);
			assign o_sel = (o_stb ? (r_a_owner ? i_a_sel : i_b_sel) : 0);
			assign o_a_ack = (o_cyc && r_a_owner ? i_ack : 1'b0);
			assign o_b_ack = (o_cyc && !r_a_owner ? i_ack : 1'b0);
			assign o_a_stall = (o_cyc && r_a_owner ? i_stall : 1'b1);
			assign o_b_stall = (o_cyc && !r_a_owner ? i_stall : 1'b1);
			assign o_a_err = (o_cyc && r_a_owner ? i_err : 1'b0);
			assign o_b_err = (o_cyc && !r_a_owner ? i_err : 1'b0);
		end
		else begin : LOW_LOGIC
			assign o_stb = (r_a_owner ? i_a_stb : i_b_stb);
			assign o_adr = (r_a_owner ? i_a_adr : i_b_adr);
			assign o_dat = (r_a_owner ? i_a_dat : i_b_dat);
			assign o_sel = (r_a_owner ? i_a_sel : i_b_sel);
			assign o_a_ack = (r_a_owner ? i_ack : 1'b0);
			assign o_b_ack = (!r_a_owner ? i_ack : 1'b0);
			assign o_a_stall = (r_a_owner ? i_stall : 1'b1);
			assign o_b_stall = (!r_a_owner ? i_stall : 1'b1);
			assign o_a_err = (r_a_owner ? i_err : 1'b0);
			assign o_b_err = (!r_a_owner ? i_err : 1'b0);
		end
	endgenerate
	wire unused;
	assign unused = &{1'b0, i_reset, F_LGDEPTH, F_MAX_STALL, F_MAX_ACK_DELAY};
endmodule
