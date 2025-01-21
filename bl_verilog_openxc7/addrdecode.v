`default_nettype none
module addrdecode (
	i_clk,
	i_reset,
	i_valid,
	o_stall,
	i_addr,
	i_data,
	o_valid,
	i_stall,
	o_decode,
	o_addr,
	o_data
);
	parameter NS = 8;
	parameter AW = 32;
	parameter DW = 38;
	parameter [(NS * AW) - 1:0] SLAVE_ADDR = {3'b111, {AW - 3 {1'b0}}, 3'b110, {AW - 3 {1'b0}}, 3'b101, {AW - 3 {1'b0}}, 3'b100, {AW - 3 {1'b0}}, 3'b011, {AW - 3 {1'b0}}, 3'b010, {AW - 3 {1'b0}}, 4'b0010, {AW - 4 {1'b0}}, 4'b0000, {AW - 4 {1'b0}}};
	parameter [(NS * AW) - 1:0] SLAVE_MASK = (NS <= 1 ? 0 : {{NS - 2 {3'b111, {AW - 3 {1'b0}}}}, {2 {4'b1111, {AW - 4 {1'b0}}}}});
	parameter [NS - 1:0] ACCESS_ALLOWED = -1;
	parameter [0:0] OPT_REGISTERED = 0;
	parameter [0:0] OPT_LOWPOWER = 0;
	input wire i_clk;
	input wire i_reset;
	input wire i_valid;
	output reg o_stall;
	input wire [AW - 1:0] i_addr;
	input wire [DW - 1:0] i_data;
	output reg o_valid;
	input wire i_stall;
	output reg [NS:0] o_decode;
	output reg [AW - 1:0] o_addr;
	output reg [DW - 1:0] o_data;
	localparam [0:0] OPT_NONESEL = !ACCESS_ALLOWED[0] || (SLAVE_MASK[AW - 1:0] != 0);
	wire [NS:0] request;
	reg [NS - 1:0] prerequest;
	integer iM;
	always @(*)
		for (iM = 0; iM < NS; iM = iM + 1)
			prerequest[iM] = (((i_addr ^ SLAVE_ADDR[iM * AW+:AW]) & SLAVE_MASK[iM * AW+:AW]) == 0) && ACCESS_ALLOWED[iM];
	generate
		if (OPT_NONESEL) begin : NO_DEFAULT_REQUEST
			reg [NS - 1:0] r_request;
			always @(*) begin
				for (iM = 0; iM < NS; iM = iM + 1)
					r_request[iM] = i_valid && prerequest[iM];
				if (!OPT_NONESEL && ((NS > 1) && |prerequest[NS - 1:1]))
					r_request[0] = 1'b0;
			end
			assign request[NS - 1:0] = r_request;
		end
		else if (NS == 1) begin : SINGLE_SLAVE
			assign request[0] = i_valid;
		end
		else begin : genblk1
			reg [NS - 1:0] r_request;
			always @(*) begin
				for (iM = 0; iM < NS; iM = iM + 1)
					r_request[iM] = i_valid && prerequest[iM];
				if (!OPT_NONESEL && ((NS > 1) && |prerequest[NS - 1:1]))
					r_request[0] = 1'b0;
			end
			assign request[NS - 1:0] = r_request;
		end
		if (OPT_NONESEL) begin : genblk2
			reg r_request_NS;
			reg r_none_sel;
			always @(*) begin
				r_none_sel = i_valid && (prerequest == 0);
				r_request_NS = r_none_sel;
			end
			assign request[NS] = r_request_NS;
		end
		else begin : genblk2
			assign request[NS] = 1'b0;
		end
		if (OPT_REGISTERED) begin : genblk3
			initial o_valid = 0;
			always @(posedge i_clk)
				if (i_reset)
					o_valid <= 0;
				else if (!o_stall)
					o_valid <= i_valid;
			initial o_addr = 0;
			initial o_data = 0;
			always @(posedge i_clk)
				if (i_reset && OPT_LOWPOWER) begin
					o_addr <= 0;
					o_data <= 0;
				end
				else if ((!o_valid || !i_stall) && (i_valid || !OPT_LOWPOWER)) begin
					o_addr <= i_addr;
					o_data <= i_data;
				end
				else if (OPT_LOWPOWER && !i_stall) begin
					o_addr <= 0;
					o_data <= 0;
				end
			initial o_decode = 0;
			always @(posedge i_clk)
				if (i_reset)
					o_decode <= 0;
				else if ((!o_valid || !i_stall) && (i_valid || !OPT_LOWPOWER))
					o_decode <= request;
				else if (OPT_LOWPOWER && !i_stall)
					o_decode <= 0;
			always @(*) o_stall = o_valid && i_stall;
		end
		else begin : genblk3
			always @(*) begin
				o_valid = i_valid;
				o_stall = i_stall;
				o_addr = i_addr;
				o_data = i_data;
				o_decode = request;
			end
			wire unused;
			assign unused = &{1'b0, i_reset};
		end
	endgenerate
endmodule
