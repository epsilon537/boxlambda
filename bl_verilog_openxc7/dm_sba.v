`default_nettype none
module dm_sba (
	clk_i,
	rst_ni,
	dmactive_i,
	master_req_o,
	master_add_o,
	master_we_o,
	master_wdata_o,
	master_be_o,
	master_gnt_i,
	master_r_valid_i,
	master_r_err_i,
	master_r_other_err_i,
	master_r_rdata_i,
	sbaddress_i,
	sbaddress_write_valid_i,
	sbreadonaddr_i,
	sbaddress_o,
	sbautoincrement_i,
	sbaccess_i,
	sbreadondata_i,
	sbdata_i,
	sbdata_read_valid_i,
	sbdata_write_valid_i,
	sbdata_o,
	sbdata_valid_o,
	sbbusy_o,
	sberror_valid_o,
	sberror_o
);
	reg _sv2v_0;
	parameter [31:0] BusWidth = 32;
	parameter [0:0] ReadByteEnable = 1;
	input wire clk_i;
	input wire rst_ni;
	input wire dmactive_i;
	output wire master_req_o;
	output wire [BusWidth - 1:0] master_add_o;
	output wire master_we_o;
	output wire [BusWidth - 1:0] master_wdata_o;
	output wire [(BusWidth / 8) - 1:0] master_be_o;
	input wire master_gnt_i;
	input wire master_r_valid_i;
	input wire master_r_err_i;
	input wire master_r_other_err_i;
	input wire [BusWidth - 1:0] master_r_rdata_i;
	input wire [BusWidth - 1:0] sbaddress_i;
	input wire sbaddress_write_valid_i;
	input wire sbreadonaddr_i;
	output wire [BusWidth - 1:0] sbaddress_o;
	input wire sbautoincrement_i;
	input wire [2:0] sbaccess_i;
	input wire sbreadondata_i;
	input wire [BusWidth - 1:0] sbdata_i;
	input wire sbdata_read_valid_i;
	input wire sbdata_write_valid_i;
	output wire [BusWidth - 1:0] sbdata_o;
	output wire sbdata_valid_o;
	output wire sbbusy_o;
	output reg sberror_valid_o;
	output reg [2:0] sberror_o;
	localparam signed [31:0] BeIdxWidth = $clog2(BusWidth / 8);
	reg [2:0] state_d;
	reg [2:0] state_q;
	reg [BusWidth - 1:0] address;
	reg req;
	wire gnt;
	reg we;
	reg [(BusWidth / 8) - 1:0] be;
	reg [(BusWidth / 8) - 1:0] be_mask;
	reg [BeIdxWidth - 1:0] be_idx;
	assign sbbusy_o = state_q != 3'd0;
	function automatic signed [31:0] sv2v_cast_32_signed;
		input reg signed [31:0] inp;
		sv2v_cast_32_signed = inp;
	endfunction
	always @(*) begin : p_be_mask
		if (_sv2v_0)
			;
		be_mask = 1'sb0;
		(* full_case, parallel_case *)
		case (sbaccess_i)
			3'b000: be_mask[be_idx] = 1'sb1;
			3'b001: be_mask[sv2v_cast_32_signed({be_idx[BeIdxWidth - 1:1], 1'b0})+:2] = 1'sb1;
			3'b010:
				if (BusWidth == 32'd64)
					be_mask[sv2v_cast_32_signed({be_idx[BeIdxWidth - 1], 2'h0})+:4] = 1'sb1;
				else
					be_mask = 1'sb1;
			3'b011: be_mask = 1'sb1;
			default:
				;
		endcase
	end
	wire [BusWidth - 1:0] sbaccess_mask;
	assign sbaccess_mask = {BusWidth {1'b1}} << sbaccess_i;
	reg addr_incr_en;
	wire [BusWidth - 1:0] addr_incr;
	function automatic [BusWidth - 1:0] sv2v_cast_8CBFF;
		input reg [BusWidth - 1:0] inp;
		sv2v_cast_8CBFF = inp;
	endfunction
	assign addr_incr = (addr_incr_en ? sv2v_cast_8CBFF(1'b1) << sbaccess_i : {BusWidth {1'sb0}});
	assign sbaddress_o = sbaddress_i + addr_incr;
	function automatic [31:0] sv2v_cast_32;
		input reg [31:0] inp;
		sv2v_cast_32 = inp;
	endfunction
	always @(*) begin : p_fsm
		if (_sv2v_0)
			;
		req = 1'b0;
		address = sbaddress_i;
		we = 1'b0;
		be = 1'sb0;
		be_idx = sbaddress_i[BeIdxWidth - 1:0];
		sberror_o = 1'sb0;
		sberror_valid_o = 1'b0;
		addr_incr_en = 1'b0;
		state_d = state_q;
		(* full_case, parallel_case *)
		case (state_q)
			3'd0: begin
				if (sbaddress_write_valid_i && sbreadonaddr_i)
					state_d = 3'd1;
				if (sbdata_write_valid_i)
					state_d = 3'd2;
				if (sbdata_read_valid_i && sbreadondata_i)
					state_d = 3'd1;
			end
			3'd1: begin
				req = 1'b1;
				if (ReadByteEnable)
					be = be_mask;
				if (gnt)
					state_d = 3'd3;
			end
			3'd2: begin
				req = 1'b1;
				we = 1'b1;
				be = be_mask;
				if (gnt)
					state_d = 3'd4;
			end
			3'd3:
				if (sbdata_valid_o) begin
					state_d = 3'd0;
					addr_incr_en = sbautoincrement_i;
					if (master_r_other_err_i) begin
						sberror_valid_o = 1'b1;
						sberror_o = 3'd7;
					end
					else if (master_r_err_i) begin
						sberror_valid_o = 1'b1;
						sberror_o = 3'd2;
					end
				end
			3'd4:
				if (sbdata_valid_o) begin
					state_d = 3'd0;
					addr_incr_en = sbautoincrement_i;
					if (master_r_other_err_i) begin
						sberror_valid_o = 1'b1;
						sberror_o = 3'd7;
					end
					else if (master_r_err_i) begin
						sberror_valid_o = 1'b1;
						sberror_o = 3'd2;
					end
				end
			default: state_d = 3'd0;
		endcase
		if ((sv2v_cast_32(sbaccess_i) > BeIdxWidth) && (state_q != 3'd0)) begin
			req = 1'b0;
			state_d = 3'd0;
			sberror_valid_o = 1'b1;
			sberror_o = 3'd4;
		end
		if (|(sbaddress_i & ~sbaccess_mask) && (state_q != 3'd0)) begin
			req = 1'b0;
			state_d = 3'd0;
			sberror_valid_o = 1'b1;
			sberror_o = 3'd3;
		end
	end
	always @(posedge clk_i or negedge rst_ni) begin : p_regs
		if (!rst_ni)
			state_q <= 3'd0;
		else
			state_q <= state_d;
	end
	wire [BeIdxWidth - 1:0] be_idx_masked;
	function automatic [BeIdxWidth - 1:0] sv2v_cast_F03CB;
		input reg [BeIdxWidth - 1:0] inp;
		sv2v_cast_F03CB = inp;
	endfunction
	assign be_idx_masked = be_idx & sv2v_cast_F03CB(sbaccess_mask);
	assign master_req_o = req;
	assign master_add_o = address[BusWidth - 1:0];
	assign master_we_o = we;
	assign master_wdata_o = sbdata_i[BusWidth - 1:0] << (8 * be_idx_masked);
	assign master_be_o = be[(BusWidth / 8) - 1:0];
	assign gnt = master_gnt_i;
	assign sbdata_valid_o = master_r_valid_i;
	assign sbdata_o = master_r_rdata_i[BusWidth - 1:0] >> (8 * be_idx_masked);
	initial _sv2v_0 = 0;
endmodule
