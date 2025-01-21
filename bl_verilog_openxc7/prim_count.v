module prim_count (
	clk_i,
	rst_ni,
	clr_i,
	set_i,
	set_cnt_i,
	incr_en_i,
	decr_en_i,
	step_i,
	commit_i,
	cnt_o,
	cnt_after_commit_o,
	err_o
);
	parameter signed [31:0] Width = 2;
	parameter [Width - 1:0] ResetValue = 1'sb0;
	parameter [0:0] EnableAlertTriggerSVA = 1;
	parameter [3:0] PossibleActions = {4 {1'b1}};
	input clk_i;
	input rst_ni;
	input clr_i;
	input set_i;
	input [Width - 1:0] set_cnt_i;
	input incr_en_i;
	input decr_en_i;
	input [Width - 1:0] step_i;
	input commit_i;
	output wire [Width - 1:0] cnt_o;
	output wire [Width - 1:0] cnt_after_commit_o;
	output wire err_o;
	localparam signed [31:0] NumCnt = 2;
	localparam [(NumCnt * Width) - 1:0] ResetValues = {{Width {1'b1}} - ResetValue, ResetValue};
	wire [(NumCnt * Width) - 1:0] cnt_d;
	wire [(NumCnt * Width) - 1:0] cnt_d_committed;
	wire [(NumCnt * Width) - 1:0] cnt_q;
	wire [(NumCnt * Width) - 1:0] fpv_force;
	assign fpv_force = 1'sb0;
	genvar _gv_k_4;
	generate
		for (_gv_k_4 = 0; _gv_k_4 < NumCnt; _gv_k_4 = _gv_k_4 + 1) begin : gen_cnts
			localparam k = _gv_k_4;
			wire incr_en;
			wire decr_en;
			wire [Width - 1:0] set_val;
			if (k == 0) begin : gen_up_cnt
				assign incr_en = incr_en_i;
				assign decr_en = decr_en_i;
				assign set_val = set_cnt_i;
			end
			else begin : gen_dn_cnt
				assign incr_en = decr_en_i;
				assign decr_en = incr_en_i;
				assign set_val = {Width {1'b1}} - set_cnt_i;
			end
			wire [Width:0] ext_cnt;
			assign ext_cnt = (decr_en ? {1'b0, cnt_q[k * Width+:Width]} - {1'b0, step_i} : (incr_en ? {1'b0, cnt_q[k * Width+:Width]} + {1'b0, step_i} : {1'b0, cnt_q[k * Width+:Width]}));
			wire uflow;
			wire oflow;
			assign oflow = incr_en && ext_cnt[Width];
			assign uflow = decr_en && ext_cnt[Width];
			wire [Width - 1:0] cnt_sat;
			assign cnt_sat = (uflow ? {Width {1'sb0}} : (oflow ? {Width {1'b1}} : ext_cnt[Width - 1:0]));
			wire cnt_en;
			assign cnt_en = (incr_en ^ decr_en) && ((incr_en && !(&cnt_q[k * Width+:Width])) || (decr_en && (cnt_q[k * Width+:Width] != {Width * 1 {1'sb0}})));
			assign cnt_d[k * Width+:Width] = (clr_i ? ResetValues[k * Width+:Width] : (set_i ? set_val : (cnt_en ? cnt_sat : cnt_q[k * Width+:Width])));
			assign cnt_d_committed[k * Width+:Width] = (commit_i ? cnt_d[k * Width+:Width] : cnt_q[k * Width+:Width]);
			wire [Width - 1:0] cnt_unforced_q;
			prim_flop #(
				.Width(Width),
				.ResetValue(ResetValues[k * Width+:Width])
			) u_cnt_flop(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.d_i(cnt_d_committed[k * Width+:Width]),
				.q_o(cnt_unforced_q)
			);
			assign cnt_q[k * Width+:Width] = fpv_force[k * Width+:Width] + cnt_unforced_q;
		end
	endgenerate
	wire [Width:0] sum;
	assign sum = cnt_q[0+:Width] + cnt_q[Width+:Width];
	assign err_o = sum != {1'b0, {Width {1'b1}}};
	assign cnt_o = cnt_q[0+:Width];
	assign cnt_after_commit_o = cnt_d[0+:Width];
endmodule
