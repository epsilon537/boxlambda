`default_nettype none
module cdc_2phase_clearable_DC602 (
	src_rst_ni,
	src_clk_i,
	src_clear_i,
	src_clear_pending_o,
	src_data_i,
	src_valid_i,
	src_ready_o,
	dst_rst_ni,
	dst_clk_i,
	dst_clear_i,
	dst_clear_pending_o,
	dst_data_o,
	dst_valid_o,
	dst_ready_i
);
	parameter [31:0] SYNC_STAGES = 3;
	parameter signed [31:0] CLEAR_ON_ASYNC_RESET = 1;
	input wire src_rst_ni;
	input wire src_clk_i;
	input wire src_clear_i;
	output wire src_clear_pending_o;
	input wire [33:0] src_data_i;
	input wire src_valid_i;
	output wire src_ready_o;
	input wire dst_rst_ni;
	input wire dst_clk_i;
	input wire dst_clear_i;
	output wire dst_clear_pending_o;
	output wire [33:0] dst_data_o;
	output wire dst_valid_o;
	input wire dst_ready_i;
	wire s_src_clear_req;
	reg s_src_clear_ack_q;
	wire s_src_ready;
	wire s_src_isolate_req;
	reg s_src_isolate_ack_q;
	wire s_dst_clear_req;
	reg s_dst_clear_ack_q;
	wire s_dst_valid;
	wire s_dst_isolate_req;
	reg s_dst_isolate_ack_q;
	(* dont_touch = "true" *) wire async_req;
	(* dont_touch = "true" *) wire async_ack;
	(* dont_touch = "true" *) wire [33:0] async_data;
	generate
		if (CLEAR_ON_ASYNC_RESET) begin : gen_elaboration_assertion
			if (SYNC_STAGES < 3) begin : genblk1
				initial $display("Error [elaboration] /home/epsilon/work/boxlambda/gw/components/riscv-dbg/../../../sub/common_cells/src/cdc_2phase_clearable.sv:92:7 - cdc_2phase_clearable.gen_elaboration_assertion.genblk1\n msg: ", "The clearable 2-phase CDC with async reset", "synchronization requires at least 3 synchronizer stages for the FIFO.");
			end
		end
		else begin : gen_elaboration_assertion
			if (SYNC_STAGES < 2) begin : gen_elaboration_assertion
				initial $display("Error [elaboration] /home/epsilon/work/boxlambda/gw/components/riscv-dbg/../../../sub/common_cells/src/cdc_2phase_clearable.sv:96:7 - cdc_2phase_clearable.gen_elaboration_assertion.gen_elaboration_assertion\n msg: ", "A minimum of 2 synchronizer stages is required for proper functionality.");
			end
		end
	endgenerate
	cdc_2phase_src_clearable_B85FB #(.SYNC_STAGES(SYNC_STAGES)) i_src(
		.rst_ni(src_rst_ni),
		.clk_i(src_clk_i),
		.clear_i(s_src_clear_req),
		.data_i(src_data_i),
		.valid_i(src_valid_i & !s_src_isolate_req),
		.ready_o(s_src_ready),
		.async_req_o(async_req),
		.async_ack_i(async_ack),
		.async_data_o(async_data)
	);
	assign src_ready_o = s_src_ready & !s_src_isolate_req;
	cdc_2phase_dst_clearable_0389A #(.SYNC_STAGES(SYNC_STAGES)) i_dst(
		.rst_ni(dst_rst_ni),
		.clk_i(dst_clk_i),
		.clear_i(s_dst_clear_req),
		.data_o(dst_data_o),
		.valid_o(s_dst_valid),
		.ready_i(dst_ready_i & !s_dst_isolate_req),
		.async_req_i(async_req),
		.async_ack_o(async_ack),
		.async_data_i(async_data)
	);
	assign dst_valid_o = s_dst_valid & !s_dst_isolate_req;
	cdc_reset_ctrlr #(.SYNC_STAGES(SYNC_STAGES - 1)) i_cdc_reset_ctrlr(
		.a_clk_i(src_clk_i),
		.a_rst_ni(src_rst_ni),
		.a_clear_i(src_clear_i),
		.a_clear_o(s_src_clear_req),
		.a_clear_ack_i(s_src_clear_ack_q),
		.a_isolate_o(s_src_isolate_req),
		.a_isolate_ack_i(s_src_isolate_ack_q),
		.b_clk_i(dst_clk_i),
		.b_rst_ni(dst_rst_ni),
		.b_clear_i(dst_clear_i),
		.b_clear_o(s_dst_clear_req),
		.b_clear_ack_i(s_dst_clear_ack_q),
		.b_isolate_o(s_dst_isolate_req),
		.b_isolate_ack_i(s_dst_isolate_ack_q)
	);
	always @(posedge src_clk_i or negedge src_rst_ni)
		if (!src_rst_ni) begin
			s_src_isolate_ack_q <= 1'b0;
			s_src_clear_ack_q <= 1'b0;
		end
		else begin
			s_src_isolate_ack_q <= s_src_isolate_req;
			s_src_clear_ack_q <= s_src_clear_req;
		end
	always @(posedge dst_clk_i or negedge dst_rst_ni)
		if (!dst_rst_ni) begin
			s_dst_isolate_ack_q <= 1'b0;
			s_dst_clear_ack_q <= 1'b0;
		end
		else begin
			s_dst_isolate_ack_q <= s_dst_isolate_req;
			s_dst_clear_ack_q <= s_dst_clear_req;
		end
	assign src_clear_pending_o = s_src_isolate_req;
	assign dst_clear_pending_o = s_dst_isolate_req;
endmodule
