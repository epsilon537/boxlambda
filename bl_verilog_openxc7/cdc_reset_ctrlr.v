`default_nettype none
module cdc_reset_ctrlr (
	a_clk_i,
	a_rst_ni,
	a_clear_i,
	a_clear_o,
	a_clear_ack_i,
	a_isolate_o,
	a_isolate_ack_i,
	b_clk_i,
	b_rst_ni,
	b_clear_i,
	b_clear_o,
	b_clear_ack_i,
	b_isolate_o,
	b_isolate_ack_i
);
	parameter [31:0] SYNC_STAGES = 2;
	parameter [0:0] CLEAR_ON_ASYNC_RESET = 1'b1;
	input wire a_clk_i;
	input wire a_rst_ni;
	input wire a_clear_i;
	output wire a_clear_o;
	input wire a_clear_ack_i;
	output wire a_isolate_o;
	input wire a_isolate_ack_i;
	input wire b_clk_i;
	input wire b_rst_ni;
	input wire b_clear_i;
	output wire b_clear_o;
	input wire b_clear_ack_i;
	output wire b_isolate_o;
	input wire b_isolate_ack_i;
	(* dont_touch = "true" *) wire async_a2b_req;
	(* dont_touch = "true" *) wire async_b2a_ack;
	(* dont_touch = "true" *) wire [1:0] async_a2b_next_phase;
	(* dont_touch = "true" *) wire async_b2a_req;
	(* dont_touch = "true" *) wire async_a2b_ack;
	(* dont_touch = "true" *) wire [1:0] async_b2a_next_phase;
	cdc_reset_ctrlr_half #(
		.SYNC_STAGES(SYNC_STAGES),
		.CLEAR_ON_ASYNC_RESET(CLEAR_ON_ASYNC_RESET)
	) i_cdc_reset_ctrlr_half_a(
		.clk_i(a_clk_i),
		.rst_ni(a_rst_ni),
		.clear_i(a_clear_i),
		.clear_o(a_clear_o),
		.clear_ack_i(a_clear_ack_i),
		.isolate_o(a_isolate_o),
		.isolate_ack_i(a_isolate_ack_i),
		.async_next_phase_o(async_a2b_next_phase),
		.async_req_o(async_a2b_req),
		.async_ack_i(async_b2a_ack),
		.async_next_phase_i(async_b2a_next_phase),
		.async_req_i(async_b2a_req),
		.async_ack_o(async_a2b_ack)
	);
	cdc_reset_ctrlr_half #(
		.SYNC_STAGES(SYNC_STAGES),
		.CLEAR_ON_ASYNC_RESET(CLEAR_ON_ASYNC_RESET)
	) i_cdc_reset_ctrlr_half_b(
		.clk_i(b_clk_i),
		.rst_ni(b_rst_ni),
		.clear_i(b_clear_i),
		.clear_o(b_clear_o),
		.clear_ack_i(b_clear_ack_i),
		.isolate_o(b_isolate_o),
		.isolate_ack_i(b_isolate_ack_i),
		.async_next_phase_o(async_b2a_next_phase),
		.async_req_o(async_b2a_req),
		.async_ack_i(async_a2b_ack),
		.async_next_phase_i(async_a2b_next_phase),
		.async_req_i(async_a2b_req),
		.async_ack_o(async_b2a_ack)
	);
endmodule
