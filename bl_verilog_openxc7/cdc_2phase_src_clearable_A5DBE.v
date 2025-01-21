`default_nettype none
module cdc_2phase_src_clearable_A5DBE (
	rst_ni,
	clk_i,
	clear_i,
	data_i,
	valid_i,
	ready_o,
	async_req_o,
	async_ack_i,
	async_data_o
);
	reg _sv2v_0;
	parameter [31:0] SYNC_STAGES = 2;
	input wire rst_ni;
	input wire clk_i;
	input wire clear_i;
	input wire [40:0] data_i;
	input wire valid_i;
	output wire ready_o;
	output wire async_req_o;
	input wire async_ack_i;
	output wire [40:0] async_data_o;
	(* dont_touch = "true" *) reg req_src_d;
	(* dont_touch = "true" *) reg req_src_q;
	(* dont_touch = "true" *) wire ack_synced;
	(* dont_touch = "true" *) reg [40:0] data_src_d;
	(* dont_touch = "true" *) reg [40:0] data_src_q;
	sync #(.STAGES(SYNC_STAGES)) i_sync(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.serial_i(async_ack_i),
		.serial_o(ack_synced)
	);
	always @(*) begin
		if (_sv2v_0)
			;
		data_src_d = data_src_q;
		req_src_d = req_src_q;
		if (clear_i)
			req_src_d = 1'b0;
		else if (valid_i && ready_o) begin
			req_src_d = ~req_src_q;
			data_src_d = data_i;
		end
	end
	always @(posedge clk_i) data_src_q <= data_src_d;
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			req_src_q <= 0;
		else
			req_src_q <= req_src_d;
	assign ready_o = req_src_q == ack_synced;
	assign async_req_o = req_src_q;
	assign async_data_o = data_src_q;
	initial _sv2v_0 = 0;
endmodule
