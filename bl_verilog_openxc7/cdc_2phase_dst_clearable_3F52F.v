`default_nettype none
module cdc_2phase_dst_clearable_3F52F (
	rst_ni,
	clk_i,
	clear_i,
	data_o,
	valid_o,
	ready_i,
	async_req_i,
	async_ack_o,
	async_data_i
);
	reg _sv2v_0;
	parameter [31:0] SYNC_STAGES = 2;
	input wire rst_ni;
	input wire clk_i;
	input wire clear_i;
	output wire [40:0] data_o;
	output wire valid_o;
	input wire ready_i;
	input wire async_req_i;
	output wire async_ack_o;
	input wire [40:0] async_data_i;
	(* dont_touch = "true" *) (* async_reg = "true" *) reg ack_dst_d;
	(* dont_touch = "true" *) (* async_reg = "true" *) reg ack_dst_q;
	(* dont_touch = "true" *) (* async_reg = "true" *) wire req_synced;
	(* dont_touch = "true" *) (* async_reg = "true" *) reg req_synced_q1;
	(* dont_touch = "true" *) reg [40:0] data_dst_d;
	(* dont_touch = "true" *) reg [40:0] data_dst_q;
	sync #(.STAGES(SYNC_STAGES)) i_sync(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.serial_i(async_req_i),
		.serial_o(req_synced)
	);
	always @(*) begin
		if (_sv2v_0)
			;
		ack_dst_d = ack_dst_q;
		if (clear_i)
			ack_dst_d = 1'b0;
		else if (valid_o && ready_i)
			ack_dst_d = ~ack_dst_q;
	end
	always @(*) begin
		if (_sv2v_0)
			;
		data_dst_d = data_dst_q;
		if ((req_synced != req_synced_q1) && !valid_o)
			data_dst_d = async_data_i;
	end
	always @(posedge clk_i) data_dst_q <= data_dst_d;
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni) begin
			ack_dst_q <= 0;
			req_synced_q1 <= 1'b0;
		end
		else begin
			ack_dst_q <= ack_dst_d;
			req_synced_q1 <= req_synced;
		end
	assign valid_o = ack_dst_q != req_synced_q1;
	assign data_o = data_dst_q;
	assign async_ack_o = ack_dst_q;
	initial _sv2v_0 = 0;
endmodule
