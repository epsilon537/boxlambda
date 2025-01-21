`default_nettype none
module cdc_4phase_src_DFE1F (
	rst_ni,
	clk_i,
	data_i,
	valid_i,
	ready_o,
	async_req_o,
	async_ack_i,
	async_data_o
);
	reg _sv2v_0;
	parameter [31:0] SYNC_STAGES = 2;
	parameter [0:0] DECOUPLED = 1'b1;
	parameter [0:0] SEND_RESET_MSG = 1'b0;
	parameter [1:0] RESET_MSG = 2'b00;
	input wire rst_ni;
	input wire clk_i;
	input wire [1:0] data_i;
	input wire valid_i;
	output reg ready_o;
	output wire async_req_o;
	input wire async_ack_i;
	output wire [1:0] async_data_o;
	(* dont_touch = "true" *) reg req_src_d;
	(* dont_touch = "true" *) reg req_src_q;
	(* dont_touch = "true" *) reg [1:0] data_src_d;
	(* dont_touch = "true" *) reg [1:0] data_src_q;
	(* dont_touch = "true" *) wire ack_synced;
	reg [1:0] state_d;
	reg [1:0] state_q;
	sync #(.STAGES(SYNC_STAGES)) i_sync(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.serial_i(async_ack_i),
		.serial_o(ack_synced)
	);
	always @(*) begin
		if (_sv2v_0)
			;
		state_d = state_q;
		req_src_d = 1'b0;
		data_src_d = data_src_q;
		ready_o = 1'b0;
		case (state_q)
			2'd0: begin
				if (DECOUPLED)
					ready_o = 1'b1;
				else
					ready_o = 1'b0;
				if (valid_i) begin
					data_src_d = data_i;
					req_src_d = 1'b1;
					state_d = 2'd1;
				end
			end
			2'd1: begin
				req_src_d = 1'b1;
				if (ack_synced == 1'b1) begin
					req_src_d = 1'b0;
					state_d = 2'd2;
				end
			end
			2'd2:
				if (ack_synced == 1'b0) begin
					state_d = 2'd0;
					if (!DECOUPLED)
						ready_o = 1'b1;
				end
			default: state_d = 2'd0;
		endcase
	end
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			state_q <= 2'd0;
		else
			state_q <= state_d;
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni) begin
			if (SEND_RESET_MSG) begin
				req_src_q <= 1'b1;
				data_src_q <= RESET_MSG;
			end
			else begin
				req_src_q <= 1'b0;
				data_src_q <= 2'b00;
			end
		end
		else begin
			req_src_q <= req_src_d;
			data_src_q <= data_src_d;
		end
	assign async_req_o = req_src_q;
	assign async_data_o = data_src_q;
	initial _sv2v_0 = 0;
endmodule
