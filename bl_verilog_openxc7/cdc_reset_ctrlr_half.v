`default_nettype none
module cdc_reset_ctrlr_half (
	clk_i,
	rst_ni,
	clear_i,
	isolate_o,
	isolate_ack_i,
	clear_o,
	clear_ack_i,
	async_next_phase_o,
	async_req_o,
	async_ack_i,
	async_next_phase_i,
	async_req_i,
	async_ack_o
);
	reg _sv2v_0;
	parameter [31:0] SYNC_STAGES = 2;
	parameter [0:0] CLEAR_ON_ASYNC_RESET = 1'b1;
	input wire clk_i;
	input wire rst_ni;
	input wire clear_i;
	output wire isolate_o;
	input wire isolate_ack_i;
	output wire clear_o;
	input wire clear_ack_i;
	output wire [1:0] async_next_phase_o;
	output wire async_req_o;
	input wire async_ack_i;
	input wire [1:0] async_next_phase_i;
	input wire async_req_i;
	output wire async_ack_o;
	reg [3:0] initiator_state_d;
	reg [3:0] initiator_state_q;
	reg [1:0] initiator_clear_seq_phase;
	reg initiator_phase_transition_req;
	wire initiator_phase_transition_ack;
	reg initiator_isolate_out;
	reg initiator_clear_out;
	always @(*) begin
		if (_sv2v_0)
			;
		initiator_state_d = initiator_state_q;
		initiator_phase_transition_req = 1'b0;
		initiator_isolate_out = 1'b0;
		initiator_clear_out = 1'b0;
		initiator_clear_seq_phase = 2'd0;
		case (initiator_state_q)
			4'd0:
				if (clear_i)
					initiator_state_d = 4'd1;
			4'd1: begin
				initiator_phase_transition_req = 1'b1;
				initiator_clear_seq_phase = 2'd1;
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b0;
				if (initiator_phase_transition_ack && isolate_ack_i)
					initiator_state_d = 4'd4;
				else if (initiator_phase_transition_ack)
					initiator_state_d = 4'd3;
				else if (isolate_ack_i)
					initiator_state_d = 4'd2;
			end
			4'd3: begin
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b0;
				initiator_clear_seq_phase = 2'd1;
				if (isolate_ack_i)
					initiator_state_d = 4'd4;
			end
			4'd2: begin
				initiator_phase_transition_req = 1'b1;
				initiator_clear_seq_phase = 2'd1;
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b0;
				if (initiator_phase_transition_ack)
					initiator_state_d = 4'd4;
			end
			4'd4: begin
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b1;
				initiator_phase_transition_req = 1'b1;
				initiator_clear_seq_phase = 2'd2;
				if (initiator_phase_transition_ack && clear_ack_i)
					initiator_state_d = 4'd7;
				else if (initiator_phase_transition_ack)
					initiator_state_d = 4'd6;
				else if (clear_ack_i)
					initiator_state_d = 4'd5;
			end
			4'd6: begin
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b1;
				initiator_clear_seq_phase = 2'd2;
				if (clear_ack_i)
					initiator_state_d = 4'd7;
			end
			4'd5: begin
				initiator_phase_transition_req = 1'b1;
				initiator_clear_seq_phase = 2'd2;
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b1;
				if (initiator_phase_transition_ack)
					initiator_state_d = 4'd7;
			end
			4'd7: begin
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b0;
				initiator_phase_transition_req = 1'b1;
				initiator_clear_seq_phase = 2'd3;
				if (initiator_phase_transition_ack)
					initiator_state_d = 4'd8;
			end
			4'd8: begin
				initiator_isolate_out = 1'b1;
				initiator_clear_out = 1'b0;
				initiator_phase_transition_req = 1'b1;
				initiator_clear_seq_phase = 2'd0;
				if (initiator_phase_transition_ack)
					initiator_state_d = 4'd0;
			end
			default: initiator_state_d = 4'd1;
		endcase
	end
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni) begin
			if (CLEAR_ON_ASYNC_RESET)
				initiator_state_q <= 4'd1;
			else
				initiator_state_q <= 4'd0;
		end
		else
			initiator_state_q <= initiator_state_d;
	cdc_4phase_src_DFE1F #(
		.SYNC_STAGES(2),
		.DECOUPLED(0),
		.SEND_RESET_MSG(CLEAR_ON_ASYNC_RESET),
		.RESET_MSG(2'd1)
	) i_state_transition_cdc_src(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.data_i(initiator_clear_seq_phase),
		.valid_i(initiator_phase_transition_req),
		.ready_o(initiator_phase_transition_ack),
		.async_req_o(async_req_o),
		.async_ack_i(async_ack_i),
		.async_data_o(async_next_phase_o)
	);
	reg [1:0] receiver_phase_q;
	wire [1:0] receiver_next_phase;
	wire receiver_phase_req;
	reg receiver_phase_ack;
	reg receiver_isolate_out;
	reg receiver_clear_out;
	cdc_4phase_dst_A46CE #(
		.SYNC_STAGES(2),
		.DECOUPLED(0)
	) i_state_transition_cdc_dst(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.data_o(receiver_next_phase),
		.valid_o(receiver_phase_req),
		.ready_i(receiver_phase_ack),
		.async_req_i(async_req_i),
		.async_ack_o(async_ack_o),
		.async_data_i(async_next_phase_i)
	);
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			receiver_phase_q <= 2'd0;
		else if (receiver_phase_req && receiver_phase_ack)
			receiver_phase_q <= receiver_next_phase;
	always @(*) begin
		if (_sv2v_0)
			;
		receiver_isolate_out = 1'b0;
		receiver_clear_out = 1'b0;
		receiver_phase_ack = 1'b0;
		if (receiver_phase_req)
			case (receiver_next_phase)
				2'd0: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b0;
					receiver_phase_ack = 1'b1;
				end
				2'd1: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b1;
					receiver_phase_ack = isolate_ack_i;
				end
				2'd2: begin
					receiver_clear_out = 1'b1;
					receiver_isolate_out = 1'b1;
					receiver_phase_ack = clear_ack_i;
				end
				2'd3: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b1;
					receiver_phase_ack = 1'b1;
				end
				default: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b0;
					receiver_phase_ack = 1'b0;
				end
			endcase
		else
			case (receiver_phase_q)
				2'd0: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b0;
				end
				2'd1: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b1;
				end
				2'd2: begin
					receiver_clear_out = 1'b1;
					receiver_isolate_out = 1'b1;
				end
				2'd3: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b1;
				end
				default: begin
					receiver_clear_out = 1'b0;
					receiver_isolate_out = 1'b0;
					receiver_phase_ack = 1'b0;
				end
			endcase
	end
	assign clear_o = initiator_clear_out || receiver_clear_out;
	assign isolate_o = initiator_isolate_out || receiver_isolate_out;
	initial _sv2v_0 = 0;
endmodule
