`default_nettype none
module cdc_4phase_dst_A46CE (
	rst_ni,
	clk_i,
	data_o,
	valid_o,
	ready_i,
	async_req_i,
	async_ack_o,
	async_data_i
);
	reg _sv2v_0;
	parameter [31:0] SYNC_STAGES = 2;
	parameter [0:0] DECOUPLED = 1;
	input wire rst_ni;
	input wire clk_i;
	output wire [1:0] data_o;
	output wire valid_o;
	input wire ready_i;
	input wire async_req_i;
	output wire async_ack_o;
	input wire [1:0] async_data_i;
	(* dont_touch = "true" *) reg ack_dst_d;
	(* dont_touch = "true" *) reg ack_dst_q;
	(* dont_touch = "true" *) wire req_synced;
	reg data_valid;
	wire output_ready;
	reg [1:0] state_d;
	reg [1:0] state_q;
	sync #(.STAGES(SYNC_STAGES)) i_sync(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.serial_i(async_req_i),
		.serial_o(req_synced)
	);
	always @(*) begin
		if (_sv2v_0)
			;
		state_d = state_q;
		data_valid = 1'b0;
		ack_dst_d = 1'b0;
		case (state_q)
			2'd0:
				if (req_synced == 1'b1) begin
					data_valid = 1'b1;
					if (output_ready == 1'b1)
						state_d = 2'd2;
					else
						state_d = 2'd1;
				end
			2'd1: begin
				data_valid = 1'b1;
				if (output_ready == 1'b1) begin
					state_d = 2'd2;
					ack_dst_d = 1'b1;
				end
			end
			2'd2: begin
				ack_dst_d = 1'b1;
				if (req_synced == 1'b0) begin
					ack_dst_d = 1'b0;
					state_d = 2'd0;
				end
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
		if (!rst_ni)
			ack_dst_q <= 1'b0;
		else
			ack_dst_q <= ack_dst_d;
	generate
		if (DECOUPLED) begin : gen_decoupled
			spill_register_8294E #(.Bypass(1'b0)) i_spill_register(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.valid_i(data_valid),
				.ready_o(output_ready),
				.data_i(async_data_i),
				.valid_o(valid_o),
				.ready_i(ready_i),
				.data_o(data_o)
			);
		end
		else begin : gen_not_decoupled
			assign valid_o = data_valid;
			assign output_ready = ready_i;
			assign data_o = async_data_i;
		end
	endgenerate
	assign async_ack_o = ack_dst_q;
	initial _sv2v_0 = 0;
endmodule
