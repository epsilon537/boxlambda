`default_nettype none
module dmi_jtag (
	clk_i,
	rst_ni,
	testmode_i,
	dmi_rst_no,
	dmi_req_o,
	dmi_req_valid_o,
	dmi_req_ready_i,
	dmi_resp_i,
	dmi_resp_ready_o,
	dmi_resp_valid_i,
	tck_i,
	tms_i,
	trst_ni,
	td_i,
	td_o,
	tdo_oe_o
);
	reg _sv2v_0;
	parameter [31:0] IdcodeValue = 32'h00000db3;
	input wire clk_i;
	input wire rst_ni;
	input wire testmode_i;
	output wire dmi_rst_no;
	output wire [40:0] dmi_req_o;
	output wire dmi_req_valid_o;
	input wire dmi_req_ready_i;
	input wire [33:0] dmi_resp_i;
	output wire dmi_resp_ready_o;
	input wire dmi_resp_valid_i;
	input wire tck_i;
	input wire tms_i;
	input wire trst_ni;
	input wire td_i;
	output wire td_o;
	output wire tdo_oe_o;
	reg [1:0] error_d;
	reg [1:0] error_q;
	wire tck;
	wire jtag_dmi_clear;
	wire dmi_clear;
	wire update;
	wire capture;
	wire shift;
	wire tdi;
	wire dtmcs_select;
	reg [31:0] dtmcs_q;
	assign dmi_clear = jtag_dmi_clear || ((dtmcs_select && update) && dtmcs_q[17]);
	reg [31:0] dtmcs_d;
	function automatic [30:0] sv2v_cast_31;
		input reg [30:0] inp;
		sv2v_cast_31 = inp;
	endfunction
	always @(*) begin
		if (_sv2v_0)
			;
		dtmcs_d = dtmcs_q;
		if (capture) begin
			if (dtmcs_select)
				dtmcs_d = {20'h00001, error_q, 10'h071};
		end
		if (shift) begin
			if (dtmcs_select)
				dtmcs_d = {tdi, sv2v_cast_31(dtmcs_q >> 1)};
		end
	end
	always @(posedge tck or negedge trst_ni)
		if (!trst_ni)
			dtmcs_q <= 1'sb0;
		else
			dtmcs_q <= dtmcs_d;
	wire dmi_select;
	wire dmi_tdo;
	wire [40:0] dmi_req;
	wire dmi_req_ready;
	reg dmi_req_valid;
	wire [33:0] dmi_resp;
	wire dmi_resp_valid;
	wire dmi_resp_ready;
	reg [2:0] state_d;
	reg [2:0] state_q;
	reg [40:0] dr_d;
	reg [40:0] dr_q;
	reg [6:0] address_d;
	reg [6:0] address_q;
	reg [31:0] data_d;
	reg [31:0] data_q;
	wire [40:0] dmi;
	assign dmi = dr_q;
	assign dmi_req[40-:7] = address_q;
	assign dmi_req[31-:32] = data_q;
	assign dmi_req[33-:2] = (state_q == 3'd3 ? 2'h2 : 2'h1);
	assign dmi_resp_ready = 1'b1;
	reg error_dmi_busy;
	reg error_dmi_op_failed;
	function automatic [1:0] sv2v_cast_2;
		input reg [1:0] inp;
		sv2v_cast_2 = inp;
	endfunction
	always @(*) begin : p_fsm
		if (_sv2v_0)
			;
		error_dmi_busy = 1'b0;
		error_dmi_op_failed = 1'b0;
		state_d = state_q;
		address_d = address_q;
		data_d = data_q;
		error_d = error_q;
		dmi_req_valid = 1'b0;
		if (dmi_clear) begin
			state_d = 3'd0;
			data_d = 1'sb0;
			error_d = 2'h0;
			address_d = 1'sb0;
		end
		else begin
			(* full_case, parallel_case *)
			case (state_q)
				3'd0:
					if ((dmi_select && update) && (error_q == 2'h0)) begin
						address_d = dmi[40-:7];
						data_d = dmi[33-:32];
						if (sv2v_cast_2(dmi[1-:2]) == 2'h1)
							state_d = 3'd1;
						else if (sv2v_cast_2(dmi[1-:2]) == 2'h2)
							state_d = 3'd3;
					end
				3'd1: begin
					dmi_req_valid = 1'b1;
					if (dmi_req_ready)
						state_d = 3'd2;
				end
				3'd2:
					if (dmi_resp_valid) begin
						(* full_case, parallel_case *)
						case (dmi_resp[1-:2])
							2'h0: data_d = dmi_resp[33-:32];
							2'h2: begin
								data_d = 32'hdeadbeef;
								error_dmi_op_failed = 1'b1;
							end
							2'h3: begin
								data_d = 32'hb051b051;
								error_dmi_busy = 1'b1;
							end
							default: data_d = 32'hbaadc0de;
						endcase
						state_d = 3'd0;
					end
				3'd3: begin
					dmi_req_valid = 1'b1;
					if (dmi_req_ready)
						state_d = 3'd4;
				end
				3'd4:
					if (dmi_resp_valid) begin
						(* full_case, parallel_case *)
						case (dmi_resp[1-:2])
							2'h2: error_dmi_op_failed = 1'b1;
							2'h3: error_dmi_busy = 1'b1;
							default:
								;
						endcase
						state_d = 3'd0;
					end
				default:
					if (dmi_resp_valid)
						state_d = 3'd0;
			endcase
			if (update && (state_q != 3'd0))
				error_dmi_busy = 1'b1;
			if (capture && |{state_q == 3'd1, state_q == 3'd2})
				error_dmi_busy = 1'b1;
			if (error_dmi_busy && (error_q == 2'h0))
				error_d = 2'h3;
			if (error_dmi_op_failed && (error_q == 2'h0))
				error_d = 2'h2;
			if ((update && dtmcs_q[16]) && dtmcs_select)
				error_d = 2'h0;
		end
	end
	assign dmi_tdo = dr_q[0];
	always @(*) begin : p_shift
		if (_sv2v_0)
			;
		dr_d = dr_q;
		if (dmi_clear)
			dr_d = 1'sb0;
		else begin
			if (capture) begin
				if (dmi_select) begin
					if ((error_q == 2'h0) && !error_dmi_busy)
						dr_d = {address_q, data_q, 2'h0};
					else if ((error_q == 2'h3) || error_dmi_busy)
						dr_d = {address_q, data_q, 2'h3};
				end
			end
			if (shift) begin
				if (dmi_select)
					dr_d = {tdi, dr_q[40:1]};
			end
		end
	end
	always @(posedge tck or negedge trst_ni)
		if (!trst_ni) begin
			dr_q <= 1'sb0;
			state_q <= 3'd0;
			address_q <= 1'sb0;
			data_q <= 1'sb0;
			error_q <= 2'h0;
		end
		else begin
			dr_q <= dr_d;
			state_q <= state_d;
			address_q <= address_d;
			data_q <= data_d;
			error_q <= error_d;
		end
	dmi_jtag_tap #(
		.IrLength(5),
		.IdcodeValue(IdcodeValue)
	) i_dmi_jtag_tap(
		.tck_i(tck_i),
		.tms_i(tms_i),
		.trst_ni(trst_ni),
		.td_i(td_i),
		.td_o(td_o),
		.tdo_oe_o(tdo_oe_o),
		.testmode_i(testmode_i),
		.tck_o(tck),
		.dmi_clear_o(jtag_dmi_clear),
		.update_o(update),
		.capture_o(capture),
		.shift_o(shift),
		.tdi_o(tdi),
		.dtmcs_select_o(dtmcs_select),
		.dtmcs_tdo_i(dtmcs_q[0]),
		.dmi_select_o(dmi_select),
		.dmi_tdo_i(dmi_tdo)
	);
	dmi_cdc i_dmi_cdc(
		.tck_i(tck),
		.trst_ni(trst_ni),
		.jtag_dmi_cdc_clear_i(dmi_clear),
		.jtag_dmi_req_i(dmi_req),
		.jtag_dmi_ready_o(dmi_req_ready),
		.jtag_dmi_valid_i(dmi_req_valid),
		.jtag_dmi_resp_o(dmi_resp),
		.jtag_dmi_valid_o(dmi_resp_valid),
		.jtag_dmi_ready_i(dmi_resp_ready),
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.core_dmi_rst_no(dmi_rst_no),
		.core_dmi_req_o(dmi_req_o),
		.core_dmi_valid_o(dmi_req_valid_o),
		.core_dmi_ready_i(dmi_req_ready_i),
		.core_dmi_resp_i(dmi_resp_i),
		.core_dmi_ready_o(dmi_resp_ready_o),
		.core_dmi_valid_i(dmi_resp_valid_i)
	);
	initial _sv2v_0 = 0;
endmodule
