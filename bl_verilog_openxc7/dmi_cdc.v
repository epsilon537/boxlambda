`default_nettype none
module dmi_cdc (
	tck_i,
	trst_ni,
	jtag_dmi_req_i,
	jtag_dmi_ready_o,
	jtag_dmi_valid_i,
	jtag_dmi_cdc_clear_i,
	jtag_dmi_resp_o,
	jtag_dmi_valid_o,
	jtag_dmi_ready_i,
	clk_i,
	rst_ni,
	core_dmi_rst_no,
	core_dmi_req_o,
	core_dmi_valid_o,
	core_dmi_ready_i,
	core_dmi_resp_i,
	core_dmi_ready_o,
	core_dmi_valid_i
);
	input wire tck_i;
	input wire trst_ni;
	input wire [40:0] jtag_dmi_req_i;
	output wire jtag_dmi_ready_o;
	input wire jtag_dmi_valid_i;
	input wire jtag_dmi_cdc_clear_i;
	output wire [33:0] jtag_dmi_resp_o;
	output wire jtag_dmi_valid_o;
	input wire jtag_dmi_ready_i;
	input wire clk_i;
	input wire rst_ni;
	output wire core_dmi_rst_no;
	output wire [40:0] core_dmi_req_o;
	output wire core_dmi_valid_o;
	input wire core_dmi_ready_i;
	input wire [33:0] core_dmi_resp_i;
	output wire core_dmi_ready_o;
	input wire core_dmi_valid_i;
	wire core_clear_pending;
	cdc_2phase_clearable_88D17 i_cdc_req(
		.src_rst_ni(trst_ni),
		.src_clear_i(jtag_dmi_cdc_clear_i),
		.src_clk_i(tck_i),
		.src_clear_pending_o(),
		.src_data_i(jtag_dmi_req_i),
		.src_valid_i(jtag_dmi_valid_i),
		.src_ready_o(jtag_dmi_ready_o),
		.dst_rst_ni(rst_ni),
		.dst_clear_i(1'b0),
		.dst_clear_pending_o(core_clear_pending),
		.dst_clk_i(clk_i),
		.dst_data_o(core_dmi_req_o),
		.dst_valid_o(core_dmi_valid_o),
		.dst_ready_i(core_dmi_ready_i)
	);
	cdc_2phase_clearable_DC602 i_cdc_resp(
		.src_rst_ni(rst_ni),
		.src_clear_i(1'b0),
		.src_clear_pending_o(),
		.src_clk_i(clk_i),
		.src_data_i(core_dmi_resp_i),
		.src_valid_i(core_dmi_valid_i),
		.src_ready_o(core_dmi_ready_o),
		.dst_rst_ni(trst_ni),
		.dst_clear_i(jtag_dmi_cdc_clear_i),
		.dst_clear_pending_o(),
		.dst_clk_i(tck_i),
		.dst_data_o(jtag_dmi_resp_o),
		.dst_valid_o(jtag_dmi_valid_o),
		.dst_ready_i(jtag_dmi_ready_i)
	);
	reg core_clear_pending_q;
	reg core_dmi_rst_nq;
	wire clear_pending_rise_edge_detect;
	assign clear_pending_rise_edge_detect = !core_clear_pending_q && core_clear_pending;
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni) begin
			core_dmi_rst_nq <= 1'b1;
			core_clear_pending_q <= 1'b0;
		end
		else begin
			core_dmi_rst_nq <= ~clear_pending_rise_edge_detect;
			core_clear_pending_q <= core_clear_pending;
		end
	assign core_dmi_rst_no = core_dmi_rst_nq;
endmodule
