module wb_timer (
	clk_i,
	rst_i,
	wb_cyc_i,
	wb_stb_i,
	wb_we_i,
	wb_addr_i,
	wb_data_i,
	wb_sel_i,
	wb_stall_o,
	wb_ack_o,
	wb_err_o,
	wb_data_o,
	timer_irq_o
);
	input wire clk_i;
	input wire rst_i;
	input wire wb_cyc_i;
	input wire wb_stb_i;
	input wire wb_we_i;
	input wire [7:0] wb_addr_i;
	input wire [31:0] wb_data_i;
	input wire [3:0] wb_sel_i;
	output wire wb_stall_o;
	output wire wb_ack_o;
	output wire wb_err_o;
	output wire [31:0] wb_data_o;
	output wire timer_irq_o;
	assign wb_stall_o = 1'b0;
	timer #(
		.DataWidth(32),
		.AddressWidth(10)
	) timer_inst(
		.clk_i(clk_i),
		.rst_ni(~rst_i),
		.timer_req_i(wb_cyc_i & wb_stb_i),
		.timer_addr_i({wb_addr_i, 2'b00}),
		.timer_we_i(wb_we_i),
		.timer_be_i(wb_sel_i),
		.timer_wdata_i(wb_data_i),
		.timer_rvalid_o(wb_ack_o),
		.timer_rdata_o(wb_data_o),
		.timer_err_o(wb_err_o),
		.timer_intr_o(timer_irq_o)
	);
endmodule
