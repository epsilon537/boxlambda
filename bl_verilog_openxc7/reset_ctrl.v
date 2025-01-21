`default_nettype none
module reset_ctrl (
	sys_clk,
	usb_clk,
	sys_pll_locked_i,
	usb_pll_locked_i,
	ndm_reset_i,
	ext_reset_i,
	ndm_reset_o,
	dm_reset_o,
	usb_reset_o,
	por_completed_o,
	wb_adr,
	wb_dat_w,
	wb_dat_r,
	wb_sel,
	wb_stall,
	wb_cyc,
	wb_stb,
	wb_ack,
	wb_we,
	wb_err
);
	reg _sv2v_0;
	input wire sys_clk;
	input wire usb_clk;
	input wire sys_pll_locked_i;
	input wire usb_pll_locked_i;
	input wire ndm_reset_i;
	input wire ext_reset_i;
	output wire ndm_reset_o;
	output wire dm_reset_o;
	output wire usb_reset_o;
	output wire por_completed_o;
	input wire wb_adr;
	input wire [31:0] wb_dat_w;
	output wire [31:0] wb_dat_r;
	input wire [3:0] wb_sel;
	output wire wb_stall;
	input wire wb_cyc;
	input wire wb_stb;
	output wire wb_ack;
	input wire wb_we;
	output wire wb_err;
	localparam [5:0] RESET_REASON_POR = 1;
	localparam [5:0] RESET_REASON_WB_NDM = 2;
	localparam [5:0] RESET_REASON_WB_DM = 4;
	localparam [5:0] RESET_REASON_NDM = 8;
	localparam [5:0] RESET_REASON_EXT = 16;
	localparam [5:0] RESET_REASON_WB_USB = 32;
	reg ndm_reset_o_reg;
	reg dm_reset_o_reg;
	reg usb_reset_o_reg;
	reg [31:0] por_state_reg;
	reg [5:0] por_assert_counter;
	wire wb_por;
	wire usb_por;
	reg [5:0] reset_reason_reg;
	reg [5:0] reset_reason_next;
	initial begin
		por_state_reg = 32'd0;
		por_assert_counter = 6'b000000;
		reset_reason_reg = 6'b000000;
		ndm_reset_o_reg = 1'b0;
		dm_reset_o_reg = 1'b0;
	end
	wire wb_usb_pll_locked;
	sync3 usb_pll_locked_sync(
		.q(wb_usb_pll_locked),
		.d(usb_pll_locked_i),
		.clk(sys_clk),
		.rst_n(1'b1)
	);
	always @(posedge sys_clk)
		case (por_state_reg)
			32'd0:
				if (sys_pll_locked_i && wb_usb_pll_locked)
					por_state_reg <= 32'd1;
			32'd1: begin
				if (por_assert_counter == 6'b111111)
					por_state_reg <= 32'd2;
				por_assert_counter <= por_assert_counter + 6'b000001;
			end
			32'd2: begin
				if (por_assert_counter == 6'b111111)
					por_state_reg <= 32'd3;
				por_assert_counter <= por_assert_counter + 6'b000001;
			end
			32'd3:
				;
		endcase
	assign wb_por = por_state_reg == 32'd2;
	assign por_completed_o = por_state_reg == 32'd3;
	wire wb_ext_reset_conditioned;
	wire usb_ext_reset_conditioned;
	button_conditioner button_conditioner_instr(
		.clk(sys_clk),
		.btn(ext_reset_i),
		.out(wb_ext_reset_conditioned)
	);
	reg wb_adr_reg;
	reg do_ack_reg;
	wire do_wb_wr;
	reg unused = &{wb_sel, wb_adr, wb_dat_w[31:3]};
	initial wb_adr_reg = 1'b0;
	assign do_wb_wr = (wb_cyc & wb_stb) & wb_we;
	always @(posedge sys_clk) begin
		do_ack_reg <= 1'b0;
		if (wb_stb) begin
			do_ack_reg <= 1'b1;
			wb_adr_reg <= wb_adr;
		end
	end
	assign wb_dat_r = {26'b00000000000000000000000000, reset_reason_reg};
	assign wb_ack = do_ack_reg & wb_cyc;
	assign wb_stall = 1'b0;
	assign wb_err = 1'b0;
	wire wb_ndm_reset_stb;
	wire wb_dm_reset_stb;
	wire wb_usb_reset_stb;
	wire usb_wb_reset_stb;
	assign wb_ndm_reset_stb = do_wb_wr & wb_dat_w[0];
	assign wb_dm_reset_stb = do_wb_wr & wb_dat_w[1];
	assign wb_usb_reset_stb = do_wb_wr & wb_dat_w[2];
	always @(*) begin
		if (_sv2v_0)
			;
		reset_reason_next = reset_reason_reg;
		if (wb_por)
			reset_reason_next = reset_reason_next | RESET_REASON_POR;
		if (wb_ndm_reset_stb)
			reset_reason_next = reset_reason_next | RESET_REASON_WB_NDM;
		if (wb_dm_reset_stb)
			reset_reason_next = reset_reason_next | RESET_REASON_WB_DM;
		if (ndm_reset_i)
			reset_reason_next = reset_reason_next | RESET_REASON_NDM;
		if (wb_ext_reset_conditioned)
			reset_reason_next = reset_reason_next | RESET_REASON_EXT;
		if (wb_usb_reset_stb)
			reset_reason_next = reset_reason_next | RESET_REASON_WB_USB;
		if (wb_adr_reg && do_ack_reg)
			reset_reason_next = 6'b000000;
	end
	always @(posedge sys_clk) begin
		reset_reason_reg <= reset_reason_next;
		ndm_reset_o_reg <= ((wb_por | wb_ext_reset_conditioned) | ndm_reset_i) | wb_ndm_reset_stb;
		dm_reset_o_reg <= (wb_por | wb_ext_reset_conditioned) | wb_dm_reset_stb;
	end
	assign ndm_reset_o = ndm_reset_o_reg;
	assign dm_reset_o = dm_reset_o_reg;
	syncpls usb_wb_reset_sync(
		.t_clk(sys_clk),
		.t_rst_n(1'b1),
		.t_pulse(wb_usb_reset_stb),
		.r_clk(usb_clk),
		.r_rst_n(1'b1),
		.r_pulse(usb_wb_reset_stb)
	);
	sync3 usb_ext_reset_sync(
		.q(usb_ext_reset_conditioned),
		.d(wb_ext_reset_conditioned),
		.clk(usb_clk),
		.rst_n(1'b1)
	);
	sync3 usb_por_sync(
		.q(usb_por),
		.d(wb_por),
		.clk(usb_clk),
		.rst_n(1'b1)
	);
	initial usb_reset_o_reg = 1'b0;
	always @(posedge usb_clk) usb_reset_o_reg <= (usb_wb_reset_stb | usb_por) | usb_ext_reset_conditioned;
	assign usb_reset_o = usb_reset_o_reg;
	initial _sv2v_0 = 0;
endmodule
