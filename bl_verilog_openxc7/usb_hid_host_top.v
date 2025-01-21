`default_nettype none
module usb_hid_host_top (
	wb_clk,
	usb_clk,
	usb_rst_n,
	wb_rst_n,
	usb_dm_i,
	usb_dp_i,
	usb_dm_o,
	usb_dp_o,
	usb_oe,
	irq,
	wbs_adr,
	wbs_dat_w,
	wbs_dat_r,
	wbs_sel,
	wbs_stall,
	wbs_cyc,
	wbs_stb,
	wbs_ack,
	wbs_we,
	wbs_err
);
	input wire wb_clk;
	input wire usb_clk;
	input wire usb_rst_n;
	input wire wb_rst_n;
	input wire usb_dm_i;
	input wire usb_dp_i;
	output wire usb_dm_o;
	output wire usb_dp_o;
	output wire usb_oe;
	output wire irq;
	input wire [3:0] wbs_adr;
	input wire [31:0] wbs_dat_w;
	output reg [31:0] wbs_dat_r;
	input wire [3:0] wbs_sel;
	output wire wbs_stall;
	input wire wbs_cyc;
	input wire wbs_stb;
	output wire wbs_ack;
	input wire wbs_we;
	output wire wbs_err;
	wire [1:0] usb_typ;
	wire usb_conn_err;
	wire [7:0] usb_key_modifiers;
	wire [7:0] usb_key1;
	wire [7:0] usb_key2;
	wire [7:0] usb_key3;
	wire [7:0] usb_key4;
	wire [7:0] usb_mouse_btn;
	wire signed [7:0] usb_mouse_dx;
	wire signed [7:0] usb_mouse_dy;
	wire usb_game_l;
	wire usb_game_r;
	wire usb_game_u;
	wire usb_game_d;
	wire usb_game_a;
	wire usb_game_b;
	wire usb_game_x;
	wire usb_game_y;
	wire usb_game_sel;
	wire usb_game_sta;
	wire [63:0] usb_dbg_hid_report;
	wire [3:0] leds;
	wire wb_update_leds_stb;
	wire usb_update_leds_stb;
	wire wb_ack_update_leds_stb;
	wire usb_ack_update_leds_stb;
	wire wb_report_stb;
	wire usb_report_stb;
	wire wb_usb_rst_n;
	sync3 wb_usb_rst_n_sync(
		.q(wb_usb_rst_n),
		.d(usb_rst_n),
		.clk(wb_clk),
		.rst_n(wb_rst_n)
	);
	wire [32:1] sv2v_tmp_wb_usb_hid_host_inst_wbs_dat_r;
	always @(*) wbs_dat_r = sv2v_tmp_wb_usb_hid_host_inst_wbs_dat_r;
	wb_usb_hid_host wb_usb_hid_host_inst(
		.wb_clk(wb_clk),
		.wb_rst_n(wb_rst_n),
		.irq(irq),
		.wbs_adr(wbs_adr),
		.wbs_dat_w(wbs_dat_w),
		.wbs_dat_r(sv2v_tmp_wb_usb_hid_host_inst_wbs_dat_r),
		.wbs_sel(wbs_sel),
		.wbs_stall(wbs_stall),
		.wbs_cyc(wbs_cyc),
		.wbs_stb(wbs_stb),
		.wbs_ack(wbs_ack),
		.wbs_we(wbs_we),
		.wbs_err(wbs_err),
		.wb_usb_rst_n(wb_usb_rst_n),
		.usb_typ(usb_typ),
		.usb_report_stb(wb_report_stb),
		.usb_conn_err(usb_conn_err),
		.usb_key_modifiers(usb_key_modifiers),
		.usb_key1(usb_key1),
		.usb_key2(usb_key2),
		.usb_key3(usb_key3),
		.usb_key4(usb_key4),
		.usb_mouse_btn(usb_mouse_btn),
		.usb_mouse_dx(usb_mouse_dx),
		.usb_mouse_dy(usb_mouse_dy),
		.usb_game_l(usb_game_l),
		.usb_game_r(usb_game_r),
		.usb_game_u(usb_game_u),
		.usb_game_d(usb_game_d),
		.usb_game_a(usb_game_a),
		.usb_game_b(usb_game_b),
		.usb_game_x(usb_game_x),
		.usb_game_y(usb_game_y),
		.usb_game_sel(usb_game_sel),
		.usb_game_sta(usb_game_sta),
		.usb_dbg_hid_report(usb_dbg_hid_report),
		.update_leds_stb(wb_update_leds_stb),
		.leds(leds),
		.ack_update_leds_stb(wb_ack_update_leds_stb)
	);
	syncpls update_leds_syncpls(
		.t_clk(wb_clk),
		.t_rst_n(wb_rst_n),
		.t_pulse(wb_update_leds_stb),
		.r_clk(usb_clk),
		.r_rst_n(usb_rst_n),
		.r_pulse(usb_update_leds_stb)
	);
	syncpls ack_update_leds_syncpls(
		.t_clk(usb_clk),
		.t_rst_n(usb_rst_n),
		.t_pulse(usb_ack_update_leds_stb),
		.r_clk(wb_clk),
		.r_rst_n(wb_rst_n),
		.r_pulse(wb_ack_update_leds_stb)
	);
	syncpls usb_report_syncpls(
		.t_clk(usb_clk),
		.t_rst_n(usb_rst_n),
		.t_pulse(usb_report_stb),
		.r_clk(wb_clk),
		.r_rst_n(wb_rst_n),
		.r_pulse(wb_report_stb)
	);
	usb_hid_host usb_hid_host_inst(
		.usbclk(usb_clk),
		.usbrst_n(usb_rst_n),
		.usb_dm_i(usb_dm_i),
		.usb_dp_i(usb_dp_i),
		.usb_dm_o(usb_dm_o),
		.usb_dp_o(usb_dp_o),
		.usb_oe(usb_oe),
		.update_leds_stb(usb_update_leds_stb),
		.ack_update_leds_stb(usb_ack_update_leds_stb),
		.leds(leds),
		.typ(usb_typ),
		.report(usb_report_stb),
		.conerr(usb_conn_err),
		.key_modifiers(usb_key_modifiers),
		.key1(usb_key1),
		.key2(usb_key2),
		.key3(usb_key3),
		.key4(usb_key4),
		.mouse_btn(usb_mouse_btn),
		.mouse_dx(usb_mouse_dx),
		.mouse_dy(usb_mouse_dy),
		.game_l(usb_game_l),
		.game_r(usb_game_r),
		.game_u(usb_game_u),
		.game_d(usb_game_d),
		.game_a(usb_game_a),
		.game_b(usb_game_b),
		.game_x(usb_game_x),
		.game_y(usb_game_y),
		.game_sel(usb_game_sel),
		.game_sta(usb_game_sta),
		.dbg_hid_report(usb_dbg_hid_report)
	);
endmodule
