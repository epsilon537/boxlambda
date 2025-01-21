`default_nettype none
module wb_usb_hid_host (
	wb_clk,
	wb_rst_n,
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
	wbs_err,
	wb_usb_rst_n,
	usb_typ,
	usb_report_stb,
	usb_conn_err,
	usb_key_modifiers,
	usb_key1,
	usb_key2,
	usb_key3,
	usb_key4,
	usb_mouse_btn,
	usb_mouse_dx,
	usb_mouse_dy,
	usb_game_l,
	usb_game_r,
	usb_game_u,
	usb_game_d,
	usb_game_a,
	usb_game_b,
	usb_game_x,
	usb_game_y,
	usb_game_sel,
	usb_game_sta,
	usb_dbg_hid_report,
	update_leds_stb,
	leds,
	ack_update_leds_stb
);
	input wire wb_clk;
	input wire wb_rst_n;
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
	input wire wb_usb_rst_n;
	input wire [1:0] usb_typ;
	input wire usb_report_stb;
	input wire usb_conn_err;
	input wire [7:0] usb_key_modifiers;
	input wire [7:0] usb_key1;
	input wire [7:0] usb_key2;
	input wire [7:0] usb_key3;
	input wire [7:0] usb_key4;
	input wire [7:0] usb_mouse_btn;
	input wire signed [7:0] usb_mouse_dx;
	input wire signed [7:0] usb_mouse_dy;
	input wire usb_game_l;
	input wire usb_game_r;
	input wire usb_game_u;
	input wire usb_game_d;
	input wire usb_game_a;
	input wire usb_game_b;
	input wire usb_game_x;
	input wire usb_game_y;
	input wire usb_game_sel;
	input wire usb_game_sta;
	input wire [63:0] usb_dbg_hid_report;
	output reg update_leds_stb;
	output reg [3:0] leds;
	input wire ack_update_leds_stb;
	reg [1:0] wb_usb_ien;
	reg [1:0] wb_usb_isr;
	reg [1:0] wb_usb_typ;
	reg wb_usb_conn_err;
	reg [7:0] wb_usb_key_modifiers;
	reg [7:0] wb_usb_key1;
	reg [7:0] wb_usb_key2;
	reg [7:0] wb_usb_key3;
	reg [7:0] wb_usb_key4;
	reg [7:0] wb_usb_mouse_btn;
	reg signed [7:0] wb_usb_mouse_dx;
	reg signed [7:0] wb_usb_mouse_dy;
	reg wb_usb_game_l;
	reg wb_usb_game_r;
	reg wb_usb_game_u;
	reg wb_usb_game_d;
	reg wb_usb_game_a;
	reg wb_usb_game_b;
	reg wb_usb_game_x;
	reg wb_usb_game_y;
	reg wb_usb_game_sel;
	reg wb_usb_game_sta;
	reg [63:0] wb_usb_dbg_hid_report;
	reg do_ack_wbs;
	wire do_wbs_wr_reg;
	wire unused = &{wbs_sel, wbs_dat_w[31:4]};
	assign irq = |(wb_usb_isr & wb_usb_ien);
	assign do_wbs_wr_reg = (wbs_cyc && wbs_stb) && wbs_we;
	assign wbs_ack = do_ack_wbs & wbs_cyc;
	assign wbs_stall = 1'b0;
	assign wbs_err = 1'b0;
	always @(posedge wb_clk)
		if (!wb_usb_rst_n || !wb_rst_n) begin
			if (!wb_rst_n) begin
				wb_usb_ien <= 2'b00;
				do_ack_wbs <= 1'b0;
				update_leds_stb <= 1'b0;
				leds <= 4'b0000;
			end
			wb_usb_isr <= 2'b00;
			wb_usb_typ <= 2'b00;
			wb_usb_conn_err <= 1'b0;
		end
		else begin
			do_ack_wbs <= 1'b0;
			if (wbs_stb)
				do_ack_wbs <= 1'b1;
			update_leds_stb <= 1'b0;
			if (do_wbs_wr_reg)
				case (wbs_adr)
					4'd0: wb_usb_ien <= wbs_dat_w[1:0];
					4'd1: wb_usb_isr <= wb_usb_isr & ~wbs_dat_w[1:0];
					4'd9: begin
						update_leds_stb <= 1'b1;
						leds <= wbs_dat_w[3:0];
					end
					default:
						;
				endcase
			if (ack_update_leds_stb)
				wb_usb_isr[1] <= 1'b1;
			if (usb_report_stb) begin
				wb_usb_isr[0] <= 1'b1;
				{wb_usb_typ, wb_usb_conn_err} <= {usb_typ, usb_conn_err};
				{wb_usb_key_modifiers, wb_usb_key1, wb_usb_key2, wb_usb_key3, wb_usb_key4} <= {usb_key_modifiers, usb_key1, usb_key2, usb_key3, usb_key4};
				{wb_usb_mouse_btn, wb_usb_mouse_dx, wb_usb_mouse_dy} <= {usb_mouse_btn, usb_mouse_dx, usb_mouse_dy};
				{wb_usb_game_l, wb_usb_game_r, wb_usb_game_u, wb_usb_game_d, wb_usb_game_a, wb_usb_game_b, wb_usb_game_x, wb_usb_game_y, wb_usb_game_sel, wb_usb_game_sta} <= {usb_game_l, usb_game_r, usb_game_u, usb_game_d, usb_game_a, usb_game_b, usb_game_x, usb_game_y, usb_game_sel, usb_game_sta};
				wb_usb_dbg_hid_report <= usb_dbg_hid_report;
			end
		end
	always @(*)
		case (wbs_adr)
			4'd0: wbs_dat_r = {30'b000000000000000000000000000000, wb_usb_ien};
			4'd1: wbs_dat_r = {30'b000000000000000000000000000000, wb_usb_isr};
			4'd2: wbs_dat_r = {29'b00000000000000000000000000000, wb_usb_conn_err, wb_usb_typ};
			4'd3: wbs_dat_r = {24'b000000000000000000000000, wb_usb_key_modifiers};
			4'd4: wbs_dat_r = {wb_usb_key4, wb_usb_key3, wb_usb_key2, wb_usb_key1};
			4'd5: wbs_dat_r = {8'b00000000, wb_usb_mouse_btn, wb_usb_mouse_dx, wb_usb_mouse_dy};
			4'd6: wbs_dat_r = {22'b0000000000000000000000, wb_usb_game_l, wb_usb_game_r, wb_usb_game_u, wb_usb_game_d, wb_usb_game_a, wb_usb_game_b, wb_usb_game_x, wb_usb_game_y, wb_usb_game_sel, wb_usb_game_sta};
			4'd7: wbs_dat_r = wb_usb_dbg_hid_report[31:0];
			4'd8: wbs_dat_r = wb_usb_dbg_hid_report[63:32];
			4'd9: wbs_dat_r = {28'b0000000000000000000000000000, leds};
			default: wbs_dat_r = 32'd0;
		endcase
endmodule
