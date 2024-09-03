//usb_hid_host_top is the top-level of the usb_hid_host core with a wishbone frontend.
module usb_hid_host_top (
    input  wire wb_clk,     // Wishbone clock - assumed to be faster than usb_clock, e.g. 50MHz.
    input  wire usb_clk,    // 12MHz clock
    input  wire usb_rst_n,  // USB clock domain active low reset
    input  wire wb_rst_n,   // Wishbone clock domain active low reset
    input  wire usb_dm_i,
    usb_dp_i,  // USB D- and D+ input
    output wire usb_dm_o,
    usb_dp_o,  // USB D- and D+ output
    output wire usb_oe,     // Output Enable.
    output wire irq,

    //32-bit pipelined Wishbone slave interface.
    input wire [3:0] wbs_adr,
    input wire [31:0] wbs_dat_w,
    output reg [31:0] wbs_dat_r,
    input wire [3:0] wbs_sel,
    output wire wbs_stall,
    input wire wbs_cyc,
    input wire wbs_stb,
    output wire wbs_ack,
    input wire wbs_we,
    output wire wbs_err
);

  wire [1:0] usb_typ;  //USB type
  wire usb_conn_err;  //USB connection error

  // keyboard signals
  wire [7:0] usb_key_modifiers;
  wire [7:0] usb_key1, usb_key2, usb_key3, usb_key4;

  // mouse signals
  wire        [7:0] usb_mouse_btn;  // {5'bx, middle, right, left}
  wire signed [7:0] usb_mouse_dx;  // signed 8-bit, cleared after `report` pulse
  wire signed [7:0] usb_mouse_dy;  // signed 8-bit, cleared after `report` pulse

  // gamepad signals
  wire usb_game_l, usb_game_r, usb_game_u, usb_game_d;  // left right up down
  wire usb_game_a, usb_game_b, usb_game_x, usb_game_y, usb_game_sel, usb_game_sta;  // buttons

  wire [63:0] usb_dbg_hid_report;  // last HID report

  wire [ 3:0] leds;  //Led bitmap

  wire wb_update_leds_stb, usb_update_leds_stb;
  wire wb_ack_update_leds_stb, usb_ack_update_leds_stb;
  wire wb_report_stb, usb_report_stb;
  wire wb_usb_rst_n;

  //Synchronize usb clock domain reset signal to WB clock domain.
  sync3 wb_usb_rst_n_sync (
      .q(wb_usb_rst_n),
      .d(usb_rst_n),
      .clk(wb_clk),
      .rst_n(wb_rst_n)
  );

  //Wishbone front-end for USB HID host.
  wb_usb_hid_host wb_usb_hid_host_inst (
      .wb_clk(wb_clk),
      .wb_rst_n(wb_rst_n),
      .irq(irq),
      .wbs_adr(wbs_adr),
      .wbs_dat_w(wbs_dat_w),
      .wbs_dat_r(wbs_dat_r),
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

  //Synchronize update_leds pulse to USB clock domain.
  syncpls update_leds_syncpls (
      .t_clk(wb_clk),  //transmitting clock.
      .t_rst_n(wb_rst_n),  //reset in t_clk domain.
      .t_pulse(wb_update_leds_stb),  //input pulse in t_clk domain.
      .r_clk(usb_clk),  //receiving clock.
      .r_rst_n(usb_rst_n),  //reset in r_clk_domain.
      .r_pulse(usb_update_leds_stb)
  );  //output pulse in r_clk domain.

  //Synchronize ack_update_leds pulse to WB clock domain.
  syncpls ack_update_leds_syncpls (
      .t_clk(usb_clk),  //transmitting clock.
      .t_rst_n(usb_rst_n),  //reset in t_clk domain.
      .t_pulse(usb_ack_update_leds_stb),  //input pulse in t_clk domain.
      .r_clk(wb_clk),  //receiving clock.
      .r_rst_n(wb_rst_n),  //reset in r_clk_domain.
      .r_pulse(wb_ack_update_leds_stb)
  );  //output pulse in r_clk domain.

  //Synchronize usb_report pulse to WB clock domain.
  syncpls usb_report_syncpls (
      .t_clk(usb_clk),  //transmitting clock.
      .t_rst_n(usb_rst_n),  //reset in t_clk domain.
      .t_pulse(usb_report_stb),  //input pulse in t_clk domain.
      .r_clk(wb_clk),  //receiving clock.
      .r_rst_n(wb_rst_n),  //reset in r_clk_domain.
      .r_pulse(wb_report_stb)
  );  //output pulse in r_clk domain.

  usb_hid_host usb_hid_host_inst (
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
