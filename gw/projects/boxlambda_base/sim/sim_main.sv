//
//  A simulator top-level wrapping around ibex_soc, instantiating SimJTAG.
//
module sim_main #(
    parameter int unsigned OPENOCD_PORT = 9999
) (
    input  logic        clk_i,
    input  logic        rst_ni,
    // // VGA interface
    output wire  [ 3:0] vga_r,
    output wire  [ 3:0] vga_g,
    output wire  [ 3:0] vga_b,
    output wire         vga_hsync,
    output wire         vga_vsync,
    // SDSPI interface
    output wire         sdspi_cs_n,
    output wire         sdspi_sck,
    output wire         sdspi_mosi,
    input  wire         sdspi_miso,
    input  wire         sdspi_card_detect,
    // Flash SPI interface
    output wire         spiflash_cs_n,
    output wire         spiflash_mosi,
    input  wire         spiflash_miso,
    output wire         spiflash_sck,
    // I2C signals
    input  wire         i2c_scl_i,
    input  wire         i2c_sda_i,
    output wire         i2c_scl_o,
    output wire         i2c_sda_o,
    // USB HID, two ports.
    input  wire         usb0_dm_i,
    input  wire         usb0_dp_i,
    output wire         usb0_dm_o,
    output wire         usb0_dp_o,
    output wire         usb0_oe,
    input  wire         usb1_dm_i,
    input  wire         usb1_dp_i,
    output wire         usb1_dm_o,
    output wire         usb1_dp_o,
    output wire         usb1_oe,
    // Audio interface
    output wire         audio_out,
    output wire         audio_gain,
    output wire         audio_shutdown_n,
    output wire  [15:0] pcm_out,
    output wire         acc1_overflow,
    output wire         acc2_overflow,
    input  wire         uart_rx,
    output wire         uart_tx,
    input  wire  [23:0] gp_in,
    output wire  [23:0] gp_out,
    output wire  [23:0] gp_oe,
    input  wire         gp_clk
);

  // jtag openocd bridge signals
  logic        sim_jtag_tck;
  logic        sim_jtag_tms;
  logic        sim_jtag_tdi;
  logic        sim_jtag_trstn;
  logic        sim_jtag_tdo;
  logic [31:0] sim_jtag_exit;

  SimJTAG #(
      .TICK_DELAY(1),
      .PORT(OPENOCD_PORT)
  ) simJTAG_inst (
      .clock          (clk_i),
      .reset          (~rst_ni),
      .enable         (1'b1),
      .init_done      (rst_ni),
      .jtag_TCK       (sim_jtag_tck),
      .jtag_TMS       (sim_jtag_tms),
      .jtag_TDI       (sim_jtag_tdi),
      .jtag_TRSTn     (sim_jtag_trstn),
      .jtag_TDO_data  (sim_jtag_tdo),
      .jtag_TDO_driven(1'b1),
      .exit           (sim_jtag_exit)
  );

  boxlambda_top dut (
      .ext_clk_100(clk_i),
      .ext_rst_n(rst_ni),
      .tck(sim_jtag_tck),
      .trst_n(sim_jtag_trstn),
      .tms(sim_jtag_tms),
      .tdi(sim_jtag_tdi),
      .tdo(sim_jtag_tdo),
      .pll_locked_led(),
      .init_done_led(),
      .init_err_led(),
      .sd_card_detect_led(),
      // VGA interface
      .vga_r(vga_r),
      .vga_g(vga_g),
      .vga_b(vga_b),
      .vga_hsync(vga_hsync),
      .vga_vsync(vga_vsync),
      // SDSPI interface
      .sdspi_cs_n(sdspi_cs_n),
      .sdspi_sck(sdspi_sck),
      .sdspi_mosi(sdspi_mosi),
      .sdspi_miso(sdspi_miso),
      .sdspi_card_detect_n(~sdspi_card_detect),
      // Flash SPI interface
      .spiflash_cs_n(spiflash_cs_n),
      .spiflash_mosi(spiflash_mosi),
      .spiflash_miso(spiflash_miso),
      .spiflash_sck(spiflash_sck),
      // UART and GPIO
      .uart_rx(uart_rx),
      .uart_tx(uart_tx),
      .gp_in(gp_in),
      .gp_out(gp_out),
      .gp_oe(gp_oe),
      .gp_clk(gp_clk),
      // I2C signals
      .i2c_scl_i(i2c_scl_i),
      .i2c_sda_i(i2c_sda_i),
      .i2c_scl_o(i2c_scl_o),
      .i2c_sda_o(i2c_sda_o),
      // USB signals
      .usb0_dm_i(usb0_dm_i),
      .usb0_dp_i(usb0_dp_i),
      .usb0_dm_o(usb0_dm_o),
      .usb0_dp_o(usb0_dp_o),
      .usb0_oe(usb0_oe),
      .usb1_dm_i(usb1_dm_i),
      .usb1_dp_i(usb1_dp_i),
      .usb1_dm_o(usb1_dm_o),
      .usb1_dp_o(usb1_dp_o),
      .usb1_oe(usb1_oe),

      // ym2149 interface
      .audio_out(audio_out),
      .audio_gain(audio_gain),
      .audio_shutdown_n(audio_shutdown_n),
      .pcm_out(pcm_out),
      .acc1_overflow(acc1_overflow),
      .acc2_overflow(acc2_overflow)
  );

  always_comb begin : jtag_exit_handler
    if (|sim_jtag_exit) $finish(2);  // print stats too
  end
endmodule
