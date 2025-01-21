`default_nettype wire
module boxlambda_top (
	ext_clk_100,
	ext_rst_n,
	pll_locked_led,
	init_done_led,
	init_err_led,
	sd_card_detect_led,
	ddram_a,
	ddram_ba,
	ddram_ras_n,
	ddram_cas_n,
	ddram_we_n,
	ddram_cs_n,
	ddram_dm,
	ddram_dq,
	ddram_dqs_p,
	ddram_dqs_n,
	ddram_clk_p,
	ddram_clk_n,
	ddram_cke,
	ddram_odt,
	ddram_reset_n,
	vga_r,
	vga_g,
	vga_b,
	vga_hsync,
	vga_vsync,
	sdspi_cs_n,
	sdspi_sck,
	sdspi_mosi,
	sdspi_miso,
	sdspi_card_detect_n,
	spiflash_sck,
	spiflash_cs_n,
	spiflash_mosi,
	spiflash_miso,
	i2c_scl,
	i2c_sda,
	i2c_scl_pup,
	i2c_sda_pup,
	usb0_dm,
	usb0_dp,
	usb0_dm_snoop,
	usb0_dp_snoop,
	usb1_dm,
	usb1_dp,
	usb1_dm_snoop,
	usb1_dp_snoop,
	audio_out,
	audio_gain,
	audio_shutdown_n,
	uart_rx,
	uart_tx,
	gpio,
	gp_clk
);
	input wire ext_clk_100;
	input wire ext_rst_n;
	output wire pll_locked_led;
	output wire init_done_led;
	output wire init_err_led;
	output wire sd_card_detect_led;
	output wire [13:0] ddram_a;
	output wire [2:0] ddram_ba;
	output wire ddram_ras_n;
	output wire ddram_cas_n;
	output wire ddram_we_n;
	output wire ddram_cs_n;
	output wire [1:0] ddram_dm;
	inout wire [15:0] ddram_dq;
	inout wire [1:0] ddram_dqs_p;
	inout wire [1:0] ddram_dqs_n;
	output wire ddram_clk_p;
	output wire ddram_clk_n;
	output wire ddram_cke;
	output wire ddram_odt;
	output wire ddram_reset_n;
	output wire [3:0] vga_r;
	output wire [3:0] vga_g;
	output wire [3:0] vga_b;
	output wire vga_hsync;
	output wire vga_vsync;
	output wire sdspi_cs_n;
	output wire sdspi_sck;
	output wire sdspi_mosi;
	input wire sdspi_miso;
	input wire sdspi_card_detect_n;
	output wire spiflash_sck;
	output wire spiflash_cs_n;
	output wire spiflash_mosi;
	input wire spiflash_miso;
	inout wire i2c_scl;
	inout wire i2c_sda;
	output wire i2c_scl_pup;
	output wire i2c_sda_pup;
	inout wire usb0_dm;
	inout wire usb0_dp;
	output wire usb0_dm_snoop;
	output wire usb0_dp_snoop;
	inout wire usb1_dm;
	inout wire usb1_dp;
	output wire usb1_dm_snoop;
	output wire usb1_dp_snoop;
	output wire audio_out;
	output wire audio_gain;
	output wire audio_shutdown_n;
	input wire uart_rx;
	output wire uart_tx;
	inout wire [23:0] gpio;
	input wire gp_clk;
	wire usb0_dm_i;
	wire usb0_dp_i;
	wire usb0_dm_o;
	wire usb0_dp_o;
	wire usb0_oe;
	wire usb1_dm_i;
	wire usb1_dp_i;
	wire usb1_dm_o;
	wire usb1_dp_o;
	wire usb1_oe;
	wire [23:0] gp_in;
	wire [23:0] gp_out;
	wire [23:0] gp_oe;
	wire i2c_scl_i;
	wire i2c_scl_o;
	wire i2c_sda_i;
	wire i2c_sda_o;
	assign i2c_scl = (i2c_scl_o ? 1'bz : 1'b0);
	assign i2c_sda = (i2c_sda_o ? 1'bz : 1'b0);
	assign i2c_scl_i = (i2c_scl_o ? i2c_scl : 1'b0);
	assign i2c_sda_i = (i2c_sda_o ? i2c_sda : 1'b0);
	assign i2c_scl_pup = 1'b1;
	assign i2c_sda_pup = 1'b1;
	genvar _gv_ii_2;
	generate
		for (_gv_ii_2 = 0; _gv_ii_2 < 24; _gv_ii_2 = _gv_ii_2 + 1) begin : GPIO_MUX
			localparam ii = _gv_ii_2;
			assign gp_in[ii] = (gp_oe[ii] ? 1'bz : gpio[ii]);
			assign gpio[ii] = (gp_oe[ii] ? gp_out[ii] : 1'bz);
		end
	endgenerate
	assign usb0_dm_i = (usb0_oe ? 1'bz : usb0_dm);
	assign usb0_dp_i = (usb0_oe ? 1'bz : usb0_dp);
	assign usb0_dm = (usb0_oe ? usb0_dm_o : 1'bz);
	assign usb0_dp = (usb0_oe ? usb0_dp_o : 1'bz);
	assign usb0_dm_snoop = (usb0_oe ? usb0_dm_o : usb0_dm_i);
	assign usb0_dp_snoop = (usb0_oe ? usb0_dp_o : usb0_dp_i);
	assign usb1_dm_i = (usb1_oe ? 1'bz : usb1_dm);
	assign usb1_dp_i = (usb1_oe ? 1'bz : usb1_dp);
	assign usb1_dm = (usb1_oe ? usb1_dm_o : 1'bz);
	assign usb1_dp = (usb1_oe ? usb1_dp_o : 1'bz);
	assign usb1_dm_snoop = (usb1_oe ? usb1_dm_o : usb1_dm_i);
	assign usb1_dp_snoop = (usb1_oe ? usb1_dp_o : usb1_dp_i);
	boxlambda_soc #(
		.DPRAM_BYTE_ADDR_MASK(32767),
		.VRAM_SIZE_BYTES(65536),
		.CMEM_FILE("cmem.mem")
	) boxlambda_soc_inst(
		.ext_clk_100(ext_clk_100),
		.ext_rst_n(ext_rst_n),
		.pll_locked_led(pll_locked_led),
		.init_done_led(init_done_led),
		.init_err_led(init_err_led),
		.sd_card_detect_led(sd_card_detect_led),
		.ddram_a(ddram_a),
		.ddram_ba(ddram_ba),
		.ddram_ras_n(ddram_ras_n),
		.ddram_cas_n(ddram_cas_n),
		.ddram_we_n(ddram_we_n),
		.ddram_cs_n(ddram_cs_n),
		.ddram_dm(ddram_dm),
		.ddram_dq(ddram_dq),
		.ddram_dqs_p(ddram_dqs_p),
		.ddram_dqs_n(ddram_dqs_n),
		.ddram_clk_p(ddram_clk_p),
		.ddram_clk_n(ddram_clk_n),
		.ddram_cke(ddram_cke),
		.ddram_odt(ddram_odt),
		.ddram_reset_n(ddram_reset_n),
		.vga_r(vga_r),
		.vga_g(vga_g),
		.vga_b(vga_b),
		.vga_hsync(vga_hsync),
		.vga_vsync(vga_vsync),
		.sdspi_cs_n(sdspi_cs_n),
		.sdspi_sck(sdspi_sck),
		.sdspi_mosi(sdspi_mosi),
		.sdspi_miso(sdspi_miso),
		.sdspi_card_detect_n(sdspi_card_detect_n),
		.spiflash_sck(spiflash_sck),
		.spiflash_cs_n(spiflash_cs_n),
		.spiflash_mosi(spiflash_mosi),
		.spiflash_miso(spiflash_miso),
		.i2c_scl_i(i2c_scl_i),
		.i2c_sda_i(i2c_sda_i),
		.i2c_scl_o(i2c_scl_o),
		.i2c_sda_o(i2c_sda_o),
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
		.audio_out(audio_out),
		.audio_gain(audio_gain),
		.audio_shutdown_n(audio_shutdown_n),
		.uart_rx(uart_rx),
		.uart_tx(uart_tx),
		.gp_in(gp_in),
		.gp_out(gp_out),
		.gp_oe(gp_oe),
		.gp_clk(gp_clk)
	);
endmodule
