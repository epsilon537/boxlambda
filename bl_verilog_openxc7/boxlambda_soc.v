`default_nettype wire
module boxlambda_soc (
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
	i2c_scl_i,
	i2c_sda_i,
	i2c_scl_o,
	i2c_sda_o,
	usb0_dm_i,
	usb0_dp_i,
	usb0_dm_o,
	usb0_dp_o,
	usb0_oe,
	usb1_dm_i,
	usb1_dp_i,
	usb1_dm_o,
	usb1_dp_o,
	usb1_oe,
	audio_out,
	audio_gain,
	audio_shutdown_n,
	uart_rx,
	uart_tx,
	gp_in,
	gp_out,
	gp_oe,
	gp_clk
);
	parameter DPRAM_BYTE_ADDR_MASK = 'h1ffff;
	parameter VRAM_SIZE_BYTES = 131072;
	parameter DEBUG_MODULE_ACTIVE = 1;
	parameter DRAM_ACTIVE = 1;
	parameter VERA_ACTIVE = 1;
	parameter SDSPI_ACTIVE = 1;
	parameter YM2149_ACTIVE = 1;
	parameter PICORV_ACTIVE = 1;
	parameter USB_HID_ACTIVE = 1;
	parameter SPIFLASH_ACTIVE = 1;
	parameter I2C_ACTIVE = 1;
	parameter DFX_ACTIVE = 0;
	parameter VS0_ACTIVE = 1;
	parameter ACK_INVALID_ADDR = 1;
	parameter CMEM_FILE = "";
	parameter DMEM_FILE = "";
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
	input wire i2c_scl_i;
	input wire i2c_sda_i;
	output wire i2c_scl_o;
	output wire i2c_sda_o;
	input wire usb0_dm_i;
	input wire usb0_dp_i;
	output wire usb0_dm_o;
	output wire usb0_dp_o;
	output wire usb0_oe;
	input wire usb1_dm_i;
	input wire usb1_dp_i;
	output wire usb1_dm_o;
	output wire usb1_dp_o;
	output wire usb1_oe;
	output wire audio_out;
	output wire audio_gain;
	output wire audio_shutdown_n;
	input wire uart_rx;
	output wire uart_tx;
	input wire [23:0] gp_in;
	output wire [23:0] gp_out;
	output wire [23:0] gp_oe;
	input wire gp_clk;
	localparam DPRAM_AW = $clog2(DPRAM_BYTE_ADDR_MASK) - 2;
	localparam AW = 28;
	localparam DW = 32;
	localparam NUM_ARBITER_MASTERS = 2;
	localparam NUM_ARBITER_SLAVES = 1;
	localparam NUM_XBAR_MASTERS = 6;
	localparam NUM_XBAR_SLAVES = 8;
	localparam NUM_SHARED_BUS_MASTERS = 1;
	localparam NUM_SHARED_BUS_SLAVES = 15;
	localparam PICORV_BASE_ADDRESS = 'h10002000;
	function automatic [27:0] sv2v_cast_28;
		input reg [27:0] inp;
		sv2v_cast_28 = inp;
	endfunction
	localparam [419:0] SHARED_BUS_SLAVE_ADDRS = {sv2v_cast_28('h11000000 >> 2), sv2v_cast_28('h10040000 >> 2), sv2v_cast_28('h10030000 >> 2), sv2v_cast_28('h10020000 >> 2), sv2v_cast_28('h10010000 >> 2), sv2v_cast_28(PICORV_BASE_ADDRESS >> 2), sv2v_cast_28('h10001000 >> 2), sv2v_cast_28('h10000400 >> 2), sv2v_cast_28('h10000200 >> 2), sv2v_cast_28('h10000100 >> 2), sv2v_cast_28('h100000d0 >> 2), sv2v_cast_28('h100000c0 >> 2), sv2v_cast_28('h10000080 >> 2), sv2v_cast_28('h10000040 >> 2), sv2v_cast_28('h10000020 >> 2)};
	localparam [419:0] SHARED_BUS_SLAVE_ADDR_MASKS = {sv2v_cast_28(~('hffffff >> 2)), sv2v_cast_28(~('hffff >> 2)), sv2v_cast_28(~('hffff >> 2)), sv2v_cast_28(~('h3ff >> 2)), sv2v_cast_28(~('h1f >> 2)), sv2v_cast_28(~('h1fff >> 2)), sv2v_cast_28(~('h3ff >> 2)), sv2v_cast_28(~('h7f >> 2)), sv2v_cast_28(~('h1ff >> 2)), sv2v_cast_28(~('h3f >> 2)), sv2v_cast_28(~('h7 >> 2)), sv2v_cast_28(~('h7 >> 2)), sv2v_cast_28(~('h3f >> 2)), sv2v_cast_28(~('h3f >> 2)), sv2v_cast_28(~('h1f >> 2))};
	localparam [223:0] XBAR_SLAVE_ADDRS = {sv2v_cast_28('h20000000 >> 2), sv2v_cast_28('h13000000 >> 2), sv2v_cast_28('h12000000 >> 2), sv2v_cast_28('h10000000 >> 2), sv2v_cast_28('h120000 >> 2), sv2v_cast_28('h100000 >> 2), sv2v_cast_28('h20000 >> 2), sv2v_cast_28('h0 >> 2)};
	localparam [223:0] XBAR_SLAVE_ADDR_MASKS = {sv2v_cast_28(~('hfffffff >> 2)), sv2v_cast_28(~('hfffff >> 2)), sv2v_cast_28(~('h7ffff >> 2)), sv2v_cast_28(~('h1ffffff >> 2)), sv2v_cast_28(~(DPRAM_BYTE_ADDR_MASK >> 2)), sv2v_cast_28(~(DPRAM_BYTE_ADDR_MASK >> 2)), sv2v_cast_28(~(DPRAM_BYTE_ADDR_MASK >> 2)), sv2v_cast_28(~(DPRAM_BYTE_ADDR_MASK >> 2))};
	wire sys_clk;
	wire usb_clk;
	wire clk_50;
	wire clk_100;
	wire usb_pll_locked;
	wire sys_pll_locked;
	wire pre_pll_locked;
	wire litedram_pll_locked;
	wire [14:0] fast_irqs;
	wire timer_irq;
	wire ndm_reset_req;
	wire ndm_reset;
	wire dm_reset;
	wire usb_reset;
	wire vs0_reset;
	wire por_completed;
	wire debug_req;
	genvar _arr_3FC86;
	generate
		for (_arr_3FC86 = 0; _arr_3FC86 <= 1; _arr_3FC86 = _arr_3FC86 + 1) begin : arbiter_wbm
			wire rst;
			wire clk;
			wire ack;
			reg [27:0] adr;
			reg cyc;
			wire stall;
			reg stb;
			reg we;
			reg [3:0] sel;
			wire err;
			reg [31:0] dat_m;
			wire [31:0] dat_s;
		end
		for (_arr_3FC86 = 0; _arr_3FC86 <= 1; _arr_3FC86 = _arr_3FC86 + 1) begin : arbiter_wbm_port_bindings
			assign arbiter_wbm[_arr_3FC86].rst = ndm_reset;
			assign arbiter_wbm[_arr_3FC86].clk = sys_clk;
		end
	endgenerate
	genvar _arr_D0F67;
	generate
		for (_arr_D0F67 = 0; _arr_D0F67 <= 5; _arr_D0F67 = _arr_D0F67 + 1) begin : xbar_wbm
			wire rst;
			wire clk;
			wire ack;
			reg [27:0] adr;
			reg cyc;
			wire stall;
			reg stb;
			reg we;
			reg [3:0] sel;
			wire err;
			reg [31:0] dat_m;
			wire [31:0] dat_s;
		end
		for (_arr_D0F67 = 0; _arr_D0F67 <= 5; _arr_D0F67 = _arr_D0F67 + 1) begin : xbar_wbm_port_bindings
			assign xbar_wbm[_arr_D0F67].rst = ndm_reset;
			assign xbar_wbm[_arr_D0F67].clk = sys_clk;
		end
	endgenerate
	genvar _arr_551F9;
	generate
		for (_arr_551F9 = 0; _arr_551F9 <= 7; _arr_551F9 = _arr_551F9 + 1) begin : xbar_wbs
			wire rst;
			wire clk;
			wire ack;
			wire [27:0] adr;
			wire cyc;
			wire stall;
			wire stb;
			wire we;
			wire [3:0] sel;
			wire err;
			wire [31:0] dat_m;
			wire [31:0] dat_s;
		end
		for (_arr_551F9 = 0; _arr_551F9 <= 7; _arr_551F9 = _arr_551F9 + 1) begin : xbar_wbs_port_bindings
			assign xbar_wbs[_arr_551F9].rst = dm_reset;
			assign xbar_wbs[_arr_551F9].clk = sys_clk;
		end
	endgenerate
	genvar _arr_E16FC;
	generate
		for (_arr_E16FC = 0; _arr_E16FC <= 14; _arr_E16FC = _arr_E16FC + 1) begin : shared_bus_wbs
			wire rst;
			wire clk;
			reg ack;
			wire [27:0] adr;
			wire cyc;
			wire stall;
			wire stb;
			wire we;
			wire [3:0] sel;
			wire err;
			wire [31:0] dat_m;
			wire [31:0] dat_s;
		end
		for (_arr_E16FC = 0; _arr_E16FC <= 14; _arr_E16FC = _arr_E16FC + 1) begin : shared_bus_wbs_port_bindings
			assign shared_bus_wbs[_arr_E16FC].rst = dm_reset;
			assign shared_bus_wbs[_arr_E16FC].clk = sys_clk;
		end
	endgenerate
	wire [5:0] xbar_mcyc;
	wire [5:0] xbar_mstb;
	wire [5:0] xbar_mwe;
	wire [167:0] xbar_maddr;
	wire [191:0] xbar_mdata_w;
	wire [23:0] xbar_msel;
	wire [5:0] xbar_mstall;
	wire [5:0] xbar_mack;
	wire [191:0] xbar_mdata_r;
	wire [5:0] xbar_merr;
	wire [7:0] xbar_scyc;
	wire [7:0] xbar_sstb;
	wire [7:0] xbar_swe;
	wire [223:0] xbar_saddr;
	wire [255:0] xbar_sdata_w;
	wire [31:0] xbar_ssel;
	wire [7:0] xbar_sstall;
	wire [7:0] xbar_sack;
	wire [255:0] xbar_sdata_r;
	wire [7:0] xbar_serr;
	wire [0:0] shared_bus_mcyc;
	wire [0:0] shared_bus_mstb;
	wire [0:0] shared_bus_mwe;
	wire [27:0] shared_bus_maddr;
	wire [31:0] shared_bus_mdata_w;
	wire [3:0] shared_bus_msel;
	wire [0:0] shared_bus_mstall;
	wire [0:0] shared_bus_mack;
	wire [31:0] shared_bus_mdata_r;
	wire [0:0] shared_bus_merr;
	wire [14:0] shared_bus_scyc;
	wire [14:0] shared_bus_sstb;
	wire [14:0] shared_bus_swe;
	wire [419:0] shared_bus_saddr;
	wire [479:0] shared_bus_sdata_w;
	wire [59:0] shared_bus_ssel;
	wire [14:0] shared_bus_sstall;
	wire [14:0] shared_bus_sack;
	wire [479:0] shared_bus_sdata_r;
	wire [14:0] shared_bus_serr;
	genvar _gv_ii_1;
	generate
		for (_gv_ii_1 = 0; _gv_ii_1 < NUM_SHARED_BUS_SLAVES; _gv_ii_1 = _gv_ii_1 + 1) begin : CONNECT_SLAVES_TO_SHARED_BUS
			localparam ii = _gv_ii_1;
			assign shared_bus_wbs[ii].cyc = shared_bus_scyc[ii];
			assign shared_bus_wbs[ii].stb = shared_bus_sstb[ii];
			assign shared_bus_wbs[ii].we = shared_bus_swe[ii];
			assign shared_bus_wbs[ii].adr = shared_bus_saddr[((ii + 1) * AW) - 1:ii * AW];
			assign shared_bus_wbs[ii].dat_m = shared_bus_sdata_w[((ii + 1) * DW) - 1:ii * DW];
			assign shared_bus_wbs[ii].sel = shared_bus_ssel[(((ii + 1) * DW) / 8) - 1:(ii * DW) / 8];
			assign shared_bus_sstall[ii] = shared_bus_wbs[ii].stall;
			assign shared_bus_sack[ii] = shared_bus_wbs[ii].ack;
			assign shared_bus_sdata_r[((ii + 1) * DW) - 1:ii * DW] = shared_bus_wbs[ii].dat_s;
			assign shared_bus_serr[ii] = shared_bus_wbs[ii].err;
		end
		for (_gv_ii_1 = 0; _gv_ii_1 < NUM_XBAR_SLAVES; _gv_ii_1 = _gv_ii_1 + 1) begin : CONNECT_SLAVES_TO_XBAR
			localparam ii = _gv_ii_1;
			assign xbar_wbs[ii].cyc = xbar_scyc[ii];
			assign xbar_wbs[ii].stb = xbar_sstb[ii];
			assign xbar_wbs[ii].we = xbar_swe[ii];
			assign xbar_wbs[ii].adr = xbar_saddr[((ii + 1) * AW) - 1:ii * AW];
			assign xbar_wbs[ii].dat_m = xbar_sdata_w[((ii + 1) * DW) - 1:ii * DW];
			assign xbar_wbs[ii].sel = xbar_ssel[(((ii + 1) * DW) / 8) - 1:(ii * DW) / 8];
			assign xbar_sstall[ii] = xbar_wbs[ii].stall;
			assign xbar_sack[ii] = xbar_wbs[ii].ack;
			assign xbar_sdata_r[((ii + 1) * DW) - 1:ii * DW] = xbar_wbs[ii].dat_s;
			assign xbar_serr[ii] = xbar_wbs[ii].err;
		end
		for (_gv_ii_1 = 0; _gv_ii_1 < NUM_XBAR_MASTERS; _gv_ii_1 = _gv_ii_1 + 1) begin : CONNECT_MASTERS_TO_XBAR
			localparam ii = _gv_ii_1;
			assign xbar_mcyc[ii] = xbar_wbm[ii].cyc;
			assign xbar_mstb[ii] = xbar_wbm[ii].stb;
			assign xbar_mwe[ii] = xbar_wbm[ii].we;
			assign xbar_maddr[((ii + 1) * AW) - 1:ii * AW] = xbar_wbm[ii].adr;
			assign xbar_mdata_w[((ii + 1) * DW) - 1:ii * DW] = xbar_wbm[ii].dat_m;
			assign xbar_msel[(((ii + 1) * DW) / 8) - 1:(ii * DW) / 8] = xbar_wbm[ii].sel;
			assign xbar_wbm[ii].stall = xbar_mstall[ii];
			assign xbar_wbm[ii].ack = xbar_mack[ii];
			assign xbar_wbm[ii].dat_s = xbar_mdata_r[((ii + 1) * DW) - 1:ii * DW];
			assign xbar_wbm[ii].err = xbar_merr[ii];
		end
	endgenerate
	wire [1:1] sv2v_tmp_wb_arbiter_o_cyc;
	always @(*) xbar_wbm[32'd5].cyc = sv2v_tmp_wb_arbiter_o_cyc;
	wire [1:1] sv2v_tmp_wb_arbiter_o_stb;
	always @(*) xbar_wbm[32'd5].stb = sv2v_tmp_wb_arbiter_o_stb;
	wire [1:1] sv2v_tmp_wb_arbiter_o_we;
	always @(*) xbar_wbm[32'd5].we = sv2v_tmp_wb_arbiter_o_we;
	wire [28:1] sv2v_tmp_wb_arbiter_o_adr;
	always @(*) xbar_wbm[32'd5].adr = sv2v_tmp_wb_arbiter_o_adr;
	wire [32:1] sv2v_tmp_wb_arbiter_o_dat;
	always @(*) xbar_wbm[32'd5].dat_m = sv2v_tmp_wb_arbiter_o_dat;
	wire [4:1] sv2v_tmp_wb_arbiter_o_sel;
	always @(*) xbar_wbm[32'd5].sel = sv2v_tmp_wb_arbiter_o_sel;
	wbarbiter #(.AW(AW)) wb_arbiter(
		.i_clk(sys_clk),
		.i_reset(ndm_reset),
		.i_a_cyc(arbiter_wbm[32'd0].cyc),
		.i_a_stb(arbiter_wbm[32'd0].stb),
		.i_a_we(arbiter_wbm[32'd0].we),
		.i_a_adr(arbiter_wbm[32'd0].adr),
		.i_a_dat(arbiter_wbm[32'd0].dat_m),
		.i_a_sel(arbiter_wbm[32'd0].sel),
		.o_a_ack(arbiter_wbm[32'd0].ack),
		.o_a_stall(arbiter_wbm[32'd0].stall),
		.o_a_err(arbiter_wbm[32'd0].err),
		.i_b_cyc(arbiter_wbm[32'd1].cyc),
		.i_b_stb(arbiter_wbm[32'd1].stb),
		.i_b_we(arbiter_wbm[32'd1].we),
		.i_b_adr(arbiter_wbm[32'd1].adr),
		.i_b_dat(arbiter_wbm[32'd1].dat_m),
		.i_b_sel(arbiter_wbm[32'd1].sel),
		.o_b_ack(arbiter_wbm[32'd1].ack),
		.o_b_stall(arbiter_wbm[32'd1].stall),
		.o_b_err(arbiter_wbm[32'd1].err),
		.o_cyc(sv2v_tmp_wb_arbiter_o_cyc),
		.o_stb(sv2v_tmp_wb_arbiter_o_stb),
		.o_we(sv2v_tmp_wb_arbiter_o_we),
		.o_adr(sv2v_tmp_wb_arbiter_o_adr),
		.o_dat(sv2v_tmp_wb_arbiter_o_dat),
		.o_sel(sv2v_tmp_wb_arbiter_o_sel),
		.i_ack(xbar_wbm[32'd5].ack),
		.i_stall(xbar_wbm[32'd5].stall),
		.i_err(xbar_wbm[32'd5].err)
	);
	assign arbiter_wbm[32'd0].dat_s = xbar_wbm[32'd5].dat_s;
	assign arbiter_wbm[32'd1].dat_s = xbar_wbm[32'd5].dat_s;
	wbxbar #(
		.NM(NUM_XBAR_MASTERS),
		.NS(NUM_XBAR_SLAVES),
		.AW(AW),
		.DW(32),
		.SLAVE_ADDR(XBAR_SLAVE_ADDRS),
		.SLAVE_MASK(XBAR_SLAVE_ADDR_MASKS),
		.LGMAXBURST(3),
		.OPT_TIMEOUT((ACK_INVALID_ADDR ? 511 : 0)),
		.OPT_DBLBUFFER(1'b0),
		.OPT_LOWPOWER(1'b0),
		.OPT_ACK_INVALID_ADDR(ACK_INVALID_ADDR)
	) wb_xbar(
		.i_clk(sys_clk),
		.i_reset(ndm_reset),
		.i_mcyc(xbar_mcyc),
		.i_mstb(xbar_mstb),
		.i_mwe(xbar_mwe),
		.i_maddr(xbar_maddr),
		.i_mdata(xbar_mdata_w),
		.i_msel(xbar_msel),
		.o_mstall(xbar_mstall),
		.o_mack(xbar_mack),
		.o_mdata(xbar_mdata_r),
		.o_merr(xbar_merr),
		.o_scyc(xbar_scyc),
		.o_sstb(xbar_sstb),
		.o_swe(xbar_swe),
		.o_saddr(xbar_saddr),
		.o_sdata(xbar_sdata_w),
		.o_ssel(xbar_ssel),
		.i_sstall(xbar_sstall),
		.i_sack(xbar_sack),
		.i_sdata(xbar_sdata_r),
		.i_serr(xbar_serr)
	);
	assign shared_bus_mcyc = xbar_scyc[32'd4];
	assign shared_bus_mstb = xbar_sstb[32'd4];
	assign shared_bus_mwe = xbar_swe[32'd4];
	assign shared_bus_maddr = xbar_saddr[139:112];
	assign shared_bus_mdata_w = xbar_sdata_w[159:128];
	assign shared_bus_msel = xbar_ssel[19:16];
	assign xbar_sstall[32'd4] = shared_bus_mstall;
	assign xbar_sack[32'd4] = shared_bus_mack;
	assign xbar_sdata_r[159:128] = shared_bus_mdata_r;
	assign xbar_serr[32'd4] = shared_bus_merr;
	wbxbar #(
		.NM(NUM_SHARED_BUS_MASTERS),
		.NS(NUM_SHARED_BUS_SLAVES),
		.AW(AW),
		.DW(32),
		.SLAVE_ADDR(SHARED_BUS_SLAVE_ADDRS),
		.SLAVE_MASK(SHARED_BUS_SLAVE_ADDR_MASKS),
		.LGMAXBURST(3),
		.OPT_TIMEOUT((ACK_INVALID_ADDR ? 511 : 0)),
		.OPT_DBLBUFFER(1'b0),
		.OPT_LOWPOWER(1'b0),
		.OPT_ACK_INVALID_ADDR(ACK_INVALID_ADDR)
	) wb_shared_bus(
		.i_clk(sys_clk),
		.i_reset(ndm_reset),
		.i_mcyc(shared_bus_mcyc),
		.i_mstb(shared_bus_mstb),
		.i_mwe(shared_bus_mwe),
		.i_maddr(shared_bus_maddr),
		.i_mdata(shared_bus_mdata_w),
		.i_msel(shared_bus_msel),
		.o_mstall(shared_bus_mstall),
		.o_mack(shared_bus_mack),
		.o_mdata(shared_bus_mdata_r),
		.o_merr(shared_bus_merr),
		.o_scyc(shared_bus_scyc),
		.o_sstb(shared_bus_sstb),
		.o_swe(shared_bus_swe),
		.o_saddr(shared_bus_saddr),
		.o_sdata(shared_bus_sdata_w),
		.o_ssel(shared_bus_ssel),
		.i_sstall(shared_bus_sstall),
		.i_sack(shared_bus_sack),
		.i_sdata(shared_bus_sdata_r),
		.i_serr(shared_bus_serr)
	);
	wire [1:1] sv2v_tmp_reset_ctrl_inst_wb_ack;
	always @(*) shared_bus_wbs[32'd4].ack = sv2v_tmp_reset_ctrl_inst_wb_ack;
	reset_ctrl reset_ctrl_inst(
		.sys_clk(sys_clk),
		.usb_clk(usb_clk),
		.sys_pll_locked_i(sys_pll_locked),
		.usb_pll_locked_i(usb_pll_locked),
		.ndm_reset_i(ndm_reset_req),
		.ext_reset_i(~ext_rst_n),
		.ndm_reset_o(ndm_reset),
		.dm_reset_o(dm_reset),
		.usb_reset_o(usb_reset),
		.por_completed_o(por_completed),
		.wb_adr(shared_bus_wbs[32'd4].adr[0]),
		.wb_dat_w(shared_bus_wbs[32'd4].dat_m),
		.wb_dat_r(shared_bus_wbs[32'd4].dat_s),
		.wb_sel(shared_bus_wbs[32'd4].sel),
		.wb_stall(shared_bus_wbs[32'd4].stall),
		.wb_cyc(shared_bus_wbs[32'd4].cyc),
		.wb_stb(shared_bus_wbs[32'd4].stb),
		.wb_ack(sv2v_tmp_reset_ctrl_inst_wb_ack),
		.wb_we(shared_bus_wbs[32'd4].we),
		.wb_err(shared_bus_wbs[32'd4].err)
	);
	boxlambda_clk_gen clkgen(
		.ext_clk_100(ext_clk_100),
		.rst_n(1'b1),
		.clk_50(clk_50),
		.clk_100(clk_100),
		.clk_12(usb_clk),
		.locked(pre_pll_locked)
	);
	assign usb_pll_locked = pre_pll_locked;
	generate
		if (DFX_ACTIVE) begin : GENERATE_DFX_CONTROLLER
			wb_dfx_controller dfx_controller_inst(
				.clk(sys_clk),
				.rst(ndm_reset),
				.wbm_adr_o(arbiter_wbm[32'd1].adr),
				.wbm_dat_o(arbiter_wbm[32'd1].dat_m),
				.wbm_dat_i(arbiter_wbm[32'd1].dat_s),
				.wbm_we_o(arbiter_wbm[32'd1].we),
				.wbm_sel_o(arbiter_wbm[32'd1].sel),
				.wbm_stb_o(arbiter_wbm[32'd1].stb),
				.wbm_ack_i(arbiter_wbm[32'd1].ack),
				.wbm_stall_i(arbiter_wbm[32'd1].stall),
				.wbm_cyc_o(arbiter_wbm[32'd1].cyc),
				.wbm_err_i(arbiter_wbm[32'd1].err),
				.wbs_adr(shared_bus_wbs[32'd7].adr[4:0]),
				.wbs_dat_w(shared_bus_wbs[32'd7].dat_m),
				.wbs_dat_r(shared_bus_wbs[32'd7].dat_s),
				.wbs_sel(shared_bus_wbs[32'd7].sel),
				.wbs_stall(shared_bus_wbs[32'd7].stall),
				.wbs_cyc(shared_bus_wbs[32'd7].cyc),
				.wbs_stb(shared_bus_wbs[32'd7].stb),
				.wbs_ack(shared_bus_wbs[32'd7].ack),
				.wbs_we(shared_bus_wbs[32'd7].we),
				.wbs_err(shared_bus_wbs[32'd7].err),
				.vsm_VS_0_rm_reset(vs0_reset),
				.vsm_VS_0_event_error(fast_irqs[32'd1])
			);
		end
		else begin : genblk4
			wire [28:1] sv2v_tmp_64532;
			assign sv2v_tmp_64532 = 0;
			always @(*) arbiter_wbm[32'd1].adr = sv2v_tmp_64532;
			wire [32:1] sv2v_tmp_AECE0;
			assign sv2v_tmp_AECE0 = 0;
			always @(*) arbiter_wbm[32'd1].dat_m = sv2v_tmp_AECE0;
			wire [4:1] sv2v_tmp_6D64F;
			assign sv2v_tmp_6D64F = 0;
			always @(*) arbiter_wbm[32'd1].sel = sv2v_tmp_6D64F;
			wire [1:1] sv2v_tmp_62F4C;
			assign sv2v_tmp_62F4C = 0;
			always @(*) arbiter_wbm[32'd1].cyc = sv2v_tmp_62F4C;
			wire [1:1] sv2v_tmp_68CA0;
			assign sv2v_tmp_68CA0 = 0;
			always @(*) arbiter_wbm[32'd1].stb = sv2v_tmp_68CA0;
			wire [1:1] sv2v_tmp_DF1A4;
			assign sv2v_tmp_DF1A4 = 0;
			always @(*) arbiter_wbm[32'd1].we = sv2v_tmp_DF1A4;
			assign fast_irqs[32'd1] = 1'b0;
			assign vs0_reset = ndm_reset;
		end
	endgenerate
	localparam [11:0] dm_DataAddr = 12'h380;
	localparam [3:0] dm_DataCount = 4'h2;
	generate
		if (DEBUG_MODULE_ACTIVE) begin : GENERATE_DEBUG_MODULE
			wire dmactive;
			reg unavailable = 1'b0;
			reg [31:0] hartinfo = {16'h0021, dm_DataCount, dm_DataAddr};
			wire dmi_rst_n;
			wire dmi_req_valid;
			wire dmi_req_ready;
			wire [40:0] dmi_req;
			wire dmi_resp_valid;
			wire dmi_resp_ready;
			wire [33:0] dmi_resp;
			localparam _bbase_E0639_wbm = 32'd0;
			localparam _bbase_E0639_wbs = 32'd13;
			if (1) begin : wb_dm
				localparam signed [31:0] NrHarts = 1;
				localparam signed [31:0] BusWidth = 32;
				localparam [0:0] SelectableHarts = 1;
				wire clk;
				wire rst_n;
				wire testmode;
				wire ndmreset;
				wire dmactive;
				wire [0:0] debug_req;
				wire [0:0] unavailable;
				wire [31:0] hartinfo;
				localparam _mbase_wbs = _bbase_E0639_wbs;
				localparam _mbase_wbm = _bbase_E0639_wbm;
				wire dmi_rst_n;
				wire dmi_req_valid;
				wire dmi_req_ready;
				wire [40:0] dmi_req;
				wire dmi_resp_valid;
				wire dmi_resp_ready;
				wire [33:0] dmi_resp;
				if (1) begin : slave_core
					wire clk;
					wire rst_n;
					wire req;
					wire gnt;
					wire rvalid;
					wire we;
					wire [3:0] be;
					wire [31:0] addr;
					wire [31:0] wdata;
					wire [31:0] rdata;
					wire err;
				end
				assign slave_core.clk = clk;
				assign slave_core.rst_n = rst_n;
				if (1) begin : master_core
					wire clk;
					wire rst_n;
					wire req;
					wire gnt;
					reg rvalid;
					wire we;
					wire [3:0] be;
					wire [31:0] addr;
					wire [31:0] wdata;
					wire [31:0] rdata;
					reg err;
				end
				assign master_core.clk = clk;
				assign master_core.rst_n = rst_n;
				dm_top #(
					.NrHarts(NrHarts),
					.BusWidth(BusWidth),
					.SelectableHarts(SelectableHarts)
				) inst_dm_top(
					.clk_i(clk),
					.rst_ni(rst_n),
					.testmode_i(testmode),
					.ndmreset_o(ndmreset),
					.dmactive_o(dmactive),
					.debug_req_o(debug_req),
					.unavailable_i(unavailable),
					.hartinfo_i(hartinfo),
					.slave_req_i(slave_core.req),
					.slave_we_i(slave_core.we),
					.slave_addr_i(slave_core.addr),
					.slave_be_i(slave_core.be),
					.slave_wdata_i(slave_core.wdata),
					.slave_rdata_o(slave_core.rdata),
					.master_req_o(master_core.req),
					.master_add_o(master_core.addr),
					.master_we_o(master_core.we),
					.master_wdata_o(master_core.wdata),
					.master_be_o(master_core.be),
					.master_gnt_i(master_core.gnt),
					.master_r_valid_i(master_core.rvalid),
					.master_r_err_i(master_core.err),
					.master_r_rdata_i(master_core.rdata),
					.master_r_other_err_i(1'b0),
					.dmi_rst_ni(dmi_rst_n),
					.dmi_req_valid_i(dmi_req_valid),
					.dmi_req_ready_o(dmi_req_ready),
					.dmi_req_i(dmi_req),
					.dmi_resp_valid_o(dmi_resp_valid),
					.dmi_resp_ready_i(dmi_resp_ready),
					.dmi_resp_o(dmi_resp)
				);
				assign slave_core.gnt = 1'b1;
				assign slave_core.rvalid = 1'b0;
				assign slave_core.err = 1'b0;
				localparam _bbase_2C510_wb = 32'd13;
				if (1) begin : slave_core2wb
					localparam _mbase_wb = _bbase_2C510_wb;
					wire valid;
					assign valid = boxlambda_soc.shared_bus_wbs[_mbase_wb].cyc & boxlambda_soc.shared_bus_wbs[_mbase_wb].stb;
					assign boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.req = valid;
					assign boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.we = boxlambda_soc.shared_bus_wbs[_mbase_wb].we;
					assign boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.addr = {2'b00, boxlambda_soc.shared_bus_wbs[_mbase_wb].adr, 2'b00};
					assign boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.be = boxlambda_soc.shared_bus_wbs[_mbase_wb].sel;
					assign boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.wdata = boxlambda_soc.shared_bus_wbs[_mbase_wb].dat_m;
					assign boxlambda_soc.shared_bus_wbs[_mbase_wb].dat_s = boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.rdata;
					assign boxlambda_soc.shared_bus_wbs[_mbase_wb].stall = ~boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.gnt;
					assign boxlambda_soc.shared_bus_wbs[_mbase_wb].err = boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.slave_core.err;
					always @(posedge boxlambda_soc.shared_bus_wbs[_mbase_wb].clk)
						if (boxlambda_soc.shared_bus_wbs[_mbase_wb].rst)
							boxlambda_soc.shared_bus_wbs[_mbase_wb].ack <= 1'b0;
						else
							boxlambda_soc.shared_bus_wbs[_mbase_wb].ack <= valid & ~boxlambda_soc.shared_bus_wbs[_mbase_wb].stall;
				end
				localparam _bbase_62907_wb = 32'd0;
				if (1) begin : master_core2wb
					reg _sv2v_0;
					localparam _mbase_wb = _bbase_62907_wb;
					reg transaction_ongoing_reg;
					reg wbm_stb_reg;
					reg wbm_we_reg;
					reg [31:0] dat_m_reg;
					reg [27:0] adr_reg;
					reg [3:0] sel_reg;
					initial begin
						wbm_stb_reg = 1'b0;
						wbm_we_reg = 1'b0;
						dat_m_reg = 0;
						adr_reg = 0;
						sel_reg = 0;
						transaction_ongoing_reg = 1'b0;
					end
					always @(*) begin
						if (_sv2v_0)
							;
						if (!transaction_ongoing_reg) begin
							boxlambda_soc.arbiter_wbm[_mbase_wb].sel = (boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.we ? boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.be : {4 {1'sb1}});
							boxlambda_soc.arbiter_wbm[_mbase_wb].adr = boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.addr[29:2];
							boxlambda_soc.arbiter_wbm[_mbase_wb].dat_m = boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.wdata;
							boxlambda_soc.arbiter_wbm[_mbase_wb].we = boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.we;
							boxlambda_soc.arbiter_wbm[_mbase_wb].stb = boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.req;
							boxlambda_soc.arbiter_wbm[_mbase_wb].cyc = boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.req;
							boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.rvalid = 1'b0;
							boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.err = 1'b0;
						end
						else begin
							boxlambda_soc.arbiter_wbm[_mbase_wb].sel = sel_reg;
							boxlambda_soc.arbiter_wbm[_mbase_wb].adr = adr_reg;
							boxlambda_soc.arbiter_wbm[_mbase_wb].dat_m = dat_m_reg;
							boxlambda_soc.arbiter_wbm[_mbase_wb].we = wbm_we_reg;
							boxlambda_soc.arbiter_wbm[_mbase_wb].stb = wbm_stb_reg;
							boxlambda_soc.arbiter_wbm[_mbase_wb].cyc = 1'b1;
							boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.rvalid = boxlambda_soc.arbiter_wbm[_mbase_wb].ack;
							boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.err = boxlambda_soc.arbiter_wbm[_mbase_wb].err;
						end
					end
					always @(posedge boxlambda_soc.arbiter_wbm[_mbase_wb].clk)
						if (boxlambda_soc.arbiter_wbm[_mbase_wb].rst) begin
							sel_reg <= 0;
							adr_reg <= 0;
							dat_m_reg <= 0;
							wbm_we_reg <= 1'b0;
							wbm_stb_reg <= 1'b0;
							transaction_ongoing_reg <= 1'b0;
						end
						else if (!transaction_ongoing_reg) begin
							if (boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.req) begin
								transaction_ongoing_reg <= 1'b1;
								wbm_stb_reg <= boxlambda_soc.arbiter_wbm[_mbase_wb].stall;
								wbm_we_reg <= boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.we;
								dat_m_reg <= boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.wdata;
								adr_reg <= boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.addr[29:2];
								sel_reg <= (boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.we ? boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.be : {4 {1'sb1}});
							end
						end
						else begin
							if (!boxlambda_soc.arbiter_wbm[_mbase_wb].stall)
								wbm_stb_reg <= 1'b0;
							if (boxlambda_soc.arbiter_wbm[_mbase_wb].ack || boxlambda_soc.arbiter_wbm[_mbase_wb].err)
								transaction_ongoing_reg <= 1'b0;
						end
					assign boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.gnt = ~transaction_ongoing_reg;
					assign boxlambda_soc.GENERATE_DEBUG_MODULE.wb_dm.master_core.rdata = boxlambda_soc.arbiter_wbm[_mbase_wb].dat_s;
					initial _sv2v_0 = 0;
				end
			end
			assign wb_dm.clk = sys_clk;
			assign wb_dm.rst_n = ~dm_reset;
			assign wb_dm.testmode = 1'b0;
			assign ndm_reset_req = wb_dm.ndmreset;
			assign dmactive = wb_dm.dmactive;
			assign debug_req = wb_dm.debug_req;
			assign wb_dm.dmi_rst_n = dmi_rst_n;
			assign wb_dm.unavailable = unavailable;
			assign wb_dm.hartinfo = hartinfo;
			assign wb_dm.dmi_req_valid = dmi_req_valid;
			assign dmi_req_ready = wb_dm.dmi_req_ready;
			assign wb_dm.dmi_req = dmi_req;
			assign dmi_resp_valid = wb_dm.dmi_resp_valid;
			assign wb_dm.dmi_resp_ready = dmi_resp_ready;
			assign dmi_resp = wb_dm.dmi_resp;
			dmi_jtag #(.IdcodeValue(32'h249511c3)) dmi_jtag_inst(
				.clk_i(sys_clk),
				.rst_ni(~dm_reset),
				.testmode_i(1'b0),
				.dmi_rst_no(dmi_rst_n),
				.dmi_req_o(dmi_req),
				.dmi_req_valid_o(dmi_req_valid),
				.dmi_req_ready_i(dmi_req_ready),
				.dmi_resp_i(dmi_resp),
				.dmi_resp_ready_o(dmi_resp_ready),
				.dmi_resp_valid_i(dmi_resp_valid),
				.tck_i(1'b0),
				.tms_i(1'b0),
				.trst_ni(1'b1),
				.td_i(1'b0),
				.td_o(),
				.tdo_oe_o()
			);
			reg unused = &{1'b0, dmactive, 1'b0};
		end
		else begin : genblk5
			assign ndm_reset_req = 1'b0;
			assign debug_req = 1'b0;
		end
	endgenerate
	localparam _bbase_B3747_instr_wb = 32'd0;
	localparam _bbase_B3747_data_wb = 32'd1;
	localparam _param_B3747_RV32M = 32'sd1;
	localparam _param_B3747_RV32B = 32'sd0;
	localparam _param_B3747_RegFile = 32'sd1;
	localparam _param_B3747_BranchTargetALU = 1'b0;
	localparam _param_B3747_WritebackStage = 1'b0;
	localparam _param_B3747_DbgTriggerEn = 1'b1;
	localparam _param_B3747_DmHaltAddr = {2'b00, SHARED_BUS_SLAVE_ADDRS[391:364], 2'b00} + 32'h00000800;
	localparam _param_B3747_DmExceptionAddr = {2'b00, SHARED_BUS_SLAVE_ADDRS[391:364], 2'b00} + 32'h00000808;
	generate
		if (1) begin : wb_ibex_core
			localparam [0:0] PMPEnable = 1'b0;
			localparam [31:0] PMPGranularity = 0;
			localparam [31:0] PMPNumRegions = 4;
			localparam [31:0] MHPMCounterNum = 0;
			localparam [31:0] MHPMCounterWidth = 40;
			localparam [0:0] RV32E = 1'b0;
			localparam integer RV32M = _param_B3747_RV32M;
			localparam integer RV32B = _param_B3747_RV32B;
			localparam integer RegFile = _param_B3747_RegFile;
			localparam [0:0] BranchTargetALU = _param_B3747_BranchTargetALU;
			localparam [0:0] WritebackStage = _param_B3747_WritebackStage;
			localparam [0:0] DbgTriggerEn = _param_B3747_DbgTriggerEn;
			localparam [31:0] DmHaltAddr = _param_B3747_DmHaltAddr;
			localparam [31:0] DmExceptionAddr = _param_B3747_DmExceptionAddr;
			wire clk;
			wire rst_n;
			localparam _mbase_instr_wb = _bbase_B3747_instr_wb;
			localparam _mbase_data_wb = _bbase_B3747_data_wb;
			wire test_en;
			wire [31:0] hart_id;
			wire [31:0] boot_addr;
			wire irq_software;
			wire irq_timer;
			wire irq_external;
			wire [14:0] irq_fast;
			wire irq_nm;
			wire debug_req;
			localparam signed [31:0] ibex_pkg_IbexMuBiWidth = 4;
			wire [3:0] fetch_enable;
			wire core_sleep;
			if (1) begin : instr_core
				wire clk;
				wire rst_n;
				wire req;
				wire gnt;
				reg rvalid;
				wire we;
				wire [3:0] be;
				wire [31:0] addr;
				wire [31:0] wdata;
				wire [31:0] rdata;
				reg err;
			end
			assign instr_core.clk = clk;
			assign instr_core.rst_n = rst_n;
			if (1) begin : data_core
				wire clk;
				wire rst_n;
				wire req;
				wire gnt;
				reg rvalid;
				wire we;
				wire [3:0] be;
				wire [31:0] addr;
				wire [31:0] wdata;
				wire [31:0] rdata;
				reg err;
			end
			assign data_core.clk = clk;
			assign data_core.rst_n = rst_n;
			localparam [6:0] sv2v_uu_u_top_ext_instr_rdata_intg_i_0 = 1'sb0;
			localparam [6:0] sv2v_uu_u_top_ext_data_rdata_intg_i_0 = 1'sb0;
			ibex_top #(
				.PMPEnable(PMPEnable),
				.PMPGranularity(PMPGranularity),
				.PMPNumRegions(PMPNumRegions),
				.MHPMCounterNum(MHPMCounterNum),
				.MHPMCounterWidth(MHPMCounterWidth),
				.RV32E(RV32E),
				.RV32M(RV32M),
				.RV32B(RV32B),
				.RegFile(RegFile),
				.BranchTargetALU(BranchTargetALU),
				.WritebackStage(WritebackStage),
				.DbgTriggerEn(DbgTriggerEn),
				.DmHaltAddr(DmHaltAddr),
				.DmExceptionAddr(DmExceptionAddr)
			) u_top(
				.clk_i(clk),
				.rst_ni(rst_n),
				.test_en_i(test_en),
				.scan_rst_ni(1'b1),
				.ram_cfg_i(10'b0000000000),
				.hart_id_i(hart_id),
				.boot_addr_i(boot_addr),
				.instr_req_o(instr_core.req),
				.instr_gnt_i(instr_core.gnt),
				.instr_rvalid_i(instr_core.rvalid),
				.instr_addr_o(instr_core.addr),
				.instr_rdata_i(instr_core.rdata),
				.instr_rdata_intg_i(sv2v_uu_u_top_ext_instr_rdata_intg_i_0),
				.instr_err_i(instr_core.err),
				.data_req_o(data_core.req),
				.data_gnt_i(data_core.gnt),
				.data_rvalid_i(data_core.rvalid),
				.data_we_o(data_core.we),
				.data_be_o(data_core.be),
				.data_addr_o(data_core.addr),
				.data_wdata_o(data_core.wdata),
				.data_wdata_intg_o(),
				.data_rdata_i(data_core.rdata),
				.data_rdata_intg_i(sv2v_uu_u_top_ext_data_rdata_intg_i_0),
				.data_err_i(data_core.err),
				.irq_software_i(irq_software),
				.irq_timer_i(irq_timer),
				.irq_external_i(irq_external),
				.irq_fast_i(irq_fast),
				.irq_nm_i(irq_nm),
				.debug_req_i(debug_req),
				.crash_dump_o(),
				.fetch_enable_i(fetch_enable),
				.alert_minor_o(),
				.alert_major_internal_o(),
				.alert_major_bus_o(),
				.core_sleep_o(core_sleep)
			);
			assign instr_core.we = 1'b0;
			assign instr_core.be = 1'sb0;
			assign instr_core.wdata = 1'sb0;
			localparam _bbase_D0E8E_wb = 32'd0;
			if (1) begin : instr_core2wb
				reg _sv2v_0;
				localparam _mbase_wb = _bbase_D0E8E_wb;
				reg transaction_ongoing_reg;
				reg wbm_stb_reg;
				reg wbm_we_reg;
				reg [31:0] dat_m_reg;
				reg [27:0] adr_reg;
				reg [3:0] sel_reg;
				initial begin
					wbm_stb_reg = 1'b0;
					wbm_we_reg = 1'b0;
					dat_m_reg = 0;
					adr_reg = 0;
					sel_reg = 0;
					transaction_ongoing_reg = 1'b0;
				end
				always @(*) begin
					if (_sv2v_0)
						;
					if (!transaction_ongoing_reg) begin
						boxlambda_soc.xbar_wbm[_mbase_wb].sel = (boxlambda_soc.wb_ibex_core.instr_core.we ? boxlambda_soc.wb_ibex_core.instr_core.be : {4 {1'sb1}});
						boxlambda_soc.xbar_wbm[_mbase_wb].adr = boxlambda_soc.wb_ibex_core.instr_core.addr[29:2];
						boxlambda_soc.xbar_wbm[_mbase_wb].dat_m = boxlambda_soc.wb_ibex_core.instr_core.wdata;
						boxlambda_soc.xbar_wbm[_mbase_wb].we = boxlambda_soc.wb_ibex_core.instr_core.we;
						boxlambda_soc.xbar_wbm[_mbase_wb].stb = boxlambda_soc.wb_ibex_core.instr_core.req;
						boxlambda_soc.xbar_wbm[_mbase_wb].cyc = boxlambda_soc.wb_ibex_core.instr_core.req;
						boxlambda_soc.wb_ibex_core.instr_core.rvalid = 1'b0;
						boxlambda_soc.wb_ibex_core.instr_core.err = 1'b0;
					end
					else begin
						boxlambda_soc.xbar_wbm[_mbase_wb].sel = sel_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].adr = adr_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].dat_m = dat_m_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].we = wbm_we_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].stb = wbm_stb_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].cyc = 1'b1;
						boxlambda_soc.wb_ibex_core.instr_core.rvalid = boxlambda_soc.xbar_wbm[_mbase_wb].ack;
						boxlambda_soc.wb_ibex_core.instr_core.err = boxlambda_soc.xbar_wbm[_mbase_wb].err;
					end
				end
				always @(posedge boxlambda_soc.xbar_wbm[_mbase_wb].clk)
					if (boxlambda_soc.xbar_wbm[_mbase_wb].rst) begin
						sel_reg <= 0;
						adr_reg <= 0;
						dat_m_reg <= 0;
						wbm_we_reg <= 1'b0;
						wbm_stb_reg <= 1'b0;
						transaction_ongoing_reg <= 1'b0;
					end
					else if (!transaction_ongoing_reg) begin
						if (boxlambda_soc.wb_ibex_core.instr_core.req) begin
							transaction_ongoing_reg <= 1'b1;
							wbm_stb_reg <= boxlambda_soc.xbar_wbm[_mbase_wb].stall;
							wbm_we_reg <= boxlambda_soc.wb_ibex_core.instr_core.we;
							dat_m_reg <= boxlambda_soc.wb_ibex_core.instr_core.wdata;
							adr_reg <= boxlambda_soc.wb_ibex_core.instr_core.addr[29:2];
							sel_reg <= (boxlambda_soc.wb_ibex_core.instr_core.we ? boxlambda_soc.wb_ibex_core.instr_core.be : {4 {1'sb1}});
						end
					end
					else begin
						if (!boxlambda_soc.xbar_wbm[_mbase_wb].stall)
							wbm_stb_reg <= 1'b0;
						if (boxlambda_soc.xbar_wbm[_mbase_wb].ack || boxlambda_soc.xbar_wbm[_mbase_wb].err)
							transaction_ongoing_reg <= 1'b0;
					end
				assign boxlambda_soc.wb_ibex_core.instr_core.gnt = ~transaction_ongoing_reg;
				assign boxlambda_soc.wb_ibex_core.instr_core.rdata = boxlambda_soc.xbar_wbm[_mbase_wb].dat_s;
				initial _sv2v_0 = 0;
			end
			localparam _bbase_715DD_wb = 32'd1;
			if (1) begin : data_core2wb
				reg _sv2v_0;
				localparam _mbase_wb = _bbase_715DD_wb;
				reg transaction_ongoing_reg;
				reg wbm_stb_reg;
				reg wbm_we_reg;
				reg [31:0] dat_m_reg;
				reg [27:0] adr_reg;
				reg [3:0] sel_reg;
				initial begin
					wbm_stb_reg = 1'b0;
					wbm_we_reg = 1'b0;
					dat_m_reg = 0;
					adr_reg = 0;
					sel_reg = 0;
					transaction_ongoing_reg = 1'b0;
				end
				always @(*) begin
					if (_sv2v_0)
						;
					if (!transaction_ongoing_reg) begin
						boxlambda_soc.xbar_wbm[_mbase_wb].sel = (boxlambda_soc.wb_ibex_core.data_core.we ? boxlambda_soc.wb_ibex_core.data_core.be : {4 {1'sb1}});
						boxlambda_soc.xbar_wbm[_mbase_wb].adr = boxlambda_soc.wb_ibex_core.data_core.addr[29:2];
						boxlambda_soc.xbar_wbm[_mbase_wb].dat_m = boxlambda_soc.wb_ibex_core.data_core.wdata;
						boxlambda_soc.xbar_wbm[_mbase_wb].we = boxlambda_soc.wb_ibex_core.data_core.we;
						boxlambda_soc.xbar_wbm[_mbase_wb].stb = boxlambda_soc.wb_ibex_core.data_core.req;
						boxlambda_soc.xbar_wbm[_mbase_wb].cyc = boxlambda_soc.wb_ibex_core.data_core.req;
						boxlambda_soc.wb_ibex_core.data_core.rvalid = 1'b0;
						boxlambda_soc.wb_ibex_core.data_core.err = 1'b0;
					end
					else begin
						boxlambda_soc.xbar_wbm[_mbase_wb].sel = sel_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].adr = adr_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].dat_m = dat_m_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].we = wbm_we_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].stb = wbm_stb_reg;
						boxlambda_soc.xbar_wbm[_mbase_wb].cyc = 1'b1;
						boxlambda_soc.wb_ibex_core.data_core.rvalid = boxlambda_soc.xbar_wbm[_mbase_wb].ack;
						boxlambda_soc.wb_ibex_core.data_core.err = boxlambda_soc.xbar_wbm[_mbase_wb].err;
					end
				end
				always @(posedge boxlambda_soc.xbar_wbm[_mbase_wb].clk)
					if (boxlambda_soc.xbar_wbm[_mbase_wb].rst) begin
						sel_reg <= 0;
						adr_reg <= 0;
						dat_m_reg <= 0;
						wbm_we_reg <= 1'b0;
						wbm_stb_reg <= 1'b0;
						transaction_ongoing_reg <= 1'b0;
					end
					else if (!transaction_ongoing_reg) begin
						if (boxlambda_soc.wb_ibex_core.data_core.req) begin
							transaction_ongoing_reg <= 1'b1;
							wbm_stb_reg <= boxlambda_soc.xbar_wbm[_mbase_wb].stall;
							wbm_we_reg <= boxlambda_soc.wb_ibex_core.data_core.we;
							dat_m_reg <= boxlambda_soc.wb_ibex_core.data_core.wdata;
							adr_reg <= boxlambda_soc.wb_ibex_core.data_core.addr[29:2];
							sel_reg <= (boxlambda_soc.wb_ibex_core.data_core.we ? boxlambda_soc.wb_ibex_core.data_core.be : {4 {1'sb1}});
						end
					end
					else begin
						if (!boxlambda_soc.xbar_wbm[_mbase_wb].stall)
							wbm_stb_reg <= 1'b0;
						if (boxlambda_soc.xbar_wbm[_mbase_wb].ack || boxlambda_soc.xbar_wbm[_mbase_wb].err)
							transaction_ongoing_reg <= 1'b0;
					end
				assign boxlambda_soc.wb_ibex_core.data_core.gnt = ~transaction_ongoing_reg;
				assign boxlambda_soc.wb_ibex_core.data_core.rdata = boxlambda_soc.xbar_wbm[_mbase_wb].dat_s;
				initial _sv2v_0 = 0;
			end
		end
	endgenerate
	assign wb_ibex_core.clk = sys_clk;
	assign wb_ibex_core.rst_n = ~ndm_reset;
	assign wb_ibex_core.test_en = 1'b0;
	assign wb_ibex_core.hart_id = 32'h00000000;
	assign wb_ibex_core.boot_addr = 32'h00000000;
	assign wb_ibex_core.irq_software = 1'b0;
	assign wb_ibex_core.irq_timer = timer_irq;
	assign wb_ibex_core.irq_fast = fast_irqs;
	assign wb_ibex_core.irq_nm = 1'b0;
	assign wb_ibex_core.debug_req = debug_req;
	assign wb_ibex_core.fetch_enable = {3'b000, por_completed};
	wb_dp_ram_wrapper #(
		.ADDR_WIDTH(DPRAM_AW),
		.INIT_FILE(CMEM_FILE)
	) cmem(
		.clk(sys_clk),
		.rst(ndm_reset),
		.a_adr_i(xbar_wbs[32'd0].adr[DPRAM_AW - 1:0]),
		.a_dat_i(xbar_wbs[32'd0].dat_m),
		.a_dat_o(xbar_wbs[32'd0].dat_s),
		.a_we_i(xbar_wbs[32'd0].we),
		.a_sel_i(xbar_wbs[32'd0].sel),
		.a_stb_i(xbar_wbs[32'd0].stb),
		.a_stall_o(xbar_wbs[32'd0].stall),
		.a_ack_o(xbar_wbs[32'd0].ack),
		.a_err_o(xbar_wbs[32'd0].err),
		.a_cyc_i(xbar_wbs[32'd0].cyc),
		.b_adr_i(xbar_wbs[32'd2].adr[DPRAM_AW - 1:0]),
		.b_dat_i(xbar_wbs[32'd2].dat_m),
		.b_dat_o(xbar_wbs[32'd2].dat_s),
		.b_we_i(xbar_wbs[32'd2].we),
		.b_sel_i(xbar_wbs[32'd2].sel),
		.b_stb_i(xbar_wbs[32'd2].stb),
		.b_stall_o(xbar_wbs[32'd2].stall),
		.b_ack_o(xbar_wbs[32'd2].ack),
		.b_err_o(xbar_wbs[32'd2].err),
		.b_cyc_i(xbar_wbs[32'd2].cyc)
	);
	wb_dp_ram_wrapper #(
		.ADDR_WIDTH(DPRAM_AW),
		.INIT_FILE(DMEM_FILE)
	) dmem(
		.clk(sys_clk),
		.rst(ndm_reset),
		.a_adr_i(xbar_wbs[32'd1].adr[DPRAM_AW - 1:0]),
		.a_dat_i(xbar_wbs[32'd1].dat_m),
		.a_dat_o(xbar_wbs[32'd1].dat_s),
		.a_we_i(xbar_wbs[32'd1].we),
		.a_sel_i(xbar_wbs[32'd1].sel),
		.a_stb_i(xbar_wbs[32'd1].stb),
		.a_stall_o(xbar_wbs[32'd1].stall),
		.a_ack_o(xbar_wbs[32'd1].ack),
		.a_err_o(xbar_wbs[32'd1].err),
		.a_cyc_i(xbar_wbs[32'd1].cyc),
		.b_adr_i(xbar_wbs[32'd3].adr[DPRAM_AW - 1:0]),
		.b_dat_i(xbar_wbs[32'd3].dat_m),
		.b_dat_o(xbar_wbs[32'd3].dat_s),
		.b_we_i(xbar_wbs[32'd3].we),
		.b_sel_i(xbar_wbs[32'd3].sel),
		.b_stb_i(xbar_wbs[32'd3].stb),
		.b_stall_o(xbar_wbs[32'd3].stall),
		.b_ack_o(xbar_wbs[32'd3].ack),
		.b_err_o(xbar_wbs[32'd3].err),
		.b_cyc_i(xbar_wbs[32'd3].cyc)
	);
	generate
		if (VS0_ACTIVE) begin : GENERATE_VS0_MODULE
			wire [28:1] sv2v_tmp_vs0_inst_wbm_0_adr_o;
			always @(*) xbar_wbm[32'd3].adr = sv2v_tmp_vs0_inst_wbm_0_adr_o;
			wire [32:1] sv2v_tmp_vs0_inst_wbm_0_dat_o;
			always @(*) xbar_wbm[32'd3].dat_m = sv2v_tmp_vs0_inst_wbm_0_dat_o;
			wire [4:1] sv2v_tmp_vs0_inst_wbm_0_sel_o;
			always @(*) xbar_wbm[32'd3].sel = sv2v_tmp_vs0_inst_wbm_0_sel_o;
			wire [1:1] sv2v_tmp_vs0_inst_wbm_0_cyc_o;
			always @(*) xbar_wbm[32'd3].cyc = sv2v_tmp_vs0_inst_wbm_0_cyc_o;
			wire [1:1] sv2v_tmp_vs0_inst_wbm_0_stb_o;
			always @(*) xbar_wbm[32'd3].stb = sv2v_tmp_vs0_inst_wbm_0_stb_o;
			wire [1:1] sv2v_tmp_vs0_inst_wbm_0_we_o;
			always @(*) xbar_wbm[32'd3].we = sv2v_tmp_vs0_inst_wbm_0_we_o;
			wire [28:1] sv2v_tmp_vs0_inst_wbm_1_adr_o;
			always @(*) xbar_wbm[32'd4].adr = sv2v_tmp_vs0_inst_wbm_1_adr_o;
			wire [32:1] sv2v_tmp_vs0_inst_wbm_1_dat_o;
			always @(*) xbar_wbm[32'd4].dat_m = sv2v_tmp_vs0_inst_wbm_1_dat_o;
			wire [4:1] sv2v_tmp_vs0_inst_wbm_1_sel_o;
			always @(*) xbar_wbm[32'd4].sel = sv2v_tmp_vs0_inst_wbm_1_sel_o;
			wire [1:1] sv2v_tmp_vs0_inst_wbm_1_cyc_o;
			always @(*) xbar_wbm[32'd4].cyc = sv2v_tmp_vs0_inst_wbm_1_cyc_o;
			wire [1:1] sv2v_tmp_vs0_inst_wbm_1_stb_o;
			always @(*) xbar_wbm[32'd4].stb = sv2v_tmp_vs0_inst_wbm_1_stb_o;
			wire [1:1] sv2v_tmp_vs0_inst_wbm_1_we_o;
			always @(*) xbar_wbm[32'd4].we = sv2v_tmp_vs0_inst_wbm_1_we_o;
			vs0 vs0_inst(
				.sys_clk(sys_clk),
				.rst(vs0_reset),
				.wbm_0_adr_o(sv2v_tmp_vs0_inst_wbm_0_adr_o),
				.wbm_0_dat_o(sv2v_tmp_vs0_inst_wbm_0_dat_o),
				.wbm_0_dat_i(xbar_wbm[32'd3].dat_s),
				.wbm_0_sel_o(sv2v_tmp_vs0_inst_wbm_0_sel_o),
				.wbm_0_stall_i(xbar_wbm[32'd3].stall),
				.wbm_0_cyc_o(sv2v_tmp_vs0_inst_wbm_0_cyc_o),
				.wbm_0_stb_o(sv2v_tmp_vs0_inst_wbm_0_stb_o),
				.wbm_0_ack_i(xbar_wbm[32'd3].ack),
				.wbm_0_we_o(sv2v_tmp_vs0_inst_wbm_0_we_o),
				.wbm_0_err_i(xbar_wbm[32'd3].err),
				.wbm_1_adr_o(sv2v_tmp_vs0_inst_wbm_1_adr_o),
				.wbm_1_dat_o(sv2v_tmp_vs0_inst_wbm_1_dat_o),
				.wbm_1_dat_i(xbar_wbm[32'd4].dat_s),
				.wbm_1_sel_o(sv2v_tmp_vs0_inst_wbm_1_sel_o),
				.wbm_1_stall_i(xbar_wbm[32'd4].stall),
				.wbm_1_cyc_o(sv2v_tmp_vs0_inst_wbm_1_cyc_o),
				.wbm_1_stb_o(sv2v_tmp_vs0_inst_wbm_1_stb_o),
				.wbm_1_ack_i(xbar_wbm[32'd4].ack),
				.wbm_1_we_o(sv2v_tmp_vs0_inst_wbm_1_we_o),
				.wbm_1_err_i(xbar_wbm[32'd4].err),
				.wbs_adr(xbar_wbs[32'd6].adr[19:0]),
				.wbs_dat_w(xbar_wbs[32'd6].dat_m),
				.wbs_dat_r(xbar_wbs[32'd6].dat_s),
				.wbs_sel(xbar_wbs[32'd6].sel),
				.wbs_stall(xbar_wbs[32'd6].stall),
				.wbs_cyc(xbar_wbs[32'd6].cyc),
				.wbs_stb(xbar_wbs[32'd6].stb),
				.wbs_ack(xbar_wbs[32'd6].ack),
				.wbs_we(xbar_wbs[32'd6].we),
				.wbs_err(xbar_wbs[32'd6].err),
				.irq_in({1'b0, fast_irqs & ~(1'b1 << 32'd12), 8'b00000000, timer_irq, 7'b0000000}),
				.irq_out(fast_irqs[32'd12])
			);
		end
		else begin : genblk6
			assign fast_irqs[32'd12] = 1'b0;
		end
	endgenerate
	wire [1:1] sv2v_tmp_wbuart_inst_o_wb_ack;
	always @(*) shared_bus_wbs[32'd10].ack = sv2v_tmp_wbuart_inst_o_wb_ack;
	wbuart #(
		.HARDWARE_FLOW_CONTROL_PRESENT(1'b0),
		.INITIAL_SETUP(31'd25),
		.LGFLEN(4'd4)
	) wbuart_inst(
		.i_clk(sys_clk),
		.i_reset(ndm_reset),
		.i_wb_cyc(shared_bus_wbs[32'd10].cyc),
		.i_wb_stb(shared_bus_wbs[32'd10].stb),
		.i_wb_we(shared_bus_wbs[32'd10].we),
		.i_wb_addr(shared_bus_wbs[32'd10].adr[2:0]),
		.i_wb_data(shared_bus_wbs[32'd10].dat_m),
		.i_wb_sel(shared_bus_wbs[32'd10].sel),
		.o_wb_stall(shared_bus_wbs[32'd10].stall),
		.o_wb_ack(sv2v_tmp_wbuart_inst_o_wb_ack),
		.o_wb_err(shared_bus_wbs[32'd10].err),
		.o_wb_data(shared_bus_wbs[32'd10].dat_s),
		.i_uart_rx(uart_rx),
		.o_uart_tx(uart_tx),
		.i_cts_n(1'b0),
		.o_rts_n(),
		.o_uart_int(fast_irqs[32'd5])
	);
	wire [1:1] sv2v_tmp_wb_timer_inst_wb_ack_o;
	always @(*) shared_bus_wbs[32'd11].ack = sv2v_tmp_wb_timer_inst_wb_ack_o;
	wb_timer wb_timer_inst(
		.clk_i(sys_clk),
		.rst_i(ndm_reset),
		.wb_cyc_i(shared_bus_wbs[32'd11].cyc),
		.wb_stb_i(shared_bus_wbs[32'd11].stb),
		.wb_we_i(shared_bus_wbs[32'd11].we),
		.wb_addr_i(shared_bus_wbs[32'd11].adr[7:0]),
		.wb_data_i(shared_bus_wbs[32'd11].dat_m),
		.wb_sel_i(shared_bus_wbs[32'd11].sel),
		.wb_stall_o(shared_bus_wbs[32'd11].stall),
		.wb_ack_o(sv2v_tmp_wb_timer_inst_wb_ack_o),
		.wb_err_o(shared_bus_wbs[32'd11].err),
		.wb_data_o(shared_bus_wbs[32'd11].dat_s),
		.timer_irq_o(timer_irq)
	);
	wire [1:1] sv2v_tmp_gpio_inst_wb_ack_o;
	always @(*) shared_bus_wbs[32'd5].ack = sv2v_tmp_gpio_inst_wb_ack_o;
	gpio_top gpio_inst(
		.wb_clk_i(sys_clk),
		.wb_rst_i(ndm_reset),
		.wb_cyc_i(shared_bus_wbs[32'd5].cyc),
		.wb_adr_i(shared_bus_wbs[32'd5].adr[3:0]),
		.wb_dat_i(shared_bus_wbs[32'd5].dat_m),
		.wb_sel_i(shared_bus_wbs[32'd5].sel),
		.wb_we_i(shared_bus_wbs[32'd5].we),
		.wb_stb_i(shared_bus_wbs[32'd5].stb),
		.wb_dat_o(shared_bus_wbs[32'd5].dat_s),
		.wb_ack_o(sv2v_tmp_gpio_inst_wb_ack_o),
		.wb_err_o(shared_bus_wbs[32'd5].err),
		.wb_inta_o(fast_irqs[32'd9]),
		.ext_pad_i(gp_in),
		.ext_pad_o(gp_out),
		.ext_padoe_o(gp_oe),
		.clk_pad_i(gp_clk)
	);
	generate
		if (DRAM_ACTIVE) begin : GENERATE_DRAM_MODULE
			wire litedram_pll_locked_i;
			wire litedram_rst_o;
			wire [1:1] sv2v_tmp_litedram_wrapper_inst_wb_ctrl_ack;
			always @(*) shared_bus_wbs[32'd12].ack = sv2v_tmp_litedram_wrapper_inst_wb_ctrl_ack;
			litedram_wrapper litedram_wrapper_inst(
				.clk(clk_100),
				.rst(1'b0),
				.sys_clkx2(),
				.sys_clk(sys_clk),
				.sys_rst(litedram_rst_o),
				.pll_locked(litedram_pll_locked_i),
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
				.init_done(init_done_led),
				.init_error(init_err_led),
				.wb_ctrl_adr(shared_bus_wbs[32'd12].adr),
				.wb_ctrl_dat_w(shared_bus_wbs[32'd12].dat_m),
				.wb_ctrl_dat_r(shared_bus_wbs[32'd12].dat_s),
				.wb_ctrl_sel(shared_bus_wbs[32'd12].sel),
				.wb_ctrl_stall(shared_bus_wbs[32'd12].stall),
				.wb_ctrl_cyc(shared_bus_wbs[32'd12].cyc),
				.wb_ctrl_stb(shared_bus_wbs[32'd12].stb),
				.wb_ctrl_ack(sv2v_tmp_litedram_wrapper_inst_wb_ctrl_ack),
				.wb_ctrl_we(shared_bus_wbs[32'd12].we),
				.wb_ctrl_err(shared_bus_wbs[32'd12].err),
				.user_port_wishbone_p_0_adr(xbar_wbs[32'd7].adr),
				.user_port_wishbone_p_0_dat_w(xbar_wbs[32'd7].dat_m),
				.user_port_wishbone_p_0_dat_r(xbar_wbs[32'd7].dat_s),
				.user_port_wishbone_p_0_sel(xbar_wbs[32'd7].sel),
				.user_port_wishbone_p_0_stall(xbar_wbs[32'd7].stall),
				.user_port_wishbone_p_0_cyc(xbar_wbs[32'd7].cyc),
				.user_port_wishbone_p_0_stb(xbar_wbs[32'd7].stb),
				.user_port_wishbone_p_0_ack(xbar_wbs[32'd7].ack),
				.user_port_wishbone_p_0_we(xbar_wbs[32'd7].we),
				.user_port_wishbone_p_0_err(xbar_wbs[32'd7].err)
			);
			assign litedram_pll_locked = ~litedram_rst_o & litedram_pll_locked_i;
			assign sys_pll_locked = litedram_pll_locked;
		end
		else begin : genblk7
			assign sys_clk = clk_50;
			assign litedram_pll_locked = 1'b1;
			assign init_done_led = 1'b1;
			assign init_err_led = 1'b0;
			assign sys_pll_locked = pre_pll_locked;
			OBUFDS OBUFDS(
				.I(1'b0),
				.O(ddram_clk_p),
				.OB(ddram_clk_n)
			);
		end
	endgenerate
	assign pll_locked_led = sys_pll_locked;
	generate
		if (VERA_ACTIVE) begin : GENERATE_VERA_MODULE
			wire vera_irq_n;
			assign fast_irqs[32'd13] = ~vera_irq_n;
			vera_top #(.VRAM_SIZE_BYTES(VRAM_SIZE_BYTES)) vera_inst(
				.clk(sys_clk),
				.reset(ndm_reset),
				.wb_adr(xbar_wbs[32'd5].adr[16:0]),
				.wb_dat_w(xbar_wbs[32'd5].dat_m),
				.wb_dat_r(xbar_wbs[32'd5].dat_s),
				.wb_sel(xbar_wbs[32'd5].sel),
				.wb_stall(xbar_wbs[32'd5].stall),
				.wb_cyc(xbar_wbs[32'd5].cyc),
				.wb_stb(xbar_wbs[32'd5].stb),
				.wb_ack(xbar_wbs[32'd5].ack),
				.wb_we(xbar_wbs[32'd5].we),
				.wb_err(xbar_wbs[32'd5].err),
				.irq_n(vera_irq_n),
				.vga_r(vga_r),
				.vga_g(vga_g),
				.vga_b(vga_b),
				.vga_hsync(vga_hsync),
				.vga_vsync(vga_vsync)
			);
		end
		else begin : NO_VGA
			assign vga_r = 4'b0000;
			assign vga_g = 4'b0000;
			assign vga_b = 4'b0000;
			assign vga_hsync = 1'b0;
			assign vga_vsync = 1'b0;
			assign fast_irqs[32'd13] = 1'b0;
		end
		if (SDSPI_ACTIVE) begin : GENERATE_SDSPI_MODULE
			wire [1:1] sv2v_tmp_sdspi_inst_o_wb_ack;
			always @(*) shared_bus_wbs[32'd0].ack = sv2v_tmp_sdspi_inst_o_wb_ack;
			sdspi #(.OPT_LITTLE_ENDIAN(1'b1)) sdspi_inst(
				.i_clk(sys_clk),
				.i_sd_reset(ndm_reset),
				.i_wb_cyc(shared_bus_wbs[32'd0].cyc),
				.i_wb_stb(shared_bus_wbs[32'd0].stb),
				.i_wb_we(shared_bus_wbs[32'd0].we),
				.i_wb_addr(shared_bus_wbs[32'd0].adr[2:0]),
				.i_wb_data(shared_bus_wbs[32'd0].dat_m),
				.i_wb_sel(shared_bus_wbs[32'd0].sel),
				.o_wb_stall(shared_bus_wbs[32'd0].stall),
				.o_wb_ack(sv2v_tmp_sdspi_inst_o_wb_ack),
				.o_wb_data(shared_bus_wbs[32'd0].dat_s),
				.o_cs_n(sdspi_cs_n),
				.o_sck(sdspi_sck),
				.o_mosi(sdspi_mosi),
				.i_miso(sdspi_miso),
				.i_card_detect(~sdspi_card_detect_n),
				.o_int(fast_irqs[32'd10]),
				.i_bus_grant(1'b1),
				.o_debug()
			);
			assign shared_bus_wbs[32'd0].err = 1'b0;
			assign sd_card_detect_led = ~sdspi_card_detect_n;
		end
		else begin : NO_SDSPI
			assign sdspi_cs_n = 1'b0;
			assign sdspi_sck = 1'b0;
			assign sdspi_mosi = 1'b0;
			assign fast_irqs[32'd10] = 1'b0;
			assign sd_card_detect_led = 1'b0;
		end
		if (YM2149_ACTIVE) begin : GENERATE_YM2149_MODULE
			wire signed [15:0] ym2149_sound;
			wire [1:1] sv2v_tmp_ym2149_sys_inst_wb_ack;
			always @(*) shared_bus_wbs[32'd8].ack = sv2v_tmp_ym2149_sys_inst_wb_ack;
			YM2149_PSG_system_wb #(
				.CLK_IN_HZ(50000000),
				.CLK_PSG_HZ(2000000),
				.YM2149_DAC_BITS(10),
				.MIXER_DAC_BITS(16)
			) ym2149_sys_inst(
				.clk(sys_clk),
				.clk_i2s(1'b0),
				.rst(ndm_reset),
				.wb_adr(shared_bus_wbs[32'd8].adr[7:0]),
				.wb_dat_w(shared_bus_wbs[32'd8].dat_m),
				.wb_dat_r(shared_bus_wbs[32'd8].dat_s),
				.wb_sel(shared_bus_wbs[32'd8].sel),
				.wb_stall(shared_bus_wbs[32'd8].stall),
				.wb_cyc(shared_bus_wbs[32'd8].cyc),
				.wb_stb(shared_bus_wbs[32'd8].stb),
				.wb_ack(sv2v_tmp_ym2149_sys_inst_wb_ack),
				.wb_we(shared_bus_wbs[32'd8].we),
				.wb_err(shared_bus_wbs[32'd8].err),
				.i2s_sclk(),
				.i2s_lrclk(),
				.i2s_data(),
				.sound(ym2149_sound),
				.sound_right()
			);
			reg [1:0] div_by_4_ctr;
			always @(posedge sys_clk) div_by_4_ctr <= div_by_4_ctr - 1;
			one_bit_dac dac_inst(
				.clk(sys_clk),
				.clk_en(div_by_4_ctr == 0),
				.in(ym2149_sound),
				.out(audio_out)
			);
			assign audio_gain = 1'b1;
			assign audio_shutdown_n = 1'b1;
		end
		else begin : NO_AUDIO
			assign audio_out = 1'b0;
			assign audio_gain = 1'b0;
			assign audio_shutdown_n = 1'b0;
		end
		if (PICORV_ACTIVE) begin : GENERATE_PICORV_MODULE
			wire [28:1] sv2v_tmp_picorv_dma_inst_wbm_adr_o;
			always @(*) xbar_wbm[32'd2].adr = sv2v_tmp_picorv_dma_inst_wbm_adr_o;
			wire [32:1] sv2v_tmp_picorv_dma_inst_wbm_dat_o;
			always @(*) xbar_wbm[32'd2].dat_m = sv2v_tmp_picorv_dma_inst_wbm_dat_o;
			wire [4:1] sv2v_tmp_picorv_dma_inst_wbm_sel_o;
			always @(*) xbar_wbm[32'd2].sel = sv2v_tmp_picorv_dma_inst_wbm_sel_o;
			wire [1:1] sv2v_tmp_picorv_dma_inst_wbm_cyc_o;
			always @(*) xbar_wbm[32'd2].cyc = sv2v_tmp_picorv_dma_inst_wbm_cyc_o;
			wire [1:1] sv2v_tmp_picorv_dma_inst_wbm_stb_o;
			always @(*) xbar_wbm[32'd2].stb = sv2v_tmp_picorv_dma_inst_wbm_stb_o;
			wire [1:1] sv2v_tmp_picorv_dma_inst_wbm_we_o;
			always @(*) xbar_wbm[32'd2].we = sv2v_tmp_picorv_dma_inst_wbm_we_o;
			wire [1:1] sv2v_tmp_picorv_dma_inst_wbs_ack;
			always @(*) shared_bus_wbs[32'd9].ack = sv2v_tmp_picorv_dma_inst_wbs_ack;
			picorv_dma_top #(.BASE_ADDR(PICORV_BASE_ADDRESS)) picorv_dma_inst(
				.sys_clk(sys_clk),
				.sys_clkx2(sys_clk),
				.rst(ndm_reset),
				.wbm_adr_o(sv2v_tmp_picorv_dma_inst_wbm_adr_o),
				.wbm_dat_o(sv2v_tmp_picorv_dma_inst_wbm_dat_o),
				.wbm_dat_i(xbar_wbm[32'd2].dat_s),
				.wbm_sel_o(sv2v_tmp_picorv_dma_inst_wbm_sel_o),
				.wbm_stall_i(xbar_wbm[32'd2].stall),
				.wbm_cyc_o(sv2v_tmp_picorv_dma_inst_wbm_cyc_o),
				.wbm_stb_o(sv2v_tmp_picorv_dma_inst_wbm_stb_o),
				.wbm_ack_i(xbar_wbm[32'd2].ack),
				.wbm_we_o(sv2v_tmp_picorv_dma_inst_wbm_we_o),
				.wbm_err_i(xbar_wbm[32'd2].err),
				.wbs_adr(shared_bus_wbs[32'd9].adr[10:0]),
				.wbs_dat_w(shared_bus_wbs[32'd9].dat_m),
				.wbs_dat_r(shared_bus_wbs[32'd9].dat_s),
				.wbs_sel(shared_bus_wbs[32'd9].sel),
				.wbs_stall(shared_bus_wbs[32'd9].stall),
				.wbs_cyc(shared_bus_wbs[32'd9].cyc),
				.wbs_stb(shared_bus_wbs[32'd9].stb),
				.wbs_ack(sv2v_tmp_picorv_dma_inst_wbs_ack),
				.wbs_we(shared_bus_wbs[32'd9].we),
				.wbs_err(shared_bus_wbs[32'd9].err),
				.irq_in({1'b0, fast_irqs & ~(1'b1 << 32'd11), 8'b00000000, timer_irq, 7'b0000000}),
				.irq_out(fast_irqs[32'd11])
			);
		end
		else begin : genblk11
			assign fast_irqs[32'd11] = 1'b0;
		end
		if (USB_HID_ACTIVE) begin : GENERATE_USB_HID_MODULES
			wire [1:1] sv2v_tmp_usb_hid0_host_inst_wbs_ack;
			always @(*) shared_bus_wbs[32'd1].ack = sv2v_tmp_usb_hid0_host_inst_wbs_ack;
			usb_hid_host_top usb_hid0_host_inst(
				.wb_clk(sys_clk),
				.usb_clk(usb_clk),
				.usb_rst_n(~usb_reset),
				.wb_rst_n(~ndm_reset),
				.usb_dm_i(usb0_dm_i),
				.usb_dp_i(usb0_dp_i),
				.usb_dm_o(usb0_dm_o),
				.usb_dp_o(usb0_dp_o),
				.usb_oe(usb0_oe),
				.irq(fast_irqs[32'd7]),
				.wbs_adr(shared_bus_wbs[32'd1].adr[3:0]),
				.wbs_dat_w(shared_bus_wbs[32'd1].dat_m),
				.wbs_dat_r(shared_bus_wbs[32'd1].dat_s),
				.wbs_sel(shared_bus_wbs[32'd1].sel),
				.wbs_stall(shared_bus_wbs[32'd1].stall),
				.wbs_cyc(shared_bus_wbs[32'd1].cyc),
				.wbs_stb(shared_bus_wbs[32'd1].stb),
				.wbs_ack(sv2v_tmp_usb_hid0_host_inst_wbs_ack),
				.wbs_we(shared_bus_wbs[32'd1].we),
				.wbs_err(shared_bus_wbs[32'd1].err)
			);
			wire [1:1] sv2v_tmp_usb_hid1_host_inst_wbs_ack;
			always @(*) shared_bus_wbs[32'd2].ack = sv2v_tmp_usb_hid1_host_inst_wbs_ack;
			usb_hid_host_top usb_hid1_host_inst(
				.wb_clk(sys_clk),
				.usb_clk(usb_clk),
				.usb_rst_n(~usb_reset),
				.wb_rst_n(~ndm_reset),
				.usb_dm_i(usb1_dm_i),
				.usb_dp_i(usb1_dp_i),
				.usb_dm_o(usb1_dm_o),
				.usb_dp_o(usb1_dp_o),
				.usb_oe(usb1_oe),
				.irq(fast_irqs[32'd8]),
				.wbs_adr(shared_bus_wbs[32'd2].adr[3:0]),
				.wbs_dat_w(shared_bus_wbs[32'd2].dat_m),
				.wbs_dat_r(shared_bus_wbs[32'd2].dat_s),
				.wbs_sel(shared_bus_wbs[32'd2].sel),
				.wbs_stall(shared_bus_wbs[32'd2].stall),
				.wbs_cyc(shared_bus_wbs[32'd2].cyc),
				.wbs_stb(shared_bus_wbs[32'd2].stb),
				.wbs_ack(sv2v_tmp_usb_hid1_host_inst_wbs_ack),
				.wbs_we(shared_bus_wbs[32'd2].we),
				.wbs_err(shared_bus_wbs[32'd2].err)
			);
		end
		else begin : genblk12
			assign usb0_dm_o = 1'b0;
			assign usb0_dp_o = 1'b0;
			assign usb0_oe = 1'b0;
			assign usb1_dm_o = 1'b0;
			assign usb1_dp_o = 1'b0;
			assign usb1_oe = 1'b0;
			assign fast_irqs[32'd7] = 1'b0;
			assign fast_irqs[32'd8] = 1'b0;
		end
		if (SPIFLASH_ACTIVE) begin : GENERATE_SPIFLASH_MODULE
			wire flash_wb_cyc;
			wire flash_wb_stb;
			wire flash_cfg_stb;
			wire flash_wb_we;
			wire [21:0] flash_wb_addr;
			wire [31:0] flash_wb_dat_m;
			wire flash_wb_stall;
			wire flash_wb_ack;
			wire [31:0] flash_wb_dat_s;
			assign flash_wb_cyc = shared_bus_wbs[32'd14].cyc | shared_bus_wbs[32'd3].cyc;
			assign flash_wb_stb = shared_bus_wbs[32'd14].stb;
			assign flash_cfg_stb = shared_bus_wbs[32'd3].stb;
			assign flash_wb_we = (shared_bus_wbs[32'd14].cyc ? shared_bus_wbs[32'd14].we : shared_bus_wbs[32'd3].we);
			assign flash_wb_addr = (shared_bus_wbs[32'd14].cyc ? shared_bus_wbs[32'd14].adr[21:0] : shared_bus_wbs[32'd3].adr[21:0]);
			assign flash_wb_dat_m = (shared_bus_wbs[32'd14].cyc ? shared_bus_wbs[32'd14].dat_m : shared_bus_wbs[32'd3].dat_m);
			assign shared_bus_wbs[32'd14].stall = flash_wb_stall;
			assign shared_bus_wbs[32'd3].stall = flash_wb_stall;
			wire [1:1] sv2v_tmp_91540;
			assign sv2v_tmp_91540 = flash_wb_ack;
			always @(*) shared_bus_wbs[32'd14].ack = sv2v_tmp_91540;
			wire [1:1] sv2v_tmp_ECBD7;
			assign sv2v_tmp_ECBD7 = flash_wb_ack;
			always @(*) shared_bus_wbs[32'd3].ack = sv2v_tmp_ECBD7;
			assign shared_bus_wbs[32'd14].dat_s = flash_wb_dat_s;
			assign shared_bus_wbs[32'd3].dat_s = flash_wb_dat_s;
			spiflash #(.SCK_CLKDIV(2)) spiflash_inst(
				.i_clk(sys_clk),
				.i_reset(ndm_reset),
				.i_wb_cyc(flash_wb_cyc),
				.i_wb_stb(flash_wb_stb),
				.i_cfg_stb(flash_cfg_stb),
				.i_wb_we(flash_wb_we),
				.i_wb_addr(flash_wb_addr),
				.i_wb_data(flash_wb_dat_m),
				.o_wb_stall(flash_wb_stall),
				.o_wb_ack(flash_wb_ack),
				.o_wb_data(flash_wb_dat_s),
				.o_spi_cs_n(spiflash_cs_n),
				.o_spi_sck(spiflash_sck),
				.o_spi_mosi(spiflash_mosi),
				.i_spi_miso(spiflash_miso)
			);
		end
		else begin : genblk13
			assign spiflash_sck = 1'b0;
			assign spiflash_cs_n = 1'b1;
			assign spiflash_mosi = 1'b0;
		end
		if (I2C_ACTIVE) begin : GENERATE_I2C_MODULE
			wire [1:1] sv2v_tmp_i2cmaster_inst_o_wb_ack;
			always @(*) shared_bus_wbs[32'd6].ack = sv2v_tmp_i2cmaster_inst_o_wb_ack;
			wbi2cmaster #(
				.MEM_ADDR_BITS(8),
				.LITTLE_ENDIAN(1'b1)
			) i2cmaster_inst(
				.i_clk(sys_clk),
				.i_reset(ndm_reset),
				.i_wb_cyc(shared_bus_wbs[32'd6].cyc),
				.i_wb_stb(shared_bus_wbs[32'd6].stb),
				.i_wb_we(shared_bus_wbs[32'd6].we),
				.i_wb_addr(shared_bus_wbs[32'd6].adr[6:0]),
				.i_wb_data(shared_bus_wbs[32'd6].dat_m),
				.i_wb_sel(shared_bus_wbs[32'd6].sel),
				.o_wb_stall(shared_bus_wbs[32'd6].stall),
				.o_wb_ack(sv2v_tmp_i2cmaster_inst_o_wb_ack),
				.o_wb_data(shared_bus_wbs[32'd6].dat_s),
				.i_i2c_scl(i2c_scl_i),
				.i_i2c_sda(i2c_sda_i),
				.o_i2c_scl(i2c_scl_o),
				.o_i2c_sda(i2c_sda_o),
				.o_int(fast_irqs[32'd6]),
				.o_dbg()
			);
		end
		else begin : genblk14
			assign i2c_scl_o = 1'b1;
			assign i2c_sda_o = 1'b1;
			assign fast_irqs[32'd6] = 1'b0;
		end
	endgenerate
	assign shared_bus_wbs[32'd6].err = 1'b0;
	assign fast_irqs[32'd14] = 1'b0;
	assign fast_irqs[32'd2] = 1'b0;
	assign fast_irqs[32'd3] = 1'b0;
	assign fast_irqs[32'd4] = 1'b0;
	assign fast_irqs[32'd0] = 1'b0;
endmodule
