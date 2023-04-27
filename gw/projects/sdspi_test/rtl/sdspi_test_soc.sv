`default_nettype none

module sdspi_test_soc(
  input  wire       ext_clk, /*External clock: 100MHz on FPGA, 50MHz in simulation.*/
   
  inout  wire [7:0] gpio0,
  inout  wire [3:0] gpio1,
  
  input  wire       ext_rst_n, /*External reset, asynchronous, active-low.*/
  
  input  wire       uart_rx,
  output wire       uart_tx,
  
  /*These JTAG signals are not used on FPGA (they are used in simulation).
   *On FPGA, the JTAG signals are driven by a BSCANE2 primitive inside the jtag tap module dmi_bscane_tap.sv.
   */
  input  wire       tck,
  input  wire       trst_n,
  input  wire       tms,
  input  wire       tdi,
  output wire       tdo,

  output wire       pll_locked_led,
  output wire       init_done_led,
  output wire       init_err_led
`ifdef SYNTHESIS
  , /*The simulation build doesn't export DDR pins.*/
  output wire [13:0] ddram_a,
  output wire [2:0] ddram_ba,
  output wire ddram_ras_n,
	output wire ddram_cas_n,
	output wire ddram_we_n,
	output wire ddram_cs_n,
	output wire [1:0] ddram_dm,
	inout  wire [15:0] ddram_dq,
	inout  wire [1:0] ddram_dqs_p,
	inout  wire [1:0] ddram_dqs_n,
	output wire ddram_clk_p,
	output wire ddram_clk_n,
	output wire ddram_cke,
	output wire ddram_odt,
	output wire ddram_reset_n
`endif  
  , // VGA interface
  output wire  [3:0] vga_r,       
  output wire  [3:0] vga_g,       
  output wire  [3:0] vga_b,       
  output wire        vga_hsync,   
  output wire        vga_vsync,
    // SDSPI interface
  output wire  sdspi_cs_n, 
  output wire  sdspi_sck, 
  output wire  sdspi_mosi,
	input	 wire	 sdspi_miso, 
  input  wire  sdspi_card_detect
  );
    
  typedef enum {
    DM_M,
    COREI_M,
    CORED_M
  } wb_master_e;

  typedef enum {
    RAM_S,
    GPIO0_S,
    GPIO1_S,
    SDSPI_S,
    UART_S,
    TIMER_S,
    DDR_CTRL_S,
    VERA_S,
    DM_S,
    DDR_USR0_S,
    DDR_USR1_S
} wb_slave_e;

  localparam NrMaster = 3;
  localparam NrSlave  = 11;

  typedef logic [31:0] Wb_base_addr [NrSlave];

  function Wb_base_addr wb_base_addresses();
     wb_base_addresses[RAM_S]      = 32'h00000000;
     wb_base_addresses[GPIO0_S]    = 32'h10000000;
     wb_base_addresses[GPIO1_S]    = 32'h10000010;
     wb_base_addresses[SDSPI_S]    = 32'h10000020;
     wb_base_addresses[UART_S]     = 32'h10010000;
     wb_base_addresses[TIMER_S]    = 32'h10020000;
     wb_base_addresses[DDR_CTRL_S] = 32'h10030000;
     wb_base_addresses[VERA_S]     = 32'h10100000;
     wb_base_addresses[DM_S]       = 32'h1A110000;
     wb_base_addresses[DDR_USR0_S] = 32'h40000000;
     wb_base_addresses[DDR_USR1_S] = 32'h50000000;

  endfunction // wb_base_addresses

  localparam Wb_base_addr wb_base_addr = wb_base_addresses();
   
  typedef logic [31:0] Wb_size [NrSlave];

  //These are address range sizes in bytes
  function Wb_size wb_sizes();
    wb_sizes[DM_S]       = 32'h10000;
    wb_sizes[RAM_S]      = 32'h10000;
    wb_sizes[GPIO0_S]    = 32'h00010;
    wb_sizes[GPIO1_S]    = 32'h00010;
    wb_sizes[SDSPI_S]    = 32'h00010;
    wb_sizes[UART_S]     = 32'h00010;
    wb_sizes[TIMER_S]    = 32'h00010;
    wb_sizes[DDR_CTRL_S] = 32'h10000;
    wb_sizes[VERA_S]     = 32'h80000;
    wb_sizes[DDR_USR0_S]  = 32'h10000000; /*256MB*/
    wb_sizes[DDR_USR1_S]  = 32'h10000000; /*256MB*/
  endfunction // wb_sizes

  localparam Wb_size wb_size = wb_sizes();

  //sys_clk is a 50MHz clock.  
  logic sys_clk, sys_rst_n;

  //Non-Debug-Module-Reset, i.e. reset everything except Debug Module.
  //Driven by Debug Module itself to implement target reset function.
  logic ndmreset;

  wb_if wbm[NrMaster](.rst(ndmreset | (~sys_rst_n)), .clk(sys_clk));
  wb_if wbs[NrSlave](.rst(ndmreset | (~sys_rst_n)), .clk(sys_clk));
  
  // define the macro if you want to use debugger
`ifdef DEBUG_MODULE_ACTIVE
  logic          dmactive;
  logic          debug_req;
  logic          unavailable = 1'b0;
  dm::hartinfo_t hartinfo = '{zero1: 0,
                              nscratch: 2,
                              zero0: 0,
                              dataaccess: 1,
                              datasize: dm::DataCount,
                              dataaddr: dm::DataAddr};
  logic          dmi_rst_n;
  logic          dmi_req_valid;
  logic          dmi_req_ready;
  dm::dmi_req_t  dmi_req;
  logic          dmi_resp_valid;
  logic          dmi_resp_ready;
  dm::dmi_resp_t dmi_resp;
  logic          tdo_o;
  logic          tdo_oe;

  assign tdo = tdo_oe ? tdo_o : 1'bz;

  wb_dm_top wb_dm (
    .clk       (sys_clk),
    .rst_n     (sys_rst_n),
    .testmode  (1'b0),
    .ndmreset  (ndmreset),
    .dmactive  (dmactive),
    .debug_req (debug_req),
    .wbm       (wbm[DM_M]),
    .wbs       (wbs[DM_S]),
    .dmi_rst_n (dmi_rst_n),
    .*);

  dmi_jtag #(
        .IdcodeValue          ( 32'h249511C3    )
    ) dmi_jtag_inst (
    .clk_i            (sys_clk),
    .rst_ni           (sys_rst_n),
    .testmode_i       (1'b0),
    .dmi_rst_no       (dmi_rst_n),
    .dmi_req_o        (dmi_req),
    .dmi_req_valid_o  (dmi_req_valid),
    .dmi_req_ready_i  (dmi_req_ready),
    .dmi_resp_i       (dmi_resp),
    .dmi_resp_ready_o (dmi_resp_ready),
    .dmi_resp_valid_i (dmi_resp_valid),
    .tck_i            (tck),
    .tms_i            (tms),
    .trst_ni          (trst_n),
    .td_i             (tdi),
    .td_o             (tdo_o),
    .tdo_oe_o         (tdo_oe));

   logic 	 unused = &{1'b0, dmactive, 1'b0};
`else
   logic 	 unused = &{1'b0, tck, trst_n, tms, tdi,1'b0};
   logic 	 debug_req;

   assign ndmreset = 1'b0;
   assign debug_req = 1'b0;
   assign tdo = 1'bz;
`endif

  wb_ibex_core #(
    .RV32M(ibex_pkg::RV32MFast),
    .RV32B(ibex_pkg::RV32BBalanced),
    .RegFile(`PRIM_DEFAULT_IMPL == prim_pkg::ImplGeneric ? ibex_pkg::RegFileFF : ibex_pkg::RegFileFPGA)
  ) wb_ibex_core (
    .clk          (sys_clk),
    .rst_n        (sys_rst_n & (~ndmreset)),
    .instr_wb     (wbm[COREI_M]),
    .data_wb      (wbm[CORED_M]),
    .test_en      (1'b0),
    .hart_id      (32'h0),
    .boot_addr    (32'h0),
    .irq_software (1'b0),
    .irq_timer    (1'b0),
    .irq_external (1'b0),
    .irq_fast     (15'b0),
    .irq_nm       (1'b0),
    .debug_req    (debug_req),
    .fetch_enable (4'b1),
    .core_sleep   (),
    .*);

  wb_interconnect_sharedbus #(
    .numm      (NrMaster),
    .nums      (NrSlave),
    .base_addr (wb_base_addr),
    .size      (wb_size)
  ) wb_intercon (.*);

  wb_spramx32 #(
    .size(wb_size[RAM_S]),
    .init_file("spram.mem") //This is the .mem file that our SW build should produce.
  ) wb_spram (
    .wb(wbs[RAM_S]));
  
  wb_gpio #(
    .size (8)
  ) wb_gpio0 (
    .gpio (gpio0),
    .wb (wbs[GPIO0_S]));
  
  wb_gpio #(
    .size (4)
  ) wb_gpio1 (
    .gpio (gpio1),
    .wb (wbs[GPIO1_S]));
  
  wb_wbuart_wrap #(
    .HARDWARE_FLOW_CONTROL_PRESENT  (1'b0),
    .INITIAL_SETUP                  (31'd25),
    .LGFLEN                         (4'd4)
  ) wb_uart (
    .wb (wbs[UART_S]),
    .i_uart_rx (uart_rx),
    .o_uart_tx (uart_tx),
    .i_cts_n   (1'b0),
    .o_rts_n(),
    .o_uart_rx_int(),
    .o_uart_tx_int(),
    .o_uart_rxfifo_int(),
    .o_uart_txfifo_int());

  wb_timer timer (
    .wb (wbs[TIMER_S]));

`ifdef DRAM
  logic pll_locked, sys_rst;

  litedram_wrapper litedram_wrapper_inst (
	.clk(ext_clk), /*External clock is input for LiteDRAM module. On FPGA this is a 100MHz clock, in simulation it's a 50MHz clock.*/
  .rst(~ext_rst_n), /*External reset goes into a reset synchronizer inside the litedram module. The output of that synchronizer is sys_rst.*/
  .sys_clk(sys_clk), /*LiteDRAM outputs 50MHz system clock. On FPGA a divide-by-2 of the ext_clk is done. In simulation sys_clk = ext_clk.*/
	.sys_rst(sys_rst), /*LiteDRAM outputs system reset.*/
	.pll_locked(pll_locked),
`ifdef SYNTHESIS
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
`endif
	.init_done(init_done_led),
	.init_error(init_err_led),
	.wb_ctrl_adr(wbs[DDR_CTRL_S].adr),
	.wb_ctrl_dat_w(wbs[DDR_CTRL_S].dat_m),
	.wb_ctrl_dat_r(wbs[DDR_CTRL_S].dat_s),
	.wb_ctrl_sel(wbs[DDR_CTRL_S].sel),
  .wb_ctrl_stall(wbs[DDR_CTRL_S].stall),
	.wb_ctrl_cyc(wbs[DDR_CTRL_S].cyc),
	.wb_ctrl_stb(wbs[DDR_CTRL_S].stb),
	.wb_ctrl_ack(wbs[DDR_CTRL_S].ack),
	.wb_ctrl_we(wbs[DDR_CTRL_S].we),
	.wb_ctrl_err(wbs[DDR_CTRL_S].err),

  /*Eventually we're going to have two system buses, but for the time being, to allow testing,
   *we hook up both user ports to our one shared bus.
   *Both ports address the same 256MB of DDR memory, one at base address 'h40000000, the other at 'h50000000.*/
	.user_port_wishbone_p_0_adr(wbs[DDR_USR0_S].adr),
	.user_port_wishbone_p_0_dat_w(wbs[DDR_USR0_S].dat_m),
	.user_port_wishbone_p_0_dat_r(wbs[DDR_USR0_S].dat_s),
	.user_port_wishbone_p_0_sel(wbs[DDR_USR0_S].sel),
	.user_port_wishbone_p_0_stall(wbs[DDR_USR0_S].stall),
	.user_port_wishbone_p_0_cyc(wbs[DDR_USR0_S].cyc),
	.user_port_wishbone_p_0_stb(wbs[DDR_USR0_S].stb),
	.user_port_wishbone_p_0_ack(wbs[DDR_USR0_S].ack),
	.user_port_wishbone_p_0_we(wbs[DDR_USR0_S].we),
	.user_port_wishbone_p_0_err(wbs[DDR_USR0_S].err),

	.user_port_wishbone_p_1_adr(wbs[DDR_USR1_S].adr),
	.user_port_wishbone_p_1_dat_w(wbs[DDR_USR1_S].dat_m),
	.user_port_wishbone_p_1_dat_r(wbs[DDR_USR1_S].dat_s),
	.user_port_wishbone_p_1_sel(wbs[DDR_USR1_S].sel),
	.user_port_wishbone_p_1_stall(wbs[DDR_USR1_S].stall),
	.user_port_wishbone_p_1_cyc(wbs[DDR_USR1_S].cyc),
	.user_port_wishbone_p_1_stb(wbs[DDR_USR1_S].stb),
	.user_port_wishbone_p_1_ack(wbs[DDR_USR1_S].ack),
	.user_port_wishbone_p_1_we(wbs[DDR_USR1_S].we),
	.user_port_wishbone_p_1_err(wbs[DDR_USR1_S].err)
  );
  
  //Take system out of reset when litedram deasserts the (synchronized) sys_rst, when the pll is locked.
  assign sys_rst_n = ~sys_rst & pll_locked;
  assign pll_locked_led = pll_locked;

`else //No DRAM:

//LiteDRAM provides the clock generator. Without DRAM we have to provide one here.
`ifdef SYNTHESIS
  //This clkgen does a divide-by-2 of the 100MHz ext_clk => sys_clk runs at 50MHz.
  clkgen_xil7series clkgen (
    .IO_CLK     (ext_clk),
    .IO_RST_N   (ext_rst_n),
    .clk_sys    (sys_clk),
    .rst_sys_n  (sys_rst_n));
`else
  //In simulation ext_clk runs at 50MHz and sys_clk = ext_clk;
  assign sys_clk = ext_clk;
  assign sys_rst_n = ext_rst_n;
`endif //SYNTHESIS/No SYNTHESIS

  assign pll_locked_led = 1'b1;
  assign init_done_led = 1'b1;
  assign init_err_led = 1'b0;
`endif //DRAM/No DRAM.

`ifdef VERA
  vera_top #(`VRAM_SIZE_BYTES) vera_inst(
    .clk(sys_clk),
    .reset(ndmreset | (~sys_rst_n)),
	  .wb_adr(wbs[VERA_S].adr[18:2]),
    .wb_dat_w(wbs[VERA_S].dat_m),
    .wb_dat_r(wbs[VERA_S].dat_s),
    .wb_sel(wbs[VERA_S].sel),
    .wb_stall(wbs[VERA_S].stall),
    .wb_cyc(wbs[VERA_S].cyc),
    .wb_stb(wbs[VERA_S].stb),
    .wb_ack(wbs[VERA_S].ack),
    .wb_we(wbs[VERA_S].we),
    .wb_err(wbs[VERA_S].err),
    .irq_n(),
    .vga_r(vga_r),       
    .vga_g(vga_g),       
    .vga_b(vga_b),       
    .vga_hsync(vga_hsync),
    .vga_vsync(vga_vsync)
    );
`endif //VERA

`ifdef SDSPI
  sdspi #(
		.INITIAL_CLKDIV(8'd62) //FIXME: check: spec says fspi = fclk/2(clkdiv+1), but code comment omits factor 2. Target: 400kHz.
  ) sdspi_inst (
		.i_clk(sys_clk), .i_sd_reset(ndmreset | (~sys_rst_n)),
		// Wishbone interface
		.i_wb_cyc(wbs[SDSPI_S].cyc), .i_wb_stb(wbs[SDSPI_S].stb), .i_wb_we(wbs[SDSPI_S].we),
		.i_wb_addr(wbs[SDSPI_S].adr[3:2]),
		.i_wb_data(wbs[SDSPI_S].dat_m),
		.i_wb_sel(wbs[SDSPI_S].sel),
		.o_wb_stall(wbs[SDSPI_S].stall),
		.o_wb_ack(wbs[SDSPI_S].ack),
		.o_wb_data(wbs[SDSPI_S].dat_s),
		// SDCard interface
		.o_cs_n(sdspi_cs_n), .o_sck(sdspi_sck), .o_mosi(sdspi_mosi),
		.i_miso(sdspi_miso), .i_card_detect(sdspi_card_detect),
		// Interrupt
		.o_int(),
		// .. and whether or not we can use the SPI port
		.i_bus_grant(1'b1),
		// And some wires for debugging it all
		.o_debug()
	);
`endif //SDSPI
endmodule

`resetall
