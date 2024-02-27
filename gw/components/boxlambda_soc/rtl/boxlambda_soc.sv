/*The parameterized BoxLambda SoC.*/
module boxlambda_soc #(
		parameter DPRAM_BYTE_ADDR_MASK = 'h1ffff, /*DPRAM size as a mask value. Used both from CMEM and DMEM.*/
        parameter VRAM_SIZE_BYTES = 131072,
        parameter DEBUG_MODULE_ACTIVE = 1,
        parameter DRAM_ACTIVE = 1,
        parameter VERA_ACTIVE = 1,
        parameter SDSPI_ACTIVE = 1,
        parameter YM2149_ACTIVE = 1,
        parameter PICORV_ACTIVE = 1,
        parameter USB_HID_ACTIVE = 1,
        parameter CMEM_FILE = "",
        parameter DMEM_FILE = ""
    ) (
    input  wire       ext_clk_100, //100MHz external clock.
    input  wire       ext_rst_n,   //External reset pin.
    
`ifdef VERILATOR  
  /*These JTAG signals are not used on FPGA (they are used in simulation).
   *On FPGA, the JTAG signals are driven by a BSCANE2 primitive inside the jtag tap module dmi_bscane_tap.sv.
   */
    input  wire       tck,
    input  wire       trst_n,
    input  wire       tms,
    input  wire       tdi,
    output wire       tdo,
`endif
    output wire       pll_locked_led, //PLL locked indication.
    output wire       init_done_led, //LiteDRAM initialization done indication.
    output wire       init_err_led, //LiteDRAM initialization error indication.
`ifdef SYNTHESIS
    /*The simulation build doesn't export DDR pins.*/
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
	output wire ddram_reset_n,
`endif
    // VGA interface
    output wire  [3:0] vga_r,       
    output wire  [3:0] vga_g,       
    output wire  [3:0] vga_b,       
    output wire        vga_hsync,   
    output wire        vga_vsync,   
    // SDSPI interface
    output wire  sdspi_cs_n, 
    output wire  sdspi_sck, 
    output wire  sdspi_mosi,
	input  wire	 sdspi_miso, 
    input  wire  sdspi_card_detect_n,
    // USB HID, two ports.
    input wire usb0_dm_i, 
    input wire usb0_dp_i,
    output wire usb0_dm_o, 
    output wire usb0_dp_o,
    output wire usb0_oe,
    input wire usb1_dm_i, 
    input wire usb1_dp_i,
    output wire usb1_dm_o, 
    output wire usb1_dp_o,
    output wire usb1_oe,
    
    // Audio interface
    output wire       audio_out,
    output wire       audio_gain,
    output wire       audio_shutdown_n,
`ifdef VERILATOR
    // Audio interface signals only used in simulation
    output wire [15:0] pcm_out,
    output wire acc1_overflow,
    output wire acc2_overflow,  
`endif
    // UART and GPIO
    input  wire       uart_rx,
    output wire       uart_tx,
    inout  wire [7:0] gpio0,
    inout  wire [3:0] gpio1
    );

    //Enum of Bus Masters attached to the cross bar.
    typedef enum {
        COREI_M, /*Ibex CPU instruction port.*/
        CORED_M, /*Ibex CPU data port.*/
        PICORV_M, /*PicoRV DMA.*/
        BLACKBOX_M, /*Black Box Bus Master.*/
        DM_M /*Debug Module Bus Master port.*/
    } wb_xbar_master_e;

    //Enum of Bus Slaves attached to the cross bar.
    typedef enum {
        CMEM_0_S, /*CMEM port 0. Typically used by CPU.*/
        DMEM_0_S, /*DMEM port 0. Typically used by CPU.*/
        CMEM_1_S, /*CMEM port 1. Typically used by DMA.*/
        DMEM_1_S, /*DMEM port 1. Typically used by DMA.*/
        SHARED_BUS_S, /*Connection to the shared bus.*/
        VERA_S, /*VERA slave port.*/
        BLACKBOX_S, /*Black Box Slave.*/
        DDR_USR_S /*LiteDRAM data port.*/
    } wb_xbar_slave_e;

    //Enum of Bus Slaves attached to the shared bus.
    typedef enum {
        GPIO_0_S, /*GPIO 0*/
        GPIO_1_S, /*GPIO 1*/
        SDSPI_S, /*SDSPI*/
        RESET_CTRL_S, /*Reset Control Module.*/
        YM2149_S, /*Dual YM2149 PSG core.*/
        PICORV_S, /*PicoRV DMA slave port.*/
        UART_S, /*UART.*/
        TIMER_S, /*Timer Module.*/
        DDR_CTRL_S, /*LiteDRAM control port.*/
        DM_S, /*Debug Module slave port.*/
        USB_HID_0_S, /*USB HID keyboard or mouse*/
        USB_HID_1_S  /*USB HID keyboard or mouse*/
    } wb_shared_bus_slave_e;

    /*We used a word-addressing Wishbone bus. The Dual Port RAM word address bus width is equal to the 
     *number of bits of the Dual Port RAM Byte Address mask minus 2.*/
    localparam DPRAM_AW = $clog2(DPRAM_BYTE_ADDR_MASK)-2;

    localparam AW=28; //Wishbone Bus address width. Note that we use word addressing.
    localparam DW=32; //Wishbone Bus data width.
    localparam NUM_XBAR_MASTERS = 5;
    localparam NUM_XBAR_SLAVES  = 8;
    localparam NUM_SHARED_BUS_MASTERS = 1;
    localparam NUM_SHARED_BUS_SLAVES = 12;

    localparam PICORV_BASE_ADDRESS = 'h10002000;

    //Shared bus slave addresses. Right shift by two to convert byte address values to word address values.
    localparam [NUM_SHARED_BUS_SLAVES*AW-1:0] SHARED_BUS_SLAVE_ADDRS = {
        /*USB_HID_1*/       {AW'('h10000080>>2)},
        /*USB_HID_0*/       {AW'('h10000040>>2)},
        /*DM_S*/            {AW'('h10040000>>2)},
        /*DDR_CTRL_S*/      {AW'('h10030000>>2)},
        /*TIMER_S*/         {AW'('h10020000>>2)},
        /*UART_S*/          {AW'('h10010000>>2)},
        /*PICORV_S*/        {AW'(PICORV_BASE_ADDRESS>>2)},
        /*YM2149_S*/        {AW'('h10001000>>2)},
        /*RESET_CTRL_S*/    {AW'('h10000030>>2)},
        /*SDSPI_S*/         {AW'('h10000020>>2)},
        /*GPIO_1_S*/        {AW'('h10000010>>2)},
        /*GPIO_0_S*/        {AW'('h10000000>>2)}
    };

    //Shared bus slave address mask. Right-shift by two to convert byte size to word size.
    localparam [NUM_SHARED_BUS_SLAVES*AW-1:0] SHARED_BUS_SLAVE_ADDR_MASKS = {
        /*USB_HID_1_S*/     {AW'(~('h0000003f>>2))}, 
        /*USB_HID_0_S*/     {AW'(~('h0000003f>>2))}, 
        /*DM_S*/            {AW'(~('h0000ffff>>2))},   
        /*DDR_CTRL_S*/      {AW'(~('h0000ffff>>2))},
        /*TIMER_S*/         {AW'(~('h0000000f>>2))},
        /*UART_S*/          {AW'(~('h0000000f>>2))},
        /*PICORV_S*/        {AW'(~('h00001fff>>2))},
        /*YM2149_S*/        {AW'(~('h000003ff>>2))},
        /*RESET_CTRL_S*/    {AW'(~('h00000007>>2))},
        /*SDSPI_S*/         {AW'(~('h0000000f>>2))},
        /*GPIO_1_S*/        {AW'(~('h0000000f>>2))},
        /*GPIO_0_S*/        {AW'(~('h0000000f>>2))}
    };

    //Crossbar slave addresses. Right shift by two to convert byte address values to word address values.
    localparam [NUM_XBAR_SLAVES*AW-1:0] XBAR_SLAVE_ADDRS = {
        /*DDR_USR_S*/       {AW'('h20000000>>2)},
        /*BLACKBOX_S*/      {AW'('h10200000>>2)},
        /*VERA_S*/          {AW'('h10100000>>2)},
        /*SHARED_BUS_S*/    {AW'('h10000000>>2)},
        /*DMEM_1_S*/        {AW'('h00120000>>2)},
        /*CMEM_1_S*/        {AW'('h00100000>>2)},
        /*DMEM_0_S*/        {AW'('h00020000>>2)},
        /*CMEM_0_S*/        {AW'('h00000000>>2)}
    };

    //Crossbar slave address mask. Right-shift by two to convert byte size to word size.
    localparam [NUM_XBAR_SLAVES*AW-1:0]	XBAR_SLAVE_ADDR_MASKS = {
        /*DDR_USR_S*/       {AW'(~('h0fffffff>>2))},
        /*BLACKBOX_S*/      {AW'(~('h000fffff>>2))},
        /*VERA_S*/          {AW'(~('h0007ffff>>2))},
        /*SHARED_BUS_S*/    {AW'(~('h000fffff>>2))},
        /*DMEM_1_S*/        {AW'(~(DPRAM_BYTE_ADDR_MASK>>2))},
        /*CMEM_1_S*/        {AW'(~(DPRAM_BYTE_ADDR_MASK>>2))},
        /*DMEM_0_S*/        {AW'(~(DPRAM_BYTE_ADDR_MASK>>2))},
        /*CMEM_0_S*/        {AW'(~(DPRAM_BYTE_ADDR_MASK>>2))}
    };

    //Clock signals.
    logic sys_clk, sys_clk_2x, usb_clk, clk_50, clk_100;
    //PLL lock signals.
    logic usb_pll_locked, sys_pll_locked, pre_pll_locked, litedram_pll_locked;

    //ndm_reset_req: Non-Debug Module reset requested by Debug Module
    //ndm_reset: Non-Debug-Module-Reset issued by Reset Controller i.e. reset everything except Debug Module.
    //dm_reset: Debug-Module Reset issued by Reset Controller.
    //usbreset: Reset of the USB clock domain.
    logic ndm_reset_req, ndm_reset, dm_reset, usb_reset;
    logic por_completed; //Indicates Power-On Reset has been completed.
	logic debug_req; //Debug Request signal.

    //The crossbar wishbone bus master interfaces.
    wb_if xbar_wbm[NUM_XBAR_MASTERS](.rst(ndm_reset), .clk(sys_clk));
    //The crossbar wishbone bus slave interfaces.
    wb_if xbar_wbs[NUM_XBAR_SLAVES](.rst(dm_reset), .clk(sys_clk));
    //The shared bus wishbone bus slave interfaces. (I didn't bother to create a shared bus bus master interface).
    wb_if shared_bus_wbs[NUM_SHARED_BUS_SLAVES](.rst(dm_reset), .clk(sys_clk));

    //The bus master port vectors of the crossbar wbxbar instance.
	logic	[NUM_XBAR_MASTERS-1:0]	xbar_mcyc, xbar_mstb, xbar_mwe;
	logic	[NUM_XBAR_MASTERS*AW-1:0]	xbar_maddr;
	logic	[NUM_XBAR_MASTERS*DW-1:0]   xbar_mdata_w;
	logic	[NUM_XBAR_MASTERS*DW/8-1:0]	xbar_msel;
	
	logic	[NUM_XBAR_MASTERS-1:0]	xbar_mstall;
	logic	[NUM_XBAR_MASTERS-1:0]	xbar_mack;
	logic	[NUM_XBAR_MASTERS*DW-1:0]	xbar_mdata_r;
	logic	[NUM_XBAR_MASTERS-1:0]	xbar_merr;
	
    //The bus slave port vectors of the crossbar wbxbar instance.
	logic	[NUM_XBAR_SLAVES-1:0]	xbar_scyc, xbar_sstb, xbar_swe;
	logic	[NUM_XBAR_SLAVES*AW-1:0] xbar_saddr;
	logic	[NUM_XBAR_SLAVES*DW-1:0] xbar_sdata_w;
	logic	[NUM_XBAR_SLAVES*DW/8-1:0]	xbar_ssel;
	
	logic	[NUM_XBAR_SLAVES-1:0]	xbar_sstall, xbar_sack;
	logic	[NUM_XBAR_SLAVES*DW-1:0]	xbar_sdata_r;
	logic	[NUM_XBAR_SLAVES-1:0]	xbar_serr;

    //The bus master port vectors of the shared bus wbxbar instance.
	logic	[NUM_SHARED_BUS_MASTERS-1:0]	shared_bus_mcyc, shared_bus_mstb, shared_bus_mwe;
	logic	[NUM_SHARED_BUS_MASTERS*AW-1:0]	shared_bus_maddr;
	logic	[NUM_SHARED_BUS_MASTERS*DW-1:0]   shared_bus_mdata_w;
	logic	[NUM_SHARED_BUS_MASTERS*DW/8-1:0]	shared_bus_msel;
	
	logic	[NUM_SHARED_BUS_MASTERS-1:0]	shared_bus_mstall;
	logic	[NUM_SHARED_BUS_MASTERS-1:0]	shared_bus_mack;
	logic	[NUM_SHARED_BUS_MASTERS*DW-1:0]	shared_bus_mdata_r;
	logic	[NUM_SHARED_BUS_MASTERS-1:0]	shared_bus_merr;
	
    //The bus slave port vectors of the shared bus wbxbar instance.
	logic	[NUM_SHARED_BUS_SLAVES-1:0]	shared_bus_scyc, shared_bus_sstb, shared_bus_swe;
	logic	[NUM_SHARED_BUS_SLAVES*AW-1:0] shared_bus_saddr;
	logic	[NUM_SHARED_BUS_SLAVES*DW-1:0] shared_bus_sdata_w;
	logic	[NUM_SHARED_BUS_SLAVES*DW/8-1:0]	shared_bus_ssel;
	
	logic	[NUM_SHARED_BUS_SLAVES-1:0]	shared_bus_sstall, shared_bus_sack;
	logic	[NUM_SHARED_BUS_SLAVES*DW-1:0]	shared_bus_sdata_r;
	logic	[NUM_SHARED_BUS_SLAVES-1:0]	shared_bus_serr;

    generate
        //Connect the slaves to the shared bus. 
        genvar ii;
        for(ii=0; ii<NUM_SHARED_BUS_SLAVES; ii=ii+1)
        begin : CONNECT_SLAVES_TO_SHARED_BUS
            assign shared_bus_wbs[ii].cyc = shared_bus_scyc[ii];
            assign shared_bus_wbs[ii].stb = shared_bus_sstb[ii];
            assign shared_bus_wbs[ii].we = shared_bus_swe[ii];
            assign shared_bus_wbs[ii].adr = shared_bus_saddr[(ii+1)*AW-1:ii*AW];
            assign shared_bus_wbs[ii].dat_m = shared_bus_sdata_w[(ii+1)*DW-1:ii*DW];
            assign shared_bus_wbs[ii].sel = shared_bus_ssel[(ii+1)*DW/8-1:ii*DW/8];

            assign shared_bus_sstall[ii] = shared_bus_wbs[ii].stall;  
            assign shared_bus_sack[ii] = shared_bus_wbs[ii].ack;         
            assign shared_bus_sdata_r[(ii+1)*DW-1:ii*DW] = shared_bus_wbs[ii].dat_s;
            assign shared_bus_serr[ii] = shared_bus_wbs[ii].err;         
        end

        //Connect the slaves to the crossbar.
        for(ii=0; ii<NUM_XBAR_SLAVES; ii=ii+1)
        begin : CONNECT_SLAVES_TO_XBAR
            assign xbar_wbs[ii].cyc = xbar_scyc[ii];
            assign xbar_wbs[ii].stb = xbar_sstb[ii];
            assign xbar_wbs[ii].we = xbar_swe[ii];
            assign xbar_wbs[ii].adr = xbar_saddr[(ii+1)*AW-1:ii*AW];
            assign xbar_wbs[ii].dat_m = xbar_sdata_w[(ii+1)*DW-1:ii*DW];
            assign xbar_wbs[ii].sel = xbar_ssel[(ii+1)*DW/8-1:ii*DW/8];

            assign xbar_sstall[ii] = xbar_wbs[ii].stall;  
            assign xbar_sack[ii] = xbar_wbs[ii].ack;         
            assign xbar_sdata_r[(ii+1)*DW-1:ii*DW] = xbar_wbs[ii].dat_s;
            assign xbar_serr[ii] = xbar_wbs[ii].err;         
        end

        //Connect the masters to the crossbar.
        for(ii=0; ii<NUM_XBAR_MASTERS; ii=ii+1)
        begin : CONNECT_MASTERS_TO_XBAR
            assign xbar_mcyc[ii] = xbar_wbm[ii].cyc;
            assign xbar_mstb[ii] = xbar_wbm[ii].stb;
            assign xbar_mwe[ii] = xbar_wbm[ii].we;
            assign xbar_maddr[(ii+1)*AW-1:ii*AW] = xbar_wbm[ii].adr;
            assign xbar_mdata_w[(ii+1)*DW-1:ii*DW] = xbar_wbm[ii].dat_m;
            assign xbar_msel[(ii+1)*DW/8-1:ii*DW/8] = xbar_wbm[ii].sel;

            assign xbar_wbm[ii].stall = xbar_mstall[ii];  
            assign xbar_wbm[ii].ack = xbar_mack[ii];         
            assign xbar_wbm[ii].dat_s = xbar_mdata_r[(ii+1)*DW-1:ii*DW];
            assign xbar_wbm[ii].err = xbar_merr[ii];         
        end
    endgenerate

    //The crossbar wbxbar instance.
    wbxbar #(
		.NM(NUM_XBAR_MASTERS), .NS(NUM_XBAR_SLAVES),
		.AW(AW), .DW(32),
		.SLAVE_ADDR(XBAR_SLAVE_ADDRS),
		.SLAVE_MASK(XBAR_SLAVE_ADDR_MASKS),
        .LGMAXBURST(3),
		.OPT_DBLBUFFER(1'b0),
		.OPT_LOWPOWER(1'b0)) 
    wb_xbar (
		.i_clk(sys_clk), .i_reset(ndm_reset),
		// Here are the bus inputs from each of the WB bus masters
		.i_mcyc(xbar_mcyc), .i_mstb(xbar_mstb), .i_mwe(xbar_mwe),
		.i_maddr(xbar_maddr),
		.i_mdata(xbar_mdata_w),
		.i_msel(xbar_msel),
		// .... and their return data
		.o_mstall(xbar_mstall),
		.o_mack(xbar_mack),
		.o_mdata(xbar_mdata_r),
		.o_merr(xbar_merr),
		// Here are the output ports, used to control each of the
		// various slave ports that we are connected to
		.o_scyc(xbar_scyc), .o_sstb(xbar_sstb), .o_swe(xbar_swe),
		.o_saddr(xbar_saddr),
		.o_sdata(xbar_sdata_w),
		.o_ssel(xbar_ssel),
		// ... and their return data back to us.
		.i_sstall(xbar_sstall), .i_sack(xbar_sack),
		.i_sdata(xbar_sdata_r),
		.i_serr(xbar_serr)
    );

    /*Xbar to shared bus signals*/
    assign shared_bus_mcyc = xbar_scyc[SHARED_BUS_S];
    assign shared_bus_mstb = xbar_sstb[SHARED_BUS_S]; 
    assign shared_bus_mwe = xbar_swe[SHARED_BUS_S];
	assign shared_bus_maddr = xbar_saddr[(SHARED_BUS_S+1)*AW-1:SHARED_BUS_S*AW];
	assign shared_bus_mdata_w = xbar_sdata_w[(SHARED_BUS_S+1)*DW-1:SHARED_BUS_S*DW];
	assign shared_bus_msel = xbar_ssel[(SHARED_BUS_S+1)*DW/8-1:SHARED_BUS_S*DW/8];
	
    /*Shared bus to Xbar signals*/
	assign xbar_sstall[SHARED_BUS_S] = shared_bus_mstall;
	assign xbar_sack[SHARED_BUS_S] = shared_bus_mack;
	assign xbar_sdata_r[(SHARED_BUS_S+1)*DW-1:SHARED_BUS_S*DW] = shared_bus_mdata_r;
	assign xbar_serr[SHARED_BUS_S] = shared_bus_merr;

    //The shared bus wbxbar instance.
    wbxbar #(
		.NM(NUM_SHARED_BUS_MASTERS), .NS(NUM_SHARED_BUS_SLAVES),
		.AW(AW), .DW(32),
		.SLAVE_ADDR(SHARED_BUS_SLAVE_ADDRS),
		.SLAVE_MASK(SHARED_BUS_SLAVE_ADDR_MASKS),
        .LGMAXBURST(3),
		.OPT_DBLBUFFER(1'b0),
		.OPT_LOWPOWER(1'b0)) 
    wb_shared_bus (
		.i_clk(sys_clk), .i_reset(ndm_reset),
		// Here are the bus inputs from each of the WB bus masters
		.i_mcyc(shared_bus_mcyc), .i_mstb(shared_bus_mstb), .i_mwe(shared_bus_mwe),
		.i_maddr(shared_bus_maddr),
		.i_mdata(shared_bus_mdata_w),
		.i_msel(shared_bus_msel),
		// .... and their return data
		.o_mstall(shared_bus_mstall),
		.o_mack(shared_bus_mack),
		.o_mdata(shared_bus_mdata_r),
		.o_merr(shared_bus_merr),
		// Here are the output ports, used to control each of the
		// various slave ports that we are connected to
		.o_scyc(shared_bus_scyc), .o_sstb(shared_bus_sstb), .o_swe(shared_bus_swe),
		.o_saddr(shared_bus_saddr),
		.o_sdata(shared_bus_sdata_w),
		.o_ssel(shared_bus_ssel),
		// ... and their return data back to us.
		.i_sstall(shared_bus_sstall), .i_sack(shared_bus_sack),
		.i_sdata(shared_bus_sdata_r),
		.i_serr(shared_bus_serr)
    );

    //Reset Controller
    reset_ctrl reset_ctrl_inst(
        .sys_clk(sys_clk), //50MHz system clock
        .usb_clk(usb_clk), //12MHz USB clock
        .sys_pll_locked_i(sys_pll_locked), //System Clock PLL locked indication (input)
        .usb_pll_locked_i(usb_pll_locked), //USB Clock PLL locked indication (input)
        .ndm_reset_i(ndm_reset_req), //non-debug-module reset request input
        .ext_reset_i(~ext_rst_n), //asynchronous external reset input
        .ndm_reset_o(ndm_reset), //non-debug-module domain reset ouput
        .dm_reset_o(dm_reset), //debug-module domain reset output
        .usb_reset_o(usb_reset), //usb domain reset output
        .por_completed_o(por_completed), //Power-On-Reset completion indication (output).
        //32-bit pipelined Wishbone slave interface.
        .wb_adr(shared_bus_wbs[RESET_CTRL_S].adr[0]),
        .wb_dat_w(shared_bus_wbs[RESET_CTRL_S].dat_m),
        .wb_dat_r(shared_bus_wbs[RESET_CTRL_S].dat_s),
        .wb_sel(shared_bus_wbs[RESET_CTRL_S].sel),
        .wb_stall(shared_bus_wbs[RESET_CTRL_S].stall),
        .wb_cyc(shared_bus_wbs[RESET_CTRL_S].cyc),
        .wb_stb(shared_bus_wbs[RESET_CTRL_S].stb),
        .wb_ack(shared_bus_wbs[RESET_CTRL_S].ack),
        .wb_we(shared_bus_wbs[RESET_CTRL_S].we),
        .wb_err(shared_bus_wbs[RESET_CTRL_S].err)
    );

    /*First-stage clock generator. If LiteDRAM is synthesized-in, it includes a Second-stage clock generator.
     *If LiteDRAM is not synthesized-in, this first-stage clock generator provides the system clock
     */
    boxlambda_clk_gen clkgen (
        .ext_clk_100 (ext_clk_100), //100MHz external clock input.
        .rst_n       (1'b1),
        .clk_50      (clk_50), //50MHz clock output
        .clk_100     (clk_100), //100MHz clock output
        .clk_12      (usb_clk), //12 MHz USB clock output
        .locked      (pre_pll_locked) //PLL lock indication outpt. It's called pre-PLL because LiteDRAM (when included in the build)
                                      //introduces a second-stage PLL hanging off clk_50. The LiteDRAM PLL provides the system clock.
                                      //When LiteDRAM is not included in the build, clk_50 becomes the system clock.
    );

    assign usb_pll_locked = pre_pll_locked;
    
    /*The Debug Modules.*/
    generate if (DEBUG_MODULE_ACTIVE)
    begin : GENERATE_DEBUG_MODULE
        logic          dmactive;
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

`ifdef VERILATOR
        logic          tdo_o;
        logic          tdo_oe;

        assign tdo = tdo_oe ? tdo_o : 1'bz;
`endif

        wb_dm_top wb_dm (
            .clk       (sys_clk),
            .rst_n     (~dm_reset),
            .testmode  (1'b0),
            .ndmreset  (ndm_reset_req),
            .dmactive  (dmactive),
            .debug_req (debug_req),
            .wbm       (xbar_wbm[DM_M]),
            .wbs       (shared_bus_wbs[DM_S]),
            .dmi_rst_n (dmi_rst_n),
            .*);

        dmi_jtag #(
            .IdcodeValue(32'h249511C3)
        ) dmi_jtag_inst (
            .clk_i            (sys_clk),
            .rst_ni           (~dm_reset),
            .testmode_i       (1'b0),
            .dmi_rst_no       (dmi_rst_n),
            .dmi_req_o        (dmi_req),
            .dmi_req_valid_o  (dmi_req_valid),
            .dmi_req_ready_i  (dmi_req_ready),
            .dmi_resp_i       (dmi_resp),
            .dmi_resp_ready_o (dmi_resp_ready),
            .dmi_resp_valid_i (dmi_resp_valid),
`ifdef VERILATOR
            .tck_i            (tck),
            .tms_i            (tms),
            .trst_ni          (trst_n),
            .td_i             (tdi),
            .td_o             (tdo_o),
            .tdo_oe_o         (tdo_oe)
`else
            .tck_i            (1'b0),
            .tms_i            (1'b0),
            .trst_ni          (1'b1),
            .td_i             (1'b0),
            .td_o             (),
            .tdo_oe_o         ()
`endif
        );

        logic unused = &{1'b0, dmactive, 1'b0};
    end
    else begin /*DEBUG_MODULE_ACTIVE==0*/
        assign ndm_reset_req = 1'b0;
        assign debug_req = 1'b0;
    end
    endgenerate

    /*The Ibex CPU.*/
    wb_ibex_core #(
        .RV32M(ibex_pkg::RV32MFast),
        .RV32B(ibex_pkg::RV32BBalanced),
        .RegFile(`PRIM_DEFAULT_IMPL == prim_pkg::ImplGeneric ? ibex_pkg::RegFileFF : ibex_pkg::RegFileFPGA),
        .DmHaltAddr({2'b00, SHARED_BUS_SLAVE_ADDRS[(DM_S+1)*AW-1:DM_S*AW], 2'b00} + 32'h00000800),
        .DmExceptionAddr({2'b00, SHARED_BUS_SLAVE_ADDRS[(DM_S+1)*AW-1:DM_S*AW], 2'b00}+32'h00000808)
    ) wb_ibex_core (
        .clk          (sys_clk),
        .rst_n        (~ndm_reset),
        .instr_wb     (xbar_wbm[COREI_M]),
        .data_wb      (xbar_wbm[CORED_M]),
        .test_en      (1'b0),
        .hart_id      (32'h0),
        .boot_addr    (32'h0),
        .irq_software (1'b0),
        .irq_timer    (1'b0),
        .irq_external (1'b0),
        .irq_fast     (15'b0),
        .irq_nm       (1'b0),
        .debug_req    (debug_req),
        .fetch_enable ({3'b0, por_completed}), //Only start fetch after POR has been completed.
        .core_sleep   (),
        .*);
    
    //CMEM (Code Mem.) Dual Port Memory
    wb_dp_ram_wrapper #(
        .ADDR_WIDTH(DPRAM_AW),
        .INIT_FILE(CMEM_FILE)
    ) cmem (
        .clk(sys_clk),
        .rst(ndm_reset),

        // port A
        .a_adr_i(xbar_wbs[CMEM_0_S].adr[DPRAM_AW-1:0]),   // ADR_I() address
        .a_dat_i(xbar_wbs[CMEM_0_S].dat_m),   // DAT_I() data in
        .a_dat_o(xbar_wbs[CMEM_0_S].dat_s),   // DAT_O() data out
        .a_we_i(xbar_wbs[CMEM_0_S].we),    // WE_I write enable input
        .a_sel_i(xbar_wbs[CMEM_0_S].sel),   // SEL_I() select input
        .a_stb_i(xbar_wbs[CMEM_0_S].stb),   // STB_I strobe input
        .a_stall_o(xbar_wbs[CMEM_0_S].stall), // STALL_O stall output
        .a_ack_o(xbar_wbs[CMEM_0_S].ack),   // ACK_O acknowledge output
        .a_err_o(xbar_wbs[CMEM_0_S].err),   // ERR_O error output
        .a_cyc_i(xbar_wbs[CMEM_0_S].cyc),   // CYC_I cycle input

        // port B
        .b_adr_i(xbar_wbs[CMEM_1_S].adr[DPRAM_AW-1:0]),   // ADR_I() address
        .b_dat_i(xbar_wbs[CMEM_1_S].dat_m),   // DAT_I() data in
        .b_dat_o(xbar_wbs[CMEM_1_S].dat_s),   // DAT_O() data out
        .b_we_i(xbar_wbs[CMEM_1_S].we),    // WE_I write enable input
        .b_sel_i(xbar_wbs[CMEM_1_S].sel),   // SEL_I() select input
        .b_stb_i(xbar_wbs[CMEM_1_S].stb),   // STB_I strobe input
        .b_stall_o(xbar_wbs[CMEM_1_S].stall), // STALL_O stall output
        .b_ack_o(xbar_wbs[CMEM_1_S].ack),   // ACK_O acknowledge output
        .b_err_o(xbar_wbs[CMEM_1_S].err),   // ERR_O error output
        .b_cyc_i(xbar_wbs[CMEM_1_S].cyc)    // CYC_I cycle input
    );

    //DMEM (Data Mem.) Dual Port Memory.
    wb_dp_ram_wrapper #(
        .ADDR_WIDTH(DPRAM_AW),
        .INIT_FILE(DMEM_FILE)
    ) dmem (
        .clk(sys_clk),
        .rst(ndm_reset),

        // port A
        .a_adr_i(xbar_wbs[DMEM_0_S].adr[DPRAM_AW-1:0]),   // ADR_I() address
        .a_dat_i(xbar_wbs[DMEM_0_S].dat_m),   // DAT_I() data in
        .a_dat_o(xbar_wbs[DMEM_0_S].dat_s),   // DAT_O() data out
        .a_we_i(xbar_wbs[DMEM_0_S].we),    // WE_I write enable input
        .a_sel_i(xbar_wbs[DMEM_0_S].sel),   // SEL_I() select input
        .a_stb_i(xbar_wbs[DMEM_0_S].stb),   // STB_I strobe input
        .a_stall_o(xbar_wbs[DMEM_0_S].stall), // STALL_O stall output
        .a_ack_o(xbar_wbs[DMEM_0_S].ack),   // ACK_O acknowledge output
        .a_err_o(xbar_wbs[DMEM_0_S].err),   // ERR_O error output
        .a_cyc_i(xbar_wbs[DMEM_0_S].cyc),   // CYC_I cycle input

        // port B
        .b_adr_i(xbar_wbs[DMEM_1_S].adr[DPRAM_AW-1:0]),   // ADR_I() address
        .b_dat_i(xbar_wbs[DMEM_1_S].dat_m),   // DAT_I() data in
        .b_dat_o(xbar_wbs[DMEM_1_S].dat_s),   // DAT_O() data out
        .b_we_i(xbar_wbs[DMEM_1_S].we),    // WE_I write enable input
        .b_sel_i(xbar_wbs[DMEM_1_S].sel),   // SEL_I() select input
        .b_stb_i(xbar_wbs[DMEM_1_S].stb),   // STB_I strobe input
        .b_stall_o(xbar_wbs[DMEM_1_S].stall), // STALL_O stall output
        .b_ack_o(xbar_wbs[DMEM_1_S].ack),   // ACK_O acknowledge output
        .b_err_o(xbar_wbs[DMEM_1_S].err),   // ERR_O error output
        .b_cyc_i(xbar_wbs[DMEM_1_S].cyc)    // CYC_I cycle input
    );

    //Black Box Placeholder - does nothing.
    blackbox bb_inst (
        .wbm(xbar_wbm[BLACKBOX_M]),
        .wbs(xbar_wbs[BLACKBOX_S]));

    //The UART.
    wb_wbuart_wrap #(
        .HARDWARE_FLOW_CONTROL_PRESENT  (1'b0),
        .INITIAL_SETUP                  (31'd25),
        .LGFLEN                         (4'd4)
    ) wb_uart (
        .wb (shared_bus_wbs[UART_S]),
        .i_uart_rx (uart_rx),
        .o_uart_tx (uart_tx),
        .i_cts_n   (1'b0),
        .o_rts_n(),
        .o_uart_rx_int(),
        .o_uart_tx_int(),
        .o_uart_rxfifo_int(),
        .o_uart_txfifo_int());

    //The Timer module.
    wb_timer timer (
        .wb (shared_bus_wbs[TIMER_S]));

    //Two GPIO modules.
    wb_gpio #(
        .size (8)
    ) wb_gpio0 (
        .gpio (gpio0),
        .wb (shared_bus_wbs[GPIO_0_S]));
    
    wb_gpio #(
        .size (4)
    ) wb_gpio1 (
        .gpio (gpio1),
        .wb (shared_bus_wbs[GPIO_1_S]));

    //LiteDRAM.
    generate if (DRAM_ACTIVE)
    begin : GENERATE_DRAM_MODULE
        logic litedram_pll_locked_i, litedram_rst_o;

        litedram_wrapper litedram_wrapper_inst (
            .clk(clk_100), /*100MHz input clock, coming for the First-stage clock generator..*/
            .rst(1'b0), /*Never reset LiteDRAM.*/
            .sys_clkx2(sys_clk_2x), /*LiteDRAM outputs 100MHz double rate system clock. In phase with sys_clk.*/
            .sys_clk(sys_clk), /*LiteDRAM outputs 50MHz system clock.*/
            .sys_rst(litedram_rst_o), /*LiteDRAM outputs system reset.*/
            .pll_locked(litedram_pll_locked_i), /*LiteDRAM PLL lock indication.*/
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
            .wb_ctrl_adr(shared_bus_wbs[DDR_CTRL_S].adr),
            .wb_ctrl_dat_w(shared_bus_wbs[DDR_CTRL_S].dat_m),
            .wb_ctrl_dat_r(shared_bus_wbs[DDR_CTRL_S].dat_s),
            .wb_ctrl_sel(shared_bus_wbs[DDR_CTRL_S].sel),
            .wb_ctrl_stall(shared_bus_wbs[DDR_CTRL_S].stall),
            .wb_ctrl_cyc(shared_bus_wbs[DDR_CTRL_S].cyc),
            .wb_ctrl_stb(shared_bus_wbs[DDR_CTRL_S].stb),
            .wb_ctrl_ack(shared_bus_wbs[DDR_CTRL_S].ack),
            .wb_ctrl_we(shared_bus_wbs[DDR_CTRL_S].we),
            .wb_ctrl_err(shared_bus_wbs[DDR_CTRL_S].err),

            /*The used port addresses 256MB of DDR memory.*/
            .user_port_wishbone_p_0_adr(xbar_wbs[DDR_USR_S].adr),
            .user_port_wishbone_p_0_dat_w(xbar_wbs[DDR_USR_S].dat_m),
            .user_port_wishbone_p_0_dat_r(xbar_wbs[DDR_USR_S].dat_s),
            .user_port_wishbone_p_0_sel(xbar_wbs[DDR_USR_S].sel),
            .user_port_wishbone_p_0_stall(xbar_wbs[DDR_USR_S].stall),
            .user_port_wishbone_p_0_cyc(xbar_wbs[DDR_USR_S].cyc),
            .user_port_wishbone_p_0_stb(xbar_wbs[DDR_USR_S].stb),
            .user_port_wishbone_p_0_ack(xbar_wbs[DDR_USR_S].ack),
            .user_port_wishbone_p_0_we(xbar_wbs[DDR_USR_S].we),
            .user_port_wishbone_p_0_err(xbar_wbs[DDR_USR_S].err)
        );
        
        /*sys_pll_locked is fed to the reset controller. It's asserted when Litedram controller indicates reset is 
         *deasserted and the PLL is locked.*/
        assign litedram_pll_locked = ~litedram_rst_o & litedram_pll_locked_i;
        assign sys_pll_locked = litedram_pll_locked;
    end
    else begin //No DRAM: In this case the Stage-1 clock generator provides the system clock.
        assign sys_clk = clk_50; //50MHz system clock.
        assign sys_clk_2x = clk_100; //100MHz double-rate system clock.
        assign litedram_pll_locked = 1'b1;
        assign init_done_led = 1'b1;
        assign init_err_led = 1'b0;
        assign sys_pll_locked = pre_pll_locked;
`ifdef SYNTHESIS
        //Shut up, DRC.
        OBUFDS OBUFDS(
    	    .I(1'b0),
	        .O(ddram_clk_p),
	.       OB(ddram_clk_n)
        );
`endif
    end
    endgenerate

    assign pll_locked_led = sys_pll_locked;
    
    //Vera Graphics.
    generate if (VERA_ACTIVE)
    begin : GENERATE_VERA_MODULE
        vera_top #(VRAM_SIZE_BYTES) vera_inst(
            .clk(sys_clk),
            .reset(ndm_reset),
            .wb_adr(xbar_wbs[VERA_S].adr[16:0]),
            .wb_dat_w(xbar_wbs[VERA_S].dat_m),
            .wb_dat_r(xbar_wbs[VERA_S].dat_s),
            .wb_sel(xbar_wbs[VERA_S].sel),
            .wb_stall(xbar_wbs[VERA_S].stall),
            .wb_cyc(xbar_wbs[VERA_S].cyc),
            .wb_stb(xbar_wbs[VERA_S].stb),
            .wb_ack(xbar_wbs[VERA_S].ack),
            .wb_we(xbar_wbs[VERA_S].we),
            .wb_err(xbar_wbs[VERA_S].err),
            .irq_n(),
            .vga_r(vga_r),       
            .vga_g(vga_g),       
            .vga_b(vga_b),       
            .vga_hsync(vga_hsync),
            .vga_vsync(vga_vsync)
        );
    end
    else begin : NO_VGA
        assign vga_r = 4'b0;       
        assign vga_g = 4'b0;     
        assign vga_b = 4'b0;       
        assign vga_hsync = 1'b0;   
        assign vga_vsync = 1'b0;   
    end
    endgenerate

    //SDSPI module.
    generate if (SDSPI_ACTIVE)
    begin : GENERATE_SDSPI_MODULE
        sdspi #(.OPT_LITTLE_ENDIAN(1'b1)) sdspi_inst (
                .i_clk(sys_clk), 
                .i_sd_reset(ndm_reset),
                // Wishbone interface
                .i_wb_cyc(shared_bus_wbs[SDSPI_S].cyc), .i_wb_stb(shared_bus_wbs[SDSPI_S].stb), .i_wb_we(shared_bus_wbs[SDSPI_S].we),
                .i_wb_addr(shared_bus_wbs[SDSPI_S].adr[1:0]),
                .i_wb_data(shared_bus_wbs[SDSPI_S].dat_m),
                .i_wb_sel(shared_bus_wbs[SDSPI_S].sel),
                .o_wb_stall(shared_bus_wbs[SDSPI_S].stall),
                .o_wb_ack(shared_bus_wbs[SDSPI_S].ack),
                .o_wb_data(shared_bus_wbs[SDSPI_S].dat_s),
                // SDCard interface
                .o_cs_n(sdspi_cs_n), .o_sck(sdspi_sck), .o_mosi(sdspi_mosi),
                .i_miso(sdspi_miso), .i_card_detect(~sdspi_card_detect_n),
                // Interrupt
                .o_int(),
                // .. and whether or not we can use the SPI port
                .i_bus_grant(1'b1),
                // And some wires for debugging it all
                .o_debug()
            );

        assign shared_bus_wbs[SDSPI_S].err = 1'b0;
    end
    else begin : NO_SDSPI
        assign sdspi_cs_n = 1'b0; 
        assign sdspi_sck = 1'b0; 
        assign sdspi_mosi = 1'b0;
    end
    endgenerate

    //The Dual YM2149 PSG core and 1-bit audio DAC.
    generate if (YM2149_ACTIVE)
    begin : GENERATE_YM2149_MODULE
        wire signed  [15:0] ym2149_sound;

        YM2149_PSG_system_wb #(
            .CLK_IN_HZ(50000000),   // Input clock frequency
            .CLK_PSG_HZ(2000000),   // PSG clock frequency
            .YM2149_DAC_BITS(10),   // PSG DAC bit precision, 8 through 14 bits, the higher the bits, the higher the dynamic range.
                                    // 10 bits almost perfectly replicates the YM2149 DA converter's Normalized voltage.
                                    // With 8 bits, the lowest volumes settings will be slightly louder than normal.
                                    // With 12 bits, the lowest volume settings will be too quiet.
            .MIXER_DAC_BITS(16)     // The number of DAC bits for the BHG_jt49_filter_mixer output.
        ) ym2149_sys_inst (
            .clk(sys_clk),
            .clk_i2s(1'b0),
            .rst(ndm_reset),
            // Wishbone interface
            .wb_adr(shared_bus_wbs[YM2149_S].adr[7:0]),
            .wb_dat_w(shared_bus_wbs[YM2149_S].dat_m),
            .wb_dat_r(shared_bus_wbs[YM2149_S].dat_s),
            .wb_sel(shared_bus_wbs[YM2149_S].sel),
            .wb_stall(shared_bus_wbs[YM2149_S].stall),
            .wb_cyc(shared_bus_wbs[YM2149_S].cyc),
            .wb_stb(shared_bus_wbs[YM2149_S].stb),
            .wb_ack(shared_bus_wbs[YM2149_S].ack),
            .wb_we(shared_bus_wbs[YM2149_S].we),
            .wb_err(shared_bus_wbs[YM2149_S].err),    
            .i2s_sclk(),     // I2S is not used.
            .i2s_lrclk(),   
            .i2s_data(),     
            .sound(ym2149_sound),  // parallel   audio out, mono or left channel
            .sound_right()  // parallel   audio out, right channel
        );

        reg [1:0] div_by_4_ctr; //Divide-clock-by-4 counter.
        always_ff @(posedge sys_clk)
            div_by_4_ctr <= div_by_4_ctr - 1;

        one_bit_dac dac_inst (
            .clk(sys_clk),              // 50MHz clock
            .clk_en(div_by_4_ctr==0),   // 12.5MHz clock enable
            .in(ym2149_sound),          // input 16 PCM audio signal.
            .out(audio_out)             // one bit output signal, modulated at 12.5MHz
        `ifdef VERILATOR
        ,.acc1_overflow(acc1_overflow) //In simulation, check DAC accumulators for overflow.
        ,.acc2_overflow(acc2_overflow) //In simulation, check DAC accumulators for overflow.
        `endif    
        );

`ifdef VERILATOR
        assign pcm_out = ym2149_sound;
`endif
        assign audio_gain = 1'b1; //PMOD Amp gain fixed at 6dB.
        assign audio_shutdown_n = 1'b1;
    end
    else begin : NO_AUDIO
        assign audio_out = 1'b0;
        assign audio_gain =1'b0;
        assign audio_shutdown_n = 1'b0;
`ifdef VERILATOR
        assign pcm_out = 16'b0;
        assign acc1_overflow = 1'b0;
        assign acc2_overflow = 1'b0;  
`endif
    end
    endgenerate

    //The PicoRV DMA module.
    generate if (PICORV_ACTIVE)
    begin : GENERATE_PICORV_MODULE
        picorv_dma_top #(
            .BASE_ADDR(PICORV_BASE_ADDRESS) 
            )
            picorv_dma_inst (
            .sys_clk(sys_clk),
            .sys_clkx2(sys_clk_2x),
            .rst(ndm_reset),
            //32-bit pipelined Wishbone master interface 0.
            .wbm_adr_o(xbar_wbm[PICORV_M].adr),
            .wbm_dat_o(xbar_wbm[PICORV_M].dat_m),
            .wbm_dat_i(xbar_wbm[PICORV_M].dat_s),
            .wbm_sel_o(xbar_wbm[PICORV_M].sel),
            .wbm_stall_i(xbar_wbm[PICORV_M].stall),
            .wbm_cyc_o(xbar_wbm[PICORV_M].cyc),
            .wbm_stb_o(xbar_wbm[PICORV_M].stb),
            .wbm_ack_i(xbar_wbm[PICORV_M].ack),
            .wbm_we_o(xbar_wbm[PICORV_M].we),
            .wbm_err_i(xbar_wbm[PICORV_M].err),
            //32-bit pipelined Wishbone slave interface.
            .wbs_adr(shared_bus_wbs[PICORV_S].adr[10:0]),
            .wbs_dat_w(shared_bus_wbs[PICORV_S].dat_m),
            .wbs_dat_r(shared_bus_wbs[PICORV_S].dat_s),
            .wbs_sel(shared_bus_wbs[PICORV_S].sel),
            .wbs_stall(shared_bus_wbs[PICORV_S].stall),
            .wbs_cyc(shared_bus_wbs[PICORV_S].cyc),
            .wbs_stb(shared_bus_wbs[PICORV_S].stb),
            .wbs_ack(shared_bus_wbs[PICORV_S].ack),
            .wbs_we(shared_bus_wbs[PICORV_S].we),
            .wbs_err(shared_bus_wbs[PICORV_S].err), 
            //IRQs - TBD
            .irq_in(32'b0),
            .irq_out()
        );
    end
    endgenerate

    //Two USB HID host cores
    generate if (USB_HID_ACTIVE)
    begin : GENERATE_USB_HID_MODULES
        usb_hid_host_top usb_hid0_host_inst (
            .wb_clk(sys_clk),                  // Wishbone clock is in the system clock domain.
            .usb_clk(usb_clk),		           // 12MHz USB clock.
            .usb_rst_n(~usb_reset),            // USB clock domain active low reset
            .wb_rst_n(~ndm_reset),            // System clock domain active low reset
            .usb_dm_i(usb0_dm_i), 
            .usb_dp_i(usb0_dp_i),              // USB D- and D+ input
            .usb_dm_o(usb0_dm_o), 
            .usb_dp_o(usb0_dp_o),              // USB D- and D+ output
            .usb_oe(usb0_oe),
            .irq(),     
            
            //32-bit pipelined Wishbone slave interface.
            .wbs_adr(shared_bus_wbs[USB_HID_0_S].adr[3:0]),
            .wbs_dat_w(shared_bus_wbs[USB_HID_0_S].dat_m),
            .wbs_dat_r(shared_bus_wbs[USB_HID_0_S].dat_s),
            .wbs_sel(shared_bus_wbs[USB_HID_0_S].sel),
            .wbs_stall(shared_bus_wbs[USB_HID_0_S].stall),
            .wbs_cyc(shared_bus_wbs[USB_HID_0_S].cyc),
            .wbs_stb(shared_bus_wbs[USB_HID_0_S].stb),
            .wbs_ack(shared_bus_wbs[USB_HID_0_S].ack),
            .wbs_we(shared_bus_wbs[USB_HID_0_S].we),
            .wbs_err(shared_bus_wbs[USB_HID_0_S].err)
        );

        usb_hid_host_top usb_hid1_host_inst (
            .wb_clk(sys_clk),                  // Wishbone clock is in the system clock domain.
            .usb_clk(usb_clk),		           // 12MHz clock
            .usb_rst_n(~usb_reset),            // USB clock domain active low reset
            .wb_rst_n(~ndm_reset),            // System clock domain active low reset
            .usb_dm_i(usb1_dm_i), 
            .usb_dp_i(usb1_dp_i),              // USB D- and D+ input
            .usb_dm_o(usb1_dm_o), 
            .usb_dp_o(usb1_dp_o),              // USB D- and D+ output
            .usb_oe(usb1_oe),
            .irq(),     
            
            //32-bit pipelined Wishbone slave interface.
            .wbs_adr(shared_bus_wbs[USB_HID_1_S].adr[3:0]),
            .wbs_dat_w(shared_bus_wbs[USB_HID_1_S].dat_m),
            .wbs_dat_r(shared_bus_wbs[USB_HID_1_S].dat_s),
            .wbs_sel(shared_bus_wbs[USB_HID_1_S].sel),
            .wbs_stall(shared_bus_wbs[USB_HID_1_S].stall),
            .wbs_cyc(shared_bus_wbs[USB_HID_1_S].cyc),
            .wbs_stb(shared_bus_wbs[USB_HID_1_S].stb),
            .wbs_ack(shared_bus_wbs[USB_HID_1_S].ack),
            .wbs_we(shared_bus_wbs[USB_HID_1_S].we),
            .wbs_err(shared_bus_wbs[USB_HID_1_S].err)
        );
    end
    else begin
        assign usb0_dm_o = 1'b0; 
        assign usb0_dp_o = 1'b0;
        assign usb0_oe = 1'b0;
        assign usb1_dm_o = 1'b0; 
        assign usb1_dp_o = 1'b0;
        assign usb1_oe = 1'b0;
    end
    endgenerate
endmodule
