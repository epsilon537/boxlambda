//The BoxLambda top-level used by most gw/projects builds.
module top (
    input  wire       ext_clk_100,
    input  wire       ext_rst_n,
    
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
    output wire       pll_locked_led,
    output wire       init_done_led,
    output wire       init_err_led,
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

    // Quad SPI interface
    // On Verilator, we use separate input an output ports. On FPGA, the data ports are bidirectional
`ifdef VERILATOR
    output wire qspi_cs_n,
    output wire [3:0] qspi_dq_out,
    input wire [3:0] qspi_dq_in,
    output wire qspi_sck,
    output wire [1:0] qspi_mod,  
`else    
    output wire qspi_cs_n,
    inout  wire [3:0] qspi_dq,
    output wire qspi_sck,    
`endif

     // USB HID: On Verilator, we use separate input and output ports. On FPGA, the USB ports are bidirectional.
`ifdef VERILATOR
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
`else
    inout wire usb0_dm, 
    inout wire usb0_dp,
    output wire usb0_dm_snoop, //Snooping the USB0 port, for test/debug purposes.
    output wire usb0_dp_snoop,
    inout wire usb1_dm, 
    inout wire usb1_dp,
    output wire usb1_dm_snoop, //Snooping the USB1 ports, for test/debug purposes.
    output wire usb1_dp_snoop,
`endif
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

    input  wire       uart_rx,
    output wire       uart_tx,
    inout  wire [7:0] gpio0,
    inout  wire [3:0] gpio1
    );

`ifndef VERILATOR
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

    //(De)Muxing unidirectional to bidirectional ports.
    assign usb0_dm_i = usb0_oe ? 1'bZ : usb0_dm;
    assign usb0_dp_i = usb0_oe ? 1'bZ : usb0_dp;
    assign usb0_dm = usb0_oe ? usb0_dm_o : 1'bZ;
    assign usb0_dp = usb0_oe ? usb0_dp_o : 1'bZ;
    assign usb0_dm_snoop = usb0_oe ? usb0_dm_o : usb0_dm_i;
    assign usb0_dp_snoop = usb0_oe ? usb0_dp_o : usb0_dp_i;

    //(De)Muxing unidirectional to bidirectional ports.
    assign usb1_dm_i = usb1_oe ? 1'bZ : usb1_dm;
    assign usb1_dp_i = usb1_oe ? 1'bZ : usb1_dp;
    assign usb1_dm = usb1_oe ? usb1_dm_o : 1'bZ;
    assign usb1_dp = usb1_oe ? usb1_dp_o : 1'bZ;
    assign usb1_dm_snoop = usb1_oe ? usb1_dm_o : usb1_dm_i;
    assign usb1_dp_snoop = usb1_oe ? usb1_dp_o : usb1_dp_i;
`endif

    // Quad SPI interface
	wire [3:0] qspi_dat_o;
    wire [3:0] qspi_dat_i;

`ifdef VERILATOR
    assign qspi_dq_out = qspi_dat_o;
    assign qspi_dat_i = qspi_dq_in;
`else
    assign qspi_dq = 
        qspi_mod[1] ? {2'b11, 1'bz, qspi_dat_o[0]} : 
            qspi_mod[0] ? 4'bzzzz : qspi_dat_o[3:0];
    assign qspi_dat_i = 
        qspi_mod[1] ? {2'bzz, qspi_dq[1], 1'bz} : 
            qspi_mod[0] ? qspi_dq : 4'bzzzz;
`endif
        
    boxlambda_soc #(
		.DPRAM_BYTE_ADDR_MASK(`DPRAM_SIZE_BYTES/2-1), /*Divide by 2. DPRAM is split into two equal-size instances.*/
        .VRAM_SIZE_BYTES(`VRAM_SIZE_BYTES),
        .DEBUG_MODULE_ACTIVE(1),
`ifndef DRAM
        .DRAM_ACTIVE(0),
`endif
`ifndef VERA
        .VERA_ACTIVE(0),
`endif
`ifndef SDSPI
        .SDSPI_ACTIVE(0),
`endif
`ifndef YM2149
        .YM2149_ACTIVE(0),
`endif
`ifndef PICORV_DMA
        .PICORV_ACTIVE(0),
`endif
`ifndef USB_HID
        .USB_HID_ACTIVE(0),
`endif
`ifndef QSPI_FLASH
        .QUAD_SPI_FLASH_ACTIVE(0),
`endif
        /*We don't specify a dmem.mem. The data segment is copied into DMEM from a load segment that's part of the cmem.mem
         *image. This copy operation is part of the PicoLibc start-up code.*/
        .CMEM_FILE("cmem.mem")
    ) boxlambda_soc_inst (.*);
endmodule
