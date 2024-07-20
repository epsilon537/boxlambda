//The BoxLambda top-level used by most gw/projects builds.
module boxlambda_top (
    input wire ext_clk_100,
    input wire ext_rst_n,

`ifdef VERILATOR
    /*These JTAG signals are not used on FPGA (they are used in simulation).
   *On FPGA, the JTAG signals are driven by a BSCANE2 primitive inside the jtag tap module dmi_bscane_tap.sv.
   */
    input  wire        tck,
    input  wire        trst_n,
    input  wire        tms,
    input  wire        tdi,
    output wire        tdo,
`endif
    output wire        pll_locked_led,
    output wire        init_done_led,
    output wire        init_err_led,
`ifdef SYNTHESIS
    /*The simulation build doesn't export DDR pins.*/
    output wire [13:0] ddram_a,
    output wire [ 2:0] ddram_ba,
    output wire        ddram_ras_n,
    output wire        ddram_cas_n,
    output wire        ddram_we_n,
    output wire        ddram_cs_n,
    output wire [ 1:0] ddram_dm,
    inout  wire [15:0] ddram_dq,
    inout  wire [ 1:0] ddram_dqs_p,
    inout  wire [ 1:0] ddram_dqs_n,
    output wire        ddram_clk_p,
    output wire        ddram_clk_n,
    output wire        ddram_cke,
    output wire        ddram_odt,
    output wire        ddram_reset_n,
`endif
    // VGA interface
    output wire [ 3:0] vga_r,
    output wire [ 3:0] vga_g,
    output wire [ 3:0] vga_b,
    output wire        vga_hsync,
    output wire        vga_vsync,

    // SDSPI interface
    output wire sdspi_cs_n,
    output wire sdspi_sck,
    output wire sdspi_mosi,
    input  wire sdspi_miso,
    input  wire sdspi_card_detect_n,

    // Flash SPI interface
    output wire spiflash_sck,
    output wire spiflash_cs_n,
    output wire spiflash_mosi,
    input  wire spiflash_miso,

    // I2C signals
`ifdef VERILATOR
    input  wire i2c_scl_i,
    input  wire i2c_sda_i,
    output wire i2c_scl_o,
    output wire i2c_sda_o,
`else
    inout  wire i2c_scl,
    inout  wire i2c_sda,
    output wire i2c_scl_pup,  //SCL pull-up pin
    output wire i2c_sda_pup,  //SDA pull-up pin
`endif

    // USB HID: On Verilator, we use separate input and output ports. On FPGA, the USB ports are bidirectional.
`ifdef VERILATOR
    input  wire        usb0_dm_i,
    input  wire        usb0_dp_i,
    output wire        usb0_dm_o,
    output wire        usb0_dp_o,
    output wire        usb0_oe,
    input  wire        usb1_dm_i,
    input  wire        usb1_dp_i,
    output wire        usb1_dm_o,
    output wire        usb1_dp_o,
    output wire        usb1_oe,
`else
    inout  wire        usb0_dm,
    inout  wire        usb0_dp,
    output wire        usb0_dm_snoop,     //Snooping the USB0 port, for test/debug purposes.
    output wire        usb0_dp_snoop,
    inout  wire        usb1_dm,
    inout  wire        usb1_dp,
    output wire        usb1_dm_snoop,     //Snooping the USB1 ports, for test/debug purposes.
    output wire        usb1_dp_snoop,
`endif
    // Audio interface
    output wire        audio_out,
    output wire        audio_gain,
    output wire        audio_shutdown_n,
`ifdef VERILATOR
    // Audio interface signals only used in simulation
    output wire [15:0] pcm_out,
    output wire        acc1_overflow,
    output wire        acc2_overflow,
`endif

    input  wire        uart_rx,
    output wire        uart_tx,
`ifdef VERILATOR
    input  wire [23:0] gp_in,
    output wire [23:0] gp_out,
    output wire [23:0] gp_oe,
`else
    inout  wire [23:0] gpio,
`endif
    input  wire        gp_clk
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

  wire [23:0] gp_in;
  wire [23:0] gp_out;
  wire [23:0] gp_oe;

  wire i2c_scl_i;     // SCL-line input
  wire i2c_scl_o;     // SCL-line output (always 1'b0)
  wire i2c_sda_i;     // SDA-line input
  wire i2c_sda_o;     // SDA-line output (always 1'b0)

  //(De)Muxing unidirectional to bidirectional I2C ports.
  assign i2c_scl = i2c_scl_o ? 1'bZ : 1'b0;
  assign i2c_sda = i2c_sda_o ? 1'bZ : 1'b0;
  assign i2c_scl_i = i2c_scl_o ? i2c_scl : 1'b0;
  assign i2c_sda_i = i2c_sda_o ? i2c_sda : 1'b0;
  assign i2c_scl_pup = 1'b1;  //SCL pull-up pin.
  assign i2c_sda_pup = 1'b1;  //SDA pull-up pin.

  //(De)Muxing unidirectional to bidirectional GPIO ports.
  generate
    genvar ii;
    for (ii = 0; ii < 24; ii = ii + 1) begin : GPIO_MUX
      assign gp_in[ii] = gp_oe[ii] ? 1'bZ : gpio[ii];
      assign gpio[ii]  = gp_oe[ii] ? gp_out[ii] : 1'bZ;
    end
  endgenerate

  //(De)Muxing unidirectional to bidirectional USB ports.
  assign usb0_dm_i = usb0_oe ? 1'bZ : usb0_dm;
  assign usb0_dp_i = usb0_oe ? 1'bZ : usb0_dp;
  assign usb0_dm = usb0_oe ? usb0_dm_o : 1'bZ;
  assign usb0_dp = usb0_oe ? usb0_dp_o : 1'bZ;
  assign usb0_dm_snoop = usb0_oe ? usb0_dm_o : usb0_dm_i;
  assign usb0_dp_snoop = usb0_oe ? usb0_dp_o : usb0_dp_i;

  //(De)Muxing unidirectional to bidirectional USB ports.
  assign usb1_dm_i = usb1_oe ? 1'bZ : usb1_dm;
  assign usb1_dp_i = usb1_oe ? 1'bZ : usb1_dp;
  assign usb1_dm = usb1_oe ? usb1_dm_o : 1'bZ;
  assign usb1_dp = usb1_oe ? usb1_dp_o : 1'bZ;
  assign usb1_dm_snoop = usb1_oe ? usb1_dm_o : usb1_dm_i;
  assign usb1_dp_snoop = usb1_oe ? usb1_dp_o : usb1_dp_i;
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
`ifndef SPIFLASH
      .SPIFLASH_ACTIVE(0),
`endif
`ifndef I2C
      .I2C_ACTIVE(0),
`endif
      /*We don't specify a dmem.mem. The data segment is copied into DMEM from a load segment that's part of the cmem.mem
         *image. This copy operation is part of the PicoLibc start-up code.*/
      .CMEM_FILE("cmem.mem")
  ) boxlambda_soc_inst (
      .*
  );
endmodule
