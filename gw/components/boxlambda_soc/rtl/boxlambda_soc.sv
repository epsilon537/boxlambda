/*The parameterized BoxLambda SoC.*/
module boxlambda_soc #(
    parameter IMEM_BYTE_ADDR_MASK = 'h3ffff,  /*Internal MEM size as a mask value.*/
    parameter VRAM_SIZE_BYTES = 131072,
    parameter DEBUG_MODULE_ACTIVE = 1,
    parameter DRAM_ACTIVE = 1,
    parameter VERA_ACTIVE = 1,
    parameter SDSPI_ACTIVE = 1,
    parameter YM2149_ACTIVE = 1,
    parameter USB_HID_ACTIVE = 1,
    parameter SPIFLASH_ACTIVE = 1,
    parameter I2C_ACTIVE = 1,
    parameter DFX_ACTIVE = 0,
    parameter VS0_ACTIVE = 1,
    parameter ACK_INVALID_ADDR = 1,
    parameter IMEM_FILE = ""
) (
    input  wire        ext_clk_100,         //100MHz external clock.
    input  wire        ext_rst_n,           //External reset pin.
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
    output wire        pll_locked_led,      //PLL locked indication.
    output wire        init_done_led,       //LiteDRAM initialization done indication.
    output wire        init_err_led,        //LiteDRAM initialization error indication.
    output wire        sd_card_detect_led,
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
    input  wire i2c_scl_i,
    input  wire i2c_sda_i,
    output wire i2c_scl_o,
    output wire i2c_sda_o,

    // USB HID, two ports.
    input  wire usb0_dm_i,
    input  wire usb0_dp_i,
    output wire usb0_dm_o,
    output wire usb0_dp_o,
    output wire usb0_oe,
    input  wire usb1_dm_i,
    input  wire usb1_dp_i,
    output wire usb1_dm_o,
    output wire usb1_dp_o,
    output wire usb1_oe,

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
    // UART and GPIO
    input  wire        uart_rx,
    output wire        uart_tx,
    input  wire [23:0] gp_in,
    output wire [23:0] gp_out,
    output wire [23:0] gp_oe,
    input  wire        gp_clk
);

  //Enum of Bus Masters attached to the instruction bus.
  typedef enum {
    INSTR_BUS_IBEX_I_M,  /*Ibex CPU instruction port.*/
    INSTR_BUS_VS0_M  /*Virtual Socket 0 Bus Master port.*/
  } wb_instr_bus_master_e;

  //Enum of Bus Slaves attached to the instruction_bus
  typedef enum {
    INSTR_BUS_IMEM_S,  /*IMEM slave port.*/
    INSTR_BUS_DM_S,  /*Debug Module slave port.*/
    INSTR_BUS_FLASH_USR_S,  /*Flash controller user slave port*/
    INSTR_BUS_DDR_USR_S  /*DRAM user slave port.*/
  } wb_instr_bus_slave_e;

  //Enum of Bus Masters attached to the data bus.
  typedef enum {
    DATA_BUS_IBEX_D_M,  /*Ibex CPU data port.*/
    DATA_BUS_VS0_M,    /*Virtual Socket 0 Bus Master port.*/
    DATA_BUS_DFX_M,    /*DFX Bus Master port.*/
    DATA_BUS_DM_M     /*Debug Module Bus Master port.*/
  } wb_data_bus_master_e;

  //Enum of Bus Slaves attached to the data bus.
  typedef enum {
    DATA_BUS_IMEM_S,  /*IMEM slave port.*/
    DATA_BUS_SDSPI_S,  /*SDSPI*/
    DATA_BUS_USB_HID_0_S,  /*USB HID keyboard or mouse*/
    DATA_BUS_USB_HID_1_S,  /*USB HID keyboard or mouse*/
    DATA_BUS_FLASH_CTRL_S,  /*Flash controller control port*/
    DATA_BUS_RESET_CTRL_S,  /*Reset Control Module.*/
    DATA_BUS_GPIO_S,  /*GPIO*/
    DATA_BUS_I2C_S,  /*I2C*/
    DATA_BUS_DFX_S,  /*DFX Controller*/
    DATA_BUS_YM2149_S,  /*Dual YM2149 PSG core.*/
    DATA_BUS_UART_S,  /*UART.*/
    DATA_BUS_TIMER_S,  /*Timer Module.*/
    DATA_BUS_DDR_CTRL_S,  /*LiteDRAM control port.*/
    DATA_BUS_DM_S,  /*Debug Module slave port.*/
    DATA_BUS_FLASH_USR_S,  /*Flash controller user port*/
    DATA_BUS_VERA_S,  /*VERA slave port.*/
    DATA_BUS_VS0_S,  /*VS0 slave port.*/
    DATA_BUS_DDR_USR_S  /*DRAM user slave port.*/
  } wb_data_bus_slave_e;

  //Fast IRQ IDs
  typedef enum {
    IRQ_ID_NA_3,
    IRQ_ID_DFX,
    IRQ_ID_NA_0,
    IRQ_ID_NA_1,
    IRQ_ID_NA_2,
    IRQ_ID_UART,
    IRQ_ID_I2C,
    IRQ_ID_USB_HID_0,
    IRQ_ID_USB_HID_1,
    IRQ_ID_GPIO,
    IRQ_ID_SDSPI,
    IRQ_ID_NA_4,
    IRQ_ID_VS_0,
    IRQ_ID_VERA,
    IRQ_ID_NA_5
  } fast_irq_ids_e;

  /*We used a word-addressing Wishbone bus. The IMEM word address bus width is equal to the
   *number of bits of the Byte Address mask minus 2.*/
  localparam IMEM_AW = $clog2(IMEM_BYTE_ADDR_MASK) - 2;

  localparam AW = 28;  //Wishbone Bus address width. Note that we use word addressing.
  localparam DW = 32;  //Wishbone Bus data width.

  localparam NUM_INSTR_BUS_MASTERS = 2;
  localparam NUM_INSTR_BUS_SLAVES = 4;
  localparam NUM_DATA_BUS_MASTERS = 4;
  localparam NUM_DATA_BUS_SLAVES = 18;

  //Instruction bus bus slave addresses. Right shift by two to convert byte address values to word address values.
  localparam [NUM_INSTR_BUS_SLAVES*AW-1:0] INSTR_BUS_SLAVE_ADDRS = {
    /*INSTR_BUS_DDR_USR_S*/{AW'('h20000000 >> 2)},
    /*INSTR_BUS_FLASH_USR_S*/{AW'('h11000000 >> 2)},
    /*INSTR_BUS_DM_S*/{AW'('h10040000 >> 2)},
    /*INSTR_BUS_IMEM_S*/{AW'('h00000000 >> 2)}
  };

  //Instruction bus slave address mask. Right-shift by two to convert byte size to word size.
  localparam [NUM_INSTR_BUS_SLAVES*AW-1:0] INSTR_BUS_SLAVE_ADDR_MASKS = {
    /*INSTR_BUS_DDR_USR_S*/{AW'(~('h0fffffff >> 2))},
    /*INSTR_BUS_FLASH_USR_S*/{AW'(~('h00ffffff >> 2))},
    /*INSTR_BUS_DM_S*/{AW'(~('h0000ffff >> 2))},
    /*INSTR_BUS_IMEM_S*/{AW'(~(IMEM_BYTE_ADDR_MASK >> 2))}
  };

  //Data bus slave addresses. Right shift by two to convert byte address values to word address values.
  localparam [NUM_DATA_BUS_SLAVES*AW-1:0] DATA_BUS_SLAVE_ADDRS = {
    /*DATA_BUS_DDR_USR_S*/{AW'('h20000000 >> 2)},
    /*DATA_BUS_VS0_S*/{AW'('h13000000 >> 2)},
    /*DATA_BUS_VERA_S*/{AW'('h12000000 >> 2)},
    /*DATA_BUS_FLASH_USR_S*/{AW'('h11000000 >> 2)},
    /*DATA_BUS_DM_S*/{AW'('h10040000 >> 2)},
    /*DATA_BUS_DDR_CTRL_S*/{AW'('h10030000 >> 2)},
    /*DATA_BUS_TIMER_S*/{AW'('h10020000 >> 2)},
    /*DATA_BUS_UART_S*/{AW'('h10010000 >> 2)},
    /*DATA_BUS_YM2149_S*/{AW'('h10001000 >> 2)},
    /*DATA_BUS_DFX_S*/{AW'('h10000400 >> 2)},
    /*DATA_BUS_I2C_S*/{AW'('h10000200 >> 2)},
    /*DATA_BUS_GPIO_S*/{AW'('h10000100 >> 2)},
    /*DATA_BUS_RESET_CTRL_S*/{AW'('h100000D0 >> 2)},
    /*DATA_BUS_FLASH_CTRL_S*/{AW'('h100000C0 >> 2)},
    /*DATA_BUS_USB_HID_1_S*/{AW'('h10000080 >> 2)},
    /*DATA_BUS_USB_HID_0_S*/{AW'('h10000040 >> 2)},
    /*DATA_BUS_SDSPI_S*/{AW'('h10000020 >> 2)},
    /*DATA_BUS_IMEM_S*/{AW'('h00000000 >> 2)}
  };

  //Data bus slave address mask. Right-shift by two to convert byte size to word size.
  localparam [NUM_DATA_BUS_SLAVES*AW-1:0] DATA_BUS_SLAVE_ADDR_MASKS = {
    /*DATA_BUS_DDR_USR_S*/{AW'(~('h0fffffff >> 2))},
    /*DATA_BUS_VS0_S*/{AW'(~('h000fffff >> 2))},
    /*DATA_BUS_VERA_S*/{AW'(~('h0007ffff >> 2))},
    /*DATA_BUS_FLASH_USR_S*/{AW'(~('h00ffffff >> 2))},
    /*DATA_BUS_DM_S*/{AW'(~('h0000ffff >> 2))},
    /*DATA_BUS_DDR_CTRL_S*/{AW'(~('h0000ffff >> 2))},
    /*DATA_BUS_TIMER_S*/{AW'(~('h000003ff >> 2))},
    /*DATA_BUS_UART_S*/{AW'(~('h0000001f >> 2))},
    /*DATA_BUS_YM2149_S*/{AW'(~('h000003ff >> 2))},
    /*DATA_BUS_DFX_S*/{AW'(~('h0000007f >> 2))},
    /*DATA_BUS_I2C_S*/{AW'(~('h000001ff >> 2))},
    /*DATA_BUS_GPIO_S*/{AW'(~('h0000003f >> 2))},
    /*DATA_BUS_RESET_CTRL_S*/{AW'(~('h00000007 >> 2))},
    /*DATA_BUS_FLASH_CTRL_S*/{AW'(~('h00000007 >> 2))},
    /*DATA_BUS_USB_HID_1_S*/{AW'(~('h0000003f >> 2))},
    /*DATA_BUS_USB_HID_0_S*/{AW'(~('h0000003f >> 2))},
    /*DATA_BUS_SDSPI_S*/{AW'(~('h0000001f >> 2))},
    /*DATA_BUS_IMEM_S*/{AW'(~(IMEM_BYTE_ADDR_MASK >> 2))}
  };

  //Clock signals.
  logic sys_clk, usb_clk, clk_50, clk_100;
  //PLL lock signals.
  logic usb_pll_locked, sys_pll_locked, pre_pll_locked, litedram_pll_locked;

  //Fast IRQ bit vector
  logic [14:0] fast_irqs;
  //Timer IRQ
  logic timer_irq;

  //ndm_reset_req: Non-Debug Module reset requested by Debug Module
  //ndm_reset: Non-Debug-Module-Reset issued by Reset Controller i.e. reset everything except Debug Module.
  //dm_reset: Debug-Module Reset issued by Reset Controller.
  //usbreset: Reset of the USB clock domain.
  //vs0_reset: Reset of Virtual Socket 0, controlled by the DFX controller.
  logic ndm_reset_req, ndm_reset, dm_reset, usb_reset, vs0_reset;
  logic por_completed;  //Indicates Power-On Reset has been completed.
  logic debug_req;  //Debug Request signal.

  //The instruction path bus master interfaces.
  wb_if instruction_bus_wbm[NUM_INSTR_BUS_MASTERS] (
      .rst(dm_reset),
      .clk(sys_clk)
  );

  //The instruction bus slave interfaces.
  wb_if instruction_bus_wbs[NUM_INSTR_BUS_SLAVES] (
      .rst(dm_reset),
      .clk(sys_clk)
  );

  //The data path bus master interfaces.
  wb_if data_bus_wbm[NUM_DATA_BUS_MASTERS] (
      .rst(dm_reset),
      .clk(sys_clk)
  );

  //The data bus slave interfaces.
  wb_if data_bus_wbs[NUM_DATA_BUS_SLAVES] (
      .rst(dm_reset),
      .clk(sys_clk)
  );

  //Reset Controller
  reset_ctrl reset_ctrl_inst (
      .sys_clk(sys_clk),  //50MHz system clock
      .usb_clk(usb_clk),  //12MHz USB clock
      .sys_pll_locked_i(sys_pll_locked),  //System Clock PLL locked indication (input)
      .usb_pll_locked_i(usb_pll_locked),  //USB Clock PLL locked indication (input)
      .ndm_reset_i(ndm_reset_req),  //non-debug-module reset request input
      .ext_reset_i(~ext_rst_n),  //asynchronous external reset input
      .ndm_reset_o(ndm_reset),  //non-debug-module domain reset output
      .dm_reset_o(dm_reset),  //debug-module domain reset output
      .usb_reset_o(usb_reset),  //usb domain reset output
      .por_completed_o(por_completed),  //Power-On-Reset completion indication (output).
      //32-bit pipelined Wishbone slave interface.
      .wb_adr(data_bus_wbs[DATA_BUS_RESET_CTRL_S].adr[0]),
      .wb_dat_w(data_bus_wbs[DATA_BUS_RESET_CTRL_S].dat_m),
      .wb_dat_r(data_bus_wbs[DATA_BUS_RESET_CTRL_S].dat_s),
      .wb_sel(data_bus_wbs[DATA_BUS_RESET_CTRL_S].sel),
      .wb_stall(data_bus_wbs[DATA_BUS_RESET_CTRL_S].stall),
      .wb_cyc(data_bus_wbs[DATA_BUS_RESET_CTRL_S].cyc),
      .wb_stb(data_bus_wbs[DATA_BUS_RESET_CTRL_S].stb),
      .wb_ack(data_bus_wbs[DATA_BUS_RESET_CTRL_S].ack),
      .wb_we(data_bus_wbs[DATA_BUS_RESET_CTRL_S].we),
      .wb_err(data_bus_wbs[DATA_BUS_RESET_CTRL_S].err)
  );

  /*First-stage clock generator. If LiteDRAM is synthesized-in, it includes a Second-stage clock generator.
   *If LiteDRAM is not synthesized-in, this first-stage clock generator provides the system clock
   */
  boxlambda_clk_gen clkgen (
      .ext_clk_100(ext_clk_100),  //100MHz external clock input.
      .rst_n(1'b1),
      .clk_50(clk_50),  //50MHz clock output
      .clk_100(clk_100),  //100MHz clock output
      .clk_12(usb_clk),  //12 MHz USB clock output
      .locked(pre_pll_locked) //PLL lock indication output. It's called pre-PLL because LiteDRAM (when included in the build)
      //introduces a second-stage PLL hanging off clk_50. The LiteDRAM PLL provides the system clock.
      //When LiteDRAM is not included in the build, clk_50 becomes the system clock.
  );

  assign usb_pll_locked = pre_pll_locked;

  /*The Instruction Bus.*/
  instruction_bus #(
      .ADDR_WIDTH(AW),
      .ARB_TYPE_ROUND_ROBIN(0),
      .ARB_BLOCK_ACK(0),
      .SLAVE_ADDRESSES(INSTR_BUS_SLAVE_ADDRS),
      .SLAVE_ADDR_MASKS(INSTR_BUS_SLAVE_ADDR_MASKS)
  ) instruction_bus_inst (
      .clk(sys_clk),
      .rst(dm_reset),
      .wbm(instruction_bus_wbm),
      .wbs(instruction_bus_wbs)
  );

  /*The Data Bus.*/
  data_bus #(
      .ADDR_WIDTH(AW),
      .ARB_TYPE_ROUND_ROBIN(0),
      .ARB_BLOCK_ACK(0),
      .SLAVE_ADDRESSES(DATA_BUS_SLAVE_ADDRS),
      .SLAVE_ADDR_MASKS(DATA_BUS_SLAVE_ADDR_MASKS)
  ) data_bus_inst (
      .clk(sys_clk),
      .rst(dm_reset),
      .wbm(data_bus_wbm),
      .wbs(data_bus_wbs)
  );

  /*The Slave side 2-to-1 arbiters*/

  wb_if dm_wb_if (
      .rst(dm_reset),
      .clk(sys_clk)
  );

  wb_arbiter_2_wrapper #(
      .ADDR_WIDTH(AW),
      .ARB_TYPE_ROUND_ROBIN(0),
      .ARB_BLOCK_ACK(0),
      .ARB_DEFAULT_TO_PORT_0(1)
  ) dm_arbiter (
      .clk  (sys_clk),
      .rst  (dm_reset),
      .wbm_0(instruction_bus_wbs[INSTR_BUS_DM_S]),
      .wbm_1(data_bus_wbs[DATA_BUS_DM_S]),
      .wbs  (dm_wb_if)
  );

  wb_if flash_usr_wb_if (
      .rst(dm_reset),
      .clk(sys_clk)
  );

  wb_arbiter_2_wrapper #(
      .ADDR_WIDTH(AW),
      .ARB_TYPE_ROUND_ROBIN(0),
      .ARB_BLOCK_ACK(0),
      .ARB_DEFAULT_TO_PORT_0(1)
  ) flash_usr_arbiter (
      .clk  (sys_clk),
      .rst  (dm_reset),
      .wbm_0(instruction_bus_wbs[INSTR_BUS_FLASH_USR_S]),
      .wbm_1(data_bus_wbs[DATA_BUS_FLASH_USR_S]),
      .wbs  (flash_usr_wb_if)
  );

  wb_if ddr_usr_wb_if (
      .rst(dm_reset),
      .clk(sys_clk)
  );

  wb_arbiter_2_wrapper #(
      .ADDR_WIDTH(AW),
      .ARB_TYPE_ROUND_ROBIN(0),
      .ARB_BLOCK_ACK(0),
      .ARB_DEFAULT_TO_PORT_0(1)
  ) ddr_usr_arbiter (
      .clk  (sys_clk),
      .rst  (dm_reset),
      .wbm_0(data_bus_wbs[DATA_BUS_DDR_USR_S]),
      .wbm_1(instruction_bus_wbs[INSTR_BUS_DDR_USR_S]),
      .wbs  (ddr_usr_wb_if)
  );

  /*The DFX Controller.*/
  generate
    if (DFX_ACTIVE) begin : GENERATE_DFX_CONTROLLER
      wb_dfx_controller dfx_controller_inst (
          .clk(sys_clk),
          .rst(ndm_reset),
          //32-bit pipelined Wishbone master interface.
          .wbm_adr_o(data_bus_wbm[DATA_BUS_DFX_M].adr),
          .wbm_dat_o(data_bus_wbm[DATA_BUS_DFX_M].dat_m),
          .wbm_dat_i(data_bus_wbm[DATA_BUS_DFX_M].dat_s),
          .wbm_we_o(data_bus_wbm[DATA_BUS_DFX_M].we),
          .wbm_sel_o(data_bus_wbm[DATA_BUS_DFX_M].sel),
          .wbm_stb_o(data_bus_wbm[DATA_BUS_DFX_M].stb),
          .wbm_ack_i(data_bus_wbm[DATA_BUS_DFX_M].ack),
          .wbm_stall_i(data_bus_wbm[DATA_BUS_DFX_M].stall),
          .wbm_cyc_o(data_bus_wbm[DATA_BUS_DFX_M].cyc),
          .wbm_err_i(data_bus_wbm[DATA_BUS_DFX_M].err),
          //32-bit pipelined Wishbone slave interface.
          .wbs_adr(data_bus_wbs[DATA_BUS_DFX_S].adr[4:0]),
          .wbs_dat_w(data_bus_wbs[DATA_BUS_DFX_S].dat_m),
          .wbs_dat_r(data_bus_wbs[DATA_BUS_DFX_S].dat_s),
          .wbs_sel(data_bus_wbs[DATA_BUS_DFX_S].sel),
          .wbs_stall(data_bus_wbs[DATA_BUS_DFX_S].stall),
          .wbs_cyc(data_bus_wbs[DATA_BUS_DFX_S].cyc),
          .wbs_stb(data_bus_wbs[DATA_BUS_DFX_S].stb),
          .wbs_ack(data_bus_wbs[DATA_BUS_DFX_S].ack),
          .wbs_we(data_bus_wbs[DATA_BUS_DFX_S].we),
          .wbs_err(data_bus_wbs[DATA_BUS_DFX_S].err),
          .vsm_VS_0_rm_reset(vs0_reset),
          .vsm_VS_0_event_error(fast_irqs[IRQ_ID_DFX])
      );
    end else begin
      assign data_bus_wbm[DATA_BUS_DFX_M].adr = 0;
      assign data_bus_wbm[DATA_BUS_DFX_M].dat_m = 0;
      assign data_bus_wbm[DATA_BUS_DFX_M].sel = 0;
      assign data_bus_wbm[DATA_BUS_DFX_M].cyc = 0;
      assign data_bus_wbm[DATA_BUS_DFX_M].stb = 0;
      assign data_bus_wbm[DATA_BUS_DFX_M].we = 0;
      assign fast_irqs[IRQ_ID_DFX] = 1'b0;

      assign vs0_reset = ndm_reset;
    end
  endgenerate

  /*The Debug Modules.*/
  generate
    if (DEBUG_MODULE_ACTIVE) begin : GENERATE_DEBUG_MODULE
      logic dmactive;
      logic unavailable = 1'b0;
      dm::hartinfo_t hartinfo = '{
          zero1: 0,
          nscratch: 2,
          zero0: 0,
          dataaccess: 1,
          datasize: dm::DataCount,
          dataaddr: dm::DataAddr
      };
      logic dmi_rst_n;
      logic dmi_req_valid;
      logic dmi_req_ready;
      dm::dmi_req_t dmi_req;
      logic dmi_resp_valid;
      logic dmi_resp_ready;
      dm::dmi_resp_t dmi_resp;

`ifdef VERILATOR
      logic tdo_o;
      logic tdo_oe;

      assign tdo = tdo_oe ? tdo_o : 1'bz;
`endif

      wb_dm_top wb_dm (
          .clk      (sys_clk),
          .rst_n    (~dm_reset),
          .testmode (1'b0),
          .ndmreset (ndm_reset_req),
          .dmactive (dmactive),
          .debug_req(debug_req),
          .wbm      (data_bus_wbm[DATA_BUS_DM_M]),
          .wbs      (dm_wb_if),
          .dmi_rst_n(dmi_rst_n),
          .*
      );

      dmi_jtag #(
          .IdcodeValue(32'h249511C3)
      ) dmi_jtag_inst (
          .clk_i           (sys_clk),
          .rst_ni          (~dm_reset),
          .testmode_i      (1'b0),
          .dmi_rst_no      (dmi_rst_n),
          .dmi_req_o       (dmi_req),
          .dmi_req_valid_o (dmi_req_valid),
          .dmi_req_ready_i (dmi_req_ready),
          .dmi_resp_i      (dmi_resp),
          .dmi_resp_ready_o(dmi_resp_ready),
          .dmi_resp_valid_i(dmi_resp_valid),
`ifdef VERILATOR
          .tck_i           (tck),
          .tms_i           (tms),
          .trst_ni         (trst_n),
          .td_i            (tdi),
          .td_o            (tdo_o),
          .tdo_oe_o        (tdo_oe)
`else
          .tck_i           (1'b0),
          .tms_i           (1'b0),
          .trst_ni         (1'b1),
          .td_i            (1'b0),
          .td_o            (),
          .tdo_oe_o        ()
`endif
      );

      logic unused = &{1'b0, dmactive, 1'b0};
    end else begin  /*DEBUG_MODULE_ACTIVE==0*/
      assign data_bus_wbm[DATA_BUS_DM_M].adr = 0;
      assign data_bus_wbm[DATA_BUS_DM_M].dat_m = 0;
      assign data_bus_wbm[DATA_BUS_DM_M].sel = 0;
      assign data_bus_wbm[DATA_BUS_DM_M].cyc = 0;
      assign data_bus_wbm[DATA_BUS_DM_M].stb = 0;
      assign data_bus_wbm[DATA_BUS_DM_M].we = 0;
      assign ndm_reset_req = 1'b0;
      assign debug_req = 1'b0;
    end
  endgenerate

  /*The Ibex CPU.*/
  wb_ibex_core #(
      .RV32M(ibex_pkg::RV32MSingleCycle),
      .RV32B(ibex_pkg::RV32BBalanced),
      .RegFile(`PRIM_DEFAULT_IMPL == prim_pkg::ImplGeneric ? ibex_pkg::RegFileFF : ibex_pkg::RegFileFPGA),
      .PrefetchType(ibex_pkg::PrefetchType_Single),
      .BranchTargetALU(1'b0), //I would like to enable this, but I'm running into timing closure issues if I do.
      .WritebackStage(1'b0),
      .DbgTriggerEn(1'b1),
      .DmHaltAddr({2'b00, DATA_BUS_SLAVE_ADDRS[(DATA_BUS_DM_S+1)*AW-1:DATA_BUS_DM_S*AW], 2'b00} + 32'h00000800),
      .DmExceptionAddr({2'b00, DATA_BUS_SLAVE_ADDRS[(DATA_BUS_DM_S+1)*AW-1:DATA_BUS_DM_S*AW], 2'b00} + 32'h00000808)
  ) wb_ibex_core (
      .clk(sys_clk),
      .rst_n(~ndm_reset),
      .instr_wb(instruction_bus_wbm[INSTR_BUS_IBEX_I_M]),
      .data_wb(data_bus_wbm[DATA_BUS_IBEX_D_M]),
      .test_en(1'b0),
      .hart_id(32'h0),
      .boot_addr(32'h0),
      .irq_software(1'b0),
      .irq_timer(timer_irq),
      .irq_external(),
      .irq_fast(fast_irqs),
      .irq_nm(1'b0),
      .debug_req(debug_req),
      .fetch_enable({3'b0, por_completed}),  //Only start fetch after POR has been completed.
      .core_sleep(),
      .*
  );

  //Internal Memory
  wb_dp_ram_wrapper #(
      .ADDR_WIDTH(IMEM_AW),
      .INIT_FILE (IMEM_FILE)
  ) imem (
      .clk(sys_clk),
      .rst(ndm_reset),

      .a_adr_i(instruction_bus_wbs[INSTR_BUS_IMEM_S].adr[IMEM_AW-1:0]),  // ADR_I() address
      .a_dat_i(instruction_bus_wbs[INSTR_BUS_IMEM_S].dat_m),  // DAT_I() data in
      .a_dat_o(instruction_bus_wbs[INSTR_BUS_IMEM_S].dat_s),  // DAT_O() data out
      .a_we_i(instruction_bus_wbs[INSTR_BUS_IMEM_S].we),  // WE_I write enable input
      .a_sel_i(instruction_bus_wbs[INSTR_BUS_IMEM_S].sel),  // SEL_I() select input
      .a_stb_i(instruction_bus_wbs[INSTR_BUS_IMEM_S].stb),  // STB_I strobe input
      .a_stall_o(instruction_bus_wbs[INSTR_BUS_IMEM_S].stall),  // STALL_O stall output
      .a_ack_o(instruction_bus_wbs[INSTR_BUS_IMEM_S].ack),  // ACK_O acknowledge output
      .a_err_o(instruction_bus_wbs[INSTR_BUS_IMEM_S].err),  // ERR_O error output
      .a_cyc_i(instruction_bus_wbs[INSTR_BUS_IMEM_S].cyc),  // CYC_I cycle input

      .b_adr_i(data_bus_wbs[DATA_BUS_IMEM_S].adr[IMEM_AW-1:0]),  // ADR_I() address
      .b_dat_i(data_bus_wbs[DATA_BUS_IMEM_S].dat_m),  // DAT_I() data in
      .b_dat_o(data_bus_wbs[DATA_BUS_IMEM_S].dat_s),  // DAT_O() data out
      .b_we_i(data_bus_wbs[DATA_BUS_IMEM_S].we),  // WE_I write enable input
      .b_sel_i(data_bus_wbs[DATA_BUS_IMEM_S].sel),  // SEL_I() select input
      .b_stb_i(data_bus_wbs[DATA_BUS_IMEM_S].stb),  // STB_I strobe input
      .b_stall_o(data_bus_wbs[DATA_BUS_IMEM_S].stall),  // STALL_O stall output
      .b_ack_o(data_bus_wbs[DATA_BUS_IMEM_S].ack),  // ACK_O acknowledge output
      .b_err_o(data_bus_wbs[DATA_BUS_IMEM_S].err),  // ERR_O error output
      .b_cyc_i(data_bus_wbs[DATA_BUS_IMEM_S].cyc)  // CYC_I cycle input
  );

  generate
    if (VS0_ACTIVE) begin : GENERATE_VS0_MODULE
      //Virtual Socket 0 Interface
      vs0 vs0_inst (
          .sys_clk(sys_clk),
          .rst(vs0_reset),
          //32-bit pipelined Wishbone master interface 0.
          .wbm_0_adr_o(instruction_bus_wbm[INSTR_BUS_VS0_M].adr),
          .wbm_0_dat_o(instruction_bus_wbm[INSTR_BUS_VS0_M].dat_m),
          .wbm_0_dat_i(instruction_bus_wbm[INSTR_BUS_VS0_M].dat_s),
          .wbm_0_sel_o(instruction_bus_wbm[INSTR_BUS_VS0_M].sel),
          .wbm_0_stall_i(instruction_bus_wbm[INSTR_BUS_VS0_M].stall),
          .wbm_0_cyc_o(instruction_bus_wbm[INSTR_BUS_VS0_M].cyc),
          .wbm_0_stb_o(instruction_bus_wbm[INSTR_BUS_VS0_M].stb),
          .wbm_0_ack_i(instruction_bus_wbm[INSTR_BUS_VS0_M].ack),
          .wbm_0_we_o(instruction_bus_wbm[INSTR_BUS_VS0_M].we),
          .wbm_0_err_i(instruction_bus_wbm[INSTR_BUS_VS0_M].err),
          //32-bit pipelined Wishbone master interface 1.
          .wbm_1_adr_o(data_bus_wbm[DATA_BUS_VS0_M].adr),
          .wbm_1_dat_o(data_bus_wbm[DATA_BUS_VS0_M].dat_m),
          .wbm_1_dat_i(data_bus_wbm[DATA_BUS_VS0_M].dat_s),
          .wbm_1_sel_o(data_bus_wbm[DATA_BUS_VS0_M].sel),
          .wbm_1_stall_i(data_bus_wbm[DATA_BUS_VS0_M].stall),
          .wbm_1_cyc_o(data_bus_wbm[DATA_BUS_VS0_M].cyc),
          .wbm_1_stb_o(data_bus_wbm[DATA_BUS_VS0_M].stb),
          .wbm_1_ack_i(data_bus_wbm[DATA_BUS_VS0_M].ack),
          .wbm_1_we_o(data_bus_wbm[DATA_BUS_VS0_M].we),
          .wbm_1_err_i(data_bus_wbm[DATA_BUS_VS0_M].err),
          //32-bit pipelined Wishbone slave interface.
          .wbs_adr(data_bus_wbs[DATA_BUS_VS0_S].adr[19:0]),
          .wbs_dat_w(data_bus_wbs[DATA_BUS_VS0_S].dat_m),
          .wbs_dat_r(data_bus_wbs[DATA_BUS_VS0_S].dat_s),
          .wbs_sel(data_bus_wbs[DATA_BUS_VS0_S].sel),
          .wbs_stall(data_bus_wbs[DATA_BUS_VS0_S].stall),
          .wbs_cyc(data_bus_wbs[DATA_BUS_VS0_S].cyc),
          .wbs_stb(data_bus_wbs[DATA_BUS_VS0_S].stb),
          .wbs_ack(data_bus_wbs[DATA_BUS_VS0_S].ack),
          .wbs_we(data_bus_wbs[DATA_BUS_VS0_S].we),
          .wbs_err(data_bus_wbs[DATA_BUS_VS0_S].err),
          //Input IRQs - VS0 receives the same 32 IRQs with the same IRQ_IDs (bit
          //positions) as the Ibex CPU. The bit position assigned to the VS_0 itself is cleared, however.
          .irq_in({1'b0, fast_irqs & ~(1'b1 << IRQ_ID_VS_0), 8'b0, timer_irq, 7'b0}),
          .irq_out(fast_irqs[IRQ_ID_VS_0])
      );
    end else begin
      assign data_bus_wbm[INSTR_BUS_VS0_M].adr = 0;
      assign data_bus_wbm[INSTR_BUS_VS0_M].dat_m = 0;
      assign data_bus_wbm[INSTR_BUS_VS0_M].sel = 0;
      assign data_bus_wbm[INSTR_BUS_VS0_M].cyc = 0;
      assign data_bus_wbm[INSTR_BUS_VS0_M].stb = 0;
      assign data_bus_wbm[INSTR_BUS_VS0_M].we = 0;
      assign data_bus_wbm[DATA_BUS_VS0_M].adr = 0;
      assign data_bus_wbm[DATA_BUS_VS0_M].dat_m = 0;
      assign data_bus_wbm[DATA_BUS_VS0_M].sel = 0;
      assign data_bus_wbm[DATA_BUS_VS0_M].cyc = 0;
      assign data_bus_wbm[DATA_BUS_VS0_M].stb = 0;
      assign data_bus_wbm[DATA_BUS_VS0_M].we = 0;
      assign fast_irqs[IRQ_ID_VS_0] = 1'b0;
    end
  endgenerate

  //The UART.
  wbuart #(
      .HARDWARE_FLOW_CONTROL_PRESENT(1'b0),
      .INITIAL_SETUP                (31'd25),
      .LGFLEN                       (4'd4)
  ) wbuart_inst (
      .i_clk(sys_clk),
      .i_reset(ndm_reset),
      .i_wb_cyc(data_bus_wbs[DATA_BUS_UART_S].cyc),
      .i_wb_stb(data_bus_wbs[DATA_BUS_UART_S].stb),
      .i_wb_we(data_bus_wbs[DATA_BUS_UART_S].we),
      .i_wb_addr(data_bus_wbs[DATA_BUS_UART_S].adr[2:0]),
      .i_wb_data(data_bus_wbs[DATA_BUS_UART_S].dat_m),
      .i_wb_sel(data_bus_wbs[DATA_BUS_UART_S].sel),
      .o_wb_stall(data_bus_wbs[DATA_BUS_UART_S].stall),
      .o_wb_ack(data_bus_wbs[DATA_BUS_UART_S].ack),
      .o_wb_err(data_bus_wbs[DATA_BUS_UART_S].err),
      .o_wb_data(data_bus_wbs[DATA_BUS_UART_S].dat_s),
      .i_uart_rx(uart_rx),
      .o_uart_tx(uart_tx),
      .i_cts_n(1'b0),
      .o_rts_n(),
      .o_uart_int(fast_irqs[IRQ_ID_UART])
  );

  //The Timer module.
  wb_timer wb_timer_inst (
      .clk_i(sys_clk),
      .rst_i(ndm_reset),
      .wb_cyc_i(data_bus_wbs[DATA_BUS_TIMER_S].cyc),
      .wb_stb_i(data_bus_wbs[DATA_BUS_TIMER_S].stb),
      .wb_we_i(data_bus_wbs[DATA_BUS_TIMER_S].we),
      .wb_addr_i(data_bus_wbs[DATA_BUS_TIMER_S].adr[7:0]),
      .wb_data_i(data_bus_wbs[DATA_BUS_TIMER_S].dat_m),
      .wb_sel_i(data_bus_wbs[DATA_BUS_TIMER_S].sel),
      .wb_stall_o(data_bus_wbs[DATA_BUS_TIMER_S].stall),
      .wb_ack_o(data_bus_wbs[DATA_BUS_TIMER_S].ack),
      .wb_err_o(data_bus_wbs[DATA_BUS_TIMER_S].err),
      .wb_data_o(data_bus_wbs[DATA_BUS_TIMER_S].dat_s),
      .timer_irq_o(timer_irq)
  );

  //The GPIO module
  gpio_top gpio_inst (
      // WISHBONE Interface
      .wb_clk_i(sys_clk),
      .wb_rst_i(ndm_reset),
      .wb_cyc_i(data_bus_wbs[DATA_BUS_GPIO_S].cyc),
      .wb_adr_i(data_bus_wbs[DATA_BUS_GPIO_S].adr[3:0]),
      .wb_dat_i(data_bus_wbs[DATA_BUS_GPIO_S].dat_m),
      .wb_sel_i(data_bus_wbs[DATA_BUS_GPIO_S].sel),
      .wb_we_i(data_bus_wbs[DATA_BUS_GPIO_S].we),
      .wb_stb_i(data_bus_wbs[DATA_BUS_GPIO_S].stb),
      .wb_dat_o(data_bus_wbs[DATA_BUS_GPIO_S].dat_s),
      .wb_ack_o(data_bus_wbs[DATA_BUS_GPIO_S].ack),
      .wb_err_o(data_bus_wbs[DATA_BUS_GPIO_S].err),
      .wb_inta_o(fast_irqs[IRQ_ID_GPIO]),
      // External GPIO Interface
      .ext_pad_i(gp_in),
      .ext_pad_o(gp_out),
      .ext_padoe_o(gp_oe),
      .clk_pad_i(gp_clk)
  );

  //LiteDRAM.
  generate
    if (DRAM_ACTIVE) begin : GENERATE_DRAM_MODULE
      logic litedram_pll_locked_i, litedram_rst_o;

      litedram_wrapper litedram_wrapper_inst (
          .clk(clk_100),  /*100MHz input clock, coming for the First-stage clock generator..*/
          .rst(1'b0),  /*Never reset LiteDRAM.*/
          .sys_clkx2(),  /*Not used.*/
          .sys_clk(sys_clk),  /*LiteDRAM outputs 50MHz system clock.*/
          .sys_rst(litedram_rst_o),  /*LiteDRAM outputs system reset.*/
          .pll_locked(litedram_pll_locked_i),  /*LiteDRAM PLL lock indication.*/
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
          .wb_ctrl_adr(data_bus_wbs[DATA_BUS_DDR_CTRL_S].adr),
          .wb_ctrl_dat_w(data_bus_wbs[DATA_BUS_DDR_CTRL_S].dat_m),
          .wb_ctrl_dat_r(data_bus_wbs[DATA_BUS_DDR_CTRL_S].dat_s),
          .wb_ctrl_sel(data_bus_wbs[DATA_BUS_DDR_CTRL_S].sel),
          .wb_ctrl_stall(data_bus_wbs[DATA_BUS_DDR_CTRL_S].stall),
          .wb_ctrl_cyc(data_bus_wbs[DATA_BUS_DDR_CTRL_S].cyc),
          .wb_ctrl_stb(data_bus_wbs[DATA_BUS_DDR_CTRL_S].stb),
          .wb_ctrl_ack(data_bus_wbs[DATA_BUS_DDR_CTRL_S].ack),
          .wb_ctrl_we(data_bus_wbs[DATA_BUS_DDR_CTRL_S].we),
          .wb_ctrl_err(data_bus_wbs[DATA_BUS_DDR_CTRL_S].err),

          /*The used port addresses 256MB of DDR memory.*/
          .user_port_wishbone_p_0_adr(ddr_usr_wb_if.adr),
          .user_port_wishbone_p_0_dat_w(ddr_usr_wb_if.dat_m),
          .user_port_wishbone_p_0_dat_r(ddr_usr_wb_if.dat_s),
          .user_port_wishbone_p_0_sel(ddr_usr_wb_if.sel),
          .user_port_wishbone_p_0_stall(ddr_usr_wb_if.stall),
          .user_port_wishbone_p_0_cyc(ddr_usr_wb_if.cyc),
          .user_port_wishbone_p_0_stb(ddr_usr_wb_if.stb),
          .user_port_wishbone_p_0_ack(ddr_usr_wb_if.ack),
          .user_port_wishbone_p_0_we(ddr_usr_wb_if.we),
          .user_port_wishbone_p_0_err(ddr_usr_wb_if.err)
      );

      /*sys_pll_locked is fed to the reset controller. It's asserted when Litedram controller indicates reset is
         *deasserted and the PLL is locked.*/
      assign litedram_pll_locked = ~litedram_rst_o & litedram_pll_locked_i;
      assign sys_pll_locked = litedram_pll_locked;
    end else begin  //No DRAM: In this case the Stage-1 clock generator provides the system clock.
      assign sys_clk = clk_50;  //50MHz system clock.
      assign litedram_pll_locked = 1'b1;
      assign init_done_led = 1'b1;
      assign init_err_led = 1'b0;
      assign sys_pll_locked = pre_pll_locked;
`ifdef SYNTHESIS
      //Shut up, DRC.
      OBUFDS OBUFDS (
          .I (1'b0),
          .O (ddram_clk_p),
          .OB(ddram_clk_n)
      );
`endif
    end
  endgenerate

  assign pll_locked_led = sys_pll_locked;

  //Vera Graphics.
  generate
    if (VERA_ACTIVE) begin : GENERATE_VERA_MODULE
      logic vera_irq_n;

      assign fast_irqs[IRQ_ID_VERA] = ~vera_irq_n;

      vera_top #(VRAM_SIZE_BYTES) vera_inst (
          .clk(sys_clk),
          .reset(ndm_reset),
          .wb_adr(data_bus_wbs[DATA_BUS_VERA_S].adr[16:0]),
          .wb_dat_w(data_bus_wbs[DATA_BUS_VERA_S].dat_m),
          .wb_dat_r(data_bus_wbs[DATA_BUS_VERA_S].dat_s),
          .wb_sel(data_bus_wbs[DATA_BUS_VERA_S].sel),
          .wb_stall(data_bus_wbs[DATA_BUS_VERA_S].stall),
          .wb_cyc(data_bus_wbs[DATA_BUS_VERA_S].cyc),
          .wb_stb(data_bus_wbs[DATA_BUS_VERA_S].stb),
          .wb_ack(data_bus_wbs[DATA_BUS_VERA_S].ack),
          .wb_we(data_bus_wbs[DATA_BUS_VERA_S].we),
          .wb_err(data_bus_wbs[DATA_BUS_VERA_S].err),
          .irq_n(vera_irq_n),
          .vga_r(vga_r),
          .vga_g(vga_g),
          .vga_b(vga_b),
          .vga_hsync(vga_hsync),
          .vga_vsync(vga_vsync)
      );
    end else begin : NO_VGA
      assign vga_r = 4'b0;
      assign vga_g = 4'b0;
      assign vga_b = 4'b0;
      assign vga_hsync = 1'b0;
      assign vga_vsync = 1'b0;
      assign fast_irqs[IRQ_ID_VERA] = 1'b0;
    end
  endgenerate

  //SDSPI module.
  generate
    if (SDSPI_ACTIVE) begin : GENERATE_SDSPI_MODULE
      sdspi #(
          .OPT_LITTLE_ENDIAN(1'b1)
      ) sdspi_inst (
          .i_clk(sys_clk),
          .i_sd_reset(ndm_reset),
          // Wishbone interface
          .i_wb_cyc(data_bus_wbs[DATA_BUS_SDSPI_S].cyc),
          .i_wb_stb(data_bus_wbs[DATA_BUS_SDSPI_S].stb),
          .i_wb_we(data_bus_wbs[DATA_BUS_SDSPI_S].we),
          .i_wb_addr(data_bus_wbs[DATA_BUS_SDSPI_S].adr[2:0]),
          .i_wb_data(data_bus_wbs[DATA_BUS_SDSPI_S].dat_m),
          .i_wb_sel(data_bus_wbs[DATA_BUS_SDSPI_S].sel),
          .o_wb_stall(data_bus_wbs[DATA_BUS_SDSPI_S].stall),
          .o_wb_ack(data_bus_wbs[DATA_BUS_SDSPI_S].ack),
          .o_wb_data(data_bus_wbs[DATA_BUS_SDSPI_S].dat_s),
          // SDCard interface
          .o_cs_n(sdspi_cs_n),
          .o_sck(sdspi_sck),
          .o_mosi(sdspi_mosi),
          .i_miso(sdspi_miso),
          .i_card_detect(~sdspi_card_detect_n),
          // Interrupt
          .o_int(fast_irqs[IRQ_ID_SDSPI]),
          // .. and whether or not we can use the SPI port
          .i_bus_grant(1'b1),
          // And some wires for debugging it all
          .o_debug()
      );

      assign data_bus_wbs[DATA_BUS_SDSPI_S].err = 1'b0;
      assign sd_card_detect_led = ~sdspi_card_detect_n;
    end else begin : NO_SDSPI
      assign sdspi_cs_n = 1'b0;
      assign sdspi_sck = 1'b0;
      assign sdspi_mosi = 1'b0;
      assign fast_irqs[IRQ_ID_SDSPI] = 1'b0;
      assign sd_card_detect_led = 1'b0;
    end
  endgenerate

  //The Dual YM2149 PSG core and 1-bit audio DAC.
  generate
    if (YM2149_ACTIVE) begin : GENERATE_YM2149_MODULE
      wire signed [15:0] ym2149_sound;

      YM2149_PSG_system_wb #(
          .CLK_IN_HZ(50000000),  // Input clock frequency
          .CLK_PSG_HZ(2000000),  // PSG clock frequency
          .YM2149_DAC_BITS(10),   // PSG DAC bit precision, 8 through 14 bits, the higher the bits, the higher the dynamic range.
                                  // 10 bits almost perfectly replicates the YM2149 DA converter's Normalized voltage.
                                  // With 8 bits, the lowest volumes settings will be slightly louder than normal.
                                  // With 12 bits, the lowest volume settings will be too quiet.
          .MIXER_DAC_BITS(16)  // The number of DAC bits for the BHG_jt49_filter_mixer output.
      ) ym2149_sys_inst (
          .clk(sys_clk),
          .clk_i2s(1'b0),
          .rst(ndm_reset),
          // Wishbone interface
          .wb_adr(data_bus_wbs[DATA_BUS_YM2149_S].adr[7:0]),
          .wb_dat_w(data_bus_wbs[DATA_BUS_YM2149_S].dat_m),
          .wb_dat_r(data_bus_wbs[DATA_BUS_YM2149_S].dat_s),
          .wb_sel(data_bus_wbs[DATA_BUS_YM2149_S].sel),
          .wb_stall(data_bus_wbs[DATA_BUS_YM2149_S].stall),
          .wb_cyc(data_bus_wbs[DATA_BUS_YM2149_S].cyc),
          .wb_stb(data_bus_wbs[DATA_BUS_YM2149_S].stb),
          .wb_ack(data_bus_wbs[DATA_BUS_YM2149_S].ack),
          .wb_we(data_bus_wbs[DATA_BUS_YM2149_S].we),
          .wb_err(data_bus_wbs[DATA_BUS_YM2149_S].err),
          .i2s_sclk(),  // I2S is not used.
          .i2s_lrclk(),
          .i2s_data(),
          .sound(ym2149_sound),  // parallel   audio out, mono or left channel
          .sound_right()  // parallel   audio out, right channel
      );

      reg [1:0] div_by_4_ctr;  //Divide-clock-by-4 counter.
      always_ff @(posedge sys_clk) div_by_4_ctr <= div_by_4_ctr - 1;

      one_bit_dac dac_inst (
          .clk(sys_clk),  // 50MHz clock
          .clk_en(div_by_4_ctr == 0),  // 12.5MHz clock enable
          .in(ym2149_sound),  // input 16 PCM audio signal.
          .out(audio_out)  // one bit output signal, modulated at 12.5MHz
`ifdef VERILATOR,
          .acc1_overflow(acc1_overflow)  //In simulation, check DAC accumulators for overflow.
          , .acc2_overflow(acc2_overflow)  //In simulation, check DAC accumulators for overflow.
`endif
      );

`ifdef VERILATOR
      assign pcm_out = ym2149_sound;
`endif
      assign audio_gain = 1'b1;  //PMOD Amp gain fixed at 6dB.
      assign audio_shutdown_n = 1'b1;
    end else begin : NO_AUDIO
      assign audio_out = 1'b0;
      assign audio_gain = 1'b0;
      assign audio_shutdown_n = 1'b0;
`ifdef VERILATOR
      assign pcm_out = 16'b0;
      assign acc1_overflow = 1'b0;
      assign acc2_overflow = 1'b0;
`endif
    end
  endgenerate

  //Two USB HID host cores
  generate
    if (USB_HID_ACTIVE) begin : GENERATE_USB_HID_MODULES
      usb_hid_host_top usb_hid0_host_inst (
          .wb_clk   (sys_clk),                     // Wishbone clock is in the system clock domain.
          .usb_clk  (usb_clk),                     // 12MHz USB clock.
          .usb_rst_n(~usb_reset),                  // USB clock domain active low reset
          .wb_rst_n (~ndm_reset),                  // System clock domain active low reset
          .usb_dm_i (usb0_dm_i),
          .usb_dp_i (usb0_dp_i),                   // USB D- and D+ input
          .usb_dm_o (usb0_dm_o),
          .usb_dp_o (usb0_dp_o),                   // USB D- and D+ output
          .usb_oe   (usb0_oe),
          .irq      (fast_irqs[IRQ_ID_USB_HID_0]),

          //32-bit pipelined Wishbone slave interface.
          .wbs_adr(data_bus_wbs[DATA_BUS_USB_HID_0_S].adr[3:0]),
          .wbs_dat_w(data_bus_wbs[DATA_BUS_USB_HID_0_S].dat_m),
          .wbs_dat_r(data_bus_wbs[DATA_BUS_USB_HID_0_S].dat_s),
          .wbs_sel(data_bus_wbs[DATA_BUS_USB_HID_0_S].sel),
          .wbs_stall(data_bus_wbs[DATA_BUS_USB_HID_0_S].stall),
          .wbs_cyc(data_bus_wbs[DATA_BUS_USB_HID_0_S].cyc),
          .wbs_stb(data_bus_wbs[DATA_BUS_USB_HID_0_S].stb),
          .wbs_ack(data_bus_wbs[DATA_BUS_USB_HID_0_S].ack),
          .wbs_we(data_bus_wbs[DATA_BUS_USB_HID_0_S].we),
          .wbs_err(data_bus_wbs[DATA_BUS_USB_HID_0_S].err)
      );

      usb_hid_host_top usb_hid1_host_inst (
          .wb_clk   (sys_clk),                     // Wishbone clock is in the system clock domain.
          .usb_clk  (usb_clk),                     // 12MHz clock
          .usb_rst_n(~usb_reset),                  // USB clock domain active low reset
          .wb_rst_n (~ndm_reset),                  // System clock domain active low reset
          .usb_dm_i (usb1_dm_i),
          .usb_dp_i (usb1_dp_i),                   // USB D- and D+ input
          .usb_dm_o (usb1_dm_o),
          .usb_dp_o (usb1_dp_o),                   // USB D- and D+ output
          .usb_oe   (usb1_oe),
          .irq      (fast_irqs[IRQ_ID_USB_HID_1]),

          //32-bit pipelined Wishbone slave interface.
          .wbs_adr(data_bus_wbs[DATA_BUS_USB_HID_1_S].adr[3:0]),
          .wbs_dat_w(data_bus_wbs[DATA_BUS_USB_HID_1_S].dat_m),
          .wbs_dat_r(data_bus_wbs[DATA_BUS_USB_HID_1_S].dat_s),
          .wbs_sel(data_bus_wbs[DATA_BUS_USB_HID_1_S].sel),
          .wbs_stall(data_bus_wbs[DATA_BUS_USB_HID_1_S].stall),
          .wbs_cyc(data_bus_wbs[DATA_BUS_USB_HID_1_S].cyc),
          .wbs_stb(data_bus_wbs[DATA_BUS_USB_HID_1_S].stb),
          .wbs_ack(data_bus_wbs[DATA_BUS_USB_HID_1_S].ack),
          .wbs_we(data_bus_wbs[DATA_BUS_USB_HID_1_S].we),
          .wbs_err(data_bus_wbs[DATA_BUS_USB_HID_1_S].err)
      );
    end else begin
      assign usb0_dm_o = 1'b0;
      assign usb0_dp_o = 1'b0;
      assign usb0_oe = 1'b0;
      assign usb1_dm_o = 1'b0;
      assign usb1_dp_o = 1'b0;
      assign usb1_oe = 1'b0;
      assign fast_irqs[IRQ_ID_USB_HID_0] = 1'b0;
      assign fast_irqs[IRQ_ID_USB_HID_1] = 1'b0;
    end
  endgenerate

  //Flash SPI Core
  generate
    if (SPIFLASH_ACTIVE) begin : GENERATE_SPIFLASH_MODULE
      wire flash_wb_cyc;
      wire flash_wb_stb;
      wire flash_cfg_stb;
      wire flash_wb_we;
      wire [21:0]    flash_wb_addr;
      wire [31:0]    flash_wb_dat_m;
      wire flash_wb_stall;
      wire flash_wb_ack;
      wire [31:0] flash_wb_dat_s;

      //This is a hack: These are actually two WB ports with all signals shared
      //except the STB signal.
      assign flash_wb_cyc = flash_usr_wb_if.cyc | data_bus_wbs[DATA_BUS_FLASH_CTRL_S].cyc;
      assign flash_wb_stb = flash_usr_wb_if.stb;
      assign flash_cfg_stb = data_bus_wbs[DATA_BUS_FLASH_CTRL_S].stb;
      assign flash_wb_we = flash_usr_wb_if.cyc ? flash_usr_wb_if.we : data_bus_wbs[DATA_BUS_FLASH_CTRL_S].we;
      assign flash_wb_addr = flash_usr_wb_if.cyc ? flash_usr_wb_if.adr[21:0] : data_bus_wbs[DATA_BUS_FLASH_CTRL_S].adr[21:0];
      assign flash_wb_dat_m = flash_usr_wb_if.cyc ? flash_usr_wb_if.dat_m : data_bus_wbs[DATA_BUS_FLASH_CTRL_S].dat_m;

      assign flash_usr_wb_if.stall = flash_usr_wb_if.cyc & flash_wb_stall;
      assign data_bus_wbs[DATA_BUS_FLASH_CTRL_S].stall = data_bus_wbs[DATA_BUS_FLASH_CTRL_S].cyc & flash_wb_stall;
      assign flash_usr_wb_if.ack = flash_usr_wb_if.cyc & flash_wb_ack;
      assign data_bus_wbs[DATA_BUS_FLASH_CTRL_S].ack = data_bus_wbs[DATA_BUS_FLASH_CTRL_S].cyc & flash_wb_ack;
      assign flash_usr_wb_if.dat_s = flash_wb_dat_s;
      assign data_bus_wbs[DATA_BUS_FLASH_CTRL_S].dat_s = flash_wb_dat_s;

      spiflash #(
          .SCK_CLKDIV(2)
      ) spiflash_inst (
          .i_clk(sys_clk),
          .i_reset(ndm_reset),
          //This is a hack: These are actually two WB ports with all signals shared
          //except the STB signal.
          .i_wb_cyc(flash_wb_cyc),
          .i_wb_stb(flash_wb_stb),
          .i_cfg_stb(flash_cfg_stb),
          .i_wb_we(flash_wb_we),
          .i_wb_addr(flash_wb_addr),
          .i_wb_data(flash_wb_dat_m),
          //
          .o_wb_stall(flash_wb_stall),
          .o_wb_ack(flash_wb_ack),
          .o_wb_data(flash_wb_dat_s),
          //
          .o_spi_cs_n(spiflash_cs_n),
          .o_spi_sck(spiflash_sck),
          .o_spi_mosi(spiflash_mosi),
          .i_spi_miso(spiflash_miso)
      );
    end else begin
      assign spiflash_sck  = 1'b0;
      assign spiflash_cs_n = 1'b1;
      assign spiflash_mosi = 1'b0;
    end
  endgenerate

  //I2C Core
  generate
    if (I2C_ACTIVE) begin : GENERATE_I2C_MODULE
      wbi2cmaster #(
          .MEM_ADDR_BITS(8),
          .LITTLE_ENDIAN(1'b1)
      ) i2cmaster_inst (
          .i_clk(sys_clk),
          .i_reset(ndm_reset),
          // Wishbone
          // Input bus wires
          .i_wb_cyc(data_bus_wbs[DATA_BUS_I2C_S].cyc),
          .i_wb_stb(data_bus_wbs[DATA_BUS_I2C_S].stb),
          .i_wb_we(data_bus_wbs[DATA_BUS_I2C_S].we),
          .i_wb_addr(data_bus_wbs[DATA_BUS_I2C_S].adr[6:0]),
          .i_wb_data(data_bus_wbs[DATA_BUS_I2C_S].dat_m),
          .i_wb_sel(data_bus_wbs[DATA_BUS_I2C_S].sel),
          // Output bus wires
          .o_wb_stall(data_bus_wbs[DATA_BUS_I2C_S].stall),
          .o_wb_ack(data_bus_wbs[DATA_BUS_I2C_S].ack),
          .o_wb_data(data_bus_wbs[DATA_BUS_I2C_S].dat_s),
          // I2C clock and data wires
          .i_i2c_scl(i2c_scl_i),
          .i_i2c_sda(i2c_sda_i),
          .o_i2c_scl(i2c_scl_o),
          .o_i2c_sda(i2c_sda_o),
          // And our output interrupt
          .o_int(fast_irqs[IRQ_ID_I2C]),
          // And some debug wires
          .o_dbg()
      );
    end else begin
      assign i2c_scl_o = 1'b1;
      assign i2c_sda_o = 1'b1;
      assign fast_irqs[IRQ_ID_I2C] = 1'b0;
    end
    assign data_bus_wbs[DATA_BUS_I2C_S].err = 1'b0;
  endgenerate

  //Set the remaining bits in the fast_irq vector to 0
  assign fast_irqs[IRQ_ID_NA_0] = 1'b0;
  assign fast_irqs[IRQ_ID_NA_1] = 1'b0;
  assign fast_irqs[IRQ_ID_NA_2] = 1'b0;
  assign fast_irqs[IRQ_ID_NA_3] = 1'b0;
  assign fast_irqs[IRQ_ID_NA_4] = 1'b0;
  assign fast_irqs[IRQ_ID_NA_5] = 1'b0;
endmodule

