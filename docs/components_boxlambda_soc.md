---
hide:
  - toc
---

## The BoxLambda SoC Component

- **BoxLambda SoC Component in the BoxLambda Directory Tree**:
    [boxlambda/gw/components/boxlambda_soc](https://github.com/epsilon537/boxlambda/tree/master/gw/components/boxlambda_soc)

- **BoxLambda SoC Module**:
    [gw/components/boxlambda_soc/rtl/boxlambda_soc.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/boxlambda_soc/rtl/boxlambda_soc.sv)

- **BoxLambda Top Module**:
    [gw/projects/boxlambda_top/rtl/boxlambda_top.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/boxlambda_top/rtl/boxlambda_top.sv)

The BoxLambda SoC module is defined as a gateware component.

```
module boxlambda_soc #(
    parameter DPRAM_BYTE_ADDR_MASK = 'h1ffff, // DPRAM size as a mask value. Used for both CMEM and DMEM.
    parameter VRAM_SIZE_BYTES = 131072,
    parameter DEBUG_MODULE_ACTIVE = 1,
    parameter DRAM_ACTIVE = 1,
    parameter VERA_ACTIVE = 1,
    parameter SDSPI_ACTIVE = 1,
    parameter YM2149_ACTIVE = 1,
    parameter PICORV_ACTIVE = 1,
    parameter USB_HID_ACTIVE = 1,
    parameter SPIFLASH_ACTIVE = 1,
    parameter I2C_ACTIVE = 1,
    parameter DFX_ACTIVE = 1,
    parameter VS0_ACTIVE = 1,
    parameter CMEM_FILE = "",
    parameter DMEM_FILE = ""
) (
    input  wire        ext_clk_100,         // 100 MHz external clock.
    input  wire        ext_rst_n,           // External reset pin.
`ifdef VERILATOR
    /* These JTAG signals are not used on FPGA (they are used in simulation).
     * On FPGA, the JTAG signals are driven by a BSCANE2 primitive inside the JTAG TAP module dmi_bscane_tap.sv.
     */
    input  wire        tck,
    input  wire        trst_n,
    input  wire        tms,
    input  wire        tdi,
    output wire        tdo,
`endif
    output wire        pll_locked_led,      // PLL locked indication.
    output wire        init_done_led,       // LiteDRAM initialization done indication.
    output wire        init_err_led,        // LiteDRAM initialization error indication.
    output wire        sd_card_detect_led,
`ifdef SYNTHESIS
    /* The simulation build doesn't export DDR pins. */
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
```

The [Gateware Build Structure](build_sys_gw_build_struct.md#the-gateware-build-structure) section describes the three-layer gateware build structure. The diagram below shows how the BoxLambda SoC component fits into that build structure.

![BoxLambda SoC Component Build Diagram](assets/BoxLambda_SoC_Component_Build_Diagram.png)

*Build diagram with the BoxLambda SoC component and `boxlambda_top.sv`.*

Different *gw/project/* builds reference this *boxlambda_soc* component. The project builds vary in the way they instantiate the *boxlambda_soc* module, including or excluding specific subcomponents. Most *gw/projects* reference the same [boxlambda_top.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/boxlambda_top/rtl/boxlambda_top.sv) module but with a different combination of *defines* in their *Bender.yml* manifest.

*Gw/project/* builds also differ in the software program they run on the SoC. This is specified in the *gw/project*'s `CMakefile`.
