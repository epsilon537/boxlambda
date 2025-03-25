module interconnect_ooc #(
    parameter RAM_BYTE_ADDR_MASK = 'h1ffff,
    parameter DATA_WIDTH = 32  // width of data bus in bits (8, 16, 32, or 64)
) (
    input wire clk,
    input wire rst
);

  import wb_pkg::*;

  localparam AW = 28;

  localparam [19*AW-1:0] SLAVE_ADDRESSES_19 = {
    /*DDR_USR_S*/{AW'('h20000000 >> 2)},
    /*VS0_S*/{AW'('h13000000 >> 2)},
    /*VERA_S*/{AW'('h12000000 >> 2)},
    /*DMEM_1_S*/{AW'('h00020000 >> 2)},
    /*CMEM_1_S*/{AW'('h00000000 >> 2)},
    /*FLASH_USR_S*/{AW'('h11000000 >> 2)},
    /*DM_S*/{AW'('h10040000 >> 2)},
    /*DDR_CTRL_S*/{AW'('h10030000 >> 2)},
    /*TIMER_S*/{AW'('h10020000 >> 2)},
    /*UART_S*/{AW'('h10010000 >> 2)},
    /*YM2149_S*/{AW'('h10001000 >> 2)},
    /*DFX_S*/{AW'('h10000400 >> 2)},
    /*I2C_S*/{AW'('h10000200 >> 2)},
    /*GPIO_S*/{AW'('h10000100 >> 2)},
    /*RESET_CTRL_S*/{AW'('h100000D0 >> 2)},
    /*FLASH_CTRL_S*/{AW'('h100000C0 >> 2)},
    /*USB_HID_1_S*/{AW'('h10000080 >> 2)},
    /*USB_HID_0_S*/{AW'('h10000040 >> 2)},
    /*SDSPI_S*/{AW'('h10000020 >> 2)}
  };

  localparam [19*AW-1:0] SLAVE_ADDR_MASKS_19 = {
    /*DDR_USR_S*/{AW'(~('h0fffffff >> 2))},
    /*VS0_S*/{AW'(~('h000fffff >> 2))},
    /*VERA_S*/{AW'(~('h0007ffff >> 2))},
    /*DMEM_1_S*/{AW'(~(RAM_BYTE_ADDR_MASK >> 2))},
    /*CMEM_1_S*/{AW'(~(RAM_BYTE_ADDR_MASK >> 2))},
    /*FLASH_USR_S*/{AW'(~('h00ffffff >> 2))},
    /*DM_S*/{AW'(~('h0000ffff >> 2))},
    /*DDR_CTRL_S*/{AW'(~('h0000ffff >> 2))},
    /*TIMER_S*/{AW'(~('h000003ff >> 2))},
    /*UART_S*/{AW'(~('h0000001f >> 2))},
    /*YM2149_S*/{AW'(~('h000003ff >> 2))},
    /*DFX_S*/{AW'(~('h0000007f >> 2))},
    /*I2C_S*/{AW'(~('h000001ff >> 2))},
    /*GPIO_S*/{AW'(~('h0000003f >> 2))},
    /*RESET_CTRL_S*/{AW'(~('h00000007 >> 2))},
    /*FLASH_CTRL_S*/{AW'(~('h00000007 >> 2))},
    /*USB_HID_1_S*/{AW'(~('h0000003f >> 2))},
    /*USB_HID_0_S*/{AW'(~('h0000003f >> 2))},
    /*SDSPI_S*/{AW'(~('h0000001f >> 2))}
  };

  localparam [5*AW-1:0] SLAVE_ADDRESSES_5 = {
    /*RESET_CTRL_S*/{AW'('h100000D0 >> 2)},
    /*FLASH_CTRL_S*/{AW'('h100000C0 >> 2)},
    /*USB_HID_1_S*/{AW'('h10000080 >> 2)},
    /*USB_HID_0_S*/{AW'('h10000040 >> 2)},
    /*SDSPI_S*/{AW'('h10000020 >> 2)}
  };

  localparam [5*AW-1:0] SLAVE_ADDR_MASKS_5 = {
    /*RESET_CTRL_S*/{AW'(~('h00000007 >> 2))},
    /*FLASH_CTRL_S*/{AW'(~('h00000007 >> 2))},
    /*USB_HID_1_S*/{AW'(~('h0000003f >> 2))},
    /*USB_HID_0_S*/{AW'(~('h0000003f >> 2))},
    /*SDSPI_S*/{AW'(~('h0000001f >> 2))}
  };

  wb_if wbm_2[2] (
      .rst(rst),
      .clk(clk)
  );

  wb_if wbs_5[5] (
      .rst(rst),
      .clk(clk)
  );

  wb_if wbm_4[4] (
      .rst(rst),
      .clk(clk)
  );

  wb_if wbs_19[19] (
      .rst(rst),
      .clk(clk)
  );

  instruction_bus #(
      .DATA_WIDTH(DATA_WIDTH),
      .ADDR_WIDTH(AW),
      .ARB_TYPE_ROUND_ROBIN(1),
      .SLAVE_ADDRESSES(SLAVE_ADDRESSES_5),
      .SLAVE_ADDR_MASKS(SLAVE_ADDR_MASKS_5)
  ) u_instruction_bus (
      .clk(clk),
      .rst(rst),
      .wbm(wbm_2),
      .wbs(wbs_5)
  );

  data_bus #(
      .DATA_WIDTH(DATA_WIDTH),
      .ADDR_WIDTH(AW),
      .ARB_TYPE_ROUND_ROBIN(1),
      .SLAVE_ADDRESSES(SLAVE_ADDRESSES_19),
      .SLAVE_ADDR_MASKS(SLAVE_ADDR_MASKS_19)
  ) u_data_bus (
      .clk(clk),
      .rst(rst),
      .wbm(wbm_4),
      .wbs(wbs_19)
  );
endmodule
