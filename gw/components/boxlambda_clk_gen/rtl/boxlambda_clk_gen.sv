// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// This module is based on lowRISC's clkgen_xil7series in the ibex repo, modified with two additional
// output clocks: clk_sysx2 and clk_usb.
// IO_CLK input clock is the 100MHz external clock.
// clk_sys frequency is half in the input clock frequency -> 50MHz 
// clk_sysx2 is double clk_sys, phase aligned with clk_sys -> 100MHz
// clk_usb frequency is 12MHz
module boxlambda_clk_gen (
    input wire IO_CLK,
    input wire IO_RST_N,
    output wire clk_sys,
    output wire clk_sysx2,
    output wire clk_usb,
    output wire locked
);
`ifdef SYNTHESIS
  logic locked_pll;
  logic io_clk_buf;
  logic clk_100_buf;
  logic clk_100_unbuf;
  logic clk_50_buf;
  logic clk_50_unbuf;
  logic clk_fb_buf;
  logic clk_fb_unbuf;
  logic clk_12_unbuf;
  logic clk_12_buf;

  // input buffer
  IBUF io_clk_ibuf(
    .I (IO_CLK),
    .O (io_clk_buf)
  );

  PLLE2_ADV #(
    .BANDWIDTH            ("OPTIMIZED"),
    .COMPENSATION         ("ZHOLD"),
    .STARTUP_WAIT         ("FALSE"),
    .DIVCLK_DIVIDE        (1),
    .CLKFBOUT_MULT        (12),
    .CLKFBOUT_PHASE       (0.000),
    .CLKOUT0_DIVIDE       (24),
    .CLKOUT0_PHASE        (0.000),
    .CLKOUT0_DUTY_CYCLE   (0.500),
    .CLKOUT1_DIVIDE       (12),
    .CLKOUT1_PHASE        (0.000),
    .CLKOUT1_DUTY_CYCLE   (0.500),
    .CLKOUT2_DIVIDE       (100),
    .CLKOUT2_PHASE        (0.000),
    .CLKOUT2_DUTY_CYCLE   (0.500),
    .CLKIN1_PERIOD        (10)
  ) pll (
    .CLKFBOUT            (clk_fb_unbuf),
    .CLKOUT0             (clk_50_unbuf),
    .CLKOUT1             (clk_100_unbuf),
    .CLKOUT2             (clk_12_unbuf),
    .CLKOUT3             (),
    .CLKOUT4             (),
    .CLKOUT5             (),
     // Input clock control
    .CLKFBIN             (clk_fb_buf),
    .CLKIN1              (io_clk_buf),
    .CLKIN2              (1'b0),
     // Tied to always select the primary input clock
    .CLKINSEL            (1'b1),
    // Ports for dynamic reconfiguration
    .DADDR               (7'h0),
    .DCLK                (1'b0),
    .DEN                 (1'b0),
    .DI                  (16'h0),
    .DO                  (),
    .DRDY                (),
    .DWE                 (1'b0),
    // Other control and status signals
    .LOCKED              (locked_pll),
    .PWRDWN              (1'b0),
    // Do not reset PLL on external reset, otherwise ILA disconnects at a reset
    .RST                 (1'b0));

  // output buffering
  BUFG clk_fb_bufg (
    .I (clk_fb_unbuf),
    .O (clk_fb_buf)
  );

  BUFG clk_50_bufg (
    .I (clk_50_unbuf),
    .O (clk_50_buf)
  );

  BUFG clk_100_bufg (
    .I (clk_100_unbuf),
    .O (clk_100_buf)
  );

  BUFG clk_12_bufg (
    .I (clk_12_unbuf),
    .O (clk_12_buf)
  );

  // outputs
  // clock
  assign clk_sys = clk_50_buf;
  assign clk_sysx2 = clk_100_buf;
  assign clk_usb = clk_12_buf;

  // PLL lock ouput
  assign locked = locked_pll & IO_RST_N;
`else //Simulation:
  logic clk_sys_reg;
  logic [2:0] div_by_8_count;

  initial begin
    clk_sys_reg = 1'b0;
  end

  always_ff @(posedge IO_CLK) begin
    clk_sys_reg <= ~clk_sys_reg;
    div_by_8_count <= div_by_8_count + 3'd1;
  end

  assign clk_usb = (div_by_8_count < 3'd4);
  assign clk_sys = clk_sys_reg;
  assign clk_sysx2 = IO_CLK;
  assign locked = IO_RST_N;
`endif
endmodule
