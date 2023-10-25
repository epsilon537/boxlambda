// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// This module is based on lowRISC's clkgen_xil7series in the ibex repo, modified with an additional
// output clock: clk_sysx2.
// IO_CLK input clock is the 100MHz external clock.
// clk_sys frequency is half in the input clock frequency -> 50MHz 
// clk_sysx2 is double clk_sys, phase aligned with clk_sys -> 100MHz
module boxlambda_clk_gen (
    input IO_CLK,
    input IO_RST_N,
    output clk_sys,
    output clk_sysx2,
    output locked
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
    .CLKIN1_PERIOD        (10)
  ) pll (
    .CLKFBOUT            (clk_fb_unbuf),
    .CLKOUT0             (clk_50_unbuf),
    .CLKOUT1             (clk_100_unbuf),
    .CLKOUT2             (),
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

  // outputs
  // clock
  assign clk_sys = clk_50_buf;
  assign clk_sysx2 = clk_100_buf;

  // PLL lock ouput
  assign locked = locked_pll & IO_RST_N;
`else //Simulation:
  logic clk_sys_reg;
  initial begin
    clk_sys_reg = 1'b0;
  end

  always_ff @(posedge IO_CLK) begin
    clk_sys_reg <= ~clk_sys_reg;
  end

  assign clk_sys = clk_sys_reg;
  assign clk_sysx2 = IO_CLK;
  assign locked = IO_RST_N;
`endif
endmodule
