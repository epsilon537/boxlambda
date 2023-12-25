// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// This module is based on lowRISC's clkgen_xil7series in the ibex repo, modified with additional
// output clocks.
// ext_clk input clock is the 100MHz external clock.
// clk_50 frequency is half in the input clock frequency -> 50MHz 
// clk_100 is double clk_50, phase aligned with clk_50 -> 100MHz
// clk_12 frequency is 12MHz
module boxlambda_clk_gen (
    input wire ext_clk_100,
    input wire rst_n,
    output wire clk_50,
    output wire clk_100,
    output wire clk_12,
    output wire locked //PLL locked indication.
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
    .I (ext_clk_100),
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
  assign clk_50 = clk_50_buf;
  assign clk_100 = clk_100_buf;
  assign clk_12 = clk_12_buf;

  // PLL lock ouput
  assign locked = locked_pll & rst_n;
`else //Simulation:
  logic clk_50_reg;
  logic [2:0] div_by_8_count;

  initial begin
    clk_50_reg = 1'b0;
  end

  always_ff @(posedge ext_clk_100) begin
    clk_50_reg <= ~clk_50_reg;
    div_by_8_count <= div_by_8_count + 3'd1;
  end

  //Aproximate a 12MHz clock in simulation by doing divide-by-8 of the 100MHz input clock.
  assign clk_12 = (div_by_8_count < 3'd4);
  assign clk_50 = clk_50_reg;
  assign clk_100 = ext_clk_100;
  assign locked = rst_n;
`endif
endmodule
