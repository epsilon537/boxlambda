// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// This module is based on lowRISC's clkgen_xil7series in the ibex repo, modified with additional
// output clocks.
// ext_clk_100 input clock is the 100MHz external clock.
// sys_clk frequency is 25MHz or 50MHz depending on the SYS_CLK_25MHZ flag.
// sys_clk2x is double sys_clk, phase aligned with sys_clk.
// clk_12 frequency is 12MHz
module boxlambda_clk_gen (
    input wire ext_clk_100,
    input wire rst_n,
    output wire sys_clk,
    output wire sys_clk2x,
    output wire clk_12,
    output wire locked  //PLL locked indication.
);
`ifdef SYNTHESIS
  logic locked_pll;
  logic io_clk_buf;
  logic sys_clk2x_buf;
  logic sys_clk2x_unbuf;
  logic sys_clk_buf;
  logic sys_clk_unbuf;
  logic clk_fb_buf;
  logic clk_fb_unbuf;
  logic clk_12_unbuf;
  logic clk_12_buf;

  // input buffer
  IBUF io_clk_ibuf (
      .I(ext_clk_100),
      .O(io_clk_buf)
  );

  PLLE2_ADV #(
      .BANDWIDTH         ("OPTIMIZED"),
      .COMPENSATION      ("ZHOLD"),
      .STARTUP_WAIT      ("FALSE"),
      .DIVCLK_DIVIDE     (1),
      .CLKFBOUT_MULT     (12),
      .CLKFBOUT_PHASE    (0.000),
`ifdef SYS_CLK_25MHZ
      .CLKOUT0_DIVIDE    (48),
`else
      .CLKOUT0_DIVIDE    (24),
`endif
      .CLKOUT0_PHASE     (0.000),
      .CLKOUT0_DUTY_CYCLE(0.500),
`ifdef SYS_CLK_25MHZ
      .CLKOUT1_DIVIDE    (24),
`else
      .CLKOUT1_DIVIDE    (12),
`endif
      .CLKOUT1_PHASE     (0.000),
      .CLKOUT1_DUTY_CYCLE(0.500),
      .CLKOUT2_DIVIDE    (100),
      .CLKOUT2_PHASE     (0.000),
      .CLKOUT2_DUTY_CYCLE(0.500),
      .CLKIN1_PERIOD     (10)
  ) pll (
      .CLKFBOUT(clk_fb_unbuf),
      .CLKOUT0 (sys_clk_unbuf),
      .CLKOUT1 (sys_clk2x_unbuf),
      .CLKOUT2 (clk_12_unbuf),
      .CLKOUT3 (),
      .CLKOUT4 (),
      .CLKOUT5 (),
      // Input clock control
      .CLKFBIN (clk_fb_buf),
      .CLKIN1  (io_clk_buf),
      .CLKIN2  (1'b0),
      // Tied to always select the primary input clock
      .CLKINSEL(1'b1),
      // Ports for dynamic reconfiguration
      .DADDR   (7'h0),
      .DCLK    (1'b0),
      .DEN     (1'b0),
      .DI      (16'h0),
      .DO      (),
      .DRDY    (),
      .DWE     (1'b0),
      // Other control and status signals
      .LOCKED  (locked_pll),
      .PWRDWN  (1'b0),
      // Do not reset PLL on external reset, otherwise ILA disconnects at a reset
      .RST     (1'b0)
  );

  // output buffering
  BUFG clk_fb_bufg (
      .I(clk_fb_unbuf),
      .O(clk_fb_buf)
  );

  BUFG sys_clk_bufg (
      .I(sys_clk_unbuf),
      .O(sys_clk_buf)
  );

  BUFG sys_clk2x_bufg (
      .I(sys_clk2x_unbuf),
      .O(sys_clk2x_buf)
  );

  BUFG clk_12_bufg (
      .I(clk_12_unbuf),
      .O(clk_12_buf)
  );

  // outputs
  // clock
  assign sys_clk = sys_clk_buf;
  assign sys_clk2x = sys_clk2x_buf;
  assign clk_12 = clk_12_buf;

  // PLL lock output
  assign locked = locked_pll & rst_n;
`else  //Simulation:
  logic [1:0] sys_clk_reg;
  logic [2:0] div_by_8_count;

  initial sys_clk_reg = 0;

  always_ff @(posedge ext_clk_100) begin
`ifdef SYS_CLK_25MHZ
    sys_clk_reg <= sys_clk_reg + 2'd1;
`else
    sys_clk_reg <= sys_clk_reg + 2'd2;
`endif
    div_by_8_count <= div_by_8_count + 3'd1;
  end

  //Aproximate a 12MHz clock in simulation by doing divide-by-8 of the 100MHz input clock.
  assign clk_12  = (div_by_8_count < 3'd4);
  assign sys_clk = (sys_clk_reg < 2'd2);
`ifdef SYS_CLK_25MHZ
  logic sys_clk2x_reg;

  initial sys_clk2x_reg = 0;

  always_ff @(posedge ext_clk_100) sys_clk2x_reg <= ~sys_clk2x_reg;

  assign sys_clk2x = sys_clk2x_reg;
`else
  assign sys_clk2x = ext_clk_100;
`endif
  assign locked = rst_n;
`endif
endmodule
