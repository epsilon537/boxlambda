//FIXME: dependency tracking isn't working on include files.
`ifdef SYNTHESIS
`ifdef SYS_CLK_25MHZ
`include "fpga_25mhz/rtl/litedram.v"
`else
`include "fpga_50mhz/rtl/litedram.v"
`endif
`else
`ifdef SYS_CLK_25MHZ
`include "sim_25mhz/rtl/litedram.v"
`else
`include "sim_50mhz/rtl/litedram.v"
`endif
`endif

/*This wrapper contains wishbone pipeline-to-classic adapters for the ctrl and user port.*/
module litedram_wrapper (
    input wire clk,  /*100MHz or 50 MHz input clock.*/
    input wire rst,  /*Asynchronous reset.*/
    output wire        sys_clk, /*50MHz or 25MHz output clock, to be used as input clock for the rest of the system.*/
    output wire        sys_clkx2, /*100MHz or 25MHz output clock, i.e. twice the rate of sys_clk, in phase with sys_clk.*/
    output wire sys_rst,  /*Synchronous reset signal for the system.*/
    output wire pll_locked,
`ifdef SYNTHESIS
    /*The DDR signals are not exported in the simulation model.*/
    output wire [13:0] ddram_a,
    output wire [2:0] ddram_ba,
    output wire ddram_ras_n,
    output wire ddram_cas_n,
    output wire ddram_we_n,
    output wire ddram_cs_n,
    output wire [1:0] ddram_dm,
    inout wire [15:0] ddram_dq,
    inout wire [1:0] ddram_dqs_p,
    inout wire [1:0] ddram_dqs_n,
    output wire ddram_clk_p,
    output wire ddram_clk_n,
    output wire ddram_cke,
    output wire ddram_odt,
    output wire ddram_reset_n,
`endif
    output wire init_done,
    output wire init_error,

    /*The control port for CSR reads and writes. Wishbone pipelined signals.*/
    input  wire [27:0] wb_ctrl_adr,
    input  wire [31:0] wb_ctrl_dat_w,
    output wire [31:0] wb_ctrl_dat_r,
    input  wire [ 3:0] wb_ctrl_sel,
    output wire        wb_ctrl_stall,
    input  wire        wb_ctrl_cyc,
    input  wire        wb_ctrl_stb,
    output wire        wb_ctrl_ack,
    input  wire        wb_ctrl_we,
    output wire        wb_ctrl_err,

    /*32-bit user port. Wishbone pipelined signals.*/
    input  wire [27:0] user_port_wishbone_p_0_adr,
    input  wire [31:0] user_port_wishbone_p_0_dat_w,
    output wire [31:0] user_port_wishbone_p_0_dat_r,
    input  wire [ 3:0] user_port_wishbone_p_0_sel,
    output wire        user_port_wishbone_p_0_stall,
    input  wire        user_port_wishbone_p_0_cyc,
    input  wire        user_port_wishbone_p_0_stb,
    output wire        user_port_wishbone_p_0_ack,
    input  wire        user_port_wishbone_p_0_we,
    output wire        user_port_wishbone_p_0_err
);

  /*User port. Wishbone classic signals.*/
  logic [25:0] user_port_wishbone_c_0_adr;
  logic [31:0] user_port_wishbone_c_0_dat_w;
  logic [31:0] user_port_wishbone_c_0_dat_r;
  logic [ 3:0] user_port_wishbone_c_0_sel;
  logic        user_port_wishbone_c_0_cyc;
  logic        user_port_wishbone_c_0_stb;
  logic        user_port_wishbone_c_0_ack;
  logic        user_port_wishbone_c_0_we;
  logic        user_port_wishbone_c_0_err;

  assign user_port_wishbone_p_0_dat_r = user_port_wishbone_c_0_dat_r;
  assign user_port_wishbone_p_0_ack = user_port_wishbone_c_0_ack;
  assign user_port_wishbone_p_0_err = user_port_wishbone_c_0_err;
  /*Straight out of the Wishbone B4 spec. This is how you interface a classic slave to a pipelined master.
    *The stall signal ensures that the STB signal remains asserted until an ACK is received from the slave.*/
  assign user_port_wishbone_p_0_stall = !user_port_wishbone_p_0_cyc ? 1'b0 : !user_port_wishbone_c_0_ack;

  assign user_port_wishbone_c_0_stb = user_port_wishbone_p_0_stb;
  assign user_port_wishbone_c_0_adr = user_port_wishbone_p_0_adr[25:0];
  assign user_port_wishbone_c_0_we = user_port_wishbone_p_0_we;
  assign user_port_wishbone_c_0_dat_w = user_port_wishbone_p_0_dat_w;
  assign user_port_wishbone_c_0_sel = user_port_wishbone_p_0_sel;
  assign user_port_wishbone_c_0_cyc = user_port_wishbone_p_0_cyc;

  /*and again for the control port.*/
  logic [29:0] ctrl_port_wishbone_c_adr;
  logic [31:0] ctrl_port_wishbone_c_dat_w;
  logic [31:0] ctrl_port_wishbone_c_dat_r;
  logic [ 3:0] ctrl_port_wishbone_c_sel;
  logic        ctrl_port_wishbone_c_cyc;
  logic        ctrl_port_wishbone_c_stb;
  logic        ctrl_port_wishbone_c_ack;
  logic        ctrl_port_wishbone_c_we;
  logic        ctrl_port_wishbone_c_err;

  assign wb_ctrl_dat_r = ctrl_port_wishbone_c_dat_r;
  assign wb_ctrl_ack = ctrl_port_wishbone_c_ack;
  assign wb_ctrl_err = ctrl_port_wishbone_c_err;
  assign wb_ctrl_stall = !wb_ctrl_cyc ? 1'b0 : !ctrl_port_wishbone_c_ack;

  assign ctrl_port_wishbone_c_stb = wb_ctrl_stb;
  assign ctrl_port_wishbone_c_adr = {2'b00, wb_ctrl_adr};
  assign ctrl_port_wishbone_c_we = wb_ctrl_we;
  assign ctrl_port_wishbone_c_dat_w = wb_ctrl_dat_w;
  assign ctrl_port_wishbone_c_sel = wb_ctrl_sel;
  assign ctrl_port_wishbone_c_cyc = wb_ctrl_cyc;

`ifndef SYNTHESIS
  logic unused = &{rst, user_port_wishbone_p_0_adr[27:26]}; /*Simulation model has no reset port...*/

  assign pll_locked = 1'b1;  /*...and no PLL either.*/
`endif  //not SYNTHESIS

  litedram litedram_inst (
      .clk(clk),
`ifdef SYNTHESIS
      .rst(rst),
      .pll_locked(pll_locked),
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
`else
      .sim_trace(1'b0),
`endif
      .init_done(init_done),
      .init_error(init_error),
      .wb_ctrl_adr(ctrl_port_wishbone_c_adr),
      .wb_ctrl_dat_w(ctrl_port_wishbone_c_dat_w),
      .wb_ctrl_dat_r(ctrl_port_wishbone_c_dat_r),
      .wb_ctrl_sel(ctrl_port_wishbone_c_sel),
      .wb_ctrl_cyc(ctrl_port_wishbone_c_cyc),
      .wb_ctrl_stb(ctrl_port_wishbone_c_stb),
      .wb_ctrl_ack(ctrl_port_wishbone_c_ack),
      .wb_ctrl_we(ctrl_port_wishbone_c_we),
      .wb_ctrl_cti(3'b0),
      .wb_ctrl_bte(2'b0),
      .wb_ctrl_err(ctrl_port_wishbone_c_err),
      .user_clkx2(sys_clkx2),
      .user_clk(sys_clk),
      .user_rst(sys_rst),
      .user_port_wishbone_0_adr(user_port_wishbone_c_0_adr),
      .user_port_wishbone_0_dat_w(user_port_wishbone_c_0_dat_w),
      .user_port_wishbone_0_dat_r(user_port_wishbone_c_0_dat_r),
      .user_port_wishbone_0_sel(user_port_wishbone_c_0_sel),
      .user_port_wishbone_0_cyc(user_port_wishbone_c_0_cyc),
      .user_port_wishbone_0_stb(user_port_wishbone_c_0_stb),
      .user_port_wishbone_0_ack(user_port_wishbone_c_0_ack),
      .user_port_wishbone_0_we(user_port_wishbone_c_0_we),
      .user_port_wishbone_0_err(user_port_wishbone_c_0_err)
  );
endmodule
