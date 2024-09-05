/*This is a stub component for Reconfigurable module 0. RM0 resides on the crossbar with a Wishbone master and slave port, and irq inputs and output. */
module rm0 (
    input logic sys_clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface.
    output logic [27:0] wbm_adr_o,
    output logic [31:0] wbm_dat_o,
    input logic [31:0] wbm_dat_i,
    output logic wbm_we_o,
    output logic [3:0] wbm_sel_o,
    output logic wbm_stb_o,
    input logic wbm_ack_i,
    input logic wbm_stall_i,
    output logic wbm_cyc_o,
    input logic wbm_err_i,

    //32-bit pipelined Wishbone slave interface.
    input logic [19:0] wbs_adr,
    input logic [31:0] wbs_dat_w,
    output logic [31:0] wbs_dat_r,
    input logic [3:0] wbs_sel,
    output logic wbs_stall,
    input logic wbs_cyc,
    input logic wbs_stb,
    output logic wbs_ack,
    input logic wbs_we,
    output logic wbs_err,

    //Input IRQs from the rest of the system.
    input wire [31:0] irq_in,
    //Output IRQ signal
    output wire irq_out
);

  logic unused = &{wbm_dat_i, wbm_ack_i, wbm_stall_i, wbm_err_i, wbs_adr, wbs_dat_w, wbs_sel, wbs_we};

  //Just acknowledge incoming transactions.
  always_ff @(posedge sys_clk) begin
    wbs_ack <= wbs_stb & wbs_cyc;
  end

  assign wbm_adr_o = 28'b0;
  assign wbm_dat_o = 32'b0;
  assign wbm_we_o  = 1'b0;
  assign wbm_sel_o = 4'b0;

  assign wbm_cyc_o = 1'b0;
  assign wbm_stb_o = 1'b0;

  assign wbs_stall = 1'b0;
  assign wbs_err   = 1'b0;
  assign wbs_dat_r = 32'b0;
  assign irq_out   = 1'b0;
endmodule
