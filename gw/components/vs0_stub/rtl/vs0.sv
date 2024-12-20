// This is a stub Reconfigurable Module fitting in Virtual Socket 0 (VS0). 
// VS0 resides on the crossbar with a Wishbone master and slave port, and irq inputs and output.
// Reconfigurable Modules and Virtual sockets are part of the Dynamic Function
// Exchange (DFX) a.k.a. Partial FPGA Reconfiguration framework. Refer to
// BoxLambda's documentation for more info.
// A stub is an almost-but-not entirely empty module. All it does is properly terminate
// the VS0 interface signals, so they aren't left dangling. Wishbone reads and writes to
// the module are acknowledged but don't trigger any functionality. All wishbone read
// operations from the VS0 address range return the stub module's signature value.

module vs0 (
    input logic sys_clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface 0.
    output logic [27:0] wbm_0_adr_o,
    output logic [31:0] wbm_0_dat_o,
    input logic [31:0] wbm_0_dat_i,
    output logic wbm_0_we_o,
    output logic [3:0] wbm_0_sel_o,
    output logic wbm_0_stb_o,
    input logic wbm_0_ack_i,
    input logic wbm_0_stall_i,
    output logic wbm_0_cyc_o,
    input logic wbm_0_err_i,

    //32-bit pipelined Wishbone master interface 1.
    output logic [27:0] wbm_1_adr_o,
    output logic [31:0] wbm_1_dat_o,
    input logic [31:0] wbm_1_dat_i,
    output logic wbm_1_we_o,
    output logic [3:0] wbm_1_sel_o,
    output logic wbm_1_stb_o,
    input logic wbm_1_ack_i,
    input logic wbm_1_stall_i,
    output logic wbm_1_cyc_o,
    input logic wbm_1_err_i,

    //32-bit pipelined Wishbone slave interface.
    input logic [17:0] wbs_adr,
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

  // The static design portion of a DFX build expects to see an empty module 
  // declaration for Virtual Socket 0. That is what this ifndef is for.
`ifndef DFX_SYNTHESIZE_EMPTY_MODULE

  localparam [31:0] STUB_SIGNATURE = 32'h0000510b;  //Signature register value.

  logic unused = &{rst, irq_in, wbm_0_dat_i, wbm_0_ack_i, wbm_0_stall_i, wbm_0_err_i, wbm_1_dat_i, wbm_1_ack_i, wbm_1_stall_i, wbm_1_err_i, wbs_adr, wbs_dat_w, wbs_sel, wbs_we};

  //Just acknowledge incoming transactions.
  always_ff @(posedge sys_clk) begin
    wbs_ack <= wbs_stb & wbs_cyc;
  end

  assign wbm_0_adr_o = 28'b0;
  assign wbm_0_dat_o = 32'b0;
  assign wbm_0_we_o = 1'b0;
  assign wbm_0_sel_o = 4'b0;

  assign wbm_0_cyc_o = 1'b0;
  assign wbm_0_stb_o = 1'b0;

  assign wbm_1_adr_o = 28'b0;
  assign wbm_1_dat_o = 32'b0;
  assign wbm_1_we_o = 1'b0;
  assign wbm_1_sel_o = 4'b0;

  assign wbm_1_cyc_o = 1'b0;
  assign wbm_1_stb_o = 1'b0;

  assign wbs_stall = 1'b0;
  assign wbs_err = 1'b0;
  assign wbs_dat_r = STUB_SIGNATURE;
  assign irq_out = 1'b0;
`endif

endmodule
