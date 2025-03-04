
`ifdef __ICARUS__
`timescale 1 ns / 1 ps
`endif

//This module stalls the wbm for one clock cycle at the beginning of
//a transaction.
//It is used to separate transactions, i.e. to not have multiple back-to-back
//transactions with CYC asserted throughout.
module wb_staller #(
    parameter DATA_WIDTH   = 32,               // width of data bus in bits (8, 16, 32, or 64)
    parameter ADDR_WIDTH   = 32,               // width of address bus in bits
    parameter SELECT_WIDTH = (DATA_WIDTH / 8)  // width of word select bus (1, 2, 4, or 8)
) (
    input wire clk,
    input wire rst,

    /*
     * Wishbone master input
     */
    input  wire [  ADDR_WIDTH-1:0] wbm_adr_i,    // ADR_I() address input
    input  wire [  DATA_WIDTH-1:0] wbm_dat_i,    // DAT_I() data in
    output wire [  DATA_WIDTH-1:0] wbm_dat_o,    // DAT_O() data out
    input  wire                    wbm_we_i,     // WE_I write enable input
    input  wire [SELECT_WIDTH-1:0] wbm_sel_i,    // SEL_I() select input
    input  wire                    wbm_stb_i,    // STB_I strobe input
    output wire                    wbm_ack_o,    // ACK_O acknowledge output
    output wire                    wbm_err_o,    // ERR_O error output
    output wire                    wbm_stall_o,  // STALL_O retry output
    input  wire                    wbm_cyc_i,    // CYC_I cycle input

    /*
     * Wishbone slave output
     */
    output wire [  ADDR_WIDTH-1:0] wbs_adr_o,    // ADR_O() address output
    input  wire [  DATA_WIDTH-1:0] wbs_dat_i,    // DAT_I() data in
    output wire [  DATA_WIDTH-1:0] wbs_dat_o,    // DAT_O() data out
    output wire                    wbs_we_o,     // WE_O write enable output
    output wire [SELECT_WIDTH-1:0] wbs_sel_o,    // SEL_O() select output
    output wire                    wbs_stb_o,    // STB_O strobe output
    input  wire                    wbs_ack_i,    // ACK_I acknowledge input
    input  wire                    wbs_err_i,    // ERR_I error input
    input  wire                    wbs_stall_i,  // STALL_I retry input
    output wire                    wbs_cyc_o     // CYC_O cycle output
);

  logic transaction_ongoing_reg;

  initial begin
    transaction_ongoing_reg = 1'b0;
  end

  //At the beginning of a new transaction (i.e. no transaction ongoing yet),
  //stall the bus master.
  //If a transaction is ongoing, forward the slave's stall signal.
  assign wbm_stall_o = transaction_ongoing_reg ? wbs_stall_i : wbm_cyc_i;
  assign wbm_err_o = wbs_err_i;
  assign wbm_ack_o = wbs_ack_i;
  assign wbm_dat_o = wbs_dat_i;

  assign wbs_adr_o = wbm_adr_i;
  assign wbs_dat_o = wbm_dat_i;
  assign wbs_we_o = wbm_we_i;
  assign wbs_sel_o = wbm_sel_i;
  assign wbs_stb_o = transaction_ongoing_reg ? wbm_stb_i : 1'b0;
  assign wbs_cyc_o = transaction_ongoing_reg ? wbm_cyc_i : 1'b0;

  always_ff @(posedge clk) begin
    if (rst) begin
      transaction_ongoing_reg <= 1'b0;
    end else begin
      if (transaction_ongoing_reg) begin
        if (wbs_ack_i || wbs_err_i) begin
          transaction_ongoing_reg <= 1'b0;
        end
      end else if (wbm_cyc_i) begin
        transaction_ongoing_reg <= 1'b1;
      end
    end
  end

endmodule


