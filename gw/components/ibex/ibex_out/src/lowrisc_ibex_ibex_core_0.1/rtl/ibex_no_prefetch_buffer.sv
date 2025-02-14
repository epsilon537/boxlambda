// Copyright lowRISC contributors.
// Copyright 2018 ETH Zurich and University of Bologna, see also CREDITS.md.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

/**
 * Prefetcher Buffer for 32 bit memory interface
 *
 * Prefetch Buffer that caches instructions. This cuts overly long critical
 * paths to the instruction cache.
 */
module ibex_no_prefetch_buffer #(
    parameter bit ResetAll = 1'b0
) (
    input logic clk_i,
    input logic rst_ni,

    input logic req_i,

    input logic        branch_i,
    input logic [31:0] addr_i,

    input  logic        ready_i,
    output logic        valid_o,
    output logic [31:0] rdata_o,
    output logic [31:0] addr_o,
    output logic        err_o,
    output logic        err_plus2_o,

    // goes to instruction memory / instruction cache
    output logic        instr_req_o,
    input  logic        instr_gnt_i,
    output logic [31:0] instr_addr_o,
    input  logic [31:0] instr_rdata_i,
    input  logic        instr_err_i,
    input  logic        instr_rvalid_i,

    // Prefetch Buffer Status
    output logic busy_o
);

  logic mid_transaction_branch_req;
  logic transaction_ongoing_reg;
  logic instr_req_reg;
  logic [31:0] instr_adr_reg;

  initial begin
    mid_transaction_branch_req = 1'b0;
    instr_req_reg = 1'b0;
    instr_adr_reg = 0;
    transaction_ongoing_reg = 1'b0;
  end

  always_comb begin
    if (!transaction_ongoing_reg) begin
      //Was there a mid-transaction branch request in the previous transaction?
      if (mid_transaction_branch_req) begin
        instr_req_o  = req_i;
        instr_addr_o = instr_adr_reg;
      end else begin
        /* Put new core transactions on the bus right away...*/
        instr_req_o  = req_i & (ready_i | branch_i);
        instr_addr_o = branch_i ? addr_i : addr_o + 4;
      end
    end else begin
      /* Extend the asserted signals using their registered counterparts in case the bus is stalled. */
      instr_req_o  = instr_req_reg;
      instr_addr_o = instr_adr_reg;
    end
  end

  always_ff @(posedge clk_i) begin
    if (!rst_ni) begin
      mid_transaction_branch_req <= 1'b0;
      instr_req_reg <= 1'b0;
      instr_adr_reg <= 0;
      transaction_ongoing_reg <= 1'b0;
    end else begin
      if (!transaction_ongoing_reg) begin
        if (mid_transaction_branch_req) begin
          mid_transaction_branch_req <= 1'b0;
        end
        if (instr_req_o) begin
          transaction_ongoing_reg <= 1'b1;
          instr_req_reg <= instr_req_o & ~instr_gnt_i;
          instr_adr_reg <= instr_addr_o;
        end
      end else begin  //A transaction is ongoing...
        if (req_i && branch_i) begin
          //A Branch request arrives mid-transaction. Hang on to it until we can
          //issue a new transaction.
          mid_transaction_branch_req <= 1'b1;
          instr_adr_reg <= addr_i;
        end
        if (instr_gnt_i) begin
          instr_req_reg <= 1'b0;  //Clear the register as soon as the slave stops stalling.
        end
        if (instr_rvalid_i || instr_err_i) begin
          transaction_ongoing_reg <= 1'b0;
        end
      end
    end
  end

  /*
  assign instr_req_o  = req_i & (ready_i | branch_i);
  assign instr_addr_o = branch_i ? addr_i : addr_o + 4;

  always_ff @(posedge clk_i)
    if (!rst_ni) addr_o <= 0;
    else if (instr_req_o && instr_gnt_i) addr_o <= instr_addr_o;
  */

  assign addr_o = instr_adr_reg;
  assign rdata_o = instr_rdata_i;
  //Mask valid and error if we have a mid-transaction branch request.
  assign valid_o = instr_rvalid_i & ~(req_i && branch_i) & ~(mid_transaction_branch_req);
  assign err_o = instr_err_i & ~(req_i && branch_i) & ~(mid_transaction_branch_req);
  assign err_plus2_o = 1'b0;

  // Prefetch Buffer Status
  assign busy_o = transaction_ongoing_reg;
endmodule
