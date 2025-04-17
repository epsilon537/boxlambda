
`ifdef __ICARUS__
`timescale 1 ns / 1 ps
`endif

//This module monitors the ongoing transaction. It returns DUMMY_VALUE if
//the transaction times out, i.e. if the slave didn't respond within TIMEOUT
//clock cycles.
module wb_timeout #(
    parameter logic [31:0] DUMMY_VALUE = 'hDEADBEEF,  // dummy value returned to rdata on timeout
    parameter logic [9:0] TIMEOUT = 10'd512 // declare timeout if slave doesn't respond after TIMEOUT clock cycles.
) (
    input wire clk,
    input wire rst,
    wb_if.slave wbm,
    wb_if.master wbs
);

  import wb_pkg::*;

  logic transaction_timeout;
  logic transaction_ongoing_reg;
  logic [9:0] transaction_duration_reg;

  initial begin
    transaction_ongoing_reg  = 1'b0;
    transaction_duration_reg = 10'b0;
  end

  assign transaction_timeout = (transaction_duration_reg == TIMEOUT);

  assign wbm.stall = wbs.stall;
  assign wbm.err = wbs.err;
  assign wbm.ack = transaction_timeout ? 1'b1 : wbs.ack;
  assign wbm.dat_s = transaction_timeout ? DUMMY_VALUE : wbs.dat_s;

  assign wbs.adr = wbm.adr;
  assign wbs.dat_m = wbm.dat_m;
  assign wbs.we = wbm.we;
  assign wbs.sel = wbm.sel;
  assign wbs.stb = wbm.stb;
  assign wbs.cyc = wbm.cyc;

  always_ff @(posedge clk) begin
    if (rst) begin
      transaction_ongoing_reg  <= 1'b0;
      transaction_duration_reg <= 10'b0;
    end else begin
      if (transaction_ongoing_reg) begin
        if (wbs.ack || wbs.err || transaction_timeout) begin
          transaction_ongoing_reg <= 1'b0;
        end

        transaction_duration_reg <= transaction_duration_reg + 10'd1;

      end else if (wbm.cyc) begin
        transaction_ongoing_reg  <= 1'b1;
        transaction_duration_reg <= 10'b0;
      end
    end
  end

endmodule


