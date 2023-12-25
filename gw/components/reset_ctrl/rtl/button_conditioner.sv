`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

//based on https://alchitry.com/metastability-and-debouncing-verilog
module button_conditioner (
    input logic clk,
    input logic btn,
    output logic out
  );

`ifdef __ICARUS__
  localparam CTR_SZ=9;
`else
  localparam CTR_SZ=19;
`endif

  logic [CTR_SZ-1:0] ctr_d, ctr_q;
  logic [3:0] sync_d;
  (* ASYNC_REG = "TRUE" *) logic [3:0] sync_q;
  logic sync_q_3;

  assign out = ctr_q == {CTR_SZ{1'b1}};
  assign sync_d = {sync_q[2:0],btn};
  assign sync_q_3 = sync_q[3]; //Workaround for iverilog issue.

  always_comb begin
    ctr_d = ctr_q + 1;
 
    if (ctr_q == {CTR_SZ{1'b1}})
      ctr_d = ctr_q;
 
    if (!sync_q_3)
      ctr_d = 0;
  end
 
  initial begin
    ctr_q = {CTR_SZ{1'b0}};
    sync_q = 4'b0;
  end

  always_ff @(posedge clk) begin
    ctr_q <= ctr_d;
    sync_q <= sync_d;
  end
 
endmodule
