//ICAPE2 stub for verilator linter.
module ICAPE2 (
  output wire [31:0] O,
  input wire CLK,
  input wire CSIB,
  input wire [31:0] I,
  input wire RDWRB
);

  wire unused = &{CLK, CSIB, I, RDWRB};

  assign O = 0;

endmodule

