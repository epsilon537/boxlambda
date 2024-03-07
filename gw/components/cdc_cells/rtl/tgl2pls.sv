`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

// Pulse Generator - from: http://www.sunburst-design.com/papers/CummingsSNUG2008Boston_CDC.pdf
module tgl2pls (
    output logic pulse, q,
    input logic d,
    input logic clk, rst_n);

    initial q = 1'b0;
    
    always_ff @(posedge clk)
        if (!rst_n) q <= '0;
        else q <= d;
        
    assign pulse = q ^ d;
endmodule
