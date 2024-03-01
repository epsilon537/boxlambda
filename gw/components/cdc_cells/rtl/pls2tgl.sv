`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

// Pulse to Toggle - from: http://www.sunburst-design.com/papers/CummingsSNUG2008Boston_CDC.pdf
module pls2tgl (
    output logic tgl,
    input logic pulse,
    input logic clk, rst_n);

    logic q;
    
    initial q = 1'b0;
    
    always_ff @(posedge clk)
        if (!rst_n) q <= '0;
        else q <= pulse ^ q;
        
    assign tgl = q;
endmodule
