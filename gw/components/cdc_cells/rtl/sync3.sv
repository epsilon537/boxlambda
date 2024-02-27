`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

// sync signal to different clock domain
module sync3 (
    output logic q,
    input logic d, clk, rst_n);

    (* ASYNC_REG = "TRUE" *) reg [1:0] xfer_pipe; //Synchronization FFs.

    initial begin
        xfer_pipe = 2'b00;
        q = 1'b0;
    end

    always_ff @(posedge clk or negedge rst_n)
        if (!rst_n) {q,xfer_pipe} <= '0;
        else {q, xfer_pipe} <= {xfer_pipe, d};
        
endmodule
