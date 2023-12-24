/*This is a placeholder component for a Black Box module with a Wishbone master and slave port.*/
module blackbox(
    wb_if.master           wbm,
    wb_if.slave            wbs);

    logic unused = &{wbs.cyc, wbs.stb, wbs.adr, wbs.dat_m, wbs.sel, wbs.we, wbm.dat_s, wbm.ack, wbm.err, wbm.stall, wbm.rst, wbm.clk, wbs.rst, wbs.clk};

    assign wbm.cyc = 1'b0;
    assign wbm.stb = 1'b0;
    assign wbm.dat_m = 32'b0;
    assign wbm.adr = 28'b0;
    assign wbm.we = 1'b0;
    assign wbm.sel = 4'b0;

    //Just acknowledge incoming transactions.
    always_ff @(posedge wbs.clk) begin
        wbs.ack <= wbs.stb & wbs.cyc;
    end

    assign wbs.stall = 1'b0;
    assign wbs.err = 1'b0;
    assign wbs.dat_s = 32'b0;
endmodule
