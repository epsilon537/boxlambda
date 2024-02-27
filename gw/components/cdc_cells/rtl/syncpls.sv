`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

//syncpls transfers a one-clock-cycle-wide pulse from one clock domain to a one-clock-cyle-wide pulse to another clock domain.
//This works for both slow-to-fast and fast-to-slow clock domain transfers
//The incoming pulse is turned into a toggle. On the receiving side, the toggle is turned into a pulse.
module syncpls (
    input logic t_clk, //transmitting clock.
    input logic t_rst_n, //reset in t_clk domain.
    input logic t_pulse, //input pulse in t_clk domain.
    input logic r_clk, //receiving clock.
    input logic r_rst_n, //reset in r_clk_domain.
    output logic r_pulse); //output pulse in r_clk domain.

    logic t_tgl, r_tgl;

    pls2tgl pls2tgl_inst (
        .tgl(t_tgl),
        .pulse(t_pulse),
        .clk(t_clk), 
        .rst_n(t_rst_n));
    
    sync3 sync3_inst (
        .q(r_tgl),
        .d(t_tgl), 
        .clk(r_clk), 
        .rst_n(r_rst_n));
    
    tgl2pls tgl2pls_inst(
        .pulse(r_pulse), 
        .q(),
        .d(r_tgl),
        .clk(r_clk), 
        .rst_n(r_rst_n));
endmodule
