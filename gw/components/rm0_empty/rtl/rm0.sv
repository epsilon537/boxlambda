/*This is an empty component placeholder for Reconfigurable module 0. RM0 resides on the crossbar with a Wishbone master and slave port, and irq inputs and output. */
module rm0 (
    input logic sys_clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface.
    output logic [27:0] wbm_adr_o,
    output logic [31:0] wbm_dat_o,
    input logic [31:0] wbm_dat_i,
    output logic wbm_we_o,
    output logic [3:0] wbm_sel_o,
    output logic wbm_stb_o,
    input logic wbm_ack_i,
    input logic wbm_stall_i,
    output logic wbm_cyc_o,
    input logic wbm_err_i,

    //32-bit pipelined Wishbone slave interface.
    input logic [19:0] wbs_adr,
    input logic [31:0] wbs_dat_w,
    output logic [31:0] wbs_dat_r,
    input logic [3:0] wbs_sel,
    output logic wbs_stall,
    input logic wbs_cyc,
    input logic wbs_stb,
    output logic wbs_ack,
    input logic wbs_we,
    output logic wbs_err,

    //Input IRQs from the rest of the system.
    input wire [31:0] irq_in,
    //Output IRQ signal
    output wire irq_out
);
endmodule
