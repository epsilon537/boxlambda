/* This is an empty component placeholder for Virtual Socket 0.
 * VS0 resides on the crossbar with a Wishbone master and slave port,
 * and irq inputs and output.
 *
 * Synthesis of the static portion of a DFX (Dynamic Function Exchange,
 * a.k.a. Partial FPGA Reconfiguration) build expects an empty module
 * for each virtual socket (reconfigurable partition) in the design, to nail
 * down the interface of that virtual socke.
 * In the BoxLambda build system, the DFX enabled build project (e.g. dfx_test)
 * pulls in the empty module component as a Bender dependency.
 *
 * Note that the empty component is only used in the synthesis step of
 * a DFX enabled project. All other configurations using the boxlambda_soc
 * component (verilator, lint checker, non-DFX builds) reference a non-empty
 * virtual socket variant component such as vs0_stub or vs0_j1b.
 */
module vs0 (
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
    input logic [17:0] wbs_adr,
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
