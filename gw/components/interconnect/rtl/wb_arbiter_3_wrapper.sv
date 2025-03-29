//This module is a wb_if aware wrappper around a 3-to-1 arbiter.
module wb_arbiter_3_wrapper #(
    parameter DATA_WIDTH = 32,  // width of data bus in bits (8, 16, 32, or 64)
    parameter ADDR_WIDTH = 32,  // width of address bus in bits
    parameter SELECT_WIDTH = (DATA_WIDTH / 8),  // width of word select bus (1, 2, 4, or 8)
    parameter ARB_TYPE_ROUND_ROBIN = 0,  // select round robin arbitration
    parameter ARB_LSB_HIGH_PRIORITY = 1,  // LSB priority selection
    parameter ARB_BLOCK_ACK = 1,  // block ack generation
    parameter ARB_DEFAULT_TO_PORT_0 = 0  // BoxLambda: Default to port 0 if there is no request.
                                         // This removes arbitration overhead on port 0 when there are no requests on the other ports.
) (
    input wire clk,
    input wire rst,
    wb_if.slave wbm_0,
    wb_if.slave wbm_1,
    wb_if.slave wbm_2,
    wb_if.master wbs
);

  import wb_pkg::*;

  wb_arbiter_3 #(
      .DATA_WIDTH(DATA_WIDTH),
      .ADDR_WIDTH(ADDR_WIDTH),
      .SELECT_WIDTH(SELECT_WIDTH),
      .ARB_TYPE_ROUND_ROBIN(ARB_TYPE_ROUND_ROBIN),
      .ARB_LSB_HIGH_PRIORITY(ARB_LSB_HIGH_PRIORITY),
      .ARB_BLOCK_ACK(ARB_BLOCK_ACK),
      .ARB_DEFAULT_TO_PORT_0(ARB_DEFAULT_TO_PORT_0)
  ) u_wb_arbiter_3 (
      .clk(clk),
      .rst(rst),
      .wbm0_adr_i(wbm_0.adr),
      .wbm0_dat_i(wbm_0.dat_m),
      .wbm0_dat_o(wbm_0.dat_s),
      .wbm0_we_i(wbm_0.we),
      .wbm0_sel_i(wbm_0.sel),
      .wbm0_stb_i(wbm_0.stb),
      .wbm0_ack_o(wbm_0.ack),
      .wbm0_err_o(wbm_0.err),
      .wbm0_stall_o(wbm_0.stall),
      .wbm0_cyc_i(wbm_0.cyc),
      .wbm1_adr_i(wbm_1.adr),
      .wbm1_dat_i(wbm_1.dat_m),
      .wbm1_dat_o(wbm_1.dat_s),
      .wbm1_we_i(wbm_1.we),
      .wbm1_sel_i(wbm_1.sel),
      .wbm1_stb_i(wbm_1.stb),
      .wbm1_ack_o(wbm_1.ack),
      .wbm1_err_o(wbm_1.err),
      .wbm1_stall_o(wbm_1.stall),
      .wbm1_cyc_i(wbm_1.cyc),
      .wbm2_adr_i(wbm_2.adr),
      .wbm2_dat_i(wbm_2.dat_m),
      .wbm2_dat_o(wbm_2.dat_s),
      .wbm2_we_i(wbm_2.we),
      .wbm2_sel_i(wbm_2.sel),
      .wbm2_stb_i(wbm_2.stb),
      .wbm2_ack_o(wbm_2.ack),
      .wbm2_err_o(wbm_2.err),
      .wbm2_stall_o(wbm_2.stall),
      .wbm2_cyc_i(wbm_2.cyc),
      .wbs_adr_o(wbs.adr),
      .wbs_dat_i(wbs.dat_s),
      .wbs_dat_o(wbs.dat_m),
      .wbs_we_o(wbs.we),
      .wbs_sel_o(wbs.sel),
      .wbs_stb_o(wbs.stb),
      .wbs_ack_i(wbs.ack),
      .wbs_err_i(wbs.err),
      .wbs_stall_i(wbs.stall),
      .wbs_cyc_o(wbs.cyc)
  );

endmodule

