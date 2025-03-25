//This module is a wb_if aware wrappper around a 1-to-5 mux.
module wb_mux_5_wrapper #(
    parameter DATA_WIDTH = 32,  // width of data bus in bits (8, 16, 32, or 64)
    parameter ADDR_WIDTH = 32,  // width of address bus in bits
    parameter SELECT_WIDTH = (DATA_WIDTH / 8),  // width of word select bus (1, 2, 4, or 8)
    parameter [5*ADDR_WIDTH-1:0] SLAVE_ADDRESSES,
    parameter [5*ADDR_WIDTH-1:0] SLAVE_ADDR_MASKS
) (
    input wire clk,
    input wire rst,
    wb_if.slave wbm,
    wb_if.master wbs[5]
);

  import wb_pkg::*;

  wb_mux_5 #(
      .DATA_WIDTH  (DATA_WIDTH),
      .ADDR_WIDTH  (ADDR_WIDTH),
      .SELECT_WIDTH(SELECT_WIDTH)
  ) wb_mux_5_inst (
      .clk(clk),
      .rst(rst),

      .wbm_adr_i(wbm.adr),  // ADDR_I() address input
      .wbm_dat_i(wbm.dat_m),  // DAT_I() data in
      .wbm_dat_o(wbm.dat_s),  // DAT_O() data out
      .wbm_we_i(wbm.we),  // WE_I write enable input
      .wbm_sel_i(wbm.sel),  // SEL_I() select input
      .wbm_stb_i(wbm.stb),  // STB_I strobe input
      .wbm_ack_o(wbm.ack),  // ACK_O acknowledge output
      .wbm_err_o(wbm.err),  // ERR_O error output
      .wbm_stall_o(wbm.stall),  // STALL_O retry output
      .wbm_cyc_i(wbm.cyc),  // CYC_I cycle input

      .wbs0_adr_o(wbs[0].adr),  // ADDR_O() address output
      .wbs0_dat_i(wbs[0].dat_s),  // DAT_I() data in
      .wbs0_dat_o(wbs[0].dat_m),  // DAT_O() data out
      .wbs0_we_o(wbs[0].we),  // WE_O write enable output
      .wbs0_sel_o(wbs[0].sel),  // SEL_O() select output
      .wbs0_stb_o(wbs[0].stb),  // STB_O strobe output
      .wbs0_ack_i(wbs[0].ack),  // ACK_I acknowledge input
      .wbs0_err_i(wbs[0].err),  // ERR_I error input
      .wbs0_stall_i(wbs[0].stall),  // STALL_I retry input
      .wbs0_cyc_o(wbs[0].cyc),  // CYC_O cycle output

      .wbs0_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*0:ADDR_WIDTH*0]),
      .wbs0_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*0:ADDR_WIDTH*0]),

      .wbs1_adr_o(wbs[1].adr),  // ADDR_O() address output
      .wbs1_dat_i(wbs[1].dat_s),  // DAT_I() data in
      .wbs1_dat_o(wbs[1].dat_m),  // DAT_O() data out
      .wbs1_we_o(wbs[1].we),  // WE_O write enable output
      .wbs1_sel_o(wbs[1].sel),  // SEL_O() select output
      .wbs1_stb_o(wbs[1].stb),  // STB_O strobe output
      .wbs1_ack_i(wbs[1].ack),  // ACK_I acknowledge input
      .wbs1_err_i(wbs[1].err),  // ERR_I error input
      .wbs1_stall_i(wbs[1].stall),  // STALL_I retry input
      .wbs1_cyc_o(wbs[1].cyc),  // CYC_O cycle output

      .wbs1_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*1:ADDR_WIDTH*1]),
      .wbs1_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*1:ADDR_WIDTH*1]),

      .wbs2_adr_o(wbs[2].adr),  // ADDR_O() address output
      .wbs2_dat_i(wbs[2].dat_s),  // DAT_I() data in
      .wbs2_dat_o(wbs[2].dat_m),  // DAT_O() data out
      .wbs2_we_o(wbs[2].we),  // WE_O write enable output
      .wbs2_sel_o(wbs[2].sel),  // SEL_O() select output
      .wbs2_stb_o(wbs[2].stb),  // STB_O strobe output
      .wbs2_ack_i(wbs[2].ack),  // ACK_I acknowledge input
      .wbs2_err_i(wbs[2].err),  // ERR_I error input
      .wbs2_stall_i(wbs[2].stall),  // STALL_I retry input
      .wbs2_cyc_o(wbs[2].cyc),  // CYC_O cycle output

      .wbs2_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*2:ADDR_WIDTH*2]),
      .wbs2_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*2:ADDR_WIDTH*2]),

      .wbs3_adr_o(wbs[3].adr),  // ADDR_O() address output
      .wbs3_dat_i(wbs[3].dat_s),  // DAT_I() data in
      .wbs3_dat_o(wbs[3].dat_m),  // DAT_O() data out
      .wbs3_we_o(wbs[3].we),  // WE_O write enable output
      .wbs3_sel_o(wbs[3].sel),  // SEL_O() select output
      .wbs3_stb_o(wbs[3].stb),  // STB_O strobe output
      .wbs3_ack_i(wbs[3].ack),  // ACK_I acknowledge input
      .wbs3_err_i(wbs[3].err),  // ERR_I error input
      .wbs3_stall_i(wbs[3].stall),  // STALL_I retry input
      .wbs3_cyc_o(wbs[3].cyc),  // CYC_O cycle output

      .wbs3_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*3:ADDR_WIDTH*3]),
      .wbs3_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*3:ADDR_WIDTH*3]),

      .wbs4_adr_o(wbs[4].adr),  // ADDR_O() address output
      .wbs4_dat_i(wbs[4].dat_s),  // DAT_I() data in
      .wbs4_dat_o(wbs[4].dat_m),  // DAT_O() data out
      .wbs4_we_o(wbs[4].we),  // WE_O write enable output
      .wbs4_sel_o(wbs[4].sel),  // SEL_O() select output
      .wbs4_stb_o(wbs[4].stb),  // STB_O strobe output
      .wbs4_ack_i(wbs[4].ack),  // ACK_I acknowledge input
      .wbs4_err_i(wbs[4].err),  // ERR_I error input
      .wbs4_stall_i(wbs[4].stall),  // STALL_I retry input
      .wbs4_cyc_o(wbs[4].cyc),  // CYC_O cycle output

      .wbs4_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*4:ADDR_WIDTH*4]),
      .wbs4_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*4:ADDR_WIDTH*4])
  );

endmodule

