//This module is a wb_if aware wrappper around a 1-to-15 mux.
module wb_shared_bus_15 #(
    parameter DATA_WIDTH = 32,  // width of data bus in bits (8, 16, 32, or 64)
    parameter ADDR_WIDTH = 32,  // width of address bus in bits
    parameter SELECT_WIDTH = (DATA_WIDTH / 8),  // width of word select bus (1, 2, 4, or 8)
    parameter [15*ADDR_WIDTH-1:0] SLAVE_ADDRESSES,
    parameter [15*ADDR_WIDTH-1:0] SLAVE_ADDR_MASKS
) (
    input wire clk,
    input wire rst,
    wb_if.slave wbm,
    wb_if.master wbs[15]
);

  import wb_pkg::*;

  wb_mux_15 #(
      .DATA_WIDTH  (DATA_WIDTH),
      .ADDR_WIDTH  (ADDR_WIDTH),
      .SELECT_WIDTH(SELECT_WIDTH)
  ) wb_mux_15_inst (
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
      .wbs4_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*4:ADDR_WIDTH*4]),

      .wbs5_adr_o(wbs[5].adr),  // ADDR_O() address output
      .wbs5_dat_i(wbs[5].dat_s),  // DAT_I() data in
      .wbs5_dat_o(wbs[5].dat_m),  // DAT_O() data out
      .wbs5_we_o(wbs[5].we),  // WE_O write enable output
      .wbs5_sel_o(wbs[5].sel),  // SEL_O() select output
      .wbs5_stb_o(wbs[5].stb),  // STB_O strobe output
      .wbs5_ack_i(wbs[5].ack),  // ACK_I acknowledge input
      .wbs5_err_i(wbs[5].err),  // ERR_I error input
      .wbs5_stall_i(wbs[5].stall),  // STALL_I retry input
      .wbs5_cyc_o(wbs[5].cyc),  // CYC_O cycle output

      .wbs5_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*5:ADDR_WIDTH*5]),
      .wbs5_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*5:ADDR_WIDTH*5]),

      .wbs6_adr_o(wbs[6].adr),  // ADDR_O() address output
      .wbs6_dat_i(wbs[6].dat_s),  // DAT_I() data in
      .wbs6_dat_o(wbs[6].dat_m),  // DAT_O() data out
      .wbs6_we_o(wbs[6].we),  // WE_O write enable output
      .wbs6_sel_o(wbs[6].sel),  // SEL_O() select output
      .wbs6_stb_o(wbs[6].stb),  // STB_O strobe output
      .wbs6_ack_i(wbs[6].ack),  // ACK_I acknowledge input
      .wbs6_err_i(wbs[6].err),  // ERR_I error input
      .wbs6_stall_i(wbs[6].stall),  // STALL_I retry input
      .wbs6_cyc_o(wbs[6].cyc),  // CYC_O cycle output

      .wbs6_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*6:ADDR_WIDTH*6]),
      .wbs6_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*6:ADDR_WIDTH*6]),

      .wbs7_adr_o(wbs[7].adr),  // ADDR_O() address output
      .wbs7_dat_i(wbs[7].dat_s),  // DAT_I() data in
      .wbs7_dat_o(wbs[7].dat_m),  // DAT_O() data out
      .wbs7_we_o(wbs[7].we),  // WE_O write enable output
      .wbs7_sel_o(wbs[7].sel),  // SEL_O() select output
      .wbs7_stb_o(wbs[7].stb),  // STB_O strobe output
      .wbs7_ack_i(wbs[7].ack),  // ACK_I acknowledge input
      .wbs7_err_i(wbs[7].err),  // ERR_I error input
      .wbs7_stall_i(wbs[7].stall),  // STALL_I retry input
      .wbs7_cyc_o(wbs[7].cyc),  // CYC_O cycle output

      .wbs7_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*7:ADDR_WIDTH*7]),
      .wbs7_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*7:ADDR_WIDTH*7]),

      .wbs8_adr_o(wbs[8].adr),  // ADDR_O() address output
      .wbs8_dat_i(wbs[8].dat_s),  // DAT_I() data in
      .wbs8_dat_o(wbs[8].dat_m),  // DAT_O() data out
      .wbs8_we_o(wbs[8].we),  // WE_O write enable output
      .wbs8_sel_o(wbs[8].sel),  // SEL_O() select output
      .wbs8_stb_o(wbs[8].stb),  // STB_O strobe output
      .wbs8_ack_i(wbs[8].ack),  // ACK_I acknowledge input
      .wbs8_err_i(wbs[8].err),  // ERR_I error input
      .wbs8_stall_i(wbs[8].stall),  // STALL_I retry input
      .wbs8_cyc_o(wbs[8].cyc),  // CYC_O cycle output

      .wbs8_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*8:ADDR_WIDTH*8]),
      .wbs8_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*8:ADDR_WIDTH*8]),

      .wbs9_adr_o(wbs[9].adr),  // ADDR_O() address output
      .wbs9_dat_i(wbs[9].dat_s),  // DAT_I() data in
      .wbs9_dat_o(wbs[9].dat_m),  // DAT_O() data out
      .wbs9_we_o(wbs[9].we),  // WE_O write enable output
      .wbs9_sel_o(wbs[9].sel),  // SEL_O() select output
      .wbs9_stb_o(wbs[9].stb),  // STB_O strobe output
      .wbs9_ack_i(wbs[9].ack),  // ACK_I acknowledge input
      .wbs9_err_i(wbs[9].err),  // ERR_I error input
      .wbs9_stall_i(wbs[9].stall),  // STALL_I retry input
      .wbs9_cyc_o(wbs[9].cyc),  // CYC_O cycle output

      .wbs9_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*9:ADDR_WIDTH*9]),
      .wbs9_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*9:ADDR_WIDTH*9]),

      .wbs10_adr_o(wbs[10].adr),  // ADDR_O() address output
      .wbs10_dat_i(wbs[10].dat_s),  // DAT_I() data in
      .wbs10_dat_o(wbs[10].dat_m),  // DAT_O() data out
      .wbs10_we_o(wbs[10].we),  // WE_O write enable output
      .wbs10_sel_o(wbs[10].sel),  // SEL_O() select output
      .wbs10_stb_o(wbs[10].stb),  // STB_O strobe output
      .wbs10_ack_i(wbs[10].ack),  // ACK_I acknowledge input
      .wbs10_err_i(wbs[10].err),  // ERR_I error input
      .wbs10_stall_i(wbs[10].stall),  // STALL_I retry input
      .wbs10_cyc_o(wbs[10].cyc),  // CYC_O cycle output

      .wbs10_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*10:ADDR_WIDTH*10]),
      .wbs10_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*10:ADDR_WIDTH*10]),

      .wbs11_adr_o(wbs[11].adr),  // ADDR_O() address output
      .wbs11_dat_i(wbs[11].dat_s),  // DAT_I() data in
      .wbs11_dat_o(wbs[11].dat_m),  // DAT_O() data out
      .wbs11_we_o(wbs[11].we),  // WE_O write enable output
      .wbs11_sel_o(wbs[11].sel),  // SEL_O() select output
      .wbs11_stb_o(wbs[11].stb),  // STB_O strobe output
      .wbs11_ack_i(wbs[11].ack),  // ACK_I acknowledge input
      .wbs11_err_i(wbs[11].err),  // ERR_I error input
      .wbs11_stall_i(wbs[11].stall),  // STALL_I retry input
      .wbs11_cyc_o(wbs[11].cyc),  // CYC_O cycle output

      .wbs11_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*11:ADDR_WIDTH*11]),
      .wbs11_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*11:ADDR_WIDTH*11]),

      .wbs12_adr_o(wbs[12].adr),  // ADDR_O() address output
      .wbs12_dat_i(wbs[12].dat_s),  // DAT_I() data in
      .wbs12_dat_o(wbs[12].dat_m),  // DAT_O() data out
      .wbs12_we_o(wbs[12].we),  // WE_O write enable output
      .wbs12_sel_o(wbs[12].sel),  // SEL_O() select output
      .wbs12_stb_o(wbs[12].stb),  // STB_O strobe output
      .wbs12_ack_i(wbs[12].ack),  // ACK_I acknowledge input
      .wbs12_err_i(wbs[12].err),  // ERR_I error input
      .wbs12_stall_i(wbs[12].stall),  // STALL_I retry input
      .wbs12_cyc_o(wbs[12].cyc),  // CYC_O cycle output

      .wbs12_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*12:ADDR_WIDTH*12]),
      .wbs12_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*12:ADDR_WIDTH*12]),

      .wbs13_adr_o(wbs[13].adr),  // ADDR_O() address output
      .wbs13_dat_i(wbs[13].dat_s),  // DAT_I() data in
      .wbs13_dat_o(wbs[13].dat_m),  // DAT_O() data out
      .wbs13_we_o(wbs[13].we),  // WE_O write enable output
      .wbs13_sel_o(wbs[13].sel),  // SEL_O() select output
      .wbs13_stb_o(wbs[13].stb),  // STB_O strobe output
      .wbs13_ack_i(wbs[13].ack),  // ACK_I acknowledge input
      .wbs13_err_i(wbs[13].err),  // ERR_I error input
      .wbs13_stall_i(wbs[13].stall),  // STALL_I retry input
      .wbs13_cyc_o(wbs[13].cyc),  // CYC_O cycle output

      .wbs13_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*13:ADDR_WIDTH*13]),
      .wbs13_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*13:ADDR_WIDTH*13]),

      .wbs14_adr_o(wbs[14].adr),  // ADDR_O() address output
      .wbs14_dat_i(wbs[14].dat_s),  // DAT_I() data in
      .wbs14_dat_o(wbs[14].dat_m),  // DAT_O() data out
      .wbs14_we_o(wbs[14].we),  // WE_O write enable output
      .wbs14_sel_o(wbs[14].sel),  // SEL_O() select output
      .wbs14_stb_o(wbs[14].stb),  // STB_O strobe output
      .wbs14_ack_i(wbs[14].ack),  // ACK_I acknowledge input
      .wbs14_err_i(wbs[14].err),  // ERR_I error input
      .wbs14_stall_i(wbs[14].stall),  // STALL_I retry input
      .wbs14_cyc_o(wbs[14].cyc),  // CYC_O cycle output

      .wbs14_addr(SLAVE_ADDRESSES[ADDR_WIDTH-1+ADDR_WIDTH*14:ADDR_WIDTH*14]),
      .wbs14_addr_msk(SLAVE_ADDR_MASKS[ADDR_WIDTH-1+ADDR_WIDTH*14:ADDR_WIDTH*14])
  );

endmodule

