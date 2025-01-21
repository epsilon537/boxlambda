`default_nettype none
module litedram (
	clk,
	ddram_a,
	ddram_ba,
	ddram_cas_n,
	ddram_cke,
	ddram_clk_n,
	ddram_clk_p,
	ddram_cs_n,
	ddram_dm,
	ddram_dq,
	ddram_dqs_n,
	ddram_dqs_p,
	ddram_odt,
	ddram_ras_n,
	ddram_reset_n,
	ddram_we_n,
	init_done,
	init_error,
	pll_locked,
	rst,
	user_clk,
	user_clkx2,
	user_port_wishbone_0_ack,
	user_port_wishbone_0_adr,
	user_port_wishbone_0_cyc,
	user_port_wishbone_0_dat_r,
	user_port_wishbone_0_dat_w,
	user_port_wishbone_0_err,
	user_port_wishbone_0_sel,
	user_port_wishbone_0_stb,
	user_port_wishbone_0_we,
	user_rst,
	wb_ctrl_ack,
	wb_ctrl_adr,
	wb_ctrl_bte,
	wb_ctrl_cti,
	wb_ctrl_cyc,
	wb_ctrl_dat_r,
	wb_ctrl_dat_w,
	wb_ctrl_err,
	wb_ctrl_sel,
	wb_ctrl_stb,
	wb_ctrl_we
);
	input wire clk;
	output wire [13:0] ddram_a;
	output wire [2:0] ddram_ba;
	output wire ddram_cas_n;
	output wire ddram_cke;
	output wire ddram_clk_n;
	output wire ddram_clk_p;
	output wire ddram_cs_n;
	output wire [1:0] ddram_dm;
	inout wire [15:0] ddram_dq;
	inout wire [1:0] ddram_dqs_n;
	inout wire [1:0] ddram_dqs_p;
	output wire ddram_odt;
	output wire ddram_ras_n;
	output wire ddram_reset_n;
	output wire ddram_we_n;
	output wire init_done;
	output wire init_error;
	output wire pll_locked;
	input wire rst;
	output wire user_clk;
	output wire user_clkx2;
	output wire user_port_wishbone_0_ack;
	input wire [25:0] user_port_wishbone_0_adr;
	input wire user_port_wishbone_0_cyc;
	output wire [31:0] user_port_wishbone_0_dat_r;
	input wire [31:0] user_port_wishbone_0_dat_w;
	output wire user_port_wishbone_0_err;
	input wire [3:0] user_port_wishbone_0_sel;
	input wire user_port_wishbone_0_stb;
	input wire user_port_wishbone_0_we;
	output wire user_rst;
	output wire wb_ctrl_ack;
	input wire [29:0] wb_ctrl_adr;
	input wire [1:0] wb_ctrl_bte;
	input wire [2:0] wb_ctrl_cti;
	input wire wb_ctrl_cyc;
	output wire [31:0] wb_ctrl_dat_r;
	input wire [31:0] wb_ctrl_dat_w;
	output wire wb_ctrl_err;
	input wire [3:0] wb_ctrl_sel;
	input wire wb_ctrl_stb;
	input wire wb_ctrl_we;
	wire [13:0] builder_adr;
	reg [2:0] builder_array_muxed0 = 3'd0;
	reg [13:0] builder_array_muxed1 = 14'd0;
	reg builder_array_muxed10 = 1'd0;
	reg builder_array_muxed11 = 1'd0;
	reg builder_array_muxed12 = 1'd0;
	reg builder_array_muxed13 = 1'd0;
	reg [2:0] builder_array_muxed14 = 3'd0;
	reg [13:0] builder_array_muxed15 = 14'd0;
	reg builder_array_muxed16 = 1'd0;
	reg builder_array_muxed17 = 1'd0;
	reg builder_array_muxed18 = 1'd0;
	reg builder_array_muxed19 = 1'd0;
	reg builder_array_muxed2 = 1'd0;
	reg builder_array_muxed20 = 1'd0;
	reg [2:0] builder_array_muxed21 = 3'd0;
	reg [13:0] builder_array_muxed22 = 14'd0;
	reg builder_array_muxed23 = 1'd0;
	reg builder_array_muxed24 = 1'd0;
	reg builder_array_muxed25 = 1'd0;
	reg builder_array_muxed26 = 1'd0;
	reg builder_array_muxed27 = 1'd0;
	reg builder_array_muxed3 = 1'd0;
	reg builder_array_muxed4 = 1'd0;
	reg builder_array_muxed5 = 1'd0;
	reg builder_array_muxed6 = 1'd0;
	reg [2:0] builder_array_muxed7 = 3'd0;
	reg [13:0] builder_array_muxed8 = 14'd0;
	reg builder_array_muxed9 = 1'd0;
	wire builder_csrbank0_init_done0_r;
	reg builder_csrbank0_init_done0_re = 1'd0;
	wire builder_csrbank0_init_done0_w;
	reg builder_csrbank0_init_done0_we = 1'd0;
	wire builder_csrbank0_init_error0_r;
	reg builder_csrbank0_init_error0_re = 1'd0;
	wire builder_csrbank0_init_error0_w;
	reg builder_csrbank0_init_error0_we = 1'd0;
	wire builder_csrbank0_sel;
	wire [1:0] builder_csrbank1_dly_sel0_r;
	reg builder_csrbank1_dly_sel0_re = 1'd0;
	wire [1:0] builder_csrbank1_dly_sel0_w;
	reg builder_csrbank1_dly_sel0_we = 1'd0;
	wire [4:0] builder_csrbank1_half_sys8x_taps0_r;
	reg builder_csrbank1_half_sys8x_taps0_re = 1'd0;
	wire [4:0] builder_csrbank1_half_sys8x_taps0_w;
	reg builder_csrbank1_half_sys8x_taps0_we = 1'd0;
	wire [1:0] builder_csrbank1_rdphase0_r;
	reg builder_csrbank1_rdphase0_re = 1'd0;
	wire [1:0] builder_csrbank1_rdphase0_w;
	reg builder_csrbank1_rdphase0_we = 1'd0;
	wire builder_csrbank1_rst0_r;
	reg builder_csrbank1_rst0_re = 1'd0;
	wire builder_csrbank1_rst0_w;
	reg builder_csrbank1_rst0_we = 1'd0;
	wire builder_csrbank1_sel;
	wire builder_csrbank1_wlevel_en0_r;
	reg builder_csrbank1_wlevel_en0_re = 1'd0;
	wire builder_csrbank1_wlevel_en0_w;
	reg builder_csrbank1_wlevel_en0_we = 1'd0;
	wire [1:0] builder_csrbank1_wrphase0_r;
	reg builder_csrbank1_wrphase0_re = 1'd0;
	wire [1:0] builder_csrbank1_wrphase0_w;
	reg builder_csrbank1_wrphase0_we = 1'd0;
	wire [3:0] builder_csrbank2_dfii_control0_r;
	reg builder_csrbank2_dfii_control0_re = 1'd0;
	wire [3:0] builder_csrbank2_dfii_control0_w;
	reg builder_csrbank2_dfii_control0_we = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi0_address0_r;
	reg builder_csrbank2_dfii_pi0_address0_re = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi0_address0_w;
	reg builder_csrbank2_dfii_pi0_address0_we = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi0_baddress0_r;
	reg builder_csrbank2_dfii_pi0_baddress0_re = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi0_baddress0_w;
	reg builder_csrbank2_dfii_pi0_baddress0_we = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi0_command0_r;
	reg builder_csrbank2_dfii_pi0_command0_re = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi0_command0_w;
	reg builder_csrbank2_dfii_pi0_command0_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi0_rddata_r;
	reg builder_csrbank2_dfii_pi0_rddata_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi0_rddata_w;
	reg builder_csrbank2_dfii_pi0_rddata_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi0_wrdata0_r;
	reg builder_csrbank2_dfii_pi0_wrdata0_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi0_wrdata0_w;
	reg builder_csrbank2_dfii_pi0_wrdata0_we = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi1_address0_r;
	reg builder_csrbank2_dfii_pi1_address0_re = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi1_address0_w;
	reg builder_csrbank2_dfii_pi1_address0_we = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi1_baddress0_r;
	reg builder_csrbank2_dfii_pi1_baddress0_re = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi1_baddress0_w;
	reg builder_csrbank2_dfii_pi1_baddress0_we = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi1_command0_r;
	reg builder_csrbank2_dfii_pi1_command0_re = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi1_command0_w;
	reg builder_csrbank2_dfii_pi1_command0_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi1_rddata_r;
	reg builder_csrbank2_dfii_pi1_rddata_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi1_rddata_w;
	reg builder_csrbank2_dfii_pi1_rddata_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi1_wrdata0_r;
	reg builder_csrbank2_dfii_pi1_wrdata0_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi1_wrdata0_w;
	reg builder_csrbank2_dfii_pi1_wrdata0_we = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi2_address0_r;
	reg builder_csrbank2_dfii_pi2_address0_re = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi2_address0_w;
	reg builder_csrbank2_dfii_pi2_address0_we = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi2_baddress0_r;
	reg builder_csrbank2_dfii_pi2_baddress0_re = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi2_baddress0_w;
	reg builder_csrbank2_dfii_pi2_baddress0_we = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi2_command0_r;
	reg builder_csrbank2_dfii_pi2_command0_re = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi2_command0_w;
	reg builder_csrbank2_dfii_pi2_command0_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi2_rddata_r;
	reg builder_csrbank2_dfii_pi2_rddata_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi2_rddata_w;
	reg builder_csrbank2_dfii_pi2_rddata_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi2_wrdata0_r;
	reg builder_csrbank2_dfii_pi2_wrdata0_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi2_wrdata0_w;
	reg builder_csrbank2_dfii_pi2_wrdata0_we = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi3_address0_r;
	reg builder_csrbank2_dfii_pi3_address0_re = 1'd0;
	wire [13:0] builder_csrbank2_dfii_pi3_address0_w;
	reg builder_csrbank2_dfii_pi3_address0_we = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi3_baddress0_r;
	reg builder_csrbank2_dfii_pi3_baddress0_re = 1'd0;
	wire [2:0] builder_csrbank2_dfii_pi3_baddress0_w;
	reg builder_csrbank2_dfii_pi3_baddress0_we = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi3_command0_r;
	reg builder_csrbank2_dfii_pi3_command0_re = 1'd0;
	wire [7:0] builder_csrbank2_dfii_pi3_command0_w;
	reg builder_csrbank2_dfii_pi3_command0_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi3_rddata_r;
	reg builder_csrbank2_dfii_pi3_rddata_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi3_rddata_w;
	reg builder_csrbank2_dfii_pi3_rddata_we = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi3_wrdata0_r;
	reg builder_csrbank2_dfii_pi3_wrdata0_re = 1'd0;
	wire [31:0] builder_csrbank2_dfii_pi3_wrdata0_w;
	reg builder_csrbank2_dfii_pi3_wrdata0_we = 1'd0;
	wire builder_csrbank2_sel;
	wire [31:0] builder_dat_r;
	wire [31:0] builder_dat_w;
	reg builder_interface0_ack = 1'd0;
	wire [29:0] builder_interface0_adr;
	wire [13:0] builder_interface0_bank_bus_adr;
	reg [31:0] builder_interface0_bank_bus_dat_r = 32'd0;
	wire [31:0] builder_interface0_bank_bus_dat_w;
	wire builder_interface0_bank_bus_we;
	wire [1:0] builder_interface0_bte;
	wire [2:0] builder_interface0_cti;
	wire builder_interface0_cyc;
	reg [31:0] builder_interface0_dat_r = 32'd0;
	wire [31:0] builder_interface0_dat_w;
	reg builder_interface0_err = 1'd0;
	wire [3:0] builder_interface0_sel;
	wire builder_interface0_stb;
	wire builder_interface0_we;
	reg [13:0] builder_interface1_adr = 14'd0;
	reg [13:0] builder_interface1_adr_wishbone2csr_next_value1 = 14'd0;
	reg builder_interface1_adr_wishbone2csr_next_value_ce1 = 1'd0;
	wire [13:0] builder_interface1_bank_bus_adr;
	reg [31:0] builder_interface1_bank_bus_dat_r = 32'd0;
	wire [31:0] builder_interface1_bank_bus_dat_w;
	wire builder_interface1_bank_bus_we;
	wire [31:0] builder_interface1_dat_r;
	reg [31:0] builder_interface1_dat_w = 32'd0;
	reg [31:0] builder_interface1_dat_w_wishbone2csr_next_value0 = 32'd0;
	reg builder_interface1_dat_w_wishbone2csr_next_value_ce0 = 1'd0;
	reg builder_interface1_we = 1'd0;
	reg builder_interface1_we_wishbone2csr_next_value2 = 1'd0;
	reg builder_interface1_we_wishbone2csr_next_value_ce2 = 1'd0;
	wire [13:0] builder_interface2_bank_bus_adr;
	reg [31:0] builder_interface2_bank_bus_dat_r = 32'd0;
	wire [31:0] builder_interface2_bank_bus_dat_w;
	wire builder_interface2_bank_bus_we;
	reg [2:0] builder_litedramcore_bankmachine0_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine0_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine1_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine1_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine2_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine2_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine3_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine3_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine4_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine4_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine5_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine5_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine6_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine6_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine7_next_state = 3'd0;
	reg [2:0] builder_litedramcore_bankmachine7_state = 3'd0;
	reg builder_litedramcore_locked0 = 1'd0;
	reg builder_litedramcore_locked1 = 1'd0;
	reg builder_litedramcore_locked2 = 1'd0;
	reg builder_litedramcore_locked3 = 1'd0;
	reg builder_litedramcore_locked4 = 1'd0;
	reg builder_litedramcore_locked5 = 1'd0;
	reg builder_litedramcore_locked6 = 1'd0;
	reg builder_litedramcore_locked7 = 1'd0;
	reg [3:0] builder_litedramcore_multiplexer_next_state = 4'd0;
	reg [3:0] builder_litedramcore_multiplexer_state = 4'd0;
	reg builder_litedramcore_new_master_rdata_valid0 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid1 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid2 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid3 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid4 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid5 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid6 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid7 = 1'd0;
	reg builder_litedramcore_new_master_rdata_valid8 = 1'd0;
	reg builder_litedramcore_new_master_wdata_ready0 = 1'd0;
	reg builder_litedramcore_new_master_wdata_ready1 = 1'd0;
	reg [1:0] builder_litedramcore_next_state = 2'd0;
	reg [1:0] builder_litedramcore_refresher_next_state = 2'd0;
	reg [1:0] builder_litedramcore_refresher_state = 2'd0;
	wire builder_litedramcore_roundrobin0_ce;
	wire builder_litedramcore_roundrobin0_grant;
	wire builder_litedramcore_roundrobin0_request;
	wire builder_litedramcore_roundrobin1_ce;
	wire builder_litedramcore_roundrobin1_grant;
	wire builder_litedramcore_roundrobin1_request;
	wire builder_litedramcore_roundrobin2_ce;
	wire builder_litedramcore_roundrobin2_grant;
	wire builder_litedramcore_roundrobin2_request;
	wire builder_litedramcore_roundrobin3_ce;
	wire builder_litedramcore_roundrobin3_grant;
	wire builder_litedramcore_roundrobin3_request;
	wire builder_litedramcore_roundrobin4_ce;
	wire builder_litedramcore_roundrobin4_grant;
	wire builder_litedramcore_roundrobin4_request;
	wire builder_litedramcore_roundrobin5_ce;
	wire builder_litedramcore_roundrobin5_grant;
	wire builder_litedramcore_roundrobin5_request;
	wire builder_litedramcore_roundrobin6_ce;
	wire builder_litedramcore_roundrobin6_grant;
	wire builder_litedramcore_roundrobin6_request;
	wire builder_litedramcore_roundrobin7_ce;
	wire builder_litedramcore_roundrobin7_grant;
	wire builder_litedramcore_roundrobin7_request;
	reg [1:0] builder_litedramcore_state = 2'd0;
	reg [1:0] builder_litedramwishbone2native_next_state = 2'd0;
	reg [1:0] builder_litedramwishbone2native_state = 2'd0;
	wire builder_pll_fb;
	wire builder_reset0;
	wire builder_reset1;
	wire builder_reset2;
	wire builder_reset3;
	wire builder_reset4;
	wire builder_reset5;
	wire builder_reset6;
	wire builder_reset7;
	reg builder_rhs_array_muxed0 = 1'd0;
	reg [13:0] builder_rhs_array_muxed1 = 14'd0;
	reg builder_rhs_array_muxed10 = 1'd0;
	reg builder_rhs_array_muxed11 = 1'd0;
	reg [20:0] builder_rhs_array_muxed12 = 21'd0;
	reg builder_rhs_array_muxed13 = 1'd0;
	reg builder_rhs_array_muxed14 = 1'd0;
	reg [20:0] builder_rhs_array_muxed15 = 21'd0;
	reg builder_rhs_array_muxed16 = 1'd0;
	reg builder_rhs_array_muxed17 = 1'd0;
	reg [20:0] builder_rhs_array_muxed18 = 21'd0;
	reg builder_rhs_array_muxed19 = 1'd0;
	reg [2:0] builder_rhs_array_muxed2 = 3'd0;
	reg builder_rhs_array_muxed20 = 1'd0;
	reg [20:0] builder_rhs_array_muxed21 = 21'd0;
	reg builder_rhs_array_muxed22 = 1'd0;
	reg builder_rhs_array_muxed23 = 1'd0;
	reg [20:0] builder_rhs_array_muxed24 = 21'd0;
	reg builder_rhs_array_muxed25 = 1'd0;
	reg builder_rhs_array_muxed26 = 1'd0;
	reg [20:0] builder_rhs_array_muxed27 = 21'd0;
	reg builder_rhs_array_muxed28 = 1'd0;
	reg builder_rhs_array_muxed29 = 1'd0;
	reg builder_rhs_array_muxed3 = 1'd0;
	reg [20:0] builder_rhs_array_muxed30 = 21'd0;
	reg builder_rhs_array_muxed31 = 1'd0;
	reg builder_rhs_array_muxed32 = 1'd0;
	reg [20:0] builder_rhs_array_muxed33 = 21'd0;
	reg builder_rhs_array_muxed34 = 1'd0;
	reg builder_rhs_array_muxed35 = 1'd0;
	reg builder_rhs_array_muxed4 = 1'd0;
	reg builder_rhs_array_muxed5 = 1'd0;
	reg builder_rhs_array_muxed6 = 1'd0;
	reg [13:0] builder_rhs_array_muxed7 = 14'd0;
	reg [2:0] builder_rhs_array_muxed8 = 3'd0;
	reg builder_rhs_array_muxed9 = 1'd0;
	reg builder_t_array_muxed0 = 1'd0;
	reg builder_t_array_muxed1 = 1'd0;
	reg builder_t_array_muxed2 = 1'd0;
	reg builder_t_array_muxed3 = 1'd0;
	reg builder_t_array_muxed4 = 1'd0;
	reg builder_t_array_muxed5 = 1'd0;
	wire builder_we;
	reg [1:0] builder_wishbone2csr_next_state = 2'd0;
	reg [1:0] builder_wishbone2csr_state = 2'd0;
	wire builder_xilinxasyncresetsynchronizerimpl0;
	wire builder_xilinxasyncresetsynchronizerimpl0_rst_meta;
	wire builder_xilinxasyncresetsynchronizerimpl1;
	wire builder_xilinxasyncresetsynchronizerimpl1_rst_meta;
	wire builder_xilinxasyncresetsynchronizerimpl2;
	wire builder_xilinxasyncresetsynchronizerimpl2_expr;
	wire builder_xilinxasyncresetsynchronizerimpl2_rst_meta;
	wire builder_xilinxasyncresetsynchronizerimpl3;
	wire builder_xilinxasyncresetsynchronizerimpl3_expr;
	wire builder_xilinxasyncresetsynchronizerimpl3_rst_meta;
	wire builder_xilinxasyncresetsynchronizerimpl4;
	wire builder_xilinxasyncresetsynchronizerimpl4_expr;
	wire builder_xilinxasyncresetsynchronizerimpl4_rst_meta;
	wire iodelay_clk;
	wire iodelay_rst;
	wire main_a7ddrphy0;
	wire main_a7ddrphy1;
	reg [7:0] main_a7ddrphy_bitslip00 = 8'd0;
	reg [7:0] main_a7ddrphy_bitslip01 = 8'd0;
	reg [7:0] main_a7ddrphy_bitslip02 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip03;
	reg [7:0] main_a7ddrphy_bitslip04 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip0_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip0_r1 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip0_r2 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip0_r3 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip0_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip0_value1 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip0_value2 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip0_value3 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip10 = 8'd0;
	reg [7:0] main_a7ddrphy_bitslip100 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip101;
	reg [7:0] main_a7ddrphy_bitslip102 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip10_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip10_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip10_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip10_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip11 = 8'd0;
	reg [7:0] main_a7ddrphy_bitslip110 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip111;
	reg [7:0] main_a7ddrphy_bitslip112 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip11_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip11_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip11_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip11_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip12 = 8'd0;
	reg [7:0] main_a7ddrphy_bitslip120 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip121;
	reg [7:0] main_a7ddrphy_bitslip122 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip12_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip12_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip12_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip12_value1 = 3'd7;
	wire [7:0] main_a7ddrphy_bitslip13;
	reg [7:0] main_a7ddrphy_bitslip130 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip131;
	reg [7:0] main_a7ddrphy_bitslip132 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip13_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip13_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip13_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip13_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip14 = 8'd0;
	reg [7:0] main_a7ddrphy_bitslip140 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip141;
	reg [7:0] main_a7ddrphy_bitslip142 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip14_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip14_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip14_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip14_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip150 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip151;
	reg [7:0] main_a7ddrphy_bitslip152 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip15_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip15_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip15_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip15_value1 = 3'd7;
	reg [15:0] main_a7ddrphy_bitslip1_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip1_r1 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip1_r2 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip1_r3 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip1_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip1_value1 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip1_value2 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip1_value3 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip20 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip21;
	reg [7:0] main_a7ddrphy_bitslip22 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip2_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip2_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip2_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip2_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip30 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip31;
	reg [7:0] main_a7ddrphy_bitslip32 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip3_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip3_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip3_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip3_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip40 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip41;
	reg [7:0] main_a7ddrphy_bitslip42 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip4_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip4_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip4_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip4_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip50 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip51;
	reg [7:0] main_a7ddrphy_bitslip52 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip5_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip5_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip5_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip5_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip60 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip61;
	reg [7:0] main_a7ddrphy_bitslip62 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip6_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip6_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip6_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip6_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip70 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip71;
	reg [7:0] main_a7ddrphy_bitslip72 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip7_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip7_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip7_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip7_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip80 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip81;
	reg [7:0] main_a7ddrphy_bitslip82 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip8_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip8_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip8_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip8_value1 = 3'd7;
	reg [7:0] main_a7ddrphy_bitslip90 = 8'd0;
	wire [7:0] main_a7ddrphy_bitslip91;
	reg [7:0] main_a7ddrphy_bitslip92 = 8'd0;
	reg [15:0] main_a7ddrphy_bitslip9_r0 = 16'd0;
	reg [15:0] main_a7ddrphy_bitslip9_r1 = 16'd0;
	reg [2:0] main_a7ddrphy_bitslip9_value0 = 3'd7;
	reg [2:0] main_a7ddrphy_bitslip9_value1 = 3'd7;
	wire main_a7ddrphy_dfi_p0_act_n;
	wire [13:0] main_a7ddrphy_dfi_p0_address;
	wire [2:0] main_a7ddrphy_dfi_p0_bank;
	wire main_a7ddrphy_dfi_p0_cas_n;
	wire main_a7ddrphy_dfi_p0_cke;
	wire main_a7ddrphy_dfi_p0_cs_n;
	wire main_a7ddrphy_dfi_p0_odt;
	wire main_a7ddrphy_dfi_p0_ras_n;
	reg [31:0] main_a7ddrphy_dfi_p0_rddata = 32'd0;
	wire main_a7ddrphy_dfi_p0_rddata_en;
	wire main_a7ddrphy_dfi_p0_rddata_valid;
	wire main_a7ddrphy_dfi_p0_reset_n;
	wire main_a7ddrphy_dfi_p0_we_n;
	wire [31:0] main_a7ddrphy_dfi_p0_wrdata;
	wire main_a7ddrphy_dfi_p0_wrdata_en;
	wire [3:0] main_a7ddrphy_dfi_p0_wrdata_mask;
	wire main_a7ddrphy_dfi_p1_act_n;
	wire [13:0] main_a7ddrphy_dfi_p1_address;
	wire [2:0] main_a7ddrphy_dfi_p1_bank;
	wire main_a7ddrphy_dfi_p1_cas_n;
	wire main_a7ddrphy_dfi_p1_cke;
	wire main_a7ddrphy_dfi_p1_cs_n;
	wire main_a7ddrphy_dfi_p1_odt;
	wire main_a7ddrphy_dfi_p1_ras_n;
	reg [31:0] main_a7ddrphy_dfi_p1_rddata = 32'd0;
	wire main_a7ddrphy_dfi_p1_rddata_en;
	wire main_a7ddrphy_dfi_p1_rddata_valid;
	wire main_a7ddrphy_dfi_p1_reset_n;
	wire main_a7ddrphy_dfi_p1_we_n;
	wire [31:0] main_a7ddrphy_dfi_p1_wrdata;
	wire main_a7ddrphy_dfi_p1_wrdata_en;
	wire [3:0] main_a7ddrphy_dfi_p1_wrdata_mask;
	wire main_a7ddrphy_dfi_p2_act_n;
	wire [13:0] main_a7ddrphy_dfi_p2_address;
	wire [2:0] main_a7ddrphy_dfi_p2_bank;
	wire main_a7ddrphy_dfi_p2_cas_n;
	wire main_a7ddrphy_dfi_p2_cke;
	wire main_a7ddrphy_dfi_p2_cs_n;
	wire main_a7ddrphy_dfi_p2_odt;
	wire main_a7ddrphy_dfi_p2_ras_n;
	reg [31:0] main_a7ddrphy_dfi_p2_rddata = 32'd0;
	wire main_a7ddrphy_dfi_p2_rddata_en;
	wire main_a7ddrphy_dfi_p2_rddata_valid;
	wire main_a7ddrphy_dfi_p2_reset_n;
	wire main_a7ddrphy_dfi_p2_we_n;
	wire [31:0] main_a7ddrphy_dfi_p2_wrdata;
	wire main_a7ddrphy_dfi_p2_wrdata_en;
	wire [3:0] main_a7ddrphy_dfi_p2_wrdata_mask;
	wire main_a7ddrphy_dfi_p3_act_n;
	wire [13:0] main_a7ddrphy_dfi_p3_address;
	wire [2:0] main_a7ddrphy_dfi_p3_bank;
	wire main_a7ddrphy_dfi_p3_cas_n;
	wire main_a7ddrphy_dfi_p3_cke;
	wire main_a7ddrphy_dfi_p3_cs_n;
	wire main_a7ddrphy_dfi_p3_odt;
	wire main_a7ddrphy_dfi_p3_ras_n;
	reg [31:0] main_a7ddrphy_dfi_p3_rddata = 32'd0;
	wire main_a7ddrphy_dfi_p3_rddata_en;
	wire main_a7ddrphy_dfi_p3_rddata_valid;
	wire main_a7ddrphy_dfi_p3_reset_n;
	wire main_a7ddrphy_dfi_p3_we_n;
	wire [31:0] main_a7ddrphy_dfi_p3_wrdata;
	wire main_a7ddrphy_dfi_p3_wrdata_en;
	wire [3:0] main_a7ddrphy_dfi_p3_wrdata_mask;
	reg main_a7ddrphy_dly_sel_re = 1'd0;
	reg [1:0] main_a7ddrphy_dly_sel_storage = 2'd0;
	wire main_a7ddrphy_dq_i_delayed0;
	wire main_a7ddrphy_dq_i_delayed1;
	wire main_a7ddrphy_dq_i_delayed10;
	wire main_a7ddrphy_dq_i_delayed11;
	wire main_a7ddrphy_dq_i_delayed12;
	wire main_a7ddrphy_dq_i_delayed13;
	wire main_a7ddrphy_dq_i_delayed14;
	wire main_a7ddrphy_dq_i_delayed15;
	wire main_a7ddrphy_dq_i_delayed2;
	wire main_a7ddrphy_dq_i_delayed3;
	wire main_a7ddrphy_dq_i_delayed4;
	wire main_a7ddrphy_dq_i_delayed5;
	wire main_a7ddrphy_dq_i_delayed6;
	wire main_a7ddrphy_dq_i_delayed7;
	wire main_a7ddrphy_dq_i_delayed8;
	wire main_a7ddrphy_dq_i_delayed9;
	wire main_a7ddrphy_dq_i_nodelay0;
	wire main_a7ddrphy_dq_i_nodelay1;
	wire main_a7ddrphy_dq_i_nodelay10;
	wire main_a7ddrphy_dq_i_nodelay11;
	wire main_a7ddrphy_dq_i_nodelay12;
	wire main_a7ddrphy_dq_i_nodelay13;
	wire main_a7ddrphy_dq_i_nodelay14;
	wire main_a7ddrphy_dq_i_nodelay15;
	wire main_a7ddrphy_dq_i_nodelay2;
	wire main_a7ddrphy_dq_i_nodelay3;
	wire main_a7ddrphy_dq_i_nodelay4;
	wire main_a7ddrphy_dq_i_nodelay5;
	wire main_a7ddrphy_dq_i_nodelay6;
	wire main_a7ddrphy_dq_i_nodelay7;
	wire main_a7ddrphy_dq_i_nodelay8;
	wire main_a7ddrphy_dq_i_nodelay9;
	wire main_a7ddrphy_dq_o_nodelay0;
	wire main_a7ddrphy_dq_o_nodelay1;
	wire main_a7ddrphy_dq_o_nodelay10;
	wire main_a7ddrphy_dq_o_nodelay11;
	wire main_a7ddrphy_dq_o_nodelay12;
	wire main_a7ddrphy_dq_o_nodelay13;
	wire main_a7ddrphy_dq_o_nodelay14;
	wire main_a7ddrphy_dq_o_nodelay15;
	wire main_a7ddrphy_dq_o_nodelay2;
	wire main_a7ddrphy_dq_o_nodelay3;
	wire main_a7ddrphy_dq_o_nodelay4;
	wire main_a7ddrphy_dq_o_nodelay5;
	wire main_a7ddrphy_dq_o_nodelay6;
	wire main_a7ddrphy_dq_o_nodelay7;
	wire main_a7ddrphy_dq_o_nodelay8;
	wire main_a7ddrphy_dq_o_nodelay9;
	wire main_a7ddrphy_dq_oe;
	wire main_a7ddrphy_dq_oe_delay_tappeddelayline;
	reg main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline0 = 1'd0;
	reg main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1 = 1'd0;
	wire main_a7ddrphy_dq_t0;
	wire main_a7ddrphy_dq_t1;
	wire main_a7ddrphy_dq_t10;
	wire main_a7ddrphy_dq_t11;
	wire main_a7ddrphy_dq_t12;
	wire main_a7ddrphy_dq_t13;
	wire main_a7ddrphy_dq_t14;
	wire main_a7ddrphy_dq_t15;
	wire main_a7ddrphy_dq_t2;
	wire main_a7ddrphy_dq_t3;
	wire main_a7ddrphy_dq_t4;
	wire main_a7ddrphy_dq_t5;
	wire main_a7ddrphy_dq_t6;
	wire main_a7ddrphy_dq_t7;
	wire main_a7ddrphy_dq_t8;
	wire main_a7ddrphy_dq_t9;
	wire main_a7ddrphy_dqs_o_no_delay0;
	wire main_a7ddrphy_dqs_o_no_delay1;
	reg main_a7ddrphy_dqs_oe = 1'd0;
	wire main_a7ddrphy_dqs_oe_delay_tappeddelayline;
	reg main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline0 = 1'd0;
	reg main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline1 = 1'd0;
	wire main_a7ddrphy_dqs_postamble;
	wire main_a7ddrphy_dqs_preamble;
	wire main_a7ddrphy_dqs_t0;
	wire main_a7ddrphy_dqs_t1;
	reg main_a7ddrphy_dqspattern0 = 1'd0;
	reg main_a7ddrphy_dqspattern1 = 1'd0;
	reg [7:0] main_a7ddrphy_dqspattern_o0 = 8'd0;
	reg [7:0] main_a7ddrphy_dqspattern_o1 = 8'd0;
	reg main_a7ddrphy_half_sys8x_taps_re = 1'd0;
	reg [4:0] main_a7ddrphy_half_sys8x_taps_storage = 5'd16;
	wire [2:0] main_a7ddrphy_pads_ba;
	reg main_a7ddrphy_rddata_en_tappeddelayline0 = 1'd0;
	reg main_a7ddrphy_rddata_en_tappeddelayline1 = 1'd0;
	reg main_a7ddrphy_rddata_en_tappeddelayline2 = 1'd0;
	reg main_a7ddrphy_rddata_en_tappeddelayline3 = 1'd0;
	reg main_a7ddrphy_rddata_en_tappeddelayline4 = 1'd0;
	reg main_a7ddrphy_rddata_en_tappeddelayline5 = 1'd0;
	reg main_a7ddrphy_rddata_en_tappeddelayline6 = 1'd0;
	reg main_a7ddrphy_rddata_en_tappeddelayline7 = 1'd0;
	wire main_a7ddrphy_rdly_dq_bitslip_r;
	reg main_a7ddrphy_rdly_dq_bitslip_re = 1'd0;
	wire main_a7ddrphy_rdly_dq_bitslip_rst_r;
	reg main_a7ddrphy_rdly_dq_bitslip_rst_re = 1'd0;
	reg main_a7ddrphy_rdly_dq_bitslip_rst_w = 1'd0;
	reg main_a7ddrphy_rdly_dq_bitslip_rst_we = 1'd0;
	reg main_a7ddrphy_rdly_dq_bitslip_w = 1'd0;
	reg main_a7ddrphy_rdly_dq_bitslip_we = 1'd0;
	wire main_a7ddrphy_rdly_dq_inc_r;
	reg main_a7ddrphy_rdly_dq_inc_re = 1'd0;
	reg main_a7ddrphy_rdly_dq_inc_w = 1'd0;
	reg main_a7ddrphy_rdly_dq_inc_we = 1'd0;
	wire main_a7ddrphy_rdly_dq_rst_r;
	reg main_a7ddrphy_rdly_dq_rst_re = 1'd0;
	reg main_a7ddrphy_rdly_dq_rst_w = 1'd0;
	reg main_a7ddrphy_rdly_dq_rst_we = 1'd0;
	reg main_a7ddrphy_rdphase_re = 1'd0;
	reg [1:0] main_a7ddrphy_rdphase_storage = 2'd2;
	reg main_a7ddrphy_rst_re = 1'd0;
	reg main_a7ddrphy_rst_storage = 1'd0;
	wire main_a7ddrphy_sd_clk_se_nodelay;
	wire main_a7ddrphy_wdly_dq_bitslip_r;
	reg main_a7ddrphy_wdly_dq_bitslip_re = 1'd0;
	wire main_a7ddrphy_wdly_dq_bitslip_rst_r;
	reg main_a7ddrphy_wdly_dq_bitslip_rst_re = 1'd0;
	reg main_a7ddrphy_wdly_dq_bitslip_rst_w = 1'd0;
	reg main_a7ddrphy_wdly_dq_bitslip_rst_we = 1'd0;
	reg main_a7ddrphy_wdly_dq_bitslip_w = 1'd0;
	reg main_a7ddrphy_wdly_dq_bitslip_we = 1'd0;
	reg main_a7ddrphy_wlevel_en_re = 1'd0;
	reg main_a7ddrphy_wlevel_en_storage = 1'd0;
	wire main_a7ddrphy_wlevel_strobe_r;
	reg main_a7ddrphy_wlevel_strobe_re = 1'd0;
	reg main_a7ddrphy_wlevel_strobe_w = 1'd0;
	reg main_a7ddrphy_wlevel_strobe_we = 1'd0;
	reg main_a7ddrphy_wrdata_en_tappeddelayline0 = 1'd0;
	reg main_a7ddrphy_wrdata_en_tappeddelayline1 = 1'd0;
	reg main_a7ddrphy_wrdata_en_tappeddelayline2 = 1'd0;
	reg main_a7ddrphy_wrphase_re = 1'd0;
	reg [1:0] main_a7ddrphy_wrphase_storage = 2'd3;
	reg main_aborted = 1'd0;
	reg main_aborted_litedramwishbone2native_next_value = 1'd0;
	reg main_aborted_litedramwishbone2native_next_value_ce = 1'd0;
	reg main_bankmachine0_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine0_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine0_cmd_payload_ba;
	reg main_bankmachine0_cmd_payload_cas = 1'd0;
	reg main_bankmachine0_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine0_cmd_payload_is_read = 1'd0;
	reg main_bankmachine0_cmd_payload_is_write = 1'd0;
	reg main_bankmachine0_cmd_payload_ras = 1'd0;
	reg main_bankmachine0_cmd_payload_we = 1'd0;
	reg main_bankmachine0_cmd_ready = 1'd0;
	reg main_bankmachine0_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine0_consume = 4'd0;
	wire main_bankmachine0_do_read;
	wire main_bankmachine0_fifo_in_first;
	wire main_bankmachine0_fifo_in_last;
	wire [20:0] main_bankmachine0_fifo_in_payload_addr;
	wire main_bankmachine0_fifo_in_payload_we;
	wire main_bankmachine0_fifo_out_first;
	wire main_bankmachine0_fifo_out_last;
	wire [20:0] main_bankmachine0_fifo_out_payload_addr;
	wire main_bankmachine0_fifo_out_payload_we;
	reg [4:0] main_bankmachine0_level = 5'd0;
	wire main_bankmachine0_pipe_valid_sink_first;
	wire main_bankmachine0_pipe_valid_sink_last;
	wire [20:0] main_bankmachine0_pipe_valid_sink_payload_addr;
	wire main_bankmachine0_pipe_valid_sink_payload_we;
	wire main_bankmachine0_pipe_valid_sink_ready;
	wire main_bankmachine0_pipe_valid_sink_valid;
	reg main_bankmachine0_pipe_valid_source_first = 1'd0;
	reg main_bankmachine0_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine0_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine0_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine0_pipe_valid_source_ready;
	reg main_bankmachine0_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine0_produce = 4'd0;
	wire [3:0] main_bankmachine0_rdport_adr;
	wire [23:0] main_bankmachine0_rdport_dat_r;
	reg main_bankmachine0_refresh_gnt = 1'd0;
	wire main_bankmachine0_refresh_req;
	reg main_bankmachine0_replace = 1'd0;
	wire [20:0] main_bankmachine0_req_addr;
	wire main_bankmachine0_req_lock;
	reg main_bankmachine0_req_rdata_valid = 1'd0;
	wire main_bankmachine0_req_ready;
	wire main_bankmachine0_req_valid;
	reg main_bankmachine0_req_wdata_ready = 1'd0;
	wire main_bankmachine0_req_we;
	reg [13:0] main_bankmachine0_row = 14'd0;
	reg main_bankmachine0_row_close = 1'd0;
	reg main_bankmachine0_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine0_row_hit;
	reg main_bankmachine0_row_open = 1'd0;
	reg main_bankmachine0_row_opened = 1'd0;
	reg main_bankmachine0_sink_first = 1'd0;
	reg main_bankmachine0_sink_last = 1'd0;
	wire [20:0] main_bankmachine0_sink_payload_addr;
	wire main_bankmachine0_sink_payload_we;
	wire main_bankmachine0_sink_ready;
	wire main_bankmachine0_sink_sink_first;
	wire main_bankmachine0_sink_sink_last;
	wire [20:0] main_bankmachine0_sink_sink_payload_addr;
	wire main_bankmachine0_sink_sink_payload_we;
	wire main_bankmachine0_sink_sink_ready;
	wire main_bankmachine0_sink_sink_valid;
	wire main_bankmachine0_sink_valid;
	wire main_bankmachine0_source_first;
	wire main_bankmachine0_source_last;
	wire [20:0] main_bankmachine0_source_payload_addr;
	wire main_bankmachine0_source_payload_we;
	wire main_bankmachine0_source_ready;
	wire main_bankmachine0_source_source_first;
	wire main_bankmachine0_source_source_last;
	wire [20:0] main_bankmachine0_source_source_payload_addr;
	wire main_bankmachine0_source_source_payload_we;
	wire main_bankmachine0_source_source_ready;
	wire main_bankmachine0_source_source_valid;
	wire main_bankmachine0_source_valid;
	wire [23:0] main_bankmachine0_syncfifo0_din;
	wire [23:0] main_bankmachine0_syncfifo0_dout;
	wire main_bankmachine0_syncfifo0_re;
	wire main_bankmachine0_syncfifo0_readable;
	wire main_bankmachine0_syncfifo0_we;
	wire main_bankmachine0_syncfifo0_writable;
	reg [1:0] main_bankmachine0_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine0_trascon_ready = 1'd0;
	wire main_bankmachine0_trascon_valid;
	reg [1:0] main_bankmachine0_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine0_trccon_ready = 1'd0;
	wire main_bankmachine0_trccon_valid;
	reg [2:0] main_bankmachine0_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine0_twtpcon_ready = 1'd0;
	wire main_bankmachine0_twtpcon_valid;
	reg [3:0] main_bankmachine0_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine0_wrport_dat_r;
	wire [23:0] main_bankmachine0_wrport_dat_w;
	wire main_bankmachine0_wrport_we;
	reg main_bankmachine1_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine1_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine1_cmd_payload_ba;
	reg main_bankmachine1_cmd_payload_cas = 1'd0;
	reg main_bankmachine1_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine1_cmd_payload_is_read = 1'd0;
	reg main_bankmachine1_cmd_payload_is_write = 1'd0;
	reg main_bankmachine1_cmd_payload_ras = 1'd0;
	reg main_bankmachine1_cmd_payload_we = 1'd0;
	reg main_bankmachine1_cmd_ready = 1'd0;
	reg main_bankmachine1_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine1_consume = 4'd0;
	wire main_bankmachine1_do_read;
	wire main_bankmachine1_fifo_in_first;
	wire main_bankmachine1_fifo_in_last;
	wire [20:0] main_bankmachine1_fifo_in_payload_addr;
	wire main_bankmachine1_fifo_in_payload_we;
	wire main_bankmachine1_fifo_out_first;
	wire main_bankmachine1_fifo_out_last;
	wire [20:0] main_bankmachine1_fifo_out_payload_addr;
	wire main_bankmachine1_fifo_out_payload_we;
	reg [4:0] main_bankmachine1_level = 5'd0;
	wire main_bankmachine1_pipe_valid_sink_first;
	wire main_bankmachine1_pipe_valid_sink_last;
	wire [20:0] main_bankmachine1_pipe_valid_sink_payload_addr;
	wire main_bankmachine1_pipe_valid_sink_payload_we;
	wire main_bankmachine1_pipe_valid_sink_ready;
	wire main_bankmachine1_pipe_valid_sink_valid;
	reg main_bankmachine1_pipe_valid_source_first = 1'd0;
	reg main_bankmachine1_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine1_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine1_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine1_pipe_valid_source_ready;
	reg main_bankmachine1_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine1_produce = 4'd0;
	wire [3:0] main_bankmachine1_rdport_adr;
	wire [23:0] main_bankmachine1_rdport_dat_r;
	reg main_bankmachine1_refresh_gnt = 1'd0;
	wire main_bankmachine1_refresh_req;
	reg main_bankmachine1_replace = 1'd0;
	wire [20:0] main_bankmachine1_req_addr;
	wire main_bankmachine1_req_lock;
	reg main_bankmachine1_req_rdata_valid = 1'd0;
	wire main_bankmachine1_req_ready;
	wire main_bankmachine1_req_valid;
	reg main_bankmachine1_req_wdata_ready = 1'd0;
	wire main_bankmachine1_req_we;
	reg [13:0] main_bankmachine1_row = 14'd0;
	reg main_bankmachine1_row_close = 1'd0;
	reg main_bankmachine1_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine1_row_hit;
	reg main_bankmachine1_row_open = 1'd0;
	reg main_bankmachine1_row_opened = 1'd0;
	reg main_bankmachine1_sink_first = 1'd0;
	reg main_bankmachine1_sink_last = 1'd0;
	wire [20:0] main_bankmachine1_sink_payload_addr;
	wire main_bankmachine1_sink_payload_we;
	wire main_bankmachine1_sink_ready;
	wire main_bankmachine1_sink_sink_first;
	wire main_bankmachine1_sink_sink_last;
	wire [20:0] main_bankmachine1_sink_sink_payload_addr;
	wire main_bankmachine1_sink_sink_payload_we;
	wire main_bankmachine1_sink_sink_ready;
	wire main_bankmachine1_sink_sink_valid;
	wire main_bankmachine1_sink_valid;
	wire main_bankmachine1_source_first;
	wire main_bankmachine1_source_last;
	wire [20:0] main_bankmachine1_source_payload_addr;
	wire main_bankmachine1_source_payload_we;
	wire main_bankmachine1_source_ready;
	wire main_bankmachine1_source_source_first;
	wire main_bankmachine1_source_source_last;
	wire [20:0] main_bankmachine1_source_source_payload_addr;
	wire main_bankmachine1_source_source_payload_we;
	wire main_bankmachine1_source_source_ready;
	wire main_bankmachine1_source_source_valid;
	wire main_bankmachine1_source_valid;
	wire [23:0] main_bankmachine1_syncfifo1_din;
	wire [23:0] main_bankmachine1_syncfifo1_dout;
	wire main_bankmachine1_syncfifo1_re;
	wire main_bankmachine1_syncfifo1_readable;
	wire main_bankmachine1_syncfifo1_we;
	wire main_bankmachine1_syncfifo1_writable;
	reg [1:0] main_bankmachine1_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine1_trascon_ready = 1'd0;
	wire main_bankmachine1_trascon_valid;
	reg [1:0] main_bankmachine1_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine1_trccon_ready = 1'd0;
	wire main_bankmachine1_trccon_valid;
	reg [2:0] main_bankmachine1_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine1_twtpcon_ready = 1'd0;
	wire main_bankmachine1_twtpcon_valid;
	reg [3:0] main_bankmachine1_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine1_wrport_dat_r;
	wire [23:0] main_bankmachine1_wrport_dat_w;
	wire main_bankmachine1_wrport_we;
	reg main_bankmachine2_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine2_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine2_cmd_payload_ba;
	reg main_bankmachine2_cmd_payload_cas = 1'd0;
	reg main_bankmachine2_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine2_cmd_payload_is_read = 1'd0;
	reg main_bankmachine2_cmd_payload_is_write = 1'd0;
	reg main_bankmachine2_cmd_payload_ras = 1'd0;
	reg main_bankmachine2_cmd_payload_we = 1'd0;
	reg main_bankmachine2_cmd_ready = 1'd0;
	reg main_bankmachine2_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine2_consume = 4'd0;
	wire main_bankmachine2_do_read;
	wire main_bankmachine2_fifo_in_first;
	wire main_bankmachine2_fifo_in_last;
	wire [20:0] main_bankmachine2_fifo_in_payload_addr;
	wire main_bankmachine2_fifo_in_payload_we;
	wire main_bankmachine2_fifo_out_first;
	wire main_bankmachine2_fifo_out_last;
	wire [20:0] main_bankmachine2_fifo_out_payload_addr;
	wire main_bankmachine2_fifo_out_payload_we;
	reg [4:0] main_bankmachine2_level = 5'd0;
	wire main_bankmachine2_pipe_valid_sink_first;
	wire main_bankmachine2_pipe_valid_sink_last;
	wire [20:0] main_bankmachine2_pipe_valid_sink_payload_addr;
	wire main_bankmachine2_pipe_valid_sink_payload_we;
	wire main_bankmachine2_pipe_valid_sink_ready;
	wire main_bankmachine2_pipe_valid_sink_valid;
	reg main_bankmachine2_pipe_valid_source_first = 1'd0;
	reg main_bankmachine2_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine2_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine2_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine2_pipe_valid_source_ready;
	reg main_bankmachine2_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine2_produce = 4'd0;
	wire [3:0] main_bankmachine2_rdport_adr;
	wire [23:0] main_bankmachine2_rdport_dat_r;
	reg main_bankmachine2_refresh_gnt = 1'd0;
	wire main_bankmachine2_refresh_req;
	reg main_bankmachine2_replace = 1'd0;
	wire [20:0] main_bankmachine2_req_addr;
	wire main_bankmachine2_req_lock;
	reg main_bankmachine2_req_rdata_valid = 1'd0;
	wire main_bankmachine2_req_ready;
	wire main_bankmachine2_req_valid;
	reg main_bankmachine2_req_wdata_ready = 1'd0;
	wire main_bankmachine2_req_we;
	reg [13:0] main_bankmachine2_row = 14'd0;
	reg main_bankmachine2_row_close = 1'd0;
	reg main_bankmachine2_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine2_row_hit;
	reg main_bankmachine2_row_open = 1'd0;
	reg main_bankmachine2_row_opened = 1'd0;
	reg main_bankmachine2_sink_first = 1'd0;
	reg main_bankmachine2_sink_last = 1'd0;
	wire [20:0] main_bankmachine2_sink_payload_addr;
	wire main_bankmachine2_sink_payload_we;
	wire main_bankmachine2_sink_ready;
	wire main_bankmachine2_sink_sink_first;
	wire main_bankmachine2_sink_sink_last;
	wire [20:0] main_bankmachine2_sink_sink_payload_addr;
	wire main_bankmachine2_sink_sink_payload_we;
	wire main_bankmachine2_sink_sink_ready;
	wire main_bankmachine2_sink_sink_valid;
	wire main_bankmachine2_sink_valid;
	wire main_bankmachine2_source_first;
	wire main_bankmachine2_source_last;
	wire [20:0] main_bankmachine2_source_payload_addr;
	wire main_bankmachine2_source_payload_we;
	wire main_bankmachine2_source_ready;
	wire main_bankmachine2_source_source_first;
	wire main_bankmachine2_source_source_last;
	wire [20:0] main_bankmachine2_source_source_payload_addr;
	wire main_bankmachine2_source_source_payload_we;
	wire main_bankmachine2_source_source_ready;
	wire main_bankmachine2_source_source_valid;
	wire main_bankmachine2_source_valid;
	wire [23:0] main_bankmachine2_syncfifo2_din;
	wire [23:0] main_bankmachine2_syncfifo2_dout;
	wire main_bankmachine2_syncfifo2_re;
	wire main_bankmachine2_syncfifo2_readable;
	wire main_bankmachine2_syncfifo2_we;
	wire main_bankmachine2_syncfifo2_writable;
	reg [1:0] main_bankmachine2_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine2_trascon_ready = 1'd0;
	wire main_bankmachine2_trascon_valid;
	reg [1:0] main_bankmachine2_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine2_trccon_ready = 1'd0;
	wire main_bankmachine2_trccon_valid;
	reg [2:0] main_bankmachine2_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine2_twtpcon_ready = 1'd0;
	wire main_bankmachine2_twtpcon_valid;
	reg [3:0] main_bankmachine2_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine2_wrport_dat_r;
	wire [23:0] main_bankmachine2_wrport_dat_w;
	wire main_bankmachine2_wrport_we;
	reg main_bankmachine3_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine3_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine3_cmd_payload_ba;
	reg main_bankmachine3_cmd_payload_cas = 1'd0;
	reg main_bankmachine3_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine3_cmd_payload_is_read = 1'd0;
	reg main_bankmachine3_cmd_payload_is_write = 1'd0;
	reg main_bankmachine3_cmd_payload_ras = 1'd0;
	reg main_bankmachine3_cmd_payload_we = 1'd0;
	reg main_bankmachine3_cmd_ready = 1'd0;
	reg main_bankmachine3_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine3_consume = 4'd0;
	wire main_bankmachine3_do_read;
	wire main_bankmachine3_fifo_in_first;
	wire main_bankmachine3_fifo_in_last;
	wire [20:0] main_bankmachine3_fifo_in_payload_addr;
	wire main_bankmachine3_fifo_in_payload_we;
	wire main_bankmachine3_fifo_out_first;
	wire main_bankmachine3_fifo_out_last;
	wire [20:0] main_bankmachine3_fifo_out_payload_addr;
	wire main_bankmachine3_fifo_out_payload_we;
	reg [4:0] main_bankmachine3_level = 5'd0;
	wire main_bankmachine3_pipe_valid_sink_first;
	wire main_bankmachine3_pipe_valid_sink_last;
	wire [20:0] main_bankmachine3_pipe_valid_sink_payload_addr;
	wire main_bankmachine3_pipe_valid_sink_payload_we;
	wire main_bankmachine3_pipe_valid_sink_ready;
	wire main_bankmachine3_pipe_valid_sink_valid;
	reg main_bankmachine3_pipe_valid_source_first = 1'd0;
	reg main_bankmachine3_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine3_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine3_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine3_pipe_valid_source_ready;
	reg main_bankmachine3_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine3_produce = 4'd0;
	wire [3:0] main_bankmachine3_rdport_adr;
	wire [23:0] main_bankmachine3_rdport_dat_r;
	reg main_bankmachine3_refresh_gnt = 1'd0;
	wire main_bankmachine3_refresh_req;
	reg main_bankmachine3_replace = 1'd0;
	wire [20:0] main_bankmachine3_req_addr;
	wire main_bankmachine3_req_lock;
	reg main_bankmachine3_req_rdata_valid = 1'd0;
	wire main_bankmachine3_req_ready;
	wire main_bankmachine3_req_valid;
	reg main_bankmachine3_req_wdata_ready = 1'd0;
	wire main_bankmachine3_req_we;
	reg [13:0] main_bankmachine3_row = 14'd0;
	reg main_bankmachine3_row_close = 1'd0;
	reg main_bankmachine3_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine3_row_hit;
	reg main_bankmachine3_row_open = 1'd0;
	reg main_bankmachine3_row_opened = 1'd0;
	reg main_bankmachine3_sink_first = 1'd0;
	reg main_bankmachine3_sink_last = 1'd0;
	wire [20:0] main_bankmachine3_sink_payload_addr;
	wire main_bankmachine3_sink_payload_we;
	wire main_bankmachine3_sink_ready;
	wire main_bankmachine3_sink_sink_first;
	wire main_bankmachine3_sink_sink_last;
	wire [20:0] main_bankmachine3_sink_sink_payload_addr;
	wire main_bankmachine3_sink_sink_payload_we;
	wire main_bankmachine3_sink_sink_ready;
	wire main_bankmachine3_sink_sink_valid;
	wire main_bankmachine3_sink_valid;
	wire main_bankmachine3_source_first;
	wire main_bankmachine3_source_last;
	wire [20:0] main_bankmachine3_source_payload_addr;
	wire main_bankmachine3_source_payload_we;
	wire main_bankmachine3_source_ready;
	wire main_bankmachine3_source_source_first;
	wire main_bankmachine3_source_source_last;
	wire [20:0] main_bankmachine3_source_source_payload_addr;
	wire main_bankmachine3_source_source_payload_we;
	wire main_bankmachine3_source_source_ready;
	wire main_bankmachine3_source_source_valid;
	wire main_bankmachine3_source_valid;
	wire [23:0] main_bankmachine3_syncfifo3_din;
	wire [23:0] main_bankmachine3_syncfifo3_dout;
	wire main_bankmachine3_syncfifo3_re;
	wire main_bankmachine3_syncfifo3_readable;
	wire main_bankmachine3_syncfifo3_we;
	wire main_bankmachine3_syncfifo3_writable;
	reg [1:0] main_bankmachine3_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine3_trascon_ready = 1'd0;
	wire main_bankmachine3_trascon_valid;
	reg [1:0] main_bankmachine3_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine3_trccon_ready = 1'd0;
	wire main_bankmachine3_trccon_valid;
	reg [2:0] main_bankmachine3_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine3_twtpcon_ready = 1'd0;
	wire main_bankmachine3_twtpcon_valid;
	reg [3:0] main_bankmachine3_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine3_wrport_dat_r;
	wire [23:0] main_bankmachine3_wrport_dat_w;
	wire main_bankmachine3_wrport_we;
	reg main_bankmachine4_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine4_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine4_cmd_payload_ba;
	reg main_bankmachine4_cmd_payload_cas = 1'd0;
	reg main_bankmachine4_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine4_cmd_payload_is_read = 1'd0;
	reg main_bankmachine4_cmd_payload_is_write = 1'd0;
	reg main_bankmachine4_cmd_payload_ras = 1'd0;
	reg main_bankmachine4_cmd_payload_we = 1'd0;
	reg main_bankmachine4_cmd_ready = 1'd0;
	reg main_bankmachine4_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine4_consume = 4'd0;
	wire main_bankmachine4_do_read;
	wire main_bankmachine4_fifo_in_first;
	wire main_bankmachine4_fifo_in_last;
	wire [20:0] main_bankmachine4_fifo_in_payload_addr;
	wire main_bankmachine4_fifo_in_payload_we;
	wire main_bankmachine4_fifo_out_first;
	wire main_bankmachine4_fifo_out_last;
	wire [20:0] main_bankmachine4_fifo_out_payload_addr;
	wire main_bankmachine4_fifo_out_payload_we;
	reg [4:0] main_bankmachine4_level = 5'd0;
	wire main_bankmachine4_pipe_valid_sink_first;
	wire main_bankmachine4_pipe_valid_sink_last;
	wire [20:0] main_bankmachine4_pipe_valid_sink_payload_addr;
	wire main_bankmachine4_pipe_valid_sink_payload_we;
	wire main_bankmachine4_pipe_valid_sink_ready;
	wire main_bankmachine4_pipe_valid_sink_valid;
	reg main_bankmachine4_pipe_valid_source_first = 1'd0;
	reg main_bankmachine4_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine4_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine4_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine4_pipe_valid_source_ready;
	reg main_bankmachine4_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine4_produce = 4'd0;
	wire [3:0] main_bankmachine4_rdport_adr;
	wire [23:0] main_bankmachine4_rdport_dat_r;
	reg main_bankmachine4_refresh_gnt = 1'd0;
	wire main_bankmachine4_refresh_req;
	reg main_bankmachine4_replace = 1'd0;
	wire [20:0] main_bankmachine4_req_addr;
	wire main_bankmachine4_req_lock;
	reg main_bankmachine4_req_rdata_valid = 1'd0;
	wire main_bankmachine4_req_ready;
	wire main_bankmachine4_req_valid;
	reg main_bankmachine4_req_wdata_ready = 1'd0;
	wire main_bankmachine4_req_we;
	reg [13:0] main_bankmachine4_row = 14'd0;
	reg main_bankmachine4_row_close = 1'd0;
	reg main_bankmachine4_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine4_row_hit;
	reg main_bankmachine4_row_open = 1'd0;
	reg main_bankmachine4_row_opened = 1'd0;
	reg main_bankmachine4_sink_first = 1'd0;
	reg main_bankmachine4_sink_last = 1'd0;
	wire [20:0] main_bankmachine4_sink_payload_addr;
	wire main_bankmachine4_sink_payload_we;
	wire main_bankmachine4_sink_ready;
	wire main_bankmachine4_sink_sink_first;
	wire main_bankmachine4_sink_sink_last;
	wire [20:0] main_bankmachine4_sink_sink_payload_addr;
	wire main_bankmachine4_sink_sink_payload_we;
	wire main_bankmachine4_sink_sink_ready;
	wire main_bankmachine4_sink_sink_valid;
	wire main_bankmachine4_sink_valid;
	wire main_bankmachine4_source_first;
	wire main_bankmachine4_source_last;
	wire [20:0] main_bankmachine4_source_payload_addr;
	wire main_bankmachine4_source_payload_we;
	wire main_bankmachine4_source_ready;
	wire main_bankmachine4_source_source_first;
	wire main_bankmachine4_source_source_last;
	wire [20:0] main_bankmachine4_source_source_payload_addr;
	wire main_bankmachine4_source_source_payload_we;
	wire main_bankmachine4_source_source_ready;
	wire main_bankmachine4_source_source_valid;
	wire main_bankmachine4_source_valid;
	wire [23:0] main_bankmachine4_syncfifo4_din;
	wire [23:0] main_bankmachine4_syncfifo4_dout;
	wire main_bankmachine4_syncfifo4_re;
	wire main_bankmachine4_syncfifo4_readable;
	wire main_bankmachine4_syncfifo4_we;
	wire main_bankmachine4_syncfifo4_writable;
	reg [1:0] main_bankmachine4_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine4_trascon_ready = 1'd0;
	wire main_bankmachine4_trascon_valid;
	reg [1:0] main_bankmachine4_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine4_trccon_ready = 1'd0;
	wire main_bankmachine4_trccon_valid;
	reg [2:0] main_bankmachine4_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine4_twtpcon_ready = 1'd0;
	wire main_bankmachine4_twtpcon_valid;
	reg [3:0] main_bankmachine4_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine4_wrport_dat_r;
	wire [23:0] main_bankmachine4_wrport_dat_w;
	wire main_bankmachine4_wrport_we;
	reg main_bankmachine5_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine5_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine5_cmd_payload_ba;
	reg main_bankmachine5_cmd_payload_cas = 1'd0;
	reg main_bankmachine5_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine5_cmd_payload_is_read = 1'd0;
	reg main_bankmachine5_cmd_payload_is_write = 1'd0;
	reg main_bankmachine5_cmd_payload_ras = 1'd0;
	reg main_bankmachine5_cmd_payload_we = 1'd0;
	reg main_bankmachine5_cmd_ready = 1'd0;
	reg main_bankmachine5_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine5_consume = 4'd0;
	wire main_bankmachine5_do_read;
	wire main_bankmachine5_fifo_in_first;
	wire main_bankmachine5_fifo_in_last;
	wire [20:0] main_bankmachine5_fifo_in_payload_addr;
	wire main_bankmachine5_fifo_in_payload_we;
	wire main_bankmachine5_fifo_out_first;
	wire main_bankmachine5_fifo_out_last;
	wire [20:0] main_bankmachine5_fifo_out_payload_addr;
	wire main_bankmachine5_fifo_out_payload_we;
	reg [4:0] main_bankmachine5_level = 5'd0;
	wire main_bankmachine5_pipe_valid_sink_first;
	wire main_bankmachine5_pipe_valid_sink_last;
	wire [20:0] main_bankmachine5_pipe_valid_sink_payload_addr;
	wire main_bankmachine5_pipe_valid_sink_payload_we;
	wire main_bankmachine5_pipe_valid_sink_ready;
	wire main_bankmachine5_pipe_valid_sink_valid;
	reg main_bankmachine5_pipe_valid_source_first = 1'd0;
	reg main_bankmachine5_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine5_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine5_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine5_pipe_valid_source_ready;
	reg main_bankmachine5_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine5_produce = 4'd0;
	wire [3:0] main_bankmachine5_rdport_adr;
	wire [23:0] main_bankmachine5_rdport_dat_r;
	reg main_bankmachine5_refresh_gnt = 1'd0;
	wire main_bankmachine5_refresh_req;
	reg main_bankmachine5_replace = 1'd0;
	wire [20:0] main_bankmachine5_req_addr;
	wire main_bankmachine5_req_lock;
	reg main_bankmachine5_req_rdata_valid = 1'd0;
	wire main_bankmachine5_req_ready;
	wire main_bankmachine5_req_valid;
	reg main_bankmachine5_req_wdata_ready = 1'd0;
	wire main_bankmachine5_req_we;
	reg [13:0] main_bankmachine5_row = 14'd0;
	reg main_bankmachine5_row_close = 1'd0;
	reg main_bankmachine5_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine5_row_hit;
	reg main_bankmachine5_row_open = 1'd0;
	reg main_bankmachine5_row_opened = 1'd0;
	reg main_bankmachine5_sink_first = 1'd0;
	reg main_bankmachine5_sink_last = 1'd0;
	wire [20:0] main_bankmachine5_sink_payload_addr;
	wire main_bankmachine5_sink_payload_we;
	wire main_bankmachine5_sink_ready;
	wire main_bankmachine5_sink_sink_first;
	wire main_bankmachine5_sink_sink_last;
	wire [20:0] main_bankmachine5_sink_sink_payload_addr;
	wire main_bankmachine5_sink_sink_payload_we;
	wire main_bankmachine5_sink_sink_ready;
	wire main_bankmachine5_sink_sink_valid;
	wire main_bankmachine5_sink_valid;
	wire main_bankmachine5_source_first;
	wire main_bankmachine5_source_last;
	wire [20:0] main_bankmachine5_source_payload_addr;
	wire main_bankmachine5_source_payload_we;
	wire main_bankmachine5_source_ready;
	wire main_bankmachine5_source_source_first;
	wire main_bankmachine5_source_source_last;
	wire [20:0] main_bankmachine5_source_source_payload_addr;
	wire main_bankmachine5_source_source_payload_we;
	wire main_bankmachine5_source_source_ready;
	wire main_bankmachine5_source_source_valid;
	wire main_bankmachine5_source_valid;
	wire [23:0] main_bankmachine5_syncfifo5_din;
	wire [23:0] main_bankmachine5_syncfifo5_dout;
	wire main_bankmachine5_syncfifo5_re;
	wire main_bankmachine5_syncfifo5_readable;
	wire main_bankmachine5_syncfifo5_we;
	wire main_bankmachine5_syncfifo5_writable;
	reg [1:0] main_bankmachine5_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine5_trascon_ready = 1'd0;
	wire main_bankmachine5_trascon_valid;
	reg [1:0] main_bankmachine5_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine5_trccon_ready = 1'd0;
	wire main_bankmachine5_trccon_valid;
	reg [2:0] main_bankmachine5_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine5_twtpcon_ready = 1'd0;
	wire main_bankmachine5_twtpcon_valid;
	reg [3:0] main_bankmachine5_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine5_wrport_dat_r;
	wire [23:0] main_bankmachine5_wrport_dat_w;
	wire main_bankmachine5_wrport_we;
	reg main_bankmachine6_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine6_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine6_cmd_payload_ba;
	reg main_bankmachine6_cmd_payload_cas = 1'd0;
	reg main_bankmachine6_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine6_cmd_payload_is_read = 1'd0;
	reg main_bankmachine6_cmd_payload_is_write = 1'd0;
	reg main_bankmachine6_cmd_payload_ras = 1'd0;
	reg main_bankmachine6_cmd_payload_we = 1'd0;
	reg main_bankmachine6_cmd_ready = 1'd0;
	reg main_bankmachine6_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine6_consume = 4'd0;
	wire main_bankmachine6_do_read;
	wire main_bankmachine6_fifo_in_first;
	wire main_bankmachine6_fifo_in_last;
	wire [20:0] main_bankmachine6_fifo_in_payload_addr;
	wire main_bankmachine6_fifo_in_payload_we;
	wire main_bankmachine6_fifo_out_first;
	wire main_bankmachine6_fifo_out_last;
	wire [20:0] main_bankmachine6_fifo_out_payload_addr;
	wire main_bankmachine6_fifo_out_payload_we;
	reg [4:0] main_bankmachine6_level = 5'd0;
	wire main_bankmachine6_pipe_valid_sink_first;
	wire main_bankmachine6_pipe_valid_sink_last;
	wire [20:0] main_bankmachine6_pipe_valid_sink_payload_addr;
	wire main_bankmachine6_pipe_valid_sink_payload_we;
	wire main_bankmachine6_pipe_valid_sink_ready;
	wire main_bankmachine6_pipe_valid_sink_valid;
	reg main_bankmachine6_pipe_valid_source_first = 1'd0;
	reg main_bankmachine6_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine6_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine6_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine6_pipe_valid_source_ready;
	reg main_bankmachine6_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine6_produce = 4'd0;
	wire [3:0] main_bankmachine6_rdport_adr;
	wire [23:0] main_bankmachine6_rdport_dat_r;
	reg main_bankmachine6_refresh_gnt = 1'd0;
	wire main_bankmachine6_refresh_req;
	reg main_bankmachine6_replace = 1'd0;
	wire [20:0] main_bankmachine6_req_addr;
	wire main_bankmachine6_req_lock;
	reg main_bankmachine6_req_rdata_valid = 1'd0;
	wire main_bankmachine6_req_ready;
	wire main_bankmachine6_req_valid;
	reg main_bankmachine6_req_wdata_ready = 1'd0;
	wire main_bankmachine6_req_we;
	reg [13:0] main_bankmachine6_row = 14'd0;
	reg main_bankmachine6_row_close = 1'd0;
	reg main_bankmachine6_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine6_row_hit;
	reg main_bankmachine6_row_open = 1'd0;
	reg main_bankmachine6_row_opened = 1'd0;
	reg main_bankmachine6_sink_first = 1'd0;
	reg main_bankmachine6_sink_last = 1'd0;
	wire [20:0] main_bankmachine6_sink_payload_addr;
	wire main_bankmachine6_sink_payload_we;
	wire main_bankmachine6_sink_ready;
	wire main_bankmachine6_sink_sink_first;
	wire main_bankmachine6_sink_sink_last;
	wire [20:0] main_bankmachine6_sink_sink_payload_addr;
	wire main_bankmachine6_sink_sink_payload_we;
	wire main_bankmachine6_sink_sink_ready;
	wire main_bankmachine6_sink_sink_valid;
	wire main_bankmachine6_sink_valid;
	wire main_bankmachine6_source_first;
	wire main_bankmachine6_source_last;
	wire [20:0] main_bankmachine6_source_payload_addr;
	wire main_bankmachine6_source_payload_we;
	wire main_bankmachine6_source_ready;
	wire main_bankmachine6_source_source_first;
	wire main_bankmachine6_source_source_last;
	wire [20:0] main_bankmachine6_source_source_payload_addr;
	wire main_bankmachine6_source_source_payload_we;
	wire main_bankmachine6_source_source_ready;
	wire main_bankmachine6_source_source_valid;
	wire main_bankmachine6_source_valid;
	wire [23:0] main_bankmachine6_syncfifo6_din;
	wire [23:0] main_bankmachine6_syncfifo6_dout;
	wire main_bankmachine6_syncfifo6_re;
	wire main_bankmachine6_syncfifo6_readable;
	wire main_bankmachine6_syncfifo6_we;
	wire main_bankmachine6_syncfifo6_writable;
	reg [1:0] main_bankmachine6_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine6_trascon_ready = 1'd0;
	wire main_bankmachine6_trascon_valid;
	reg [1:0] main_bankmachine6_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine6_trccon_ready = 1'd0;
	wire main_bankmachine6_trccon_valid;
	reg [2:0] main_bankmachine6_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine6_twtpcon_ready = 1'd0;
	wire main_bankmachine6_twtpcon_valid;
	reg [3:0] main_bankmachine6_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine6_wrport_dat_r;
	wire [23:0] main_bankmachine6_wrport_dat_w;
	wire main_bankmachine6_wrport_we;
	reg main_bankmachine7_auto_precharge = 1'd0;
	reg [13:0] main_bankmachine7_cmd_payload_a = 14'd0;
	wire [2:0] main_bankmachine7_cmd_payload_ba;
	reg main_bankmachine7_cmd_payload_cas = 1'd0;
	reg main_bankmachine7_cmd_payload_is_cmd = 1'd0;
	reg main_bankmachine7_cmd_payload_is_read = 1'd0;
	reg main_bankmachine7_cmd_payload_is_write = 1'd0;
	reg main_bankmachine7_cmd_payload_ras = 1'd0;
	reg main_bankmachine7_cmd_payload_we = 1'd0;
	reg main_bankmachine7_cmd_ready = 1'd0;
	reg main_bankmachine7_cmd_valid = 1'd0;
	reg [3:0] main_bankmachine7_consume = 4'd0;
	wire main_bankmachine7_do_read;
	wire main_bankmachine7_fifo_in_first;
	wire main_bankmachine7_fifo_in_last;
	wire [20:0] main_bankmachine7_fifo_in_payload_addr;
	wire main_bankmachine7_fifo_in_payload_we;
	wire main_bankmachine7_fifo_out_first;
	wire main_bankmachine7_fifo_out_last;
	wire [20:0] main_bankmachine7_fifo_out_payload_addr;
	wire main_bankmachine7_fifo_out_payload_we;
	reg [4:0] main_bankmachine7_level = 5'd0;
	wire main_bankmachine7_pipe_valid_sink_first;
	wire main_bankmachine7_pipe_valid_sink_last;
	wire [20:0] main_bankmachine7_pipe_valid_sink_payload_addr;
	wire main_bankmachine7_pipe_valid_sink_payload_we;
	wire main_bankmachine7_pipe_valid_sink_ready;
	wire main_bankmachine7_pipe_valid_sink_valid;
	reg main_bankmachine7_pipe_valid_source_first = 1'd0;
	reg main_bankmachine7_pipe_valid_source_last = 1'd0;
	reg [20:0] main_bankmachine7_pipe_valid_source_payload_addr = 21'd0;
	reg main_bankmachine7_pipe_valid_source_payload_we = 1'd0;
	wire main_bankmachine7_pipe_valid_source_ready;
	reg main_bankmachine7_pipe_valid_source_valid = 1'd0;
	reg [3:0] main_bankmachine7_produce = 4'd0;
	wire [3:0] main_bankmachine7_rdport_adr;
	wire [23:0] main_bankmachine7_rdport_dat_r;
	reg main_bankmachine7_refresh_gnt = 1'd0;
	wire main_bankmachine7_refresh_req;
	reg main_bankmachine7_replace = 1'd0;
	wire [20:0] main_bankmachine7_req_addr;
	wire main_bankmachine7_req_lock;
	reg main_bankmachine7_req_rdata_valid = 1'd0;
	wire main_bankmachine7_req_ready;
	wire main_bankmachine7_req_valid;
	reg main_bankmachine7_req_wdata_ready = 1'd0;
	wire main_bankmachine7_req_we;
	reg [13:0] main_bankmachine7_row = 14'd0;
	reg main_bankmachine7_row_close = 1'd0;
	reg main_bankmachine7_row_col_n_addr_sel = 1'd0;
	wire main_bankmachine7_row_hit;
	reg main_bankmachine7_row_open = 1'd0;
	reg main_bankmachine7_row_opened = 1'd0;
	reg main_bankmachine7_sink_first = 1'd0;
	reg main_bankmachine7_sink_last = 1'd0;
	wire [20:0] main_bankmachine7_sink_payload_addr;
	wire main_bankmachine7_sink_payload_we;
	wire main_bankmachine7_sink_ready;
	wire main_bankmachine7_sink_sink_first;
	wire main_bankmachine7_sink_sink_last;
	wire [20:0] main_bankmachine7_sink_sink_payload_addr;
	wire main_bankmachine7_sink_sink_payload_we;
	wire main_bankmachine7_sink_sink_ready;
	wire main_bankmachine7_sink_sink_valid;
	wire main_bankmachine7_sink_valid;
	wire main_bankmachine7_source_first;
	wire main_bankmachine7_source_last;
	wire [20:0] main_bankmachine7_source_payload_addr;
	wire main_bankmachine7_source_payload_we;
	wire main_bankmachine7_source_ready;
	wire main_bankmachine7_source_source_first;
	wire main_bankmachine7_source_source_last;
	wire [20:0] main_bankmachine7_source_source_payload_addr;
	wire main_bankmachine7_source_source_payload_we;
	wire main_bankmachine7_source_source_ready;
	wire main_bankmachine7_source_source_valid;
	wire main_bankmachine7_source_valid;
	wire [23:0] main_bankmachine7_syncfifo7_din;
	wire [23:0] main_bankmachine7_syncfifo7_dout;
	wire main_bankmachine7_syncfifo7_re;
	wire main_bankmachine7_syncfifo7_readable;
	wire main_bankmachine7_syncfifo7_we;
	wire main_bankmachine7_syncfifo7_writable;
	reg [1:0] main_bankmachine7_trascon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine7_trascon_ready = 1'd0;
	wire main_bankmachine7_trascon_valid;
	reg [1:0] main_bankmachine7_trccon_count = 2'd0;
	(* dont_touch = "true" *) reg main_bankmachine7_trccon_ready = 1'd0;
	wire main_bankmachine7_trccon_valid;
	reg [2:0] main_bankmachine7_twtpcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_bankmachine7_twtpcon_ready = 1'd0;
	wire main_bankmachine7_twtpcon_valid;
	reg [3:0] main_bankmachine7_wrport_adr = 4'd0;
	wire [23:0] main_bankmachine7_wrport_dat_r;
	wire [23:0] main_bankmachine7_wrport_dat_w;
	wire main_bankmachine7_wrport_we;
	wire main_cas_allowed;
	wire main_choose_cmd_ce;
	wire [13:0] main_choose_cmd_cmd_payload_a;
	wire [2:0] main_choose_cmd_cmd_payload_ba;
	reg main_choose_cmd_cmd_payload_cas = 1'd0;
	wire main_choose_cmd_cmd_payload_is_cmd;
	wire main_choose_cmd_cmd_payload_is_read;
	wire main_choose_cmd_cmd_payload_is_write;
	reg main_choose_cmd_cmd_payload_ras = 1'd0;
	reg main_choose_cmd_cmd_payload_we = 1'd0;
	reg main_choose_cmd_cmd_ready = 1'd0;
	wire main_choose_cmd_cmd_valid;
	reg [2:0] main_choose_cmd_grant = 3'd0;
	wire [7:0] main_choose_cmd_request;
	reg [7:0] main_choose_cmd_valids = 8'd0;
	reg main_choose_cmd_want_activates = 1'd0;
	reg main_choose_cmd_want_cmds = 1'd0;
	reg main_choose_cmd_want_reads = 1'd0;
	reg main_choose_cmd_want_writes = 1'd0;
	wire main_choose_req_ce;
	wire [13:0] main_choose_req_cmd_payload_a;
	wire [2:0] main_choose_req_cmd_payload_ba;
	reg main_choose_req_cmd_payload_cas = 1'd0;
	wire main_choose_req_cmd_payload_is_cmd;
	wire main_choose_req_cmd_payload_is_read;
	wire main_choose_req_cmd_payload_is_write;
	reg main_choose_req_cmd_payload_ras = 1'd0;
	reg main_choose_req_cmd_payload_we = 1'd0;
	reg main_choose_req_cmd_ready = 1'd0;
	wire main_choose_req_cmd_valid;
	reg [2:0] main_choose_req_grant = 3'd0;
	wire [7:0] main_choose_req_request;
	reg [7:0] main_choose_req_valids = 8'd0;
	reg main_choose_req_want_activates = 1'd0;
	reg main_choose_req_want_cmds = 1'd0;
	reg main_choose_req_want_reads = 1'd0;
	reg main_choose_req_want_writes = 1'd0;
	wire main_cke;
	wire main_clkin;
	wire main_clkout0;
	wire main_clkout1;
	wire main_clkout2;
	wire main_clkout3;
	wire main_clkout4;
	wire main_clkout_buf0;
	wire main_clkout_buf1;
	wire main_clkout_buf2;
	wire main_clkout_buf3;
	wire main_clkout_buf4;
	reg main_cmd_last = 1'd0;
	reg [13:0] main_cmd_payload_a = 14'd0;
	reg [2:0] main_cmd_payload_ba = 3'd0;
	reg main_cmd_payload_cas = 1'd0;
	reg main_cmd_payload_is_read = 1'd0;
	reg main_cmd_payload_is_write = 1'd0;
	reg main_cmd_payload_ras = 1'd0;
	reg main_cmd_payload_we = 1'd0;
	reg main_cmd_ready = 1'd0;
	reg main_cmd_valid = 1'd0;
	reg main_csr_dfi_p0_act_n = 1'd1;
	wire [13:0] main_csr_dfi_p0_address;
	wire [2:0] main_csr_dfi_p0_bank;
	reg main_csr_dfi_p0_cas_n = 1'd1;
	reg main_csr_dfi_p0_cke = 1'd0;
	reg main_csr_dfi_p0_cs_n = 1'd1;
	reg main_csr_dfi_p0_odt = 1'd0;
	reg main_csr_dfi_p0_ras_n = 1'd1;
	reg [31:0] main_csr_dfi_p0_rddata = 32'd0;
	wire main_csr_dfi_p0_rddata_en;
	reg main_csr_dfi_p0_rddata_valid = 1'd0;
	wire main_csr_dfi_p0_reset_n;
	reg main_csr_dfi_p0_we_n = 1'd1;
	wire [31:0] main_csr_dfi_p0_wrdata;
	wire main_csr_dfi_p0_wrdata_en;
	wire [3:0] main_csr_dfi_p0_wrdata_mask;
	reg main_csr_dfi_p1_act_n = 1'd1;
	wire [13:0] main_csr_dfi_p1_address;
	wire [2:0] main_csr_dfi_p1_bank;
	reg main_csr_dfi_p1_cas_n = 1'd1;
	reg main_csr_dfi_p1_cke = 1'd0;
	reg main_csr_dfi_p1_cs_n = 1'd1;
	reg main_csr_dfi_p1_odt = 1'd0;
	reg main_csr_dfi_p1_ras_n = 1'd1;
	reg [31:0] main_csr_dfi_p1_rddata = 32'd0;
	wire main_csr_dfi_p1_rddata_en;
	reg main_csr_dfi_p1_rddata_valid = 1'd0;
	wire main_csr_dfi_p1_reset_n;
	reg main_csr_dfi_p1_we_n = 1'd1;
	wire [31:0] main_csr_dfi_p1_wrdata;
	wire main_csr_dfi_p1_wrdata_en;
	wire [3:0] main_csr_dfi_p1_wrdata_mask;
	reg main_csr_dfi_p2_act_n = 1'd1;
	wire [13:0] main_csr_dfi_p2_address;
	wire [2:0] main_csr_dfi_p2_bank;
	reg main_csr_dfi_p2_cas_n = 1'd1;
	reg main_csr_dfi_p2_cke = 1'd0;
	reg main_csr_dfi_p2_cs_n = 1'd1;
	reg main_csr_dfi_p2_odt = 1'd0;
	reg main_csr_dfi_p2_ras_n = 1'd1;
	reg [31:0] main_csr_dfi_p2_rddata = 32'd0;
	wire main_csr_dfi_p2_rddata_en;
	reg main_csr_dfi_p2_rddata_valid = 1'd0;
	wire main_csr_dfi_p2_reset_n;
	reg main_csr_dfi_p2_we_n = 1'd1;
	wire [31:0] main_csr_dfi_p2_wrdata;
	wire main_csr_dfi_p2_wrdata_en;
	wire [3:0] main_csr_dfi_p2_wrdata_mask;
	reg main_csr_dfi_p3_act_n = 1'd1;
	wire [13:0] main_csr_dfi_p3_address;
	wire [2:0] main_csr_dfi_p3_bank;
	reg main_csr_dfi_p3_cas_n = 1'd1;
	reg main_csr_dfi_p3_cke = 1'd0;
	reg main_csr_dfi_p3_cs_n = 1'd1;
	reg main_csr_dfi_p3_odt = 1'd0;
	reg main_csr_dfi_p3_ras_n = 1'd1;
	reg [31:0] main_csr_dfi_p3_rddata = 32'd0;
	wire main_csr_dfi_p3_rddata_en;
	reg main_csr_dfi_p3_rddata_valid = 1'd0;
	wire main_csr_dfi_p3_reset_n;
	reg main_csr_dfi_p3_we_n = 1'd1;
	wire [31:0] main_csr_dfi_p3_wrdata;
	wire main_csr_dfi_p3_wrdata_en;
	wire [3:0] main_csr_dfi_p3_wrdata_mask;
	reg main_dfi_p0_act_n = 1'd1;
	reg [13:0] main_dfi_p0_address = 14'd0;
	reg [2:0] main_dfi_p0_bank = 3'd0;
	reg main_dfi_p0_cas_n = 1'd1;
	wire main_dfi_p0_cke;
	reg main_dfi_p0_cs_n = 1'd1;
	wire main_dfi_p0_odt;
	reg main_dfi_p0_ras_n = 1'd1;
	wire [31:0] main_dfi_p0_rddata;
	reg main_dfi_p0_rddata_en = 1'd0;
	wire main_dfi_p0_rddata_valid;
	wire main_dfi_p0_reset_n;
	reg main_dfi_p0_we_n = 1'd1;
	wire [31:0] main_dfi_p0_wrdata;
	reg main_dfi_p0_wrdata_en = 1'd0;
	wire [3:0] main_dfi_p0_wrdata_mask;
	reg main_dfi_p1_act_n = 1'd1;
	reg [13:0] main_dfi_p1_address = 14'd0;
	reg [2:0] main_dfi_p1_bank = 3'd0;
	reg main_dfi_p1_cas_n = 1'd1;
	wire main_dfi_p1_cke;
	reg main_dfi_p1_cs_n = 1'd1;
	wire main_dfi_p1_odt;
	reg main_dfi_p1_ras_n = 1'd1;
	wire [31:0] main_dfi_p1_rddata;
	reg main_dfi_p1_rddata_en = 1'd0;
	wire main_dfi_p1_rddata_valid;
	wire main_dfi_p1_reset_n;
	reg main_dfi_p1_we_n = 1'd1;
	wire [31:0] main_dfi_p1_wrdata;
	reg main_dfi_p1_wrdata_en = 1'd0;
	wire [3:0] main_dfi_p1_wrdata_mask;
	reg main_dfi_p2_act_n = 1'd1;
	reg [13:0] main_dfi_p2_address = 14'd0;
	reg [2:0] main_dfi_p2_bank = 3'd0;
	reg main_dfi_p2_cas_n = 1'd1;
	wire main_dfi_p2_cke;
	reg main_dfi_p2_cs_n = 1'd1;
	wire main_dfi_p2_odt;
	reg main_dfi_p2_ras_n = 1'd1;
	wire [31:0] main_dfi_p2_rddata;
	reg main_dfi_p2_rddata_en = 1'd0;
	wire main_dfi_p2_rddata_valid;
	wire main_dfi_p2_reset_n;
	reg main_dfi_p2_we_n = 1'd1;
	wire [31:0] main_dfi_p2_wrdata;
	reg main_dfi_p2_wrdata_en = 1'd0;
	wire [3:0] main_dfi_p2_wrdata_mask;
	reg main_dfi_p3_act_n = 1'd1;
	reg [13:0] main_dfi_p3_address = 14'd0;
	reg [2:0] main_dfi_p3_bank = 3'd0;
	reg main_dfi_p3_cas_n = 1'd1;
	wire main_dfi_p3_cke;
	reg main_dfi_p3_cs_n = 1'd1;
	wire main_dfi_p3_odt;
	reg main_dfi_p3_ras_n = 1'd1;
	wire [31:0] main_dfi_p3_rddata;
	reg main_dfi_p3_rddata_en = 1'd0;
	wire main_dfi_p3_rddata_valid;
	wire main_dfi_p3_reset_n;
	reg main_dfi_p3_we_n = 1'd1;
	wire [31:0] main_dfi_p3_wrdata;
	reg main_dfi_p3_wrdata_en = 1'd0;
	wire [3:0] main_dfi_p3_wrdata_mask;
	reg main_en0 = 1'd0;
	reg main_en1 = 1'd0;
	reg main_ext_dfi_p0_act_n = 1'd1;
	reg [13:0] main_ext_dfi_p0_address = 14'd0;
	reg [2:0] main_ext_dfi_p0_bank = 3'd0;
	reg main_ext_dfi_p0_cas_n = 1'd1;
	reg main_ext_dfi_p0_cke = 1'd0;
	reg main_ext_dfi_p0_cs_n = 1'd1;
	reg main_ext_dfi_p0_odt = 1'd0;
	reg main_ext_dfi_p0_ras_n = 1'd1;
	reg [31:0] main_ext_dfi_p0_rddata = 32'd0;
	reg main_ext_dfi_p0_rddata_en = 1'd0;
	reg main_ext_dfi_p0_rddata_valid = 1'd0;
	reg main_ext_dfi_p0_reset_n = 1'd0;
	reg main_ext_dfi_p0_we_n = 1'd1;
	reg [31:0] main_ext_dfi_p0_wrdata = 32'd0;
	reg main_ext_dfi_p0_wrdata_en = 1'd0;
	reg [3:0] main_ext_dfi_p0_wrdata_mask = 4'd0;
	reg main_ext_dfi_p1_act_n = 1'd1;
	reg [13:0] main_ext_dfi_p1_address = 14'd0;
	reg [2:0] main_ext_dfi_p1_bank = 3'd0;
	reg main_ext_dfi_p1_cas_n = 1'd1;
	reg main_ext_dfi_p1_cke = 1'd0;
	reg main_ext_dfi_p1_cs_n = 1'd1;
	reg main_ext_dfi_p1_odt = 1'd0;
	reg main_ext_dfi_p1_ras_n = 1'd1;
	reg [31:0] main_ext_dfi_p1_rddata = 32'd0;
	reg main_ext_dfi_p1_rddata_en = 1'd0;
	reg main_ext_dfi_p1_rddata_valid = 1'd0;
	reg main_ext_dfi_p1_reset_n = 1'd0;
	reg main_ext_dfi_p1_we_n = 1'd1;
	reg [31:0] main_ext_dfi_p1_wrdata = 32'd0;
	reg main_ext_dfi_p1_wrdata_en = 1'd0;
	reg [3:0] main_ext_dfi_p1_wrdata_mask = 4'd0;
	reg main_ext_dfi_p2_act_n = 1'd1;
	reg [13:0] main_ext_dfi_p2_address = 14'd0;
	reg [2:0] main_ext_dfi_p2_bank = 3'd0;
	reg main_ext_dfi_p2_cas_n = 1'd1;
	reg main_ext_dfi_p2_cke = 1'd0;
	reg main_ext_dfi_p2_cs_n = 1'd1;
	reg main_ext_dfi_p2_odt = 1'd0;
	reg main_ext_dfi_p2_ras_n = 1'd1;
	reg [31:0] main_ext_dfi_p2_rddata = 32'd0;
	reg main_ext_dfi_p2_rddata_en = 1'd0;
	reg main_ext_dfi_p2_rddata_valid = 1'd0;
	reg main_ext_dfi_p2_reset_n = 1'd0;
	reg main_ext_dfi_p2_we_n = 1'd1;
	reg [31:0] main_ext_dfi_p2_wrdata = 32'd0;
	reg main_ext_dfi_p2_wrdata_en = 1'd0;
	reg [3:0] main_ext_dfi_p2_wrdata_mask = 4'd0;
	reg main_ext_dfi_p3_act_n = 1'd1;
	reg [13:0] main_ext_dfi_p3_address = 14'd0;
	reg [2:0] main_ext_dfi_p3_bank = 3'd0;
	reg main_ext_dfi_p3_cas_n = 1'd1;
	reg main_ext_dfi_p3_cke = 1'd0;
	reg main_ext_dfi_p3_cs_n = 1'd1;
	reg main_ext_dfi_p3_odt = 1'd0;
	reg main_ext_dfi_p3_ras_n = 1'd1;
	reg [31:0] main_ext_dfi_p3_rddata = 32'd0;
	reg main_ext_dfi_p3_rddata_en = 1'd0;
	reg main_ext_dfi_p3_rddata_valid = 1'd0;
	reg main_ext_dfi_p3_reset_n = 1'd0;
	reg main_ext_dfi_p3_we_n = 1'd1;
	reg [31:0] main_ext_dfi_p3_wrdata = 32'd0;
	reg main_ext_dfi_p3_wrdata_en = 1'd0;
	reg [3:0] main_ext_dfi_p3_wrdata_mask = 4'd0;
	reg main_ext_dfi_sel = 1'd0;
	wire main_go_to_refresh;
	reg main_ic_reset = 1'd1;
	reg main_init_done_re = 1'd0;
	reg main_init_done_storage = 1'd0;
	reg main_init_error_re = 1'd0;
	reg main_init_error_storage = 1'd0;
	wire [20:0] main_interface_bank0_addr;
	wire main_interface_bank0_lock;
	wire main_interface_bank0_rdata_valid;
	wire main_interface_bank0_ready;
	wire main_interface_bank0_valid;
	wire main_interface_bank0_wdata_ready;
	wire main_interface_bank0_we;
	wire [20:0] main_interface_bank1_addr;
	wire main_interface_bank1_lock;
	wire main_interface_bank1_rdata_valid;
	wire main_interface_bank1_ready;
	wire main_interface_bank1_valid;
	wire main_interface_bank1_wdata_ready;
	wire main_interface_bank1_we;
	wire [20:0] main_interface_bank2_addr;
	wire main_interface_bank2_lock;
	wire main_interface_bank2_rdata_valid;
	wire main_interface_bank2_ready;
	wire main_interface_bank2_valid;
	wire main_interface_bank2_wdata_ready;
	wire main_interface_bank2_we;
	wire [20:0] main_interface_bank3_addr;
	wire main_interface_bank3_lock;
	wire main_interface_bank3_rdata_valid;
	wire main_interface_bank3_ready;
	wire main_interface_bank3_valid;
	wire main_interface_bank3_wdata_ready;
	wire main_interface_bank3_we;
	wire [20:0] main_interface_bank4_addr;
	wire main_interface_bank4_lock;
	wire main_interface_bank4_rdata_valid;
	wire main_interface_bank4_ready;
	wire main_interface_bank4_valid;
	wire main_interface_bank4_wdata_ready;
	wire main_interface_bank4_we;
	wire [20:0] main_interface_bank5_addr;
	wire main_interface_bank5_lock;
	wire main_interface_bank5_rdata_valid;
	wire main_interface_bank5_ready;
	wire main_interface_bank5_valid;
	wire main_interface_bank5_wdata_ready;
	wire main_interface_bank5_we;
	wire [20:0] main_interface_bank6_addr;
	wire main_interface_bank6_lock;
	wire main_interface_bank6_rdata_valid;
	wire main_interface_bank6_ready;
	wire main_interface_bank6_valid;
	wire main_interface_bank6_wdata_ready;
	wire main_interface_bank6_we;
	wire [20:0] main_interface_bank7_addr;
	wire main_interface_bank7_lock;
	wire main_interface_bank7_rdata_valid;
	wire main_interface_bank7_ready;
	wire main_interface_bank7_valid;
	wire main_interface_bank7_wdata_ready;
	wire main_interface_bank7_we;
	wire [127:0] main_interface_rdata;
	reg [127:0] main_interface_wdata = 128'd0;
	reg [15:0] main_interface_wdata_we = 16'd0;
	reg main_is_ongoing = 1'd0;
	wire main_litedramnativeportconverter_addr_changed;
	reg [25:0] main_litedramnativeportconverter_cmd_addr = 26'd0;
	reg [25:0] main_litedramnativeportconverter_cmd_addr_litedramcore_next_value0 = 26'd0;
	reg main_litedramnativeportconverter_cmd_addr_litedramcore_next_value_ce0 = 1'd0;
	reg main_litedramnativeportconverter_cmd_buffer_sink_first = 1'd0;
	reg main_litedramnativeportconverter_cmd_buffer_sink_last = 1'd0;
	reg [3:0] main_litedramnativeportconverter_cmd_buffer_sink_payload_sel = 4'd0;
	reg main_litedramnativeportconverter_cmd_buffer_sink_payload_we = 1'd0;
	wire main_litedramnativeportconverter_cmd_buffer_sink_ready;
	reg main_litedramnativeportconverter_cmd_buffer_sink_valid = 1'd0;
	wire main_litedramnativeportconverter_cmd_buffer_source_first;
	wire main_litedramnativeportconverter_cmd_buffer_source_last;
	wire [3:0] main_litedramnativeportconverter_cmd_buffer_source_payload_sel;
	wire main_litedramnativeportconverter_cmd_buffer_source_payload_we;
	wire main_litedramnativeportconverter_cmd_buffer_source_ready;
	wire main_litedramnativeportconverter_cmd_buffer_source_valid;
	reg main_litedramnativeportconverter_cmd_last = 1'd0;
	reg main_litedramnativeportconverter_cmd_last_litedramcore_next_value2 = 1'd0;
	reg main_litedramnativeportconverter_cmd_last_litedramcore_next_value_ce2 = 1'd0;
	reg main_litedramnativeportconverter_cmd_we = 1'd0;
	reg main_litedramnativeportconverter_cmd_we_litedramcore_next_value1 = 1'd0;
	reg main_litedramnativeportconverter_cmd_we_litedramcore_next_value_ce1 = 1'd0;
	wire main_litedramnativeportconverter_next_cmd;
	reg [3:0] main_litedramnativeportconverter_rdata_chunk = 4'd1;
	wire main_litedramnativeportconverter_rdata_chunk_valid;
	wire main_litedramnativeportconverter_rdata_converter_converter_first;
	wire main_litedramnativeportconverter_rdata_converter_converter_last;
	reg [1:0] main_litedramnativeportconverter_rdata_converter_converter_mux = 2'd0;
	wire main_litedramnativeportconverter_rdata_converter_converter_sink_first;
	wire main_litedramnativeportconverter_rdata_converter_converter_sink_last;
	reg [127:0] main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data = 128'd0;
	wire main_litedramnativeportconverter_rdata_converter_converter_sink_ready;
	wire main_litedramnativeportconverter_rdata_converter_converter_sink_valid;
	wire main_litedramnativeportconverter_rdata_converter_converter_source_first;
	wire main_litedramnativeportconverter_rdata_converter_converter_source_last;
	reg [31:0] main_litedramnativeportconverter_rdata_converter_converter_source_payload_data = 32'd0;
	wire main_litedramnativeportconverter_rdata_converter_converter_source_payload_valid_token_count;
	wire main_litedramnativeportconverter_rdata_converter_converter_source_ready;
	wire main_litedramnativeportconverter_rdata_converter_converter_source_valid;
	wire main_litedramnativeportconverter_rdata_converter_sink_first;
	wire main_litedramnativeportconverter_rdata_converter_sink_last;
	wire [127:0] main_litedramnativeportconverter_rdata_converter_sink_payload_data;
	wire main_litedramnativeportconverter_rdata_converter_sink_ready;
	wire main_litedramnativeportconverter_rdata_converter_sink_valid;
	wire main_litedramnativeportconverter_rdata_converter_source_first;
	wire main_litedramnativeportconverter_rdata_converter_source_last;
	wire [31:0] main_litedramnativeportconverter_rdata_converter_source_payload_data;
	reg main_litedramnativeportconverter_rdata_converter_source_ready = 1'd0;
	wire main_litedramnativeportconverter_rdata_converter_source_source_first;
	wire main_litedramnativeportconverter_rdata_converter_source_source_last;
	wire [31:0] main_litedramnativeportconverter_rdata_converter_source_source_payload_data;
	wire main_litedramnativeportconverter_rdata_converter_source_source_ready;
	wire main_litedramnativeportconverter_rdata_converter_source_source_valid;
	wire main_litedramnativeportconverter_rdata_converter_source_valid;
	reg [1:0] main_litedramnativeportconverter_rdata_fifo_consume = 2'd0;
	wire main_litedramnativeportconverter_rdata_fifo_do_read;
	wire main_litedramnativeportconverter_rdata_fifo_fifo_in_first;
	wire main_litedramnativeportconverter_rdata_fifo_fifo_in_last;
	wire [127:0] main_litedramnativeportconverter_rdata_fifo_fifo_in_payload_data;
	wire main_litedramnativeportconverter_rdata_fifo_fifo_out_first;
	wire main_litedramnativeportconverter_rdata_fifo_fifo_out_last;
	wire [127:0] main_litedramnativeportconverter_rdata_fifo_fifo_out_payload_data;
	reg [1:0] main_litedramnativeportconverter_rdata_fifo_level = 2'd0;
	reg [1:0] main_litedramnativeportconverter_rdata_fifo_produce = 2'd0;
	wire [1:0] main_litedramnativeportconverter_rdata_fifo_rdport_adr;
	wire [129:0] main_litedramnativeportconverter_rdata_fifo_rdport_dat_r;
	reg main_litedramnativeportconverter_rdata_fifo_replace = 1'd0;
	wire main_litedramnativeportconverter_rdata_fifo_sink_first;
	wire main_litedramnativeportconverter_rdata_fifo_sink_last;
	wire [127:0] main_litedramnativeportconverter_rdata_fifo_sink_payload_data;
	wire main_litedramnativeportconverter_rdata_fifo_sink_ready;
	wire main_litedramnativeportconverter_rdata_fifo_sink_valid;
	wire main_litedramnativeportconverter_rdata_fifo_source_first;
	wire main_litedramnativeportconverter_rdata_fifo_source_last;
	wire [127:0] main_litedramnativeportconverter_rdata_fifo_source_payload_data;
	wire main_litedramnativeportconverter_rdata_fifo_source_ready;
	wire main_litedramnativeportconverter_rdata_fifo_source_valid;
	wire [129:0] main_litedramnativeportconverter_rdata_fifo_syncfifo_din;
	wire [129:0] main_litedramnativeportconverter_rdata_fifo_syncfifo_dout;
	wire main_litedramnativeportconverter_rdata_fifo_syncfifo_re;
	wire main_litedramnativeportconverter_rdata_fifo_syncfifo_readable;
	wire main_litedramnativeportconverter_rdata_fifo_syncfifo_we;
	wire main_litedramnativeportconverter_rdata_fifo_syncfifo_writable;
	reg [1:0] main_litedramnativeportconverter_rdata_fifo_wrport_adr = 2'd0;
	wire [129:0] main_litedramnativeportconverter_rdata_fifo_wrport_dat_r;
	wire [129:0] main_litedramnativeportconverter_rdata_fifo_wrport_dat_w;
	wire main_litedramnativeportconverter_rdata_fifo_wrport_we;
	reg main_litedramnativeportconverter_rdata_finished = 1'd0;
	reg main_litedramnativeportconverter_read_lock = 1'd0;
	reg main_litedramnativeportconverter_read_unlocked = 1'd0;
	wire main_litedramnativeportconverter_rw_collision;
	reg [3:0] main_litedramnativeportconverter_sel = 4'd0;
	reg [3:0] main_litedramnativeportconverter_sel_litedramcore_next_value3 = 4'd0;
	reg main_litedramnativeportconverter_sel_litedramcore_next_value_ce3 = 1'd0;
	wire main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_first;
	wire main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_last;
	wire [127:0] main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_payload_data;
	wire [15:0] main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_payload_we;
	wire main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_ready;
	wire main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_valid;
	reg main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_first = 1'd0;
	reg main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_last = 1'd0;
	reg [127:0] main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_data = 128'd0;
	reg [15:0] main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_we = 16'd0;
	wire main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_ready;
	reg main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_valid = 1'd0;
	reg main_litedramnativeportconverter_wdata_buffer_sink_sink_first = 1'd0;
	reg main_litedramnativeportconverter_wdata_buffer_sink_sink_last = 1'd0;
	wire [127:0] main_litedramnativeportconverter_wdata_buffer_sink_sink_payload_data;
	wire [15:0] main_litedramnativeportconverter_wdata_buffer_sink_sink_payload_we;
	wire main_litedramnativeportconverter_wdata_buffer_sink_sink_ready;
	wire main_litedramnativeportconverter_wdata_buffer_sink_sink_valid;
	wire main_litedramnativeportconverter_wdata_buffer_source_source_first;
	wire main_litedramnativeportconverter_wdata_buffer_source_source_last;
	wire [127:0] main_litedramnativeportconverter_wdata_buffer_source_source_payload_data;
	wire [15:0] main_litedramnativeportconverter_wdata_buffer_source_source_payload_we;
	wire main_litedramnativeportconverter_wdata_buffer_source_source_ready;
	wire main_litedramnativeportconverter_wdata_buffer_source_source_valid;
	reg [3:0] main_litedramnativeportconverter_wdata_chunk = 4'd1;
	wire main_litedramnativeportconverter_wdata_chunk_valid;
	reg [1:0] main_litedramnativeportconverter_wdata_converter_converter_demux = 2'd0;
	wire main_litedramnativeportconverter_wdata_converter_converter_load_part;
	wire main_litedramnativeportconverter_wdata_converter_converter_sink_first;
	wire main_litedramnativeportconverter_wdata_converter_converter_sink_last;
	wire [35:0] main_litedramnativeportconverter_wdata_converter_converter_sink_payload_data;
	wire main_litedramnativeportconverter_wdata_converter_converter_sink_ready;
	wire main_litedramnativeportconverter_wdata_converter_converter_sink_valid;
	reg main_litedramnativeportconverter_wdata_converter_converter_source_first = 1'd0;
	reg main_litedramnativeportconverter_wdata_converter_converter_source_last = 1'd0;
	reg [143:0] main_litedramnativeportconverter_wdata_converter_converter_source_payload_data = 144'd0;
	reg [2:0] main_litedramnativeportconverter_wdata_converter_converter_source_payload_valid_token_count = 3'd0;
	wire main_litedramnativeportconverter_wdata_converter_converter_source_ready;
	wire main_litedramnativeportconverter_wdata_converter_converter_source_valid;
	reg main_litedramnativeportconverter_wdata_converter_converter_strobe_all = 1'd0;
	reg main_litedramnativeportconverter_wdata_converter_sink_first = 1'd0;
	reg main_litedramnativeportconverter_wdata_converter_sink_last = 1'd0;
	reg [31:0] main_litedramnativeportconverter_wdata_converter_sink_payload_data = 32'd0;
	reg [3:0] main_litedramnativeportconverter_wdata_converter_sink_payload_we = 4'd0;
	wire main_litedramnativeportconverter_wdata_converter_sink_ready;
	reg main_litedramnativeportconverter_wdata_converter_sink_valid = 1'd0;
	wire main_litedramnativeportconverter_wdata_converter_source_first;
	wire main_litedramnativeportconverter_wdata_converter_source_last;
	reg [127:0] main_litedramnativeportconverter_wdata_converter_source_payload_data = 128'd0;
	reg [15:0] main_litedramnativeportconverter_wdata_converter_source_payload_we = 16'd0;
	wire main_litedramnativeportconverter_wdata_converter_source_ready;
	wire main_litedramnativeportconverter_wdata_converter_source_source_first;
	wire main_litedramnativeportconverter_wdata_converter_source_source_last;
	wire [143:0] main_litedramnativeportconverter_wdata_converter_source_source_payload_data;
	wire main_litedramnativeportconverter_wdata_converter_source_source_ready;
	wire main_litedramnativeportconverter_wdata_converter_source_source_valid;
	wire main_litedramnativeportconverter_wdata_converter_source_valid;
	reg [1:0] main_litedramnativeportconverter_wdata_fifo_consume = 2'd0;
	wire main_litedramnativeportconverter_wdata_fifo_do_read;
	wire main_litedramnativeportconverter_wdata_fifo_fifo_in_first;
	wire main_litedramnativeportconverter_wdata_fifo_fifo_in_last;
	wire [31:0] main_litedramnativeportconverter_wdata_fifo_fifo_in_payload_data;
	wire [3:0] main_litedramnativeportconverter_wdata_fifo_fifo_in_payload_we;
	wire main_litedramnativeportconverter_wdata_fifo_fifo_out_first;
	wire main_litedramnativeportconverter_wdata_fifo_fifo_out_last;
	wire [31:0] main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_data;
	wire [3:0] main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_we;
	reg [1:0] main_litedramnativeportconverter_wdata_fifo_level = 2'd0;
	reg [1:0] main_litedramnativeportconverter_wdata_fifo_produce = 2'd0;
	wire [1:0] main_litedramnativeportconverter_wdata_fifo_rdport_adr;
	wire [37:0] main_litedramnativeportconverter_wdata_fifo_rdport_dat_r;
	reg main_litedramnativeportconverter_wdata_fifo_replace = 1'd0;
	wire main_litedramnativeportconverter_wdata_fifo_sink_first;
	wire main_litedramnativeportconverter_wdata_fifo_sink_last;
	wire [31:0] main_litedramnativeportconverter_wdata_fifo_sink_payload_data;
	wire [3:0] main_litedramnativeportconverter_wdata_fifo_sink_payload_we;
	wire main_litedramnativeportconverter_wdata_fifo_sink_ready;
	wire main_litedramnativeportconverter_wdata_fifo_sink_valid;
	wire main_litedramnativeportconverter_wdata_fifo_source_first;
	wire main_litedramnativeportconverter_wdata_fifo_source_last;
	wire [31:0] main_litedramnativeportconverter_wdata_fifo_source_payload_data;
	wire [3:0] main_litedramnativeportconverter_wdata_fifo_source_payload_we;
	reg main_litedramnativeportconverter_wdata_fifo_source_ready = 1'd0;
	wire main_litedramnativeportconverter_wdata_fifo_source_valid;
	wire [37:0] main_litedramnativeportconverter_wdata_fifo_syncfifo_din;
	wire [37:0] main_litedramnativeportconverter_wdata_fifo_syncfifo_dout;
	wire main_litedramnativeportconverter_wdata_fifo_syncfifo_re;
	wire main_litedramnativeportconverter_wdata_fifo_syncfifo_readable;
	wire main_litedramnativeportconverter_wdata_fifo_syncfifo_we;
	wire main_litedramnativeportconverter_wdata_fifo_syncfifo_writable;
	reg [1:0] main_litedramnativeportconverter_wdata_fifo_wrport_adr = 2'd0;
	wire [37:0] main_litedramnativeportconverter_wdata_fifo_wrport_dat_r;
	wire [37:0] main_litedramnativeportconverter_wdata_fifo_wrport_dat_w;
	wire main_litedramnativeportconverter_wdata_fifo_wrport_we;
	wire main_litedramnativeportconverter_wdata_finished;
	reg [15:0] main_litedramnativeportconverter_wdata_sel = 16'd0;
	wire main_locked;
	reg main_master_p0_act_n = 1'd1;
	reg [13:0] main_master_p0_address = 14'd0;
	reg [2:0] main_master_p0_bank = 3'd0;
	reg main_master_p0_cas_n = 1'd1;
	reg main_master_p0_cke = 1'd0;
	reg main_master_p0_cs_n = 1'd1;
	reg main_master_p0_odt = 1'd0;
	reg main_master_p0_ras_n = 1'd1;
	wire [31:0] main_master_p0_rddata;
	reg main_master_p0_rddata_en = 1'd0;
	wire main_master_p0_rddata_valid;
	reg main_master_p0_reset_n = 1'd0;
	reg main_master_p0_we_n = 1'd1;
	reg [31:0] main_master_p0_wrdata = 32'd0;
	reg main_master_p0_wrdata_en = 1'd0;
	reg [3:0] main_master_p0_wrdata_mask = 4'd0;
	reg main_master_p1_act_n = 1'd1;
	reg [13:0] main_master_p1_address = 14'd0;
	reg [2:0] main_master_p1_bank = 3'd0;
	reg main_master_p1_cas_n = 1'd1;
	reg main_master_p1_cke = 1'd0;
	reg main_master_p1_cs_n = 1'd1;
	reg main_master_p1_odt = 1'd0;
	reg main_master_p1_ras_n = 1'd1;
	wire [31:0] main_master_p1_rddata;
	reg main_master_p1_rddata_en = 1'd0;
	wire main_master_p1_rddata_valid;
	reg main_master_p1_reset_n = 1'd0;
	reg main_master_p1_we_n = 1'd1;
	reg [31:0] main_master_p1_wrdata = 32'd0;
	reg main_master_p1_wrdata_en = 1'd0;
	reg [3:0] main_master_p1_wrdata_mask = 4'd0;
	reg main_master_p2_act_n = 1'd1;
	reg [13:0] main_master_p2_address = 14'd0;
	reg [2:0] main_master_p2_bank = 3'd0;
	reg main_master_p2_cas_n = 1'd1;
	reg main_master_p2_cke = 1'd0;
	reg main_master_p2_cs_n = 1'd1;
	reg main_master_p2_odt = 1'd0;
	reg main_master_p2_ras_n = 1'd1;
	wire [31:0] main_master_p2_rddata;
	reg main_master_p2_rddata_en = 1'd0;
	wire main_master_p2_rddata_valid;
	reg main_master_p2_reset_n = 1'd0;
	reg main_master_p2_we_n = 1'd1;
	reg [31:0] main_master_p2_wrdata = 32'd0;
	reg main_master_p2_wrdata_en = 1'd0;
	reg [3:0] main_master_p2_wrdata_mask = 4'd0;
	reg main_master_p3_act_n = 1'd1;
	reg [13:0] main_master_p3_address = 14'd0;
	reg [2:0] main_master_p3_bank = 3'd0;
	reg main_master_p3_cas_n = 1'd1;
	reg main_master_p3_cke = 1'd0;
	reg main_master_p3_cs_n = 1'd1;
	reg main_master_p3_odt = 1'd0;
	reg main_master_p3_ras_n = 1'd1;
	wire [31:0] main_master_p3_rddata;
	reg main_master_p3_rddata_en = 1'd0;
	wire main_master_p3_rddata_valid;
	reg main_master_p3_reset_n = 1'd0;
	reg main_master_p3_we_n = 1'd1;
	reg [31:0] main_master_p3_wrdata = 32'd0;
	reg main_master_p3_wrdata_en = 1'd0;
	reg [3:0] main_master_p3_wrdata_mask = 4'd0;
	wire main_max_time0;
	wire main_max_time1;
	wire main_new_port_cmd_last;
	wire [25:0] main_new_port_cmd_payload_addr;
	wire main_new_port_cmd_payload_we;
	reg main_new_port_cmd_ready = 1'd0;
	reg main_new_port_cmd_valid = 1'd0;
	wire main_new_port_flush;
	reg [31:0] main_new_port_rdata_payload_data = 32'd0;
	wire main_new_port_rdata_ready;
	reg main_new_port_rdata_valid = 1'd0;
	reg main_new_port_wdata_first = 1'd0;
	reg main_new_port_wdata_last = 1'd0;
	wire [31:0] main_new_port_wdata_payload_data;
	wire [3:0] main_new_port_wdata_payload_we;
	wire main_new_port_wdata_ready;
	reg main_new_port_wdata_valid = 1'd0;
	reg [13:0] main_nop_a = 14'd0;
	reg [2:0] main_nop_ba = 3'd0;
	wire main_odt;
	reg main_phaseinjector0_address_re = 1'd0;
	reg [13:0] main_phaseinjector0_address_storage = 14'd0;
	reg main_phaseinjector0_baddress_re = 1'd0;
	reg [2:0] main_phaseinjector0_baddress_storage = 3'd0;
	wire main_phaseinjector0_command_issue_r;
	reg main_phaseinjector0_command_issue_re = 1'd0;
	reg main_phaseinjector0_command_issue_w = 1'd0;
	reg main_phaseinjector0_command_issue_we = 1'd0;
	reg main_phaseinjector0_command_re = 1'd0;
	reg [7:0] main_phaseinjector0_command_storage = 8'd0;
	wire main_phaseinjector0_csrfield_cas;
	wire main_phaseinjector0_csrfield_cs;
	wire main_phaseinjector0_csrfield_cs_bottom;
	wire main_phaseinjector0_csrfield_cs_top;
	wire main_phaseinjector0_csrfield_ras;
	wire main_phaseinjector0_csrfield_rden;
	wire main_phaseinjector0_csrfield_we;
	wire main_phaseinjector0_csrfield_wren;
	reg main_phaseinjector0_rddata_re = 1'd0;
	reg [31:0] main_phaseinjector0_rddata_status = 32'd0;
	wire main_phaseinjector0_rddata_we;
	reg main_phaseinjector0_wrdata_re = 1'd0;
	reg [31:0] main_phaseinjector0_wrdata_storage = 32'd0;
	reg main_phaseinjector1_address_re = 1'd0;
	reg [13:0] main_phaseinjector1_address_storage = 14'd0;
	reg main_phaseinjector1_baddress_re = 1'd0;
	reg [2:0] main_phaseinjector1_baddress_storage = 3'd0;
	wire main_phaseinjector1_command_issue_r;
	reg main_phaseinjector1_command_issue_re = 1'd0;
	reg main_phaseinjector1_command_issue_w = 1'd0;
	reg main_phaseinjector1_command_issue_we = 1'd0;
	reg main_phaseinjector1_command_re = 1'd0;
	reg [7:0] main_phaseinjector1_command_storage = 8'd0;
	wire main_phaseinjector1_csrfield_cas;
	wire main_phaseinjector1_csrfield_cs;
	wire main_phaseinjector1_csrfield_cs_bottom;
	wire main_phaseinjector1_csrfield_cs_top;
	wire main_phaseinjector1_csrfield_ras;
	wire main_phaseinjector1_csrfield_rden;
	wire main_phaseinjector1_csrfield_we;
	wire main_phaseinjector1_csrfield_wren;
	reg main_phaseinjector1_rddata_re = 1'd0;
	reg [31:0] main_phaseinjector1_rddata_status = 32'd0;
	wire main_phaseinjector1_rddata_we;
	reg main_phaseinjector1_wrdata_re = 1'd0;
	reg [31:0] main_phaseinjector1_wrdata_storage = 32'd0;
	reg main_phaseinjector2_address_re = 1'd0;
	reg [13:0] main_phaseinjector2_address_storage = 14'd0;
	reg main_phaseinjector2_baddress_re = 1'd0;
	reg [2:0] main_phaseinjector2_baddress_storage = 3'd0;
	wire main_phaseinjector2_command_issue_r;
	reg main_phaseinjector2_command_issue_re = 1'd0;
	reg main_phaseinjector2_command_issue_w = 1'd0;
	reg main_phaseinjector2_command_issue_we = 1'd0;
	reg main_phaseinjector2_command_re = 1'd0;
	reg [7:0] main_phaseinjector2_command_storage = 8'd0;
	wire main_phaseinjector2_csrfield_cas;
	wire main_phaseinjector2_csrfield_cs;
	wire main_phaseinjector2_csrfield_cs_bottom;
	wire main_phaseinjector2_csrfield_cs_top;
	wire main_phaseinjector2_csrfield_ras;
	wire main_phaseinjector2_csrfield_rden;
	wire main_phaseinjector2_csrfield_we;
	wire main_phaseinjector2_csrfield_wren;
	reg main_phaseinjector2_rddata_re = 1'd0;
	reg [31:0] main_phaseinjector2_rddata_status = 32'd0;
	wire main_phaseinjector2_rddata_we;
	reg main_phaseinjector2_wrdata_re = 1'd0;
	reg [31:0] main_phaseinjector2_wrdata_storage = 32'd0;
	reg main_phaseinjector3_address_re = 1'd0;
	reg [13:0] main_phaseinjector3_address_storage = 14'd0;
	reg main_phaseinjector3_baddress_re = 1'd0;
	reg [2:0] main_phaseinjector3_baddress_storage = 3'd0;
	wire main_phaseinjector3_command_issue_r;
	reg main_phaseinjector3_command_issue_re = 1'd0;
	reg main_phaseinjector3_command_issue_w = 1'd0;
	reg main_phaseinjector3_command_issue_we = 1'd0;
	reg main_phaseinjector3_command_re = 1'd0;
	reg [7:0] main_phaseinjector3_command_storage = 8'd0;
	wire main_phaseinjector3_csrfield_cas;
	wire main_phaseinjector3_csrfield_cs;
	wire main_phaseinjector3_csrfield_cs_bottom;
	wire main_phaseinjector3_csrfield_cs_top;
	wire main_phaseinjector3_csrfield_ras;
	wire main_phaseinjector3_csrfield_rden;
	wire main_phaseinjector3_csrfield_we;
	wire main_phaseinjector3_csrfield_wren;
	reg main_phaseinjector3_rddata_re = 1'd0;
	reg [31:0] main_phaseinjector3_rddata_status = 32'd0;
	wire main_phaseinjector3_rddata_we;
	reg main_phaseinjector3_wrdata_re = 1'd0;
	reg [31:0] main_phaseinjector3_wrdata_storage = 32'd0;
	reg [23:0] main_port_cmd_payload_addr = 24'd0;
	reg main_port_cmd_payload_we = 1'd0;
	wire main_port_cmd_ready;
	reg main_port_cmd_valid = 1'd0;
	reg main_port_rdata_first = 1'd0;
	reg main_port_rdata_last = 1'd0;
	wire [127:0] main_port_rdata_payload_data;
	wire main_port_rdata_ready;
	wire main_port_rdata_valid;
	wire main_port_wdata_first;
	wire main_port_wdata_last;
	wire [127:0] main_port_wdata_payload_data;
	wire [15:0] main_port_wdata_payload_we;
	wire main_port_wdata_ready;
	wire main_port_wdata_valid;
	reg main_postponer_count = 1'd0;
	wire main_postponer_req_i;
	reg main_postponer_req_o = 1'd0;
	reg main_power_down = 1'd0;
	wire main_ras_allowed;
	wire [1:0] main_rdcmdphase;
	reg main_re = 1'd0;
	wire main_read_available;
	wire main_reset;
	reg [3:0] main_reset_counter = 4'd15;
	wire main_reset_n;
	reg main_rst = 1'd0;
	wire main_sel;
	reg main_sequencer_count = 1'd0;
	reg [5:0] main_sequencer_counter = 6'd0;
	wire main_sequencer_done0;
	reg main_sequencer_done1 = 1'd0;
	reg main_sequencer_start0 = 1'd0;
	wire main_sequencer_start1;
	wire main_slave_p0_act_n;
	wire [13:0] main_slave_p0_address;
	wire [2:0] main_slave_p0_bank;
	wire main_slave_p0_cas_n;
	wire main_slave_p0_cke;
	wire main_slave_p0_cs_n;
	wire main_slave_p0_odt;
	wire main_slave_p0_ras_n;
	reg [31:0] main_slave_p0_rddata = 32'd0;
	wire main_slave_p0_rddata_en;
	reg main_slave_p0_rddata_valid = 1'd0;
	wire main_slave_p0_reset_n;
	wire main_slave_p0_we_n;
	wire [31:0] main_slave_p0_wrdata;
	wire main_slave_p0_wrdata_en;
	wire [3:0] main_slave_p0_wrdata_mask;
	wire main_slave_p1_act_n;
	wire [13:0] main_slave_p1_address;
	wire [2:0] main_slave_p1_bank;
	wire main_slave_p1_cas_n;
	wire main_slave_p1_cke;
	wire main_slave_p1_cs_n;
	wire main_slave_p1_odt;
	wire main_slave_p1_ras_n;
	reg [31:0] main_slave_p1_rddata = 32'd0;
	wire main_slave_p1_rddata_en;
	reg main_slave_p1_rddata_valid = 1'd0;
	wire main_slave_p1_reset_n;
	wire main_slave_p1_we_n;
	wire [31:0] main_slave_p1_wrdata;
	wire main_slave_p1_wrdata_en;
	wire [3:0] main_slave_p1_wrdata_mask;
	wire main_slave_p2_act_n;
	wire [13:0] main_slave_p2_address;
	wire [2:0] main_slave_p2_bank;
	wire main_slave_p2_cas_n;
	wire main_slave_p2_cke;
	wire main_slave_p2_cs_n;
	wire main_slave_p2_odt;
	wire main_slave_p2_ras_n;
	reg [31:0] main_slave_p2_rddata = 32'd0;
	wire main_slave_p2_rddata_en;
	reg main_slave_p2_rddata_valid = 1'd0;
	wire main_slave_p2_reset_n;
	wire main_slave_p2_we_n;
	wire [31:0] main_slave_p2_wrdata;
	wire main_slave_p2_wrdata_en;
	wire [3:0] main_slave_p2_wrdata_mask;
	wire main_slave_p3_act_n;
	wire [13:0] main_slave_p3_address;
	wire [2:0] main_slave_p3_bank;
	wire main_slave_p3_cas_n;
	wire main_slave_p3_cke;
	wire main_slave_p3_cs_n;
	wire main_slave_p3_odt;
	wire main_slave_p3_ras_n;
	reg [31:0] main_slave_p3_rddata = 32'd0;
	wire main_slave_p3_rddata_en;
	reg main_slave_p3_rddata_valid = 1'd0;
	wire main_slave_p3_reset_n;
	wire main_slave_p3_we_n;
	wire [31:0] main_slave_p3_wrdata;
	wire main_slave_p3_wrdata_en;
	wire [3:0] main_slave_p3_wrdata_mask;
	reg main_steerer0 = 1'd1;
	reg main_steerer1 = 1'd1;
	reg main_steerer2 = 1'd1;
	reg main_steerer3 = 1'd1;
	reg main_steerer4 = 1'd1;
	reg main_steerer5 = 1'd1;
	reg main_steerer6 = 1'd1;
	reg main_steerer7 = 1'd1;
	reg [1:0] main_steerer_sel0 = 2'd0;
	reg [1:0] main_steerer_sel1 = 2'd0;
	reg [1:0] main_steerer_sel2 = 2'd0;
	reg [1:0] main_steerer_sel3 = 2'd0;
	reg [3:0] main_storage = 4'd1;
	reg main_tccdcon_count = 1'd0;
	(* dont_touch = "true" *) reg main_tccdcon_ready = 1'd0;
	wire main_tccdcon_valid;
	wire [1:0] main_tfawcon_count;
	(* dont_touch = "true" *) reg main_tfawcon_ready = 1'd1;
	wire main_tfawcon_valid;
	reg [2:0] main_tfawcon_window = 3'd0;
	reg [4:0] main_time0 = 5'd0;
	reg [3:0] main_time1 = 4'd0;
	wire [8:0] main_timer_count0;
	reg [8:0] main_timer_count1 = 9'd390;
	wire main_timer_done0;
	wire main_timer_done1;
	wire main_timer_wait;
	reg main_trrdcon_count = 1'd0;
	(* dont_touch = "true" *) reg main_trrdcon_ready = 1'd0;
	wire main_trrdcon_valid;
	reg [2:0] main_twtrcon_count = 3'd0;
	(* dont_touch = "true" *) reg main_twtrcon_ready = 1'd0;
	wire main_twtrcon_valid;
	reg main_user_enable = 1'd0;
	wire main_wants_refresh;
	wire main_wants_zqcs;
	wire main_wb_bus_ack;
	wire [29:0] main_wb_bus_adr;
	wire [1:0] main_wb_bus_bte;
	wire [2:0] main_wb_bus_cti;
	wire main_wb_bus_cyc;
	wire [31:0] main_wb_bus_dat_r;
	wire [31:0] main_wb_bus_dat_w;
	wire main_wb_bus_err;
	wire [3:0] main_wb_bus_sel;
	wire main_wb_bus_stb;
	wire main_wb_bus_we;
	reg main_wb_port_ack = 1'd0;
	wire [25:0] main_wb_port_adr;
	wire main_wb_port_cyc;
	reg [31:0] main_wb_port_dat_r = 32'd0;
	wire [31:0] main_wb_port_dat_w;
	reg main_wb_port_err = 1'd0;
	wire [3:0] main_wb_port_sel;
	wire main_wb_port_stb;
	wire main_wb_port_we;
	wire [1:0] main_wrcmdphase;
	wire main_write_available;
	reg [4:0] main_zqcs_executer_counter = 5'd0;
	reg main_zqcs_executer_done = 1'd0;
	reg main_zqcs_executer_start = 1'd0;
	wire [25:0] main_zqcs_timer_count0;
	reg [25:0] main_zqcs_timer_count1 = 26'd49999999;
	wire main_zqcs_timer_done0;
	wire main_zqcs_timer_done1;
	wire main_zqcs_timer_wait;
	wire sys2x_clk;
	wire sys4x_clk;
	wire sys4x_dqs_clk;
	wire sys_clk;
	wire sys_rst;
	assign init_done = main_init_done_storage;
	assign init_error = main_init_error_storage;
	assign main_wb_bus_adr = wb_ctrl_adr;
	assign main_wb_bus_dat_w = wb_ctrl_dat_w;
	assign wb_ctrl_dat_r = main_wb_bus_dat_r;
	assign main_wb_bus_sel = wb_ctrl_sel;
	assign main_wb_bus_cyc = wb_ctrl_cyc;
	assign main_wb_bus_stb = wb_ctrl_stb;
	assign wb_ctrl_ack = main_wb_bus_ack;
	assign main_wb_bus_we = wb_ctrl_we;
	assign main_wb_bus_cti = wb_ctrl_cti;
	assign main_wb_bus_bte = wb_ctrl_bte;
	assign wb_ctrl_err = main_wb_bus_err;
	assign user_clk = sys_clk;
	assign user_clkx2 = sys2x_clk;
	assign user_rst = sys_rst;
	assign main_wb_port_adr = user_port_wishbone_0_adr;
	assign main_wb_port_dat_w = user_port_wishbone_0_dat_w;
	assign user_port_wishbone_0_dat_r = main_wb_port_dat_r;
	assign main_wb_port_sel = user_port_wishbone_0_sel;
	assign main_wb_port_cyc = user_port_wishbone_0_cyc & main_user_enable;
	assign main_wb_port_stb = user_port_wishbone_0_stb & main_user_enable;
	assign user_port_wishbone_0_ack = main_wb_port_ack & main_user_enable;
	assign main_wb_port_we = user_port_wishbone_0_we;
	assign user_port_wishbone_0_err = main_wb_port_err;
	assign builder_interface0_adr = main_wb_bus_adr;
	assign builder_interface0_dat_w = main_wb_bus_dat_w;
	assign main_wb_bus_dat_r = builder_interface0_dat_r;
	assign builder_interface0_sel = main_wb_bus_sel;
	assign builder_interface0_cyc = main_wb_bus_cyc;
	assign builder_interface0_stb = main_wb_bus_stb;
	assign main_wb_bus_ack = builder_interface0_ack;
	assign builder_interface0_we = main_wb_bus_we;
	assign builder_interface0_cti = main_wb_bus_cti;
	assign builder_interface0_bte = main_wb_bus_bte;
	assign main_wb_bus_err = builder_interface0_err;
	assign main_reset = rst | main_rst;
	assign pll_locked = main_locked;
	assign main_clkin = clk;
	assign iodelay_clk = main_clkout_buf0;
	assign sys_clk = main_clkout_buf1;
	assign sys2x_clk = main_clkout_buf2;
	assign sys4x_clk = main_clkout_buf3;
	assign sys4x_dqs_clk = main_clkout_buf4;
	assign ddram_ba = main_a7ddrphy_pads_ba;
	assign main_a7ddrphy_dqs_oe_delay_tappeddelayline = (main_a7ddrphy_dqs_preamble | main_a7ddrphy_dqs_oe) | main_a7ddrphy_dqs_postamble;
	assign main_a7ddrphy_dq_oe_delay_tappeddelayline = (main_a7ddrphy_dqs_preamble | main_a7ddrphy_dq_oe) | main_a7ddrphy_dqs_postamble;
	always @(*) begin
		main_a7ddrphy_dfi_p0_rddata <= 32'd0;
		main_a7ddrphy_dfi_p0_rddata[0] <= main_a7ddrphy_bitslip04[0];
		main_a7ddrphy_dfi_p0_rddata[16] <= main_a7ddrphy_bitslip04[1];
		main_a7ddrphy_dfi_p0_rddata[1] <= main_a7ddrphy_bitslip14[0];
		main_a7ddrphy_dfi_p0_rddata[17] <= main_a7ddrphy_bitslip14[1];
		main_a7ddrphy_dfi_p0_rddata[2] <= main_a7ddrphy_bitslip22[0];
		main_a7ddrphy_dfi_p0_rddata[18] <= main_a7ddrphy_bitslip22[1];
		main_a7ddrphy_dfi_p0_rddata[3] <= main_a7ddrphy_bitslip32[0];
		main_a7ddrphy_dfi_p0_rddata[19] <= main_a7ddrphy_bitslip32[1];
		main_a7ddrphy_dfi_p0_rddata[4] <= main_a7ddrphy_bitslip42[0];
		main_a7ddrphy_dfi_p0_rddata[20] <= main_a7ddrphy_bitslip42[1];
		main_a7ddrphy_dfi_p0_rddata[5] <= main_a7ddrphy_bitslip52[0];
		main_a7ddrphy_dfi_p0_rddata[21] <= main_a7ddrphy_bitslip52[1];
		main_a7ddrphy_dfi_p0_rddata[6] <= main_a7ddrphy_bitslip62[0];
		main_a7ddrphy_dfi_p0_rddata[22] <= main_a7ddrphy_bitslip62[1];
		main_a7ddrphy_dfi_p0_rddata[7] <= main_a7ddrphy_bitslip72[0];
		main_a7ddrphy_dfi_p0_rddata[23] <= main_a7ddrphy_bitslip72[1];
		main_a7ddrphy_dfi_p0_rddata[8] <= main_a7ddrphy_bitslip82[0];
		main_a7ddrphy_dfi_p0_rddata[24] <= main_a7ddrphy_bitslip82[1];
		main_a7ddrphy_dfi_p0_rddata[9] <= main_a7ddrphy_bitslip92[0];
		main_a7ddrphy_dfi_p0_rddata[25] <= main_a7ddrphy_bitslip92[1];
		main_a7ddrphy_dfi_p0_rddata[10] <= main_a7ddrphy_bitslip102[0];
		main_a7ddrphy_dfi_p0_rddata[26] <= main_a7ddrphy_bitslip102[1];
		main_a7ddrphy_dfi_p0_rddata[11] <= main_a7ddrphy_bitslip112[0];
		main_a7ddrphy_dfi_p0_rddata[27] <= main_a7ddrphy_bitslip112[1];
		main_a7ddrphy_dfi_p0_rddata[12] <= main_a7ddrphy_bitslip122[0];
		main_a7ddrphy_dfi_p0_rddata[28] <= main_a7ddrphy_bitslip122[1];
		main_a7ddrphy_dfi_p0_rddata[13] <= main_a7ddrphy_bitslip132[0];
		main_a7ddrphy_dfi_p0_rddata[29] <= main_a7ddrphy_bitslip132[1];
		main_a7ddrphy_dfi_p0_rddata[14] <= main_a7ddrphy_bitslip142[0];
		main_a7ddrphy_dfi_p0_rddata[30] <= main_a7ddrphy_bitslip142[1];
		main_a7ddrphy_dfi_p0_rddata[15] <= main_a7ddrphy_bitslip152[0];
		main_a7ddrphy_dfi_p0_rddata[31] <= main_a7ddrphy_bitslip152[1];
	end
	always @(*) begin
		main_a7ddrphy_dfi_p1_rddata <= 32'd0;
		main_a7ddrphy_dfi_p1_rddata[0] <= main_a7ddrphy_bitslip04[2];
		main_a7ddrphy_dfi_p1_rddata[16] <= main_a7ddrphy_bitslip04[3];
		main_a7ddrphy_dfi_p1_rddata[1] <= main_a7ddrphy_bitslip14[2];
		main_a7ddrphy_dfi_p1_rddata[17] <= main_a7ddrphy_bitslip14[3];
		main_a7ddrphy_dfi_p1_rddata[2] <= main_a7ddrphy_bitslip22[2];
		main_a7ddrphy_dfi_p1_rddata[18] <= main_a7ddrphy_bitslip22[3];
		main_a7ddrphy_dfi_p1_rddata[3] <= main_a7ddrphy_bitslip32[2];
		main_a7ddrphy_dfi_p1_rddata[19] <= main_a7ddrphy_bitslip32[3];
		main_a7ddrphy_dfi_p1_rddata[4] <= main_a7ddrphy_bitslip42[2];
		main_a7ddrphy_dfi_p1_rddata[20] <= main_a7ddrphy_bitslip42[3];
		main_a7ddrphy_dfi_p1_rddata[5] <= main_a7ddrphy_bitslip52[2];
		main_a7ddrphy_dfi_p1_rddata[21] <= main_a7ddrphy_bitslip52[3];
		main_a7ddrphy_dfi_p1_rddata[6] <= main_a7ddrphy_bitslip62[2];
		main_a7ddrphy_dfi_p1_rddata[22] <= main_a7ddrphy_bitslip62[3];
		main_a7ddrphy_dfi_p1_rddata[7] <= main_a7ddrphy_bitslip72[2];
		main_a7ddrphy_dfi_p1_rddata[23] <= main_a7ddrphy_bitslip72[3];
		main_a7ddrphy_dfi_p1_rddata[8] <= main_a7ddrphy_bitslip82[2];
		main_a7ddrphy_dfi_p1_rddata[24] <= main_a7ddrphy_bitslip82[3];
		main_a7ddrphy_dfi_p1_rddata[9] <= main_a7ddrphy_bitslip92[2];
		main_a7ddrphy_dfi_p1_rddata[25] <= main_a7ddrphy_bitslip92[3];
		main_a7ddrphy_dfi_p1_rddata[10] <= main_a7ddrphy_bitslip102[2];
		main_a7ddrphy_dfi_p1_rddata[26] <= main_a7ddrphy_bitslip102[3];
		main_a7ddrphy_dfi_p1_rddata[11] <= main_a7ddrphy_bitslip112[2];
		main_a7ddrphy_dfi_p1_rddata[27] <= main_a7ddrphy_bitslip112[3];
		main_a7ddrphy_dfi_p1_rddata[12] <= main_a7ddrphy_bitslip122[2];
		main_a7ddrphy_dfi_p1_rddata[28] <= main_a7ddrphy_bitslip122[3];
		main_a7ddrphy_dfi_p1_rddata[13] <= main_a7ddrphy_bitslip132[2];
		main_a7ddrphy_dfi_p1_rddata[29] <= main_a7ddrphy_bitslip132[3];
		main_a7ddrphy_dfi_p1_rddata[14] <= main_a7ddrphy_bitslip142[2];
		main_a7ddrphy_dfi_p1_rddata[30] <= main_a7ddrphy_bitslip142[3];
		main_a7ddrphy_dfi_p1_rddata[15] <= main_a7ddrphy_bitslip152[2];
		main_a7ddrphy_dfi_p1_rddata[31] <= main_a7ddrphy_bitslip152[3];
	end
	always @(*) begin
		main_a7ddrphy_dfi_p2_rddata <= 32'd0;
		main_a7ddrphy_dfi_p2_rddata[0] <= main_a7ddrphy_bitslip04[4];
		main_a7ddrphy_dfi_p2_rddata[16] <= main_a7ddrphy_bitslip04[5];
		main_a7ddrphy_dfi_p2_rddata[1] <= main_a7ddrphy_bitslip14[4];
		main_a7ddrphy_dfi_p2_rddata[17] <= main_a7ddrphy_bitslip14[5];
		main_a7ddrphy_dfi_p2_rddata[2] <= main_a7ddrphy_bitslip22[4];
		main_a7ddrphy_dfi_p2_rddata[18] <= main_a7ddrphy_bitslip22[5];
		main_a7ddrphy_dfi_p2_rddata[3] <= main_a7ddrphy_bitslip32[4];
		main_a7ddrphy_dfi_p2_rddata[19] <= main_a7ddrphy_bitslip32[5];
		main_a7ddrphy_dfi_p2_rddata[4] <= main_a7ddrphy_bitslip42[4];
		main_a7ddrphy_dfi_p2_rddata[20] <= main_a7ddrphy_bitslip42[5];
		main_a7ddrphy_dfi_p2_rddata[5] <= main_a7ddrphy_bitslip52[4];
		main_a7ddrphy_dfi_p2_rddata[21] <= main_a7ddrphy_bitslip52[5];
		main_a7ddrphy_dfi_p2_rddata[6] <= main_a7ddrphy_bitslip62[4];
		main_a7ddrphy_dfi_p2_rddata[22] <= main_a7ddrphy_bitslip62[5];
		main_a7ddrphy_dfi_p2_rddata[7] <= main_a7ddrphy_bitslip72[4];
		main_a7ddrphy_dfi_p2_rddata[23] <= main_a7ddrphy_bitslip72[5];
		main_a7ddrphy_dfi_p2_rddata[8] <= main_a7ddrphy_bitslip82[4];
		main_a7ddrphy_dfi_p2_rddata[24] <= main_a7ddrphy_bitslip82[5];
		main_a7ddrphy_dfi_p2_rddata[9] <= main_a7ddrphy_bitslip92[4];
		main_a7ddrphy_dfi_p2_rddata[25] <= main_a7ddrphy_bitslip92[5];
		main_a7ddrphy_dfi_p2_rddata[10] <= main_a7ddrphy_bitslip102[4];
		main_a7ddrphy_dfi_p2_rddata[26] <= main_a7ddrphy_bitslip102[5];
		main_a7ddrphy_dfi_p2_rddata[11] <= main_a7ddrphy_bitslip112[4];
		main_a7ddrphy_dfi_p2_rddata[27] <= main_a7ddrphy_bitslip112[5];
		main_a7ddrphy_dfi_p2_rddata[12] <= main_a7ddrphy_bitslip122[4];
		main_a7ddrphy_dfi_p2_rddata[28] <= main_a7ddrphy_bitslip122[5];
		main_a7ddrphy_dfi_p2_rddata[13] <= main_a7ddrphy_bitslip132[4];
		main_a7ddrphy_dfi_p2_rddata[29] <= main_a7ddrphy_bitslip132[5];
		main_a7ddrphy_dfi_p2_rddata[14] <= main_a7ddrphy_bitslip142[4];
		main_a7ddrphy_dfi_p2_rddata[30] <= main_a7ddrphy_bitslip142[5];
		main_a7ddrphy_dfi_p2_rddata[15] <= main_a7ddrphy_bitslip152[4];
		main_a7ddrphy_dfi_p2_rddata[31] <= main_a7ddrphy_bitslip152[5];
	end
	always @(*) begin
		main_a7ddrphy_dfi_p3_rddata <= 32'd0;
		main_a7ddrphy_dfi_p3_rddata[0] <= main_a7ddrphy_bitslip04[6];
		main_a7ddrphy_dfi_p3_rddata[16] <= main_a7ddrphy_bitslip04[7];
		main_a7ddrphy_dfi_p3_rddata[1] <= main_a7ddrphy_bitslip14[6];
		main_a7ddrphy_dfi_p3_rddata[17] <= main_a7ddrphy_bitslip14[7];
		main_a7ddrphy_dfi_p3_rddata[2] <= main_a7ddrphy_bitslip22[6];
		main_a7ddrphy_dfi_p3_rddata[18] <= main_a7ddrphy_bitslip22[7];
		main_a7ddrphy_dfi_p3_rddata[3] <= main_a7ddrphy_bitslip32[6];
		main_a7ddrphy_dfi_p3_rddata[19] <= main_a7ddrphy_bitslip32[7];
		main_a7ddrphy_dfi_p3_rddata[4] <= main_a7ddrphy_bitslip42[6];
		main_a7ddrphy_dfi_p3_rddata[20] <= main_a7ddrphy_bitslip42[7];
		main_a7ddrphy_dfi_p3_rddata[5] <= main_a7ddrphy_bitslip52[6];
		main_a7ddrphy_dfi_p3_rddata[21] <= main_a7ddrphy_bitslip52[7];
		main_a7ddrphy_dfi_p3_rddata[6] <= main_a7ddrphy_bitslip62[6];
		main_a7ddrphy_dfi_p3_rddata[22] <= main_a7ddrphy_bitslip62[7];
		main_a7ddrphy_dfi_p3_rddata[7] <= main_a7ddrphy_bitslip72[6];
		main_a7ddrphy_dfi_p3_rddata[23] <= main_a7ddrphy_bitslip72[7];
		main_a7ddrphy_dfi_p3_rddata[8] <= main_a7ddrphy_bitslip82[6];
		main_a7ddrphy_dfi_p3_rddata[24] <= main_a7ddrphy_bitslip82[7];
		main_a7ddrphy_dfi_p3_rddata[9] <= main_a7ddrphy_bitslip92[6];
		main_a7ddrphy_dfi_p3_rddata[25] <= main_a7ddrphy_bitslip92[7];
		main_a7ddrphy_dfi_p3_rddata[10] <= main_a7ddrphy_bitslip102[6];
		main_a7ddrphy_dfi_p3_rddata[26] <= main_a7ddrphy_bitslip102[7];
		main_a7ddrphy_dfi_p3_rddata[11] <= main_a7ddrphy_bitslip112[6];
		main_a7ddrphy_dfi_p3_rddata[27] <= main_a7ddrphy_bitslip112[7];
		main_a7ddrphy_dfi_p3_rddata[12] <= main_a7ddrphy_bitslip122[6];
		main_a7ddrphy_dfi_p3_rddata[28] <= main_a7ddrphy_bitslip122[7];
		main_a7ddrphy_dfi_p3_rddata[13] <= main_a7ddrphy_bitslip132[6];
		main_a7ddrphy_dfi_p3_rddata[29] <= main_a7ddrphy_bitslip132[7];
		main_a7ddrphy_dfi_p3_rddata[14] <= main_a7ddrphy_bitslip142[6];
		main_a7ddrphy_dfi_p3_rddata[30] <= main_a7ddrphy_bitslip142[7];
		main_a7ddrphy_dfi_p3_rddata[15] <= main_a7ddrphy_bitslip152[6];
		main_a7ddrphy_dfi_p3_rddata[31] <= main_a7ddrphy_bitslip152[7];
	end
	assign main_a7ddrphy_dfi_p0_rddata_valid = main_a7ddrphy_rddata_en_tappeddelayline7 | main_a7ddrphy_wlevel_en_storage;
	assign main_a7ddrphy_dfi_p1_rddata_valid = main_a7ddrphy_rddata_en_tappeddelayline7 | main_a7ddrphy_wlevel_en_storage;
	assign main_a7ddrphy_dfi_p2_rddata_valid = main_a7ddrphy_rddata_en_tappeddelayline7 | main_a7ddrphy_wlevel_en_storage;
	assign main_a7ddrphy_dfi_p3_rddata_valid = main_a7ddrphy_rddata_en_tappeddelayline7 | main_a7ddrphy_wlevel_en_storage;
	assign main_a7ddrphy_dq_oe = main_a7ddrphy_wrdata_en_tappeddelayline1;
	always @(*) begin
		main_a7ddrphy_dqs_oe <= 1'd0;
		if (main_a7ddrphy_wlevel_en_storage)
			main_a7ddrphy_dqs_oe <= 1'd1;
		else
			main_a7ddrphy_dqs_oe <= main_a7ddrphy_dq_oe;
	end
	assign main_a7ddrphy_dqs_preamble = main_a7ddrphy_wrdata_en_tappeddelayline0 & ~main_a7ddrphy_wrdata_en_tappeddelayline1;
	assign main_a7ddrphy_dqs_postamble = main_a7ddrphy_wrdata_en_tappeddelayline2 & ~main_a7ddrphy_wrdata_en_tappeddelayline1;
	always @(*) begin
		main_a7ddrphy_dqspattern_o0 <= 8'd0;
		main_a7ddrphy_dqspattern_o0 <= 7'd85;
		if (main_a7ddrphy_dqspattern0)
			main_a7ddrphy_dqspattern_o0 <= 5'd21;
		if (main_a7ddrphy_dqspattern1)
			main_a7ddrphy_dqspattern_o0 <= 7'd84;
		if (main_a7ddrphy_wlevel_en_storage) begin
			main_a7ddrphy_dqspattern_o0 <= 1'd0;
			if (main_a7ddrphy_wlevel_strobe_re)
				main_a7ddrphy_dqspattern_o0 <= 1'd1;
		end
	end
	always @(*) begin
		main_a7ddrphy_bitslip00 <= 8'd0;
		case (main_a7ddrphy_bitslip0_value0)
			1'd0: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[8:1];
			1'd1: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[9:2];
			2'd2: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[10:3];
			2'd3: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[11:4];
			3'd4: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[12:5];
			3'd5: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[13:6];
			3'd6: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[14:7];
			3'd7: main_a7ddrphy_bitslip00 <= main_a7ddrphy_bitslip0_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip10 <= 8'd0;
		case (main_a7ddrphy_bitslip1_value0)
			1'd0: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[8:1];
			1'd1: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[9:2];
			2'd2: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[10:3];
			2'd3: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[11:4];
			3'd4: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[12:5];
			3'd5: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[13:6];
			3'd6: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[14:7];
			3'd7: main_a7ddrphy_bitslip10 <= main_a7ddrphy_bitslip1_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip01 <= 8'd0;
		case (main_a7ddrphy_bitslip0_value1)
			1'd0: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[8:1];
			1'd1: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[9:2];
			2'd2: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[10:3];
			2'd3: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[11:4];
			3'd4: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[12:5];
			3'd5: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[13:6];
			3'd6: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[14:7];
			3'd7: main_a7ddrphy_bitslip01 <= main_a7ddrphy_bitslip0_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip11 <= 8'd0;
		case (main_a7ddrphy_bitslip1_value1)
			1'd0: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[8:1];
			1'd1: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[9:2];
			2'd2: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[10:3];
			2'd3: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[11:4];
			3'd4: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[12:5];
			3'd5: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[13:6];
			3'd6: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[14:7];
			3'd7: main_a7ddrphy_bitslip11 <= main_a7ddrphy_bitslip1_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip02 <= 8'd0;
		case (main_a7ddrphy_bitslip0_value2)
			1'd0: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[8:1];
			1'd1: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[9:2];
			2'd2: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[10:3];
			2'd3: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[11:4];
			3'd4: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[12:5];
			3'd5: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[13:6];
			3'd6: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[14:7];
			3'd7: main_a7ddrphy_bitslip02 <= main_a7ddrphy_bitslip0_r2[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip04 <= 8'd0;
		case (main_a7ddrphy_bitslip0_value3)
			1'd0: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[8:1];
			1'd1: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[9:2];
			2'd2: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[10:3];
			2'd3: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[11:4];
			3'd4: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[12:5];
			3'd5: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[13:6];
			3'd6: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[14:7];
			3'd7: main_a7ddrphy_bitslip04 <= main_a7ddrphy_bitslip0_r3[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip12 <= 8'd0;
		case (main_a7ddrphy_bitslip1_value2)
			1'd0: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[8:1];
			1'd1: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[9:2];
			2'd2: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[10:3];
			2'd3: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[11:4];
			3'd4: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[12:5];
			3'd5: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[13:6];
			3'd6: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[14:7];
			3'd7: main_a7ddrphy_bitslip12 <= main_a7ddrphy_bitslip1_r2[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip14 <= 8'd0;
		case (main_a7ddrphy_bitslip1_value3)
			1'd0: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[8:1];
			1'd1: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[9:2];
			2'd2: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[10:3];
			2'd3: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[11:4];
			3'd4: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[12:5];
			3'd5: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[13:6];
			3'd6: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[14:7];
			3'd7: main_a7ddrphy_bitslip14 <= main_a7ddrphy_bitslip1_r3[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip20 <= 8'd0;
		case (main_a7ddrphy_bitslip2_value0)
			1'd0: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[8:1];
			1'd1: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[9:2];
			2'd2: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[10:3];
			2'd3: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[11:4];
			3'd4: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[12:5];
			3'd5: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[13:6];
			3'd6: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[14:7];
			3'd7: main_a7ddrphy_bitslip20 <= main_a7ddrphy_bitslip2_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip22 <= 8'd0;
		case (main_a7ddrphy_bitslip2_value1)
			1'd0: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[8:1];
			1'd1: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[9:2];
			2'd2: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[10:3];
			2'd3: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[11:4];
			3'd4: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[12:5];
			3'd5: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[13:6];
			3'd6: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[14:7];
			3'd7: main_a7ddrphy_bitslip22 <= main_a7ddrphy_bitslip2_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip30 <= 8'd0;
		case (main_a7ddrphy_bitslip3_value0)
			1'd0: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[8:1];
			1'd1: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[9:2];
			2'd2: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[10:3];
			2'd3: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[11:4];
			3'd4: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[12:5];
			3'd5: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[13:6];
			3'd6: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[14:7];
			3'd7: main_a7ddrphy_bitslip30 <= main_a7ddrphy_bitslip3_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip32 <= 8'd0;
		case (main_a7ddrphy_bitslip3_value1)
			1'd0: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[8:1];
			1'd1: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[9:2];
			2'd2: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[10:3];
			2'd3: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[11:4];
			3'd4: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[12:5];
			3'd5: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[13:6];
			3'd6: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[14:7];
			3'd7: main_a7ddrphy_bitslip32 <= main_a7ddrphy_bitslip3_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip40 <= 8'd0;
		case (main_a7ddrphy_bitslip4_value0)
			1'd0: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[8:1];
			1'd1: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[9:2];
			2'd2: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[10:3];
			2'd3: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[11:4];
			3'd4: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[12:5];
			3'd5: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[13:6];
			3'd6: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[14:7];
			3'd7: main_a7ddrphy_bitslip40 <= main_a7ddrphy_bitslip4_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip42 <= 8'd0;
		case (main_a7ddrphy_bitslip4_value1)
			1'd0: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[8:1];
			1'd1: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[9:2];
			2'd2: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[10:3];
			2'd3: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[11:4];
			3'd4: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[12:5];
			3'd5: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[13:6];
			3'd6: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[14:7];
			3'd7: main_a7ddrphy_bitslip42 <= main_a7ddrphy_bitslip4_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip50 <= 8'd0;
		case (main_a7ddrphy_bitslip5_value0)
			1'd0: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[8:1];
			1'd1: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[9:2];
			2'd2: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[10:3];
			2'd3: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[11:4];
			3'd4: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[12:5];
			3'd5: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[13:6];
			3'd6: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[14:7];
			3'd7: main_a7ddrphy_bitslip50 <= main_a7ddrphy_bitslip5_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip52 <= 8'd0;
		case (main_a7ddrphy_bitslip5_value1)
			1'd0: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[8:1];
			1'd1: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[9:2];
			2'd2: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[10:3];
			2'd3: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[11:4];
			3'd4: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[12:5];
			3'd5: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[13:6];
			3'd6: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[14:7];
			3'd7: main_a7ddrphy_bitslip52 <= main_a7ddrphy_bitslip5_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip60 <= 8'd0;
		case (main_a7ddrphy_bitslip6_value0)
			1'd0: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[8:1];
			1'd1: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[9:2];
			2'd2: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[10:3];
			2'd3: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[11:4];
			3'd4: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[12:5];
			3'd5: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[13:6];
			3'd6: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[14:7];
			3'd7: main_a7ddrphy_bitslip60 <= main_a7ddrphy_bitslip6_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip62 <= 8'd0;
		case (main_a7ddrphy_bitslip6_value1)
			1'd0: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[8:1];
			1'd1: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[9:2];
			2'd2: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[10:3];
			2'd3: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[11:4];
			3'd4: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[12:5];
			3'd5: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[13:6];
			3'd6: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[14:7];
			3'd7: main_a7ddrphy_bitslip62 <= main_a7ddrphy_bitslip6_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip70 <= 8'd0;
		case (main_a7ddrphy_bitslip7_value0)
			1'd0: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[8:1];
			1'd1: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[9:2];
			2'd2: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[10:3];
			2'd3: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[11:4];
			3'd4: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[12:5];
			3'd5: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[13:6];
			3'd6: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[14:7];
			3'd7: main_a7ddrphy_bitslip70 <= main_a7ddrphy_bitslip7_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip72 <= 8'd0;
		case (main_a7ddrphy_bitslip7_value1)
			1'd0: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[8:1];
			1'd1: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[9:2];
			2'd2: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[10:3];
			2'd3: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[11:4];
			3'd4: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[12:5];
			3'd5: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[13:6];
			3'd6: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[14:7];
			3'd7: main_a7ddrphy_bitslip72 <= main_a7ddrphy_bitslip7_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip80 <= 8'd0;
		case (main_a7ddrphy_bitslip8_value0)
			1'd0: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[8:1];
			1'd1: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[9:2];
			2'd2: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[10:3];
			2'd3: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[11:4];
			3'd4: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[12:5];
			3'd5: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[13:6];
			3'd6: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[14:7];
			3'd7: main_a7ddrphy_bitslip80 <= main_a7ddrphy_bitslip8_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip82 <= 8'd0;
		case (main_a7ddrphy_bitslip8_value1)
			1'd0: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[8:1];
			1'd1: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[9:2];
			2'd2: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[10:3];
			2'd3: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[11:4];
			3'd4: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[12:5];
			3'd5: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[13:6];
			3'd6: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[14:7];
			3'd7: main_a7ddrphy_bitslip82 <= main_a7ddrphy_bitslip8_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip90 <= 8'd0;
		case (main_a7ddrphy_bitslip9_value0)
			1'd0: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[8:1];
			1'd1: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[9:2];
			2'd2: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[10:3];
			2'd3: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[11:4];
			3'd4: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[12:5];
			3'd5: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[13:6];
			3'd6: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[14:7];
			3'd7: main_a7ddrphy_bitslip90 <= main_a7ddrphy_bitslip9_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip92 <= 8'd0;
		case (main_a7ddrphy_bitslip9_value1)
			1'd0: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[8:1];
			1'd1: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[9:2];
			2'd2: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[10:3];
			2'd3: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[11:4];
			3'd4: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[12:5];
			3'd5: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[13:6];
			3'd6: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[14:7];
			3'd7: main_a7ddrphy_bitslip92 <= main_a7ddrphy_bitslip9_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip100 <= 8'd0;
		case (main_a7ddrphy_bitslip10_value0)
			1'd0: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[8:1];
			1'd1: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[9:2];
			2'd2: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[10:3];
			2'd3: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[11:4];
			3'd4: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[12:5];
			3'd5: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[13:6];
			3'd6: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[14:7];
			3'd7: main_a7ddrphy_bitslip100 <= main_a7ddrphy_bitslip10_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip102 <= 8'd0;
		case (main_a7ddrphy_bitslip10_value1)
			1'd0: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[8:1];
			1'd1: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[9:2];
			2'd2: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[10:3];
			2'd3: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[11:4];
			3'd4: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[12:5];
			3'd5: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[13:6];
			3'd6: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[14:7];
			3'd7: main_a7ddrphy_bitslip102 <= main_a7ddrphy_bitslip10_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip110 <= 8'd0;
		case (main_a7ddrphy_bitslip11_value0)
			1'd0: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[8:1];
			1'd1: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[9:2];
			2'd2: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[10:3];
			2'd3: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[11:4];
			3'd4: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[12:5];
			3'd5: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[13:6];
			3'd6: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[14:7];
			3'd7: main_a7ddrphy_bitslip110 <= main_a7ddrphy_bitslip11_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip112 <= 8'd0;
		case (main_a7ddrphy_bitslip11_value1)
			1'd0: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[8:1];
			1'd1: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[9:2];
			2'd2: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[10:3];
			2'd3: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[11:4];
			3'd4: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[12:5];
			3'd5: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[13:6];
			3'd6: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[14:7];
			3'd7: main_a7ddrphy_bitslip112 <= main_a7ddrphy_bitslip11_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip120 <= 8'd0;
		case (main_a7ddrphy_bitslip12_value0)
			1'd0: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[8:1];
			1'd1: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[9:2];
			2'd2: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[10:3];
			2'd3: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[11:4];
			3'd4: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[12:5];
			3'd5: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[13:6];
			3'd6: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[14:7];
			3'd7: main_a7ddrphy_bitslip120 <= main_a7ddrphy_bitslip12_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip122 <= 8'd0;
		case (main_a7ddrphy_bitslip12_value1)
			1'd0: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[8:1];
			1'd1: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[9:2];
			2'd2: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[10:3];
			2'd3: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[11:4];
			3'd4: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[12:5];
			3'd5: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[13:6];
			3'd6: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[14:7];
			3'd7: main_a7ddrphy_bitslip122 <= main_a7ddrphy_bitslip12_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip130 <= 8'd0;
		case (main_a7ddrphy_bitslip13_value0)
			1'd0: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[8:1];
			1'd1: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[9:2];
			2'd2: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[10:3];
			2'd3: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[11:4];
			3'd4: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[12:5];
			3'd5: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[13:6];
			3'd6: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[14:7];
			3'd7: main_a7ddrphy_bitslip130 <= main_a7ddrphy_bitslip13_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip132 <= 8'd0;
		case (main_a7ddrphy_bitslip13_value1)
			1'd0: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[8:1];
			1'd1: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[9:2];
			2'd2: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[10:3];
			2'd3: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[11:4];
			3'd4: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[12:5];
			3'd5: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[13:6];
			3'd6: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[14:7];
			3'd7: main_a7ddrphy_bitslip132 <= main_a7ddrphy_bitslip13_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip140 <= 8'd0;
		case (main_a7ddrphy_bitslip14_value0)
			1'd0: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[8:1];
			1'd1: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[9:2];
			2'd2: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[10:3];
			2'd3: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[11:4];
			3'd4: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[12:5];
			3'd5: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[13:6];
			3'd6: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[14:7];
			3'd7: main_a7ddrphy_bitslip140 <= main_a7ddrphy_bitslip14_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip142 <= 8'd0;
		case (main_a7ddrphy_bitslip14_value1)
			1'd0: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[8:1];
			1'd1: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[9:2];
			2'd2: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[10:3];
			2'd3: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[11:4];
			3'd4: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[12:5];
			3'd5: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[13:6];
			3'd6: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[14:7];
			3'd7: main_a7ddrphy_bitslip142 <= main_a7ddrphy_bitslip14_r1[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip150 <= 8'd0;
		case (main_a7ddrphy_bitslip15_value0)
			1'd0: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[8:1];
			1'd1: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[9:2];
			2'd2: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[10:3];
			2'd3: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[11:4];
			3'd4: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[12:5];
			3'd5: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[13:6];
			3'd6: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[14:7];
			3'd7: main_a7ddrphy_bitslip150 <= main_a7ddrphy_bitslip15_r0[15:8];
		endcase
	end
	always @(*) begin
		main_a7ddrphy_bitslip152 <= 8'd0;
		case (main_a7ddrphy_bitslip15_value1)
			1'd0: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[8:1];
			1'd1: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[9:2];
			2'd2: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[10:3];
			2'd3: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[11:4];
			3'd4: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[12:5];
			3'd5: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[13:6];
			3'd6: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[14:7];
			3'd7: main_a7ddrphy_bitslip152 <= main_a7ddrphy_bitslip15_r1[15:8];
		endcase
	end
	assign main_a7ddrphy_dfi_p0_address = main_master_p0_address;
	assign main_a7ddrphy_dfi_p0_bank = main_master_p0_bank;
	assign main_a7ddrphy_dfi_p0_cas_n = main_master_p0_cas_n;
	assign main_a7ddrphy_dfi_p0_cs_n = main_master_p0_cs_n;
	assign main_a7ddrphy_dfi_p0_ras_n = main_master_p0_ras_n;
	assign main_a7ddrphy_dfi_p0_we_n = main_master_p0_we_n;
	assign main_a7ddrphy_dfi_p0_cke = main_master_p0_cke;
	assign main_a7ddrphy_dfi_p0_odt = main_master_p0_odt;
	assign main_a7ddrphy_dfi_p0_reset_n = main_master_p0_reset_n;
	assign main_a7ddrphy_dfi_p0_act_n = main_master_p0_act_n;
	assign main_a7ddrphy_dfi_p0_wrdata = main_master_p0_wrdata;
	assign main_a7ddrphy_dfi_p0_wrdata_en = main_master_p0_wrdata_en;
	assign main_a7ddrphy_dfi_p0_wrdata_mask = main_master_p0_wrdata_mask;
	assign main_a7ddrphy_dfi_p0_rddata_en = main_master_p0_rddata_en;
	assign main_master_p0_rddata = main_a7ddrphy_dfi_p0_rddata;
	assign main_master_p0_rddata_valid = main_a7ddrphy_dfi_p0_rddata_valid;
	assign main_a7ddrphy_dfi_p1_address = main_master_p1_address;
	assign main_a7ddrphy_dfi_p1_bank = main_master_p1_bank;
	assign main_a7ddrphy_dfi_p1_cas_n = main_master_p1_cas_n;
	assign main_a7ddrphy_dfi_p1_cs_n = main_master_p1_cs_n;
	assign main_a7ddrphy_dfi_p1_ras_n = main_master_p1_ras_n;
	assign main_a7ddrphy_dfi_p1_we_n = main_master_p1_we_n;
	assign main_a7ddrphy_dfi_p1_cke = main_master_p1_cke;
	assign main_a7ddrphy_dfi_p1_odt = main_master_p1_odt;
	assign main_a7ddrphy_dfi_p1_reset_n = main_master_p1_reset_n;
	assign main_a7ddrphy_dfi_p1_act_n = main_master_p1_act_n;
	assign main_a7ddrphy_dfi_p1_wrdata = main_master_p1_wrdata;
	assign main_a7ddrphy_dfi_p1_wrdata_en = main_master_p1_wrdata_en;
	assign main_a7ddrphy_dfi_p1_wrdata_mask = main_master_p1_wrdata_mask;
	assign main_a7ddrphy_dfi_p1_rddata_en = main_master_p1_rddata_en;
	assign main_master_p1_rddata = main_a7ddrphy_dfi_p1_rddata;
	assign main_master_p1_rddata_valid = main_a7ddrphy_dfi_p1_rddata_valid;
	assign main_a7ddrphy_dfi_p2_address = main_master_p2_address;
	assign main_a7ddrphy_dfi_p2_bank = main_master_p2_bank;
	assign main_a7ddrphy_dfi_p2_cas_n = main_master_p2_cas_n;
	assign main_a7ddrphy_dfi_p2_cs_n = main_master_p2_cs_n;
	assign main_a7ddrphy_dfi_p2_ras_n = main_master_p2_ras_n;
	assign main_a7ddrphy_dfi_p2_we_n = main_master_p2_we_n;
	assign main_a7ddrphy_dfi_p2_cke = main_master_p2_cke;
	assign main_a7ddrphy_dfi_p2_odt = main_master_p2_odt;
	assign main_a7ddrphy_dfi_p2_reset_n = main_master_p2_reset_n;
	assign main_a7ddrphy_dfi_p2_act_n = main_master_p2_act_n;
	assign main_a7ddrphy_dfi_p2_wrdata = main_master_p2_wrdata;
	assign main_a7ddrphy_dfi_p2_wrdata_en = main_master_p2_wrdata_en;
	assign main_a7ddrphy_dfi_p2_wrdata_mask = main_master_p2_wrdata_mask;
	assign main_a7ddrphy_dfi_p2_rddata_en = main_master_p2_rddata_en;
	assign main_master_p2_rddata = main_a7ddrphy_dfi_p2_rddata;
	assign main_master_p2_rddata_valid = main_a7ddrphy_dfi_p2_rddata_valid;
	assign main_a7ddrphy_dfi_p3_address = main_master_p3_address;
	assign main_a7ddrphy_dfi_p3_bank = main_master_p3_bank;
	assign main_a7ddrphy_dfi_p3_cas_n = main_master_p3_cas_n;
	assign main_a7ddrphy_dfi_p3_cs_n = main_master_p3_cs_n;
	assign main_a7ddrphy_dfi_p3_ras_n = main_master_p3_ras_n;
	assign main_a7ddrphy_dfi_p3_we_n = main_master_p3_we_n;
	assign main_a7ddrphy_dfi_p3_cke = main_master_p3_cke;
	assign main_a7ddrphy_dfi_p3_odt = main_master_p3_odt;
	assign main_a7ddrphy_dfi_p3_reset_n = main_master_p3_reset_n;
	assign main_a7ddrphy_dfi_p3_act_n = main_master_p3_act_n;
	assign main_a7ddrphy_dfi_p3_wrdata = main_master_p3_wrdata;
	assign main_a7ddrphy_dfi_p3_wrdata_en = main_master_p3_wrdata_en;
	assign main_a7ddrphy_dfi_p3_wrdata_mask = main_master_p3_wrdata_mask;
	assign main_a7ddrphy_dfi_p3_rddata_en = main_master_p3_rddata_en;
	assign main_master_p3_rddata = main_a7ddrphy_dfi_p3_rddata;
	assign main_master_p3_rddata_valid = main_a7ddrphy_dfi_p3_rddata_valid;
	assign main_slave_p0_address = main_dfi_p0_address;
	assign main_slave_p0_bank = main_dfi_p0_bank;
	assign main_slave_p0_cas_n = main_dfi_p0_cas_n;
	assign main_slave_p0_cs_n = main_dfi_p0_cs_n;
	assign main_slave_p0_ras_n = main_dfi_p0_ras_n;
	assign main_slave_p0_we_n = main_dfi_p0_we_n;
	assign main_slave_p0_cke = main_dfi_p0_cke;
	assign main_slave_p0_odt = main_dfi_p0_odt;
	assign main_slave_p0_reset_n = main_dfi_p0_reset_n;
	assign main_slave_p0_act_n = main_dfi_p0_act_n;
	assign main_slave_p0_wrdata = main_dfi_p0_wrdata;
	assign main_slave_p0_wrdata_en = main_dfi_p0_wrdata_en;
	assign main_slave_p0_wrdata_mask = main_dfi_p0_wrdata_mask;
	assign main_slave_p0_rddata_en = main_dfi_p0_rddata_en;
	assign main_dfi_p0_rddata = main_slave_p0_rddata;
	assign main_dfi_p0_rddata_valid = main_slave_p0_rddata_valid;
	assign main_slave_p1_address = main_dfi_p1_address;
	assign main_slave_p1_bank = main_dfi_p1_bank;
	assign main_slave_p1_cas_n = main_dfi_p1_cas_n;
	assign main_slave_p1_cs_n = main_dfi_p1_cs_n;
	assign main_slave_p1_ras_n = main_dfi_p1_ras_n;
	assign main_slave_p1_we_n = main_dfi_p1_we_n;
	assign main_slave_p1_cke = main_dfi_p1_cke;
	assign main_slave_p1_odt = main_dfi_p1_odt;
	assign main_slave_p1_reset_n = main_dfi_p1_reset_n;
	assign main_slave_p1_act_n = main_dfi_p1_act_n;
	assign main_slave_p1_wrdata = main_dfi_p1_wrdata;
	assign main_slave_p1_wrdata_en = main_dfi_p1_wrdata_en;
	assign main_slave_p1_wrdata_mask = main_dfi_p1_wrdata_mask;
	assign main_slave_p1_rddata_en = main_dfi_p1_rddata_en;
	assign main_dfi_p1_rddata = main_slave_p1_rddata;
	assign main_dfi_p1_rddata_valid = main_slave_p1_rddata_valid;
	assign main_slave_p2_address = main_dfi_p2_address;
	assign main_slave_p2_bank = main_dfi_p2_bank;
	assign main_slave_p2_cas_n = main_dfi_p2_cas_n;
	assign main_slave_p2_cs_n = main_dfi_p2_cs_n;
	assign main_slave_p2_ras_n = main_dfi_p2_ras_n;
	assign main_slave_p2_we_n = main_dfi_p2_we_n;
	assign main_slave_p2_cke = main_dfi_p2_cke;
	assign main_slave_p2_odt = main_dfi_p2_odt;
	assign main_slave_p2_reset_n = main_dfi_p2_reset_n;
	assign main_slave_p2_act_n = main_dfi_p2_act_n;
	assign main_slave_p2_wrdata = main_dfi_p2_wrdata;
	assign main_slave_p2_wrdata_en = main_dfi_p2_wrdata_en;
	assign main_slave_p2_wrdata_mask = main_dfi_p2_wrdata_mask;
	assign main_slave_p2_rddata_en = main_dfi_p2_rddata_en;
	assign main_dfi_p2_rddata = main_slave_p2_rddata;
	assign main_dfi_p2_rddata_valid = main_slave_p2_rddata_valid;
	assign main_slave_p3_address = main_dfi_p3_address;
	assign main_slave_p3_bank = main_dfi_p3_bank;
	assign main_slave_p3_cas_n = main_dfi_p3_cas_n;
	assign main_slave_p3_cs_n = main_dfi_p3_cs_n;
	assign main_slave_p3_ras_n = main_dfi_p3_ras_n;
	assign main_slave_p3_we_n = main_dfi_p3_we_n;
	assign main_slave_p3_cke = main_dfi_p3_cke;
	assign main_slave_p3_odt = main_dfi_p3_odt;
	assign main_slave_p3_reset_n = main_dfi_p3_reset_n;
	assign main_slave_p3_act_n = main_dfi_p3_act_n;
	assign main_slave_p3_wrdata = main_dfi_p3_wrdata;
	assign main_slave_p3_wrdata_en = main_dfi_p3_wrdata_en;
	assign main_slave_p3_wrdata_mask = main_dfi_p3_wrdata_mask;
	assign main_slave_p3_rddata_en = main_dfi_p3_rddata_en;
	assign main_dfi_p3_rddata = main_slave_p3_rddata;
	assign main_dfi_p3_rddata_valid = main_slave_p3_rddata_valid;
	always @(*) begin
		main_csr_dfi_p0_rddata <= 32'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p0_rddata <= main_master_p0_rddata;
	end
	always @(*) begin
		main_csr_dfi_p0_rddata_valid <= 1'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p0_rddata_valid <= main_master_p0_rddata_valid;
	end
	always @(*) begin
		main_csr_dfi_p1_rddata <= 32'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p1_rddata <= main_master_p1_rddata;
	end
	always @(*) begin
		main_csr_dfi_p1_rddata_valid <= 1'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p1_rddata_valid <= main_master_p1_rddata_valid;
	end
	always @(*) begin
		main_csr_dfi_p2_rddata <= 32'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p2_rddata <= main_master_p2_rddata;
	end
	always @(*) begin
		main_csr_dfi_p2_rddata_valid <= 1'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p2_rddata_valid <= main_master_p2_rddata_valid;
	end
	always @(*) begin
		main_csr_dfi_p3_rddata <= 32'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p3_rddata <= main_master_p3_rddata;
	end
	always @(*) begin
		main_csr_dfi_p3_rddata_valid <= 1'd0;
		if (main_sel)
			;
		else
			main_csr_dfi_p3_rddata_valid <= main_master_p3_rddata_valid;
	end
	always @(*) begin
		main_ext_dfi_p0_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p0_rddata <= main_master_p0_rddata;
		end
	end
	always @(*) begin
		main_ext_dfi_p0_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p0_rddata_valid <= main_master_p0_rddata_valid;
		end
	end
	always @(*) begin
		main_ext_dfi_p1_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p1_rddata <= main_master_p1_rddata;
		end
	end
	always @(*) begin
		main_ext_dfi_p1_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p1_rddata_valid <= main_master_p1_rddata_valid;
		end
	end
	always @(*) begin
		main_ext_dfi_p2_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p2_rddata <= main_master_p2_rddata;
		end
	end
	always @(*) begin
		main_ext_dfi_p2_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p2_rddata_valid <= main_master_p2_rddata_valid;
		end
	end
	always @(*) begin
		main_slave_p0_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p0_rddata <= main_master_p0_rddata;
		end
	end
	always @(*) begin
		main_slave_p0_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p0_rddata_valid <= main_master_p0_rddata_valid;
		end
	end
	always @(*) begin
		main_ext_dfi_p3_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p3_rddata <= main_master_p3_rddata;
		end
	end
	always @(*) begin
		main_ext_dfi_p3_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_ext_dfi_p3_rddata_valid <= main_master_p3_rddata_valid;
		end
	end
	always @(*) begin
		main_slave_p1_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p1_rddata <= main_master_p1_rddata;
		end
	end
	always @(*) begin
		main_slave_p1_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p1_rddata_valid <= main_master_p1_rddata_valid;
		end
	end
	always @(*) begin
		main_slave_p2_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p2_rddata <= main_master_p2_rddata;
		end
	end
	always @(*) begin
		main_slave_p2_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p2_rddata_valid <= main_master_p2_rddata_valid;
		end
	end
	always @(*) begin
		main_slave_p3_rddata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p3_rddata <= main_master_p3_rddata;
		end
	end
	always @(*) begin
		main_slave_p3_rddata_valid <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				;
			else
				main_slave_p3_rddata_valid <= main_master_p3_rddata_valid;
		end
	end
	always @(*) begin
		main_master_p0_address <= 14'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_address <= main_ext_dfi_p0_address;
			else
				main_master_p0_address <= main_slave_p0_address;
		end
		else
			main_master_p0_address <= main_csr_dfi_p0_address;
	end
	always @(*) begin
		main_master_p0_bank <= 3'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_bank <= main_ext_dfi_p0_bank;
			else
				main_master_p0_bank <= main_slave_p0_bank;
		end
		else
			main_master_p0_bank <= main_csr_dfi_p0_bank;
	end
	always @(*) begin
		main_master_p0_cas_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_cas_n <= main_ext_dfi_p0_cas_n;
			else
				main_master_p0_cas_n <= main_slave_p0_cas_n;
		end
		else
			main_master_p0_cas_n <= main_csr_dfi_p0_cas_n;
	end
	always @(*) begin
		main_master_p0_cs_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_cs_n <= main_ext_dfi_p0_cs_n;
			else
				main_master_p0_cs_n <= main_slave_p0_cs_n;
		end
		else
			main_master_p0_cs_n <= main_csr_dfi_p0_cs_n;
	end
	always @(*) begin
		main_master_p0_ras_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_ras_n <= main_ext_dfi_p0_ras_n;
			else
				main_master_p0_ras_n <= main_slave_p0_ras_n;
		end
		else
			main_master_p0_ras_n <= main_csr_dfi_p0_ras_n;
	end
	always @(*) begin
		main_master_p0_we_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_we_n <= main_ext_dfi_p0_we_n;
			else
				main_master_p0_we_n <= main_slave_p0_we_n;
		end
		else
			main_master_p0_we_n <= main_csr_dfi_p0_we_n;
	end
	always @(*) begin
		main_master_p0_cke <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_cke <= main_ext_dfi_p0_cke;
			else
				main_master_p0_cke <= main_slave_p0_cke;
		end
		else
			main_master_p0_cke <= main_csr_dfi_p0_cke;
	end
	always @(*) begin
		main_master_p0_odt <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_odt <= main_ext_dfi_p0_odt;
			else
				main_master_p0_odt <= main_slave_p0_odt;
		end
		else
			main_master_p0_odt <= main_csr_dfi_p0_odt;
	end
	always @(*) begin
		main_master_p0_reset_n <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_reset_n <= main_ext_dfi_p0_reset_n;
			else
				main_master_p0_reset_n <= main_slave_p0_reset_n;
		end
		else
			main_master_p0_reset_n <= main_csr_dfi_p0_reset_n;
	end
	always @(*) begin
		main_master_p0_act_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_act_n <= main_ext_dfi_p0_act_n;
			else
				main_master_p0_act_n <= main_slave_p0_act_n;
		end
		else
			main_master_p0_act_n <= main_csr_dfi_p0_act_n;
	end
	always @(*) begin
		main_master_p0_wrdata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_wrdata <= main_ext_dfi_p0_wrdata;
			else
				main_master_p0_wrdata <= main_slave_p0_wrdata;
		end
		else
			main_master_p0_wrdata <= main_csr_dfi_p0_wrdata;
	end
	always @(*) begin
		main_master_p0_wrdata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_wrdata_en <= main_ext_dfi_p0_wrdata_en;
			else
				main_master_p0_wrdata_en <= main_slave_p0_wrdata_en;
		end
		else
			main_master_p0_wrdata_en <= main_csr_dfi_p0_wrdata_en;
	end
	always @(*) begin
		main_master_p0_wrdata_mask <= 4'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_wrdata_mask <= main_ext_dfi_p0_wrdata_mask;
			else
				main_master_p0_wrdata_mask <= main_slave_p0_wrdata_mask;
		end
		else
			main_master_p0_wrdata_mask <= main_csr_dfi_p0_wrdata_mask;
	end
	always @(*) begin
		main_master_p0_rddata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p0_rddata_en <= main_ext_dfi_p0_rddata_en;
			else
				main_master_p0_rddata_en <= main_slave_p0_rddata_en;
		end
		else
			main_master_p0_rddata_en <= main_csr_dfi_p0_rddata_en;
	end
	always @(*) begin
		main_master_p1_address <= 14'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_address <= main_ext_dfi_p1_address;
			else
				main_master_p1_address <= main_slave_p1_address;
		end
		else
			main_master_p1_address <= main_csr_dfi_p1_address;
	end
	always @(*) begin
		main_master_p1_bank <= 3'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_bank <= main_ext_dfi_p1_bank;
			else
				main_master_p1_bank <= main_slave_p1_bank;
		end
		else
			main_master_p1_bank <= main_csr_dfi_p1_bank;
	end
	always @(*) begin
		main_master_p1_cas_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_cas_n <= main_ext_dfi_p1_cas_n;
			else
				main_master_p1_cas_n <= main_slave_p1_cas_n;
		end
		else
			main_master_p1_cas_n <= main_csr_dfi_p1_cas_n;
	end
	always @(*) begin
		main_master_p1_cs_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_cs_n <= main_ext_dfi_p1_cs_n;
			else
				main_master_p1_cs_n <= main_slave_p1_cs_n;
		end
		else
			main_master_p1_cs_n <= main_csr_dfi_p1_cs_n;
	end
	always @(*) begin
		main_master_p1_ras_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_ras_n <= main_ext_dfi_p1_ras_n;
			else
				main_master_p1_ras_n <= main_slave_p1_ras_n;
		end
		else
			main_master_p1_ras_n <= main_csr_dfi_p1_ras_n;
	end
	always @(*) begin
		main_master_p1_we_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_we_n <= main_ext_dfi_p1_we_n;
			else
				main_master_p1_we_n <= main_slave_p1_we_n;
		end
		else
			main_master_p1_we_n <= main_csr_dfi_p1_we_n;
	end
	always @(*) begin
		main_master_p1_cke <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_cke <= main_ext_dfi_p1_cke;
			else
				main_master_p1_cke <= main_slave_p1_cke;
		end
		else
			main_master_p1_cke <= main_csr_dfi_p1_cke;
	end
	always @(*) begin
		main_master_p1_odt <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_odt <= main_ext_dfi_p1_odt;
			else
				main_master_p1_odt <= main_slave_p1_odt;
		end
		else
			main_master_p1_odt <= main_csr_dfi_p1_odt;
	end
	always @(*) begin
		main_master_p1_reset_n <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_reset_n <= main_ext_dfi_p1_reset_n;
			else
				main_master_p1_reset_n <= main_slave_p1_reset_n;
		end
		else
			main_master_p1_reset_n <= main_csr_dfi_p1_reset_n;
	end
	always @(*) begin
		main_master_p1_act_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_act_n <= main_ext_dfi_p1_act_n;
			else
				main_master_p1_act_n <= main_slave_p1_act_n;
		end
		else
			main_master_p1_act_n <= main_csr_dfi_p1_act_n;
	end
	always @(*) begin
		main_master_p1_wrdata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_wrdata <= main_ext_dfi_p1_wrdata;
			else
				main_master_p1_wrdata <= main_slave_p1_wrdata;
		end
		else
			main_master_p1_wrdata <= main_csr_dfi_p1_wrdata;
	end
	always @(*) begin
		main_master_p1_wrdata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_wrdata_en <= main_ext_dfi_p1_wrdata_en;
			else
				main_master_p1_wrdata_en <= main_slave_p1_wrdata_en;
		end
		else
			main_master_p1_wrdata_en <= main_csr_dfi_p1_wrdata_en;
	end
	always @(*) begin
		main_master_p1_wrdata_mask <= 4'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_wrdata_mask <= main_ext_dfi_p1_wrdata_mask;
			else
				main_master_p1_wrdata_mask <= main_slave_p1_wrdata_mask;
		end
		else
			main_master_p1_wrdata_mask <= main_csr_dfi_p1_wrdata_mask;
	end
	always @(*) begin
		main_master_p1_rddata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p1_rddata_en <= main_ext_dfi_p1_rddata_en;
			else
				main_master_p1_rddata_en <= main_slave_p1_rddata_en;
		end
		else
			main_master_p1_rddata_en <= main_csr_dfi_p1_rddata_en;
	end
	always @(*) begin
		main_master_p2_address <= 14'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_address <= main_ext_dfi_p2_address;
			else
				main_master_p2_address <= main_slave_p2_address;
		end
		else
			main_master_p2_address <= main_csr_dfi_p2_address;
	end
	always @(*) begin
		main_master_p2_bank <= 3'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_bank <= main_ext_dfi_p2_bank;
			else
				main_master_p2_bank <= main_slave_p2_bank;
		end
		else
			main_master_p2_bank <= main_csr_dfi_p2_bank;
	end
	always @(*) begin
		main_master_p2_cas_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_cas_n <= main_ext_dfi_p2_cas_n;
			else
				main_master_p2_cas_n <= main_slave_p2_cas_n;
		end
		else
			main_master_p2_cas_n <= main_csr_dfi_p2_cas_n;
	end
	always @(*) begin
		main_master_p2_cs_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_cs_n <= main_ext_dfi_p2_cs_n;
			else
				main_master_p2_cs_n <= main_slave_p2_cs_n;
		end
		else
			main_master_p2_cs_n <= main_csr_dfi_p2_cs_n;
	end
	always @(*) begin
		main_master_p2_ras_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_ras_n <= main_ext_dfi_p2_ras_n;
			else
				main_master_p2_ras_n <= main_slave_p2_ras_n;
		end
		else
			main_master_p2_ras_n <= main_csr_dfi_p2_ras_n;
	end
	always @(*) begin
		main_master_p2_we_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_we_n <= main_ext_dfi_p2_we_n;
			else
				main_master_p2_we_n <= main_slave_p2_we_n;
		end
		else
			main_master_p2_we_n <= main_csr_dfi_p2_we_n;
	end
	always @(*) begin
		main_master_p2_cke <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_cke <= main_ext_dfi_p2_cke;
			else
				main_master_p2_cke <= main_slave_p2_cke;
		end
		else
			main_master_p2_cke <= main_csr_dfi_p2_cke;
	end
	always @(*) begin
		main_master_p2_odt <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_odt <= main_ext_dfi_p2_odt;
			else
				main_master_p2_odt <= main_slave_p2_odt;
		end
		else
			main_master_p2_odt <= main_csr_dfi_p2_odt;
	end
	always @(*) begin
		main_master_p2_reset_n <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_reset_n <= main_ext_dfi_p2_reset_n;
			else
				main_master_p2_reset_n <= main_slave_p2_reset_n;
		end
		else
			main_master_p2_reset_n <= main_csr_dfi_p2_reset_n;
	end
	always @(*) begin
		main_master_p2_act_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_act_n <= main_ext_dfi_p2_act_n;
			else
				main_master_p2_act_n <= main_slave_p2_act_n;
		end
		else
			main_master_p2_act_n <= main_csr_dfi_p2_act_n;
	end
	always @(*) begin
		main_master_p2_wrdata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_wrdata <= main_ext_dfi_p2_wrdata;
			else
				main_master_p2_wrdata <= main_slave_p2_wrdata;
		end
		else
			main_master_p2_wrdata <= main_csr_dfi_p2_wrdata;
	end
	always @(*) begin
		main_master_p2_wrdata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_wrdata_en <= main_ext_dfi_p2_wrdata_en;
			else
				main_master_p2_wrdata_en <= main_slave_p2_wrdata_en;
		end
		else
			main_master_p2_wrdata_en <= main_csr_dfi_p2_wrdata_en;
	end
	always @(*) begin
		main_master_p2_wrdata_mask <= 4'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_wrdata_mask <= main_ext_dfi_p2_wrdata_mask;
			else
				main_master_p2_wrdata_mask <= main_slave_p2_wrdata_mask;
		end
		else
			main_master_p2_wrdata_mask <= main_csr_dfi_p2_wrdata_mask;
	end
	always @(*) begin
		main_master_p2_rddata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p2_rddata_en <= main_ext_dfi_p2_rddata_en;
			else
				main_master_p2_rddata_en <= main_slave_p2_rddata_en;
		end
		else
			main_master_p2_rddata_en <= main_csr_dfi_p2_rddata_en;
	end
	always @(*) begin
		main_master_p3_address <= 14'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_address <= main_ext_dfi_p3_address;
			else
				main_master_p3_address <= main_slave_p3_address;
		end
		else
			main_master_p3_address <= main_csr_dfi_p3_address;
	end
	always @(*) begin
		main_master_p3_bank <= 3'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_bank <= main_ext_dfi_p3_bank;
			else
				main_master_p3_bank <= main_slave_p3_bank;
		end
		else
			main_master_p3_bank <= main_csr_dfi_p3_bank;
	end
	always @(*) begin
		main_master_p3_cas_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_cas_n <= main_ext_dfi_p3_cas_n;
			else
				main_master_p3_cas_n <= main_slave_p3_cas_n;
		end
		else
			main_master_p3_cas_n <= main_csr_dfi_p3_cas_n;
	end
	always @(*) begin
		main_master_p3_cs_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_cs_n <= main_ext_dfi_p3_cs_n;
			else
				main_master_p3_cs_n <= main_slave_p3_cs_n;
		end
		else
			main_master_p3_cs_n <= main_csr_dfi_p3_cs_n;
	end
	always @(*) begin
		main_master_p3_ras_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_ras_n <= main_ext_dfi_p3_ras_n;
			else
				main_master_p3_ras_n <= main_slave_p3_ras_n;
		end
		else
			main_master_p3_ras_n <= main_csr_dfi_p3_ras_n;
	end
	always @(*) begin
		main_master_p3_we_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_we_n <= main_ext_dfi_p3_we_n;
			else
				main_master_p3_we_n <= main_slave_p3_we_n;
		end
		else
			main_master_p3_we_n <= main_csr_dfi_p3_we_n;
	end
	always @(*) begin
		main_master_p3_cke <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_cke <= main_ext_dfi_p3_cke;
			else
				main_master_p3_cke <= main_slave_p3_cke;
		end
		else
			main_master_p3_cke <= main_csr_dfi_p3_cke;
	end
	always @(*) begin
		main_master_p3_odt <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_odt <= main_ext_dfi_p3_odt;
			else
				main_master_p3_odt <= main_slave_p3_odt;
		end
		else
			main_master_p3_odt <= main_csr_dfi_p3_odt;
	end
	always @(*) begin
		main_master_p3_reset_n <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_reset_n <= main_ext_dfi_p3_reset_n;
			else
				main_master_p3_reset_n <= main_slave_p3_reset_n;
		end
		else
			main_master_p3_reset_n <= main_csr_dfi_p3_reset_n;
	end
	always @(*) begin
		main_master_p3_act_n <= 1'd1;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_act_n <= main_ext_dfi_p3_act_n;
			else
				main_master_p3_act_n <= main_slave_p3_act_n;
		end
		else
			main_master_p3_act_n <= main_csr_dfi_p3_act_n;
	end
	always @(*) begin
		main_master_p3_wrdata <= 32'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_wrdata <= main_ext_dfi_p3_wrdata;
			else
				main_master_p3_wrdata <= main_slave_p3_wrdata;
		end
		else
			main_master_p3_wrdata <= main_csr_dfi_p3_wrdata;
	end
	always @(*) begin
		main_master_p3_wrdata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_wrdata_en <= main_ext_dfi_p3_wrdata_en;
			else
				main_master_p3_wrdata_en <= main_slave_p3_wrdata_en;
		end
		else
			main_master_p3_wrdata_en <= main_csr_dfi_p3_wrdata_en;
	end
	always @(*) begin
		main_master_p3_wrdata_mask <= 4'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_wrdata_mask <= main_ext_dfi_p3_wrdata_mask;
			else
				main_master_p3_wrdata_mask <= main_slave_p3_wrdata_mask;
		end
		else
			main_master_p3_wrdata_mask <= main_csr_dfi_p3_wrdata_mask;
	end
	always @(*) begin
		main_master_p3_rddata_en <= 1'd0;
		if (main_sel) begin
			if (main_ext_dfi_sel)
				main_master_p3_rddata_en <= main_ext_dfi_p3_rddata_en;
			else
				main_master_p3_rddata_en <= main_slave_p3_rddata_en;
		end
		else
			main_master_p3_rddata_en <= main_csr_dfi_p3_rddata_en;
	end
	always @(*) begin
		main_csr_dfi_p0_cke <= 1'd0;
		main_csr_dfi_p0_cke <= main_cke;
	end
	always @(*) begin
		main_csr_dfi_p1_cke <= 1'd0;
		main_csr_dfi_p1_cke <= main_cke;
	end
	always @(*) begin
		main_csr_dfi_p2_cke <= 1'd0;
		main_csr_dfi_p2_cke <= main_cke;
	end
	always @(*) begin
		main_csr_dfi_p3_cke <= 1'd0;
		main_csr_dfi_p3_cke <= main_cke;
	end
	always @(*) begin
		main_csr_dfi_p0_odt <= 1'd0;
		main_csr_dfi_p0_odt <= main_odt;
	end
	always @(*) begin
		main_csr_dfi_p1_odt <= 1'd0;
		main_csr_dfi_p1_odt <= main_odt;
	end
	always @(*) begin
		main_csr_dfi_p2_odt <= 1'd0;
		main_csr_dfi_p2_odt <= main_odt;
	end
	always @(*) begin
		main_csr_dfi_p3_odt <= 1'd0;
		main_csr_dfi_p3_odt <= main_odt;
	end
	assign main_csr_dfi_p0_reset_n = main_reset_n;
	assign main_csr_dfi_p1_reset_n = main_reset_n;
	assign main_csr_dfi_p2_reset_n = main_reset_n;
	assign main_csr_dfi_p3_reset_n = main_reset_n;
	always @(*) begin
		main_csr_dfi_p0_we_n <= 1'd1;
		if (main_phaseinjector0_command_issue_re)
			main_csr_dfi_p0_we_n <= ~main_phaseinjector0_csrfield_we;
		else
			main_csr_dfi_p0_we_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p0_cas_n <= 1'd1;
		if (main_phaseinjector0_command_issue_re)
			main_csr_dfi_p0_cas_n <= ~main_phaseinjector0_csrfield_cas;
		else
			main_csr_dfi_p0_cas_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p0_cs_n <= 1'd1;
		if (main_phaseinjector0_command_issue_re) begin
			if (main_phaseinjector0_csrfield_cs_top)
				main_csr_dfi_p0_cs_n <= 2'd2;
			else if (main_phaseinjector0_csrfield_cs_bottom)
				main_csr_dfi_p0_cs_n <= 1'd1;
			else
				main_csr_dfi_p0_cs_n <= {~main_phaseinjector0_csrfield_cs};
		end
		else
			main_csr_dfi_p0_cs_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p0_ras_n <= 1'd1;
		if (main_phaseinjector0_command_issue_re)
			main_csr_dfi_p0_ras_n <= ~main_phaseinjector0_csrfield_ras;
		else
			main_csr_dfi_p0_ras_n <= 1'd1;
	end
	assign main_csr_dfi_p0_address = main_phaseinjector0_address_storage;
	assign main_csr_dfi_p0_bank = main_phaseinjector0_baddress_storage;
	assign main_csr_dfi_p0_wrdata_en = main_phaseinjector0_command_issue_re & main_phaseinjector0_csrfield_wren;
	assign main_csr_dfi_p0_rddata_en = main_phaseinjector0_command_issue_re & main_phaseinjector0_csrfield_rden;
	assign main_csr_dfi_p0_wrdata = main_phaseinjector0_wrdata_storage;
	assign main_csr_dfi_p0_wrdata_mask = 1'd0;
	always @(*) begin
		main_csr_dfi_p1_we_n <= 1'd1;
		if (main_phaseinjector1_command_issue_re)
			main_csr_dfi_p1_we_n <= ~main_phaseinjector1_csrfield_we;
		else
			main_csr_dfi_p1_we_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p1_cas_n <= 1'd1;
		if (main_phaseinjector1_command_issue_re)
			main_csr_dfi_p1_cas_n <= ~main_phaseinjector1_csrfield_cas;
		else
			main_csr_dfi_p1_cas_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p1_cs_n <= 1'd1;
		if (main_phaseinjector1_command_issue_re) begin
			if (main_phaseinjector1_csrfield_cs_top)
				main_csr_dfi_p1_cs_n <= 2'd2;
			else if (main_phaseinjector1_csrfield_cs_bottom)
				main_csr_dfi_p1_cs_n <= 1'd1;
			else
				main_csr_dfi_p1_cs_n <= {~main_phaseinjector1_csrfield_cs};
		end
		else
			main_csr_dfi_p1_cs_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p1_ras_n <= 1'd1;
		if (main_phaseinjector1_command_issue_re)
			main_csr_dfi_p1_ras_n <= ~main_phaseinjector1_csrfield_ras;
		else
			main_csr_dfi_p1_ras_n <= 1'd1;
	end
	assign main_csr_dfi_p1_address = main_phaseinjector1_address_storage;
	assign main_csr_dfi_p1_bank = main_phaseinjector1_baddress_storage;
	assign main_csr_dfi_p1_wrdata_en = main_phaseinjector1_command_issue_re & main_phaseinjector1_csrfield_wren;
	assign main_csr_dfi_p1_rddata_en = main_phaseinjector1_command_issue_re & main_phaseinjector1_csrfield_rden;
	assign main_csr_dfi_p1_wrdata = main_phaseinjector1_wrdata_storage;
	assign main_csr_dfi_p1_wrdata_mask = 1'd0;
	always @(*) begin
		main_csr_dfi_p2_we_n <= 1'd1;
		if (main_phaseinjector2_command_issue_re)
			main_csr_dfi_p2_we_n <= ~main_phaseinjector2_csrfield_we;
		else
			main_csr_dfi_p2_we_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p2_cas_n <= 1'd1;
		if (main_phaseinjector2_command_issue_re)
			main_csr_dfi_p2_cas_n <= ~main_phaseinjector2_csrfield_cas;
		else
			main_csr_dfi_p2_cas_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p2_cs_n <= 1'd1;
		if (main_phaseinjector2_command_issue_re) begin
			if (main_phaseinjector2_csrfield_cs_top)
				main_csr_dfi_p2_cs_n <= 2'd2;
			else if (main_phaseinjector2_csrfield_cs_bottom)
				main_csr_dfi_p2_cs_n <= 1'd1;
			else
				main_csr_dfi_p2_cs_n <= {~main_phaseinjector2_csrfield_cs};
		end
		else
			main_csr_dfi_p2_cs_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p2_ras_n <= 1'd1;
		if (main_phaseinjector2_command_issue_re)
			main_csr_dfi_p2_ras_n <= ~main_phaseinjector2_csrfield_ras;
		else
			main_csr_dfi_p2_ras_n <= 1'd1;
	end
	assign main_csr_dfi_p2_address = main_phaseinjector2_address_storage;
	assign main_csr_dfi_p2_bank = main_phaseinjector2_baddress_storage;
	assign main_csr_dfi_p2_wrdata_en = main_phaseinjector2_command_issue_re & main_phaseinjector2_csrfield_wren;
	assign main_csr_dfi_p2_rddata_en = main_phaseinjector2_command_issue_re & main_phaseinjector2_csrfield_rden;
	assign main_csr_dfi_p2_wrdata = main_phaseinjector2_wrdata_storage;
	assign main_csr_dfi_p2_wrdata_mask = 1'd0;
	always @(*) begin
		main_csr_dfi_p3_we_n <= 1'd1;
		if (main_phaseinjector3_command_issue_re)
			main_csr_dfi_p3_we_n <= ~main_phaseinjector3_csrfield_we;
		else
			main_csr_dfi_p3_we_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p3_cas_n <= 1'd1;
		if (main_phaseinjector3_command_issue_re)
			main_csr_dfi_p3_cas_n <= ~main_phaseinjector3_csrfield_cas;
		else
			main_csr_dfi_p3_cas_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p3_cs_n <= 1'd1;
		if (main_phaseinjector3_command_issue_re) begin
			if (main_phaseinjector3_csrfield_cs_top)
				main_csr_dfi_p3_cs_n <= 2'd2;
			else if (main_phaseinjector3_csrfield_cs_bottom)
				main_csr_dfi_p3_cs_n <= 1'd1;
			else
				main_csr_dfi_p3_cs_n <= {~main_phaseinjector3_csrfield_cs};
		end
		else
			main_csr_dfi_p3_cs_n <= 1'd1;
	end
	always @(*) begin
		main_csr_dfi_p3_ras_n <= 1'd1;
		if (main_phaseinjector3_command_issue_re)
			main_csr_dfi_p3_ras_n <= ~main_phaseinjector3_csrfield_ras;
		else
			main_csr_dfi_p3_ras_n <= 1'd1;
	end
	assign main_csr_dfi_p3_address = main_phaseinjector3_address_storage;
	assign main_csr_dfi_p3_bank = main_phaseinjector3_baddress_storage;
	assign main_csr_dfi_p3_wrdata_en = main_phaseinjector3_command_issue_re & main_phaseinjector3_csrfield_wren;
	assign main_csr_dfi_p3_rddata_en = main_phaseinjector3_command_issue_re & main_phaseinjector3_csrfield_rden;
	assign main_csr_dfi_p3_wrdata = main_phaseinjector3_wrdata_storage;
	assign main_csr_dfi_p3_wrdata_mask = 1'd0;
	assign main_bankmachine0_req_valid = main_interface_bank0_valid;
	assign main_interface_bank0_ready = main_bankmachine0_req_ready;
	assign main_bankmachine0_req_we = main_interface_bank0_we;
	assign main_bankmachine0_req_addr = main_interface_bank0_addr;
	assign main_interface_bank0_lock = main_bankmachine0_req_lock;
	assign main_interface_bank0_wdata_ready = main_bankmachine0_req_wdata_ready;
	assign main_interface_bank0_rdata_valid = main_bankmachine0_req_rdata_valid;
	assign main_bankmachine1_req_valid = main_interface_bank1_valid;
	assign main_interface_bank1_ready = main_bankmachine1_req_ready;
	assign main_bankmachine1_req_we = main_interface_bank1_we;
	assign main_bankmachine1_req_addr = main_interface_bank1_addr;
	assign main_interface_bank1_lock = main_bankmachine1_req_lock;
	assign main_interface_bank1_wdata_ready = main_bankmachine1_req_wdata_ready;
	assign main_interface_bank1_rdata_valid = main_bankmachine1_req_rdata_valid;
	assign main_bankmachine2_req_valid = main_interface_bank2_valid;
	assign main_interface_bank2_ready = main_bankmachine2_req_ready;
	assign main_bankmachine2_req_we = main_interface_bank2_we;
	assign main_bankmachine2_req_addr = main_interface_bank2_addr;
	assign main_interface_bank2_lock = main_bankmachine2_req_lock;
	assign main_interface_bank2_wdata_ready = main_bankmachine2_req_wdata_ready;
	assign main_interface_bank2_rdata_valid = main_bankmachine2_req_rdata_valid;
	assign main_bankmachine3_req_valid = main_interface_bank3_valid;
	assign main_interface_bank3_ready = main_bankmachine3_req_ready;
	assign main_bankmachine3_req_we = main_interface_bank3_we;
	assign main_bankmachine3_req_addr = main_interface_bank3_addr;
	assign main_interface_bank3_lock = main_bankmachine3_req_lock;
	assign main_interface_bank3_wdata_ready = main_bankmachine3_req_wdata_ready;
	assign main_interface_bank3_rdata_valid = main_bankmachine3_req_rdata_valid;
	assign main_bankmachine4_req_valid = main_interface_bank4_valid;
	assign main_interface_bank4_ready = main_bankmachine4_req_ready;
	assign main_bankmachine4_req_we = main_interface_bank4_we;
	assign main_bankmachine4_req_addr = main_interface_bank4_addr;
	assign main_interface_bank4_lock = main_bankmachine4_req_lock;
	assign main_interface_bank4_wdata_ready = main_bankmachine4_req_wdata_ready;
	assign main_interface_bank4_rdata_valid = main_bankmachine4_req_rdata_valid;
	assign main_bankmachine5_req_valid = main_interface_bank5_valid;
	assign main_interface_bank5_ready = main_bankmachine5_req_ready;
	assign main_bankmachine5_req_we = main_interface_bank5_we;
	assign main_bankmachine5_req_addr = main_interface_bank5_addr;
	assign main_interface_bank5_lock = main_bankmachine5_req_lock;
	assign main_interface_bank5_wdata_ready = main_bankmachine5_req_wdata_ready;
	assign main_interface_bank5_rdata_valid = main_bankmachine5_req_rdata_valid;
	assign main_bankmachine6_req_valid = main_interface_bank6_valid;
	assign main_interface_bank6_ready = main_bankmachine6_req_ready;
	assign main_bankmachine6_req_we = main_interface_bank6_we;
	assign main_bankmachine6_req_addr = main_interface_bank6_addr;
	assign main_interface_bank6_lock = main_bankmachine6_req_lock;
	assign main_interface_bank6_wdata_ready = main_bankmachine6_req_wdata_ready;
	assign main_interface_bank6_rdata_valid = main_bankmachine6_req_rdata_valid;
	assign main_bankmachine7_req_valid = main_interface_bank7_valid;
	assign main_interface_bank7_ready = main_bankmachine7_req_ready;
	assign main_bankmachine7_req_we = main_interface_bank7_we;
	assign main_bankmachine7_req_addr = main_interface_bank7_addr;
	assign main_interface_bank7_lock = main_bankmachine7_req_lock;
	assign main_interface_bank7_wdata_ready = main_bankmachine7_req_wdata_ready;
	assign main_interface_bank7_rdata_valid = main_bankmachine7_req_rdata_valid;
	assign main_timer_wait = ~main_timer_done0;
	assign main_postponer_req_i = main_timer_done0;
	assign main_wants_refresh = main_postponer_req_o;
	assign main_wants_zqcs = main_zqcs_timer_done0;
	assign main_zqcs_timer_wait = ~main_zqcs_executer_done;
	assign main_timer_done1 = main_timer_count1 == 1'd0;
	assign main_timer_done0 = main_timer_done1;
	assign main_timer_count0 = main_timer_count1;
	assign main_sequencer_start1 = main_sequencer_start0 | (main_sequencer_count != 1'd0);
	assign main_sequencer_done0 = main_sequencer_done1 & (main_sequencer_count == 1'd0);
	assign main_zqcs_timer_done1 = main_zqcs_timer_count1 == 1'd0;
	assign main_zqcs_timer_done0 = main_zqcs_timer_done1;
	assign main_zqcs_timer_count0 = main_zqcs_timer_count1;
	always @(*) begin
		builder_litedramcore_refresher_next_state <= 2'd0;
		builder_litedramcore_refresher_next_state <= builder_litedramcore_refresher_state;
		case (builder_litedramcore_refresher_state)
			1'd1:
				if (main_cmd_ready)
					builder_litedramcore_refresher_next_state <= 2'd2;
			2'd2:
				if (main_sequencer_done0) begin
					if (main_wants_zqcs)
						builder_litedramcore_refresher_next_state <= 2'd3;
					else
						builder_litedramcore_refresher_next_state <= 1'd0;
				end
			2'd3:
				if (main_zqcs_executer_done)
					builder_litedramcore_refresher_next_state <= 1'd0;
			default:
				if (main_wants_refresh)
					builder_litedramcore_refresher_next_state <= 1'd1;
		endcase
	end
	always @(*) begin
		main_sequencer_start0 <= 1'd0;
		case (builder_litedramcore_refresher_state)
			1'd1:
				if (main_cmd_ready)
					main_sequencer_start0 <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_cmd_valid <= 1'd0;
		case (builder_litedramcore_refresher_state)
			1'd1: main_cmd_valid <= 1'd1;
			2'd2: begin
				main_cmd_valid <= 1'd1;
				if (main_sequencer_done0) begin
					if (main_wants_zqcs)
						;
					else
						main_cmd_valid <= 1'd0;
				end
			end
			2'd3: begin
				main_cmd_valid <= 1'd1;
				if (main_zqcs_executer_done)
					main_cmd_valid <= 1'd0;
			end
			default:
				;
		endcase
	end
	always @(*) begin
		main_zqcs_executer_start <= 1'd0;
		case (builder_litedramcore_refresher_state)
			1'd1:
				;
			2'd2:
				if (main_sequencer_done0) begin
					if (main_wants_zqcs)
						main_zqcs_executer_start <= 1'd1;
				end
			2'd3:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_cmd_last <= 1'd0;
		case (builder_litedramcore_refresher_state)
			1'd1:
				;
			2'd2:
				if (main_sequencer_done0) begin
					if (main_wants_zqcs)
						;
					else
						main_cmd_last <= 1'd1;
				end
			2'd3:
				if (main_zqcs_executer_done)
					main_cmd_last <= 1'd1;
			default:
				;
		endcase
	end
	assign main_bankmachine0_sink_valid = main_bankmachine0_req_valid;
	assign main_bankmachine0_req_ready = main_bankmachine0_sink_ready;
	assign main_bankmachine0_sink_payload_we = main_bankmachine0_req_we;
	assign main_bankmachine0_sink_payload_addr = main_bankmachine0_req_addr;
	assign main_bankmachine0_sink_sink_valid = main_bankmachine0_source_valid;
	assign main_bankmachine0_source_ready = main_bankmachine0_sink_sink_ready;
	assign main_bankmachine0_sink_sink_first = main_bankmachine0_source_first;
	assign main_bankmachine0_sink_sink_last = main_bankmachine0_source_last;
	assign main_bankmachine0_sink_sink_payload_we = main_bankmachine0_source_payload_we;
	assign main_bankmachine0_sink_sink_payload_addr = main_bankmachine0_source_payload_addr;
	assign main_bankmachine0_source_source_ready = main_bankmachine0_req_wdata_ready | main_bankmachine0_req_rdata_valid;
	assign main_bankmachine0_req_lock = main_bankmachine0_source_valid | main_bankmachine0_source_source_valid;
	assign main_bankmachine0_row_hit = main_bankmachine0_row == main_bankmachine0_source_source_payload_addr[20:7];
	assign main_bankmachine0_cmd_payload_ba = 1'd0;
	always @(*) begin
		main_bankmachine0_cmd_payload_a <= 14'd0;
		if (main_bankmachine0_row_col_n_addr_sel)
			main_bankmachine0_cmd_payload_a <= main_bankmachine0_source_source_payload_addr[20:7];
		else
			main_bankmachine0_cmd_payload_a <= (main_bankmachine0_auto_precharge <<< 4'd10) | {main_bankmachine0_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine0_twtpcon_valid = (main_bankmachine0_cmd_valid & main_bankmachine0_cmd_ready) & main_bankmachine0_cmd_payload_is_write;
	assign main_bankmachine0_trccon_valid = (main_bankmachine0_cmd_valid & main_bankmachine0_cmd_ready) & main_bankmachine0_row_open;
	assign main_bankmachine0_trascon_valid = (main_bankmachine0_cmd_valid & main_bankmachine0_cmd_ready) & main_bankmachine0_row_open;
	always @(*) begin
		main_bankmachine0_auto_precharge <= 1'd0;
		if (main_bankmachine0_source_valid & main_bankmachine0_source_source_valid) begin
			if (main_bankmachine0_source_payload_addr[20:7] != main_bankmachine0_source_source_payload_addr[20:7])
				main_bankmachine0_auto_precharge <= main_bankmachine0_row_close == 1'd0;
		end
	end
	assign main_bankmachine0_syncfifo0_din = {main_bankmachine0_fifo_in_last, main_bankmachine0_fifo_in_first, main_bankmachine0_fifo_in_payload_addr, main_bankmachine0_fifo_in_payload_we};
	assign {main_bankmachine0_fifo_out_last, main_bankmachine0_fifo_out_first, main_bankmachine0_fifo_out_payload_addr, main_bankmachine0_fifo_out_payload_we} = main_bankmachine0_syncfifo0_dout;
	assign {main_bankmachine0_fifo_out_last, main_bankmachine0_fifo_out_first, main_bankmachine0_fifo_out_payload_addr, main_bankmachine0_fifo_out_payload_we} = main_bankmachine0_syncfifo0_dout;
	assign {main_bankmachine0_fifo_out_last, main_bankmachine0_fifo_out_first, main_bankmachine0_fifo_out_payload_addr, main_bankmachine0_fifo_out_payload_we} = main_bankmachine0_syncfifo0_dout;
	assign {main_bankmachine0_fifo_out_last, main_bankmachine0_fifo_out_first, main_bankmachine0_fifo_out_payload_addr, main_bankmachine0_fifo_out_payload_we} = main_bankmachine0_syncfifo0_dout;
	assign main_bankmachine0_sink_ready = main_bankmachine0_syncfifo0_writable;
	assign main_bankmachine0_syncfifo0_we = main_bankmachine0_sink_valid;
	assign main_bankmachine0_fifo_in_first = main_bankmachine0_sink_first;
	assign main_bankmachine0_fifo_in_last = main_bankmachine0_sink_last;
	assign main_bankmachine0_fifo_in_payload_we = main_bankmachine0_sink_payload_we;
	assign main_bankmachine0_fifo_in_payload_addr = main_bankmachine0_sink_payload_addr;
	assign main_bankmachine0_source_valid = main_bankmachine0_syncfifo0_readable;
	assign main_bankmachine0_source_first = main_bankmachine0_fifo_out_first;
	assign main_bankmachine0_source_last = main_bankmachine0_fifo_out_last;
	assign main_bankmachine0_source_payload_we = main_bankmachine0_fifo_out_payload_we;
	assign main_bankmachine0_source_payload_addr = main_bankmachine0_fifo_out_payload_addr;
	assign main_bankmachine0_syncfifo0_re = main_bankmachine0_source_ready;
	always @(*) begin
		main_bankmachine0_wrport_adr <= 4'd0;
		if (main_bankmachine0_replace)
			main_bankmachine0_wrport_adr <= main_bankmachine0_produce - 1'd1;
		else
			main_bankmachine0_wrport_adr <= main_bankmachine0_produce;
	end
	assign main_bankmachine0_wrport_dat_w = main_bankmachine0_syncfifo0_din;
	assign main_bankmachine0_wrport_we = main_bankmachine0_syncfifo0_we & (main_bankmachine0_syncfifo0_writable | main_bankmachine0_replace);
	assign main_bankmachine0_do_read = main_bankmachine0_syncfifo0_readable & main_bankmachine0_syncfifo0_re;
	assign main_bankmachine0_rdport_adr = main_bankmachine0_consume;
	assign main_bankmachine0_syncfifo0_dout = main_bankmachine0_rdport_dat_r;
	assign main_bankmachine0_syncfifo0_writable = main_bankmachine0_level != 5'd16;
	assign main_bankmachine0_syncfifo0_readable = main_bankmachine0_level != 1'd0;
	assign main_bankmachine0_pipe_valid_sink_ready = ~main_bankmachine0_pipe_valid_source_valid | main_bankmachine0_pipe_valid_source_ready;
	assign main_bankmachine0_pipe_valid_sink_valid = main_bankmachine0_sink_sink_valid;
	assign main_bankmachine0_sink_sink_ready = main_bankmachine0_pipe_valid_sink_ready;
	assign main_bankmachine0_pipe_valid_sink_first = main_bankmachine0_sink_sink_first;
	assign main_bankmachine0_pipe_valid_sink_last = main_bankmachine0_sink_sink_last;
	assign main_bankmachine0_pipe_valid_sink_payload_we = main_bankmachine0_sink_sink_payload_we;
	assign main_bankmachine0_pipe_valid_sink_payload_addr = main_bankmachine0_sink_sink_payload_addr;
	assign main_bankmachine0_source_source_valid = main_bankmachine0_pipe_valid_source_valid;
	assign main_bankmachine0_pipe_valid_source_ready = main_bankmachine0_source_source_ready;
	assign main_bankmachine0_source_source_first = main_bankmachine0_pipe_valid_source_first;
	assign main_bankmachine0_source_source_last = main_bankmachine0_pipe_valid_source_last;
	assign main_bankmachine0_source_source_payload_we = main_bankmachine0_pipe_valid_source_payload_we;
	assign main_bankmachine0_source_source_payload_addr = main_bankmachine0_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine0_next_state <= 3'd0;
		builder_litedramcore_bankmachine0_next_state <= builder_litedramcore_bankmachine0_state;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				if (main_bankmachine0_twtpcon_ready & main_bankmachine0_trascon_ready) begin
					if (main_bankmachine0_cmd_ready)
						builder_litedramcore_bankmachine0_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine0_twtpcon_ready & main_bankmachine0_trascon_ready)
					builder_litedramcore_bankmachine0_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine0_trccon_ready) begin
					if (main_bankmachine0_cmd_ready)
						builder_litedramcore_bankmachine0_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine0_refresh_req)
					builder_litedramcore_bankmachine0_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine0_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine0_next_state <= 1'd0;
			default:
				if (main_bankmachine0_refresh_req)
					builder_litedramcore_bankmachine0_next_state <= 3'd4;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit) begin
							if (main_bankmachine0_cmd_ready & main_bankmachine0_auto_precharge)
								builder_litedramcore_bankmachine0_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine0_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine0_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine0_refresh_req)
					;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit)
							main_bankmachine0_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				if (main_bankmachine0_twtpcon_ready & main_bankmachine0_trascon_ready)
					main_bankmachine0_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine0_trccon_ready)
					main_bankmachine0_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine0_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				if (main_bankmachine0_twtpcon_ready & main_bankmachine0_trascon_ready)
					main_bankmachine0_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine0_refresh_req)
					;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit) begin
							if (main_bankmachine0_source_source_payload_we)
								main_bankmachine0_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				if (main_bankmachine0_twtpcon_ready & main_bankmachine0_trascon_ready)
					main_bankmachine0_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine0_trccon_ready)
					main_bankmachine0_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine0_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine0_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine0_refresh_req)
					;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit) begin
							if (main_bankmachine0_source_source_payload_we)
								;
							else
								main_bankmachine0_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine0_refresh_req)
					;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit) begin
							if (main_bankmachine0_source_source_payload_we)
								main_bankmachine0_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine0_refresh_req)
					;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit) begin
							if (main_bankmachine0_source_source_payload_we)
								main_bankmachine0_req_wdata_ready <= main_bankmachine0_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine0_refresh_req)
					;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit) begin
							if (main_bankmachine0_source_source_payload_we)
								;
							else
								main_bankmachine0_req_rdata_valid <= main_bankmachine0_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine0_twtpcon_ready)
					main_bankmachine0_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine0_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine0_trccon_ready)
					main_bankmachine0_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine0_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				if (main_bankmachine0_twtpcon_ready & main_bankmachine0_trascon_ready)
					main_bankmachine0_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine0_trccon_ready)
					main_bankmachine0_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine0_refresh_req)
					;
				else if (main_bankmachine0_source_source_valid) begin
					if (main_bankmachine0_row_opened) begin
						if (main_bankmachine0_row_hit)
							main_bankmachine0_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine0_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1: main_bankmachine0_row_close <= 1'd1;
			2'd2: main_bankmachine0_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine0_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine0_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine0_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine0_trccon_ready)
					main_bankmachine0_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	assign main_bankmachine1_sink_valid = main_bankmachine1_req_valid;
	assign main_bankmachine1_req_ready = main_bankmachine1_sink_ready;
	assign main_bankmachine1_sink_payload_we = main_bankmachine1_req_we;
	assign main_bankmachine1_sink_payload_addr = main_bankmachine1_req_addr;
	assign main_bankmachine1_sink_sink_valid = main_bankmachine1_source_valid;
	assign main_bankmachine1_source_ready = main_bankmachine1_sink_sink_ready;
	assign main_bankmachine1_sink_sink_first = main_bankmachine1_source_first;
	assign main_bankmachine1_sink_sink_last = main_bankmachine1_source_last;
	assign main_bankmachine1_sink_sink_payload_we = main_bankmachine1_source_payload_we;
	assign main_bankmachine1_sink_sink_payload_addr = main_bankmachine1_source_payload_addr;
	assign main_bankmachine1_source_source_ready = main_bankmachine1_req_wdata_ready | main_bankmachine1_req_rdata_valid;
	assign main_bankmachine1_req_lock = main_bankmachine1_source_valid | main_bankmachine1_source_source_valid;
	assign main_bankmachine1_row_hit = main_bankmachine1_row == main_bankmachine1_source_source_payload_addr[20:7];
	assign main_bankmachine1_cmd_payload_ba = 1'd1;
	always @(*) begin
		main_bankmachine1_cmd_payload_a <= 14'd0;
		if (main_bankmachine1_row_col_n_addr_sel)
			main_bankmachine1_cmd_payload_a <= main_bankmachine1_source_source_payload_addr[20:7];
		else
			main_bankmachine1_cmd_payload_a <= (main_bankmachine1_auto_precharge <<< 4'd10) | {main_bankmachine1_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine1_twtpcon_valid = (main_bankmachine1_cmd_valid & main_bankmachine1_cmd_ready) & main_bankmachine1_cmd_payload_is_write;
	assign main_bankmachine1_trccon_valid = (main_bankmachine1_cmd_valid & main_bankmachine1_cmd_ready) & main_bankmachine1_row_open;
	assign main_bankmachine1_trascon_valid = (main_bankmachine1_cmd_valid & main_bankmachine1_cmd_ready) & main_bankmachine1_row_open;
	always @(*) begin
		main_bankmachine1_auto_precharge <= 1'd0;
		if (main_bankmachine1_source_valid & main_bankmachine1_source_source_valid) begin
			if (main_bankmachine1_source_payload_addr[20:7] != main_bankmachine1_source_source_payload_addr[20:7])
				main_bankmachine1_auto_precharge <= main_bankmachine1_row_close == 1'd0;
		end
	end
	assign main_bankmachine1_syncfifo1_din = {main_bankmachine1_fifo_in_last, main_bankmachine1_fifo_in_first, main_bankmachine1_fifo_in_payload_addr, main_bankmachine1_fifo_in_payload_we};
	assign {main_bankmachine1_fifo_out_last, main_bankmachine1_fifo_out_first, main_bankmachine1_fifo_out_payload_addr, main_bankmachine1_fifo_out_payload_we} = main_bankmachine1_syncfifo1_dout;
	assign {main_bankmachine1_fifo_out_last, main_bankmachine1_fifo_out_first, main_bankmachine1_fifo_out_payload_addr, main_bankmachine1_fifo_out_payload_we} = main_bankmachine1_syncfifo1_dout;
	assign {main_bankmachine1_fifo_out_last, main_bankmachine1_fifo_out_first, main_bankmachine1_fifo_out_payload_addr, main_bankmachine1_fifo_out_payload_we} = main_bankmachine1_syncfifo1_dout;
	assign {main_bankmachine1_fifo_out_last, main_bankmachine1_fifo_out_first, main_bankmachine1_fifo_out_payload_addr, main_bankmachine1_fifo_out_payload_we} = main_bankmachine1_syncfifo1_dout;
	assign main_bankmachine1_sink_ready = main_bankmachine1_syncfifo1_writable;
	assign main_bankmachine1_syncfifo1_we = main_bankmachine1_sink_valid;
	assign main_bankmachine1_fifo_in_first = main_bankmachine1_sink_first;
	assign main_bankmachine1_fifo_in_last = main_bankmachine1_sink_last;
	assign main_bankmachine1_fifo_in_payload_we = main_bankmachine1_sink_payload_we;
	assign main_bankmachine1_fifo_in_payload_addr = main_bankmachine1_sink_payload_addr;
	assign main_bankmachine1_source_valid = main_bankmachine1_syncfifo1_readable;
	assign main_bankmachine1_source_first = main_bankmachine1_fifo_out_first;
	assign main_bankmachine1_source_last = main_bankmachine1_fifo_out_last;
	assign main_bankmachine1_source_payload_we = main_bankmachine1_fifo_out_payload_we;
	assign main_bankmachine1_source_payload_addr = main_bankmachine1_fifo_out_payload_addr;
	assign main_bankmachine1_syncfifo1_re = main_bankmachine1_source_ready;
	always @(*) begin
		main_bankmachine1_wrport_adr <= 4'd0;
		if (main_bankmachine1_replace)
			main_bankmachine1_wrport_adr <= main_bankmachine1_produce - 1'd1;
		else
			main_bankmachine1_wrport_adr <= main_bankmachine1_produce;
	end
	assign main_bankmachine1_wrport_dat_w = main_bankmachine1_syncfifo1_din;
	assign main_bankmachine1_wrport_we = main_bankmachine1_syncfifo1_we & (main_bankmachine1_syncfifo1_writable | main_bankmachine1_replace);
	assign main_bankmachine1_do_read = main_bankmachine1_syncfifo1_readable & main_bankmachine1_syncfifo1_re;
	assign main_bankmachine1_rdport_adr = main_bankmachine1_consume;
	assign main_bankmachine1_syncfifo1_dout = main_bankmachine1_rdport_dat_r;
	assign main_bankmachine1_syncfifo1_writable = main_bankmachine1_level != 5'd16;
	assign main_bankmachine1_syncfifo1_readable = main_bankmachine1_level != 1'd0;
	assign main_bankmachine1_pipe_valid_sink_ready = ~main_bankmachine1_pipe_valid_source_valid | main_bankmachine1_pipe_valid_source_ready;
	assign main_bankmachine1_pipe_valid_sink_valid = main_bankmachine1_sink_sink_valid;
	assign main_bankmachine1_sink_sink_ready = main_bankmachine1_pipe_valid_sink_ready;
	assign main_bankmachine1_pipe_valid_sink_first = main_bankmachine1_sink_sink_first;
	assign main_bankmachine1_pipe_valid_sink_last = main_bankmachine1_sink_sink_last;
	assign main_bankmachine1_pipe_valid_sink_payload_we = main_bankmachine1_sink_sink_payload_we;
	assign main_bankmachine1_pipe_valid_sink_payload_addr = main_bankmachine1_sink_sink_payload_addr;
	assign main_bankmachine1_source_source_valid = main_bankmachine1_pipe_valid_source_valid;
	assign main_bankmachine1_pipe_valid_source_ready = main_bankmachine1_source_source_ready;
	assign main_bankmachine1_source_source_first = main_bankmachine1_pipe_valid_source_first;
	assign main_bankmachine1_source_source_last = main_bankmachine1_pipe_valid_source_last;
	assign main_bankmachine1_source_source_payload_we = main_bankmachine1_pipe_valid_source_payload_we;
	assign main_bankmachine1_source_source_payload_addr = main_bankmachine1_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine1_next_state <= 3'd0;
		builder_litedramcore_bankmachine1_next_state <= builder_litedramcore_bankmachine1_state;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				if (main_bankmachine1_twtpcon_ready & main_bankmachine1_trascon_ready) begin
					if (main_bankmachine1_cmd_ready)
						builder_litedramcore_bankmachine1_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine1_twtpcon_ready & main_bankmachine1_trascon_ready)
					builder_litedramcore_bankmachine1_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine1_trccon_ready) begin
					if (main_bankmachine1_cmd_ready)
						builder_litedramcore_bankmachine1_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine1_refresh_req)
					builder_litedramcore_bankmachine1_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine1_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine1_next_state <= 1'd0;
			default:
				if (main_bankmachine1_refresh_req)
					builder_litedramcore_bankmachine1_next_state <= 3'd4;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit) begin
							if (main_bankmachine1_cmd_ready & main_bankmachine1_auto_precharge)
								builder_litedramcore_bankmachine1_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine1_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine1_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine1_refresh_req)
					;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit) begin
							if (main_bankmachine1_source_source_payload_we)
								;
							else
								main_bankmachine1_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine1_refresh_req)
					;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit) begin
							if (main_bankmachine1_source_source_payload_we)
								main_bankmachine1_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine1_refresh_req)
					;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit) begin
							if (main_bankmachine1_source_source_payload_we)
								main_bankmachine1_req_wdata_ready <= main_bankmachine1_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine1_refresh_req)
					;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit) begin
							if (main_bankmachine1_source_source_payload_we)
								;
							else
								main_bankmachine1_req_rdata_valid <= main_bankmachine1_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine1_twtpcon_ready)
					main_bankmachine1_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine1_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine1_trccon_ready)
					main_bankmachine1_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine1_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				if (main_bankmachine1_twtpcon_ready & main_bankmachine1_trascon_ready)
					main_bankmachine1_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine1_trccon_ready)
					main_bankmachine1_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine1_refresh_req)
					;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit)
							main_bankmachine1_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1: main_bankmachine1_row_close <= 1'd1;
			2'd2: main_bankmachine1_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine1_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine1_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine1_trccon_ready)
					main_bankmachine1_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine1_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine1_refresh_req)
					;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit)
							main_bankmachine1_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				if (main_bankmachine1_twtpcon_ready & main_bankmachine1_trascon_ready)
					main_bankmachine1_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine1_trccon_ready)
					main_bankmachine1_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine1_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				if (main_bankmachine1_twtpcon_ready & main_bankmachine1_trascon_ready)
					main_bankmachine1_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine1_refresh_req)
					;
				else if (main_bankmachine1_source_source_valid) begin
					if (main_bankmachine1_row_opened) begin
						if (main_bankmachine1_row_hit) begin
							if (main_bankmachine1_source_source_payload_we)
								main_bankmachine1_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine1_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine1_state)
			1'd1:
				if (main_bankmachine1_twtpcon_ready & main_bankmachine1_trascon_ready)
					main_bankmachine1_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine1_trccon_ready)
					main_bankmachine1_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine1_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	assign main_bankmachine2_sink_valid = main_bankmachine2_req_valid;
	assign main_bankmachine2_req_ready = main_bankmachine2_sink_ready;
	assign main_bankmachine2_sink_payload_we = main_bankmachine2_req_we;
	assign main_bankmachine2_sink_payload_addr = main_bankmachine2_req_addr;
	assign main_bankmachine2_sink_sink_valid = main_bankmachine2_source_valid;
	assign main_bankmachine2_source_ready = main_bankmachine2_sink_sink_ready;
	assign main_bankmachine2_sink_sink_first = main_bankmachine2_source_first;
	assign main_bankmachine2_sink_sink_last = main_bankmachine2_source_last;
	assign main_bankmachine2_sink_sink_payload_we = main_bankmachine2_source_payload_we;
	assign main_bankmachine2_sink_sink_payload_addr = main_bankmachine2_source_payload_addr;
	assign main_bankmachine2_source_source_ready = main_bankmachine2_req_wdata_ready | main_bankmachine2_req_rdata_valid;
	assign main_bankmachine2_req_lock = main_bankmachine2_source_valid | main_bankmachine2_source_source_valid;
	assign main_bankmachine2_row_hit = main_bankmachine2_row == main_bankmachine2_source_source_payload_addr[20:7];
	assign main_bankmachine2_cmd_payload_ba = 2'd2;
	always @(*) begin
		main_bankmachine2_cmd_payload_a <= 14'd0;
		if (main_bankmachine2_row_col_n_addr_sel)
			main_bankmachine2_cmd_payload_a <= main_bankmachine2_source_source_payload_addr[20:7];
		else
			main_bankmachine2_cmd_payload_a <= (main_bankmachine2_auto_precharge <<< 4'd10) | {main_bankmachine2_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine2_twtpcon_valid = (main_bankmachine2_cmd_valid & main_bankmachine2_cmd_ready) & main_bankmachine2_cmd_payload_is_write;
	assign main_bankmachine2_trccon_valid = (main_bankmachine2_cmd_valid & main_bankmachine2_cmd_ready) & main_bankmachine2_row_open;
	assign main_bankmachine2_trascon_valid = (main_bankmachine2_cmd_valid & main_bankmachine2_cmd_ready) & main_bankmachine2_row_open;
	always @(*) begin
		main_bankmachine2_auto_precharge <= 1'd0;
		if (main_bankmachine2_source_valid & main_bankmachine2_source_source_valid) begin
			if (main_bankmachine2_source_payload_addr[20:7] != main_bankmachine2_source_source_payload_addr[20:7])
				main_bankmachine2_auto_precharge <= main_bankmachine2_row_close == 1'd0;
		end
	end
	assign main_bankmachine2_syncfifo2_din = {main_bankmachine2_fifo_in_last, main_bankmachine2_fifo_in_first, main_bankmachine2_fifo_in_payload_addr, main_bankmachine2_fifo_in_payload_we};
	assign {main_bankmachine2_fifo_out_last, main_bankmachine2_fifo_out_first, main_bankmachine2_fifo_out_payload_addr, main_bankmachine2_fifo_out_payload_we} = main_bankmachine2_syncfifo2_dout;
	assign {main_bankmachine2_fifo_out_last, main_bankmachine2_fifo_out_first, main_bankmachine2_fifo_out_payload_addr, main_bankmachine2_fifo_out_payload_we} = main_bankmachine2_syncfifo2_dout;
	assign {main_bankmachine2_fifo_out_last, main_bankmachine2_fifo_out_first, main_bankmachine2_fifo_out_payload_addr, main_bankmachine2_fifo_out_payload_we} = main_bankmachine2_syncfifo2_dout;
	assign {main_bankmachine2_fifo_out_last, main_bankmachine2_fifo_out_first, main_bankmachine2_fifo_out_payload_addr, main_bankmachine2_fifo_out_payload_we} = main_bankmachine2_syncfifo2_dout;
	assign main_bankmachine2_sink_ready = main_bankmachine2_syncfifo2_writable;
	assign main_bankmachine2_syncfifo2_we = main_bankmachine2_sink_valid;
	assign main_bankmachine2_fifo_in_first = main_bankmachine2_sink_first;
	assign main_bankmachine2_fifo_in_last = main_bankmachine2_sink_last;
	assign main_bankmachine2_fifo_in_payload_we = main_bankmachine2_sink_payload_we;
	assign main_bankmachine2_fifo_in_payload_addr = main_bankmachine2_sink_payload_addr;
	assign main_bankmachine2_source_valid = main_bankmachine2_syncfifo2_readable;
	assign main_bankmachine2_source_first = main_bankmachine2_fifo_out_first;
	assign main_bankmachine2_source_last = main_bankmachine2_fifo_out_last;
	assign main_bankmachine2_source_payload_we = main_bankmachine2_fifo_out_payload_we;
	assign main_bankmachine2_source_payload_addr = main_bankmachine2_fifo_out_payload_addr;
	assign main_bankmachine2_syncfifo2_re = main_bankmachine2_source_ready;
	always @(*) begin
		main_bankmachine2_wrport_adr <= 4'd0;
		if (main_bankmachine2_replace)
			main_bankmachine2_wrport_adr <= main_bankmachine2_produce - 1'd1;
		else
			main_bankmachine2_wrport_adr <= main_bankmachine2_produce;
	end
	assign main_bankmachine2_wrport_dat_w = main_bankmachine2_syncfifo2_din;
	assign main_bankmachine2_wrport_we = main_bankmachine2_syncfifo2_we & (main_bankmachine2_syncfifo2_writable | main_bankmachine2_replace);
	assign main_bankmachine2_do_read = main_bankmachine2_syncfifo2_readable & main_bankmachine2_syncfifo2_re;
	assign main_bankmachine2_rdport_adr = main_bankmachine2_consume;
	assign main_bankmachine2_syncfifo2_dout = main_bankmachine2_rdport_dat_r;
	assign main_bankmachine2_syncfifo2_writable = main_bankmachine2_level != 5'd16;
	assign main_bankmachine2_syncfifo2_readable = main_bankmachine2_level != 1'd0;
	assign main_bankmachine2_pipe_valid_sink_ready = ~main_bankmachine2_pipe_valid_source_valid | main_bankmachine2_pipe_valid_source_ready;
	assign main_bankmachine2_pipe_valid_sink_valid = main_bankmachine2_sink_sink_valid;
	assign main_bankmachine2_sink_sink_ready = main_bankmachine2_pipe_valid_sink_ready;
	assign main_bankmachine2_pipe_valid_sink_first = main_bankmachine2_sink_sink_first;
	assign main_bankmachine2_pipe_valid_sink_last = main_bankmachine2_sink_sink_last;
	assign main_bankmachine2_pipe_valid_sink_payload_we = main_bankmachine2_sink_sink_payload_we;
	assign main_bankmachine2_pipe_valid_sink_payload_addr = main_bankmachine2_sink_sink_payload_addr;
	assign main_bankmachine2_source_source_valid = main_bankmachine2_pipe_valid_source_valid;
	assign main_bankmachine2_pipe_valid_source_ready = main_bankmachine2_source_source_ready;
	assign main_bankmachine2_source_source_first = main_bankmachine2_pipe_valid_source_first;
	assign main_bankmachine2_source_source_last = main_bankmachine2_pipe_valid_source_last;
	assign main_bankmachine2_source_source_payload_we = main_bankmachine2_pipe_valid_source_payload_we;
	assign main_bankmachine2_source_source_payload_addr = main_bankmachine2_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine2_next_state <= 3'd0;
		builder_litedramcore_bankmachine2_next_state <= builder_litedramcore_bankmachine2_state;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				if (main_bankmachine2_twtpcon_ready & main_bankmachine2_trascon_ready) begin
					if (main_bankmachine2_cmd_ready)
						builder_litedramcore_bankmachine2_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine2_twtpcon_ready & main_bankmachine2_trascon_ready)
					builder_litedramcore_bankmachine2_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine2_trccon_ready) begin
					if (main_bankmachine2_cmd_ready)
						builder_litedramcore_bankmachine2_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine2_refresh_req)
					builder_litedramcore_bankmachine2_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine2_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine2_next_state <= 1'd0;
			default:
				if (main_bankmachine2_refresh_req)
					builder_litedramcore_bankmachine2_next_state <= 3'd4;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit) begin
							if (main_bankmachine2_cmd_ready & main_bankmachine2_auto_precharge)
								builder_litedramcore_bankmachine2_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine2_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine2_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine2_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine2_twtpcon_ready)
					main_bankmachine2_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine2_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine2_trccon_ready)
					main_bankmachine2_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine2_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				if (main_bankmachine2_twtpcon_ready & main_bankmachine2_trascon_ready)
					main_bankmachine2_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine2_trccon_ready)
					main_bankmachine2_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine2_refresh_req)
					;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit)
							main_bankmachine2_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine2_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1: main_bankmachine2_row_close <= 1'd1;
			2'd2: main_bankmachine2_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine2_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine2_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine2_trccon_ready)
					main_bankmachine2_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine2_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine2_refresh_req)
					;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit)
							main_bankmachine2_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine2_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				if (main_bankmachine2_twtpcon_ready & main_bankmachine2_trascon_ready)
					main_bankmachine2_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine2_trccon_ready)
					main_bankmachine2_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine2_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				if (main_bankmachine2_twtpcon_ready & main_bankmachine2_trascon_ready)
					main_bankmachine2_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine2_refresh_req)
					;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit) begin
							if (main_bankmachine2_source_source_payload_we)
								main_bankmachine2_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine2_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				if (main_bankmachine2_twtpcon_ready & main_bankmachine2_trascon_ready)
					main_bankmachine2_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine2_trccon_ready)
					main_bankmachine2_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine2_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine2_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine2_refresh_req)
					;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit) begin
							if (main_bankmachine2_source_source_payload_we)
								;
							else
								main_bankmachine2_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine2_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine2_refresh_req)
					;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit) begin
							if (main_bankmachine2_source_source_payload_we)
								main_bankmachine2_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine2_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine2_refresh_req)
					;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit) begin
							if (main_bankmachine2_source_source_payload_we)
								main_bankmachine2_req_wdata_ready <= main_bankmachine2_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine2_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine2_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine2_refresh_req)
					;
				else if (main_bankmachine2_source_source_valid) begin
					if (main_bankmachine2_row_opened) begin
						if (main_bankmachine2_row_hit) begin
							if (main_bankmachine2_source_source_payload_we)
								;
							else
								main_bankmachine2_req_rdata_valid <= main_bankmachine2_cmd_ready;
						end
					end
				end
		endcase
	end
	assign main_bankmachine3_sink_valid = main_bankmachine3_req_valid;
	assign main_bankmachine3_req_ready = main_bankmachine3_sink_ready;
	assign main_bankmachine3_sink_payload_we = main_bankmachine3_req_we;
	assign main_bankmachine3_sink_payload_addr = main_bankmachine3_req_addr;
	assign main_bankmachine3_sink_sink_valid = main_bankmachine3_source_valid;
	assign main_bankmachine3_source_ready = main_bankmachine3_sink_sink_ready;
	assign main_bankmachine3_sink_sink_first = main_bankmachine3_source_first;
	assign main_bankmachine3_sink_sink_last = main_bankmachine3_source_last;
	assign main_bankmachine3_sink_sink_payload_we = main_bankmachine3_source_payload_we;
	assign main_bankmachine3_sink_sink_payload_addr = main_bankmachine3_source_payload_addr;
	assign main_bankmachine3_source_source_ready = main_bankmachine3_req_wdata_ready | main_bankmachine3_req_rdata_valid;
	assign main_bankmachine3_req_lock = main_bankmachine3_source_valid | main_bankmachine3_source_source_valid;
	assign main_bankmachine3_row_hit = main_bankmachine3_row == main_bankmachine3_source_source_payload_addr[20:7];
	assign main_bankmachine3_cmd_payload_ba = 2'd3;
	always @(*) begin
		main_bankmachine3_cmd_payload_a <= 14'd0;
		if (main_bankmachine3_row_col_n_addr_sel)
			main_bankmachine3_cmd_payload_a <= main_bankmachine3_source_source_payload_addr[20:7];
		else
			main_bankmachine3_cmd_payload_a <= (main_bankmachine3_auto_precharge <<< 4'd10) | {main_bankmachine3_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine3_twtpcon_valid = (main_bankmachine3_cmd_valid & main_bankmachine3_cmd_ready) & main_bankmachine3_cmd_payload_is_write;
	assign main_bankmachine3_trccon_valid = (main_bankmachine3_cmd_valid & main_bankmachine3_cmd_ready) & main_bankmachine3_row_open;
	assign main_bankmachine3_trascon_valid = (main_bankmachine3_cmd_valid & main_bankmachine3_cmd_ready) & main_bankmachine3_row_open;
	always @(*) begin
		main_bankmachine3_auto_precharge <= 1'd0;
		if (main_bankmachine3_source_valid & main_bankmachine3_source_source_valid) begin
			if (main_bankmachine3_source_payload_addr[20:7] != main_bankmachine3_source_source_payload_addr[20:7])
				main_bankmachine3_auto_precharge <= main_bankmachine3_row_close == 1'd0;
		end
	end
	assign main_bankmachine3_syncfifo3_din = {main_bankmachine3_fifo_in_last, main_bankmachine3_fifo_in_first, main_bankmachine3_fifo_in_payload_addr, main_bankmachine3_fifo_in_payload_we};
	assign {main_bankmachine3_fifo_out_last, main_bankmachine3_fifo_out_first, main_bankmachine3_fifo_out_payload_addr, main_bankmachine3_fifo_out_payload_we} = main_bankmachine3_syncfifo3_dout;
	assign {main_bankmachine3_fifo_out_last, main_bankmachine3_fifo_out_first, main_bankmachine3_fifo_out_payload_addr, main_bankmachine3_fifo_out_payload_we} = main_bankmachine3_syncfifo3_dout;
	assign {main_bankmachine3_fifo_out_last, main_bankmachine3_fifo_out_first, main_bankmachine3_fifo_out_payload_addr, main_bankmachine3_fifo_out_payload_we} = main_bankmachine3_syncfifo3_dout;
	assign {main_bankmachine3_fifo_out_last, main_bankmachine3_fifo_out_first, main_bankmachine3_fifo_out_payload_addr, main_bankmachine3_fifo_out_payload_we} = main_bankmachine3_syncfifo3_dout;
	assign main_bankmachine3_sink_ready = main_bankmachine3_syncfifo3_writable;
	assign main_bankmachine3_syncfifo3_we = main_bankmachine3_sink_valid;
	assign main_bankmachine3_fifo_in_first = main_bankmachine3_sink_first;
	assign main_bankmachine3_fifo_in_last = main_bankmachine3_sink_last;
	assign main_bankmachine3_fifo_in_payload_we = main_bankmachine3_sink_payload_we;
	assign main_bankmachine3_fifo_in_payload_addr = main_bankmachine3_sink_payload_addr;
	assign main_bankmachine3_source_valid = main_bankmachine3_syncfifo3_readable;
	assign main_bankmachine3_source_first = main_bankmachine3_fifo_out_first;
	assign main_bankmachine3_source_last = main_bankmachine3_fifo_out_last;
	assign main_bankmachine3_source_payload_we = main_bankmachine3_fifo_out_payload_we;
	assign main_bankmachine3_source_payload_addr = main_bankmachine3_fifo_out_payload_addr;
	assign main_bankmachine3_syncfifo3_re = main_bankmachine3_source_ready;
	always @(*) begin
		main_bankmachine3_wrport_adr <= 4'd0;
		if (main_bankmachine3_replace)
			main_bankmachine3_wrport_adr <= main_bankmachine3_produce - 1'd1;
		else
			main_bankmachine3_wrport_adr <= main_bankmachine3_produce;
	end
	assign main_bankmachine3_wrport_dat_w = main_bankmachine3_syncfifo3_din;
	assign main_bankmachine3_wrport_we = main_bankmachine3_syncfifo3_we & (main_bankmachine3_syncfifo3_writable | main_bankmachine3_replace);
	assign main_bankmachine3_do_read = main_bankmachine3_syncfifo3_readable & main_bankmachine3_syncfifo3_re;
	assign main_bankmachine3_rdport_adr = main_bankmachine3_consume;
	assign main_bankmachine3_syncfifo3_dout = main_bankmachine3_rdport_dat_r;
	assign main_bankmachine3_syncfifo3_writable = main_bankmachine3_level != 5'd16;
	assign main_bankmachine3_syncfifo3_readable = main_bankmachine3_level != 1'd0;
	assign main_bankmachine3_pipe_valid_sink_ready = ~main_bankmachine3_pipe_valid_source_valid | main_bankmachine3_pipe_valid_source_ready;
	assign main_bankmachine3_pipe_valid_sink_valid = main_bankmachine3_sink_sink_valid;
	assign main_bankmachine3_sink_sink_ready = main_bankmachine3_pipe_valid_sink_ready;
	assign main_bankmachine3_pipe_valid_sink_first = main_bankmachine3_sink_sink_first;
	assign main_bankmachine3_pipe_valid_sink_last = main_bankmachine3_sink_sink_last;
	assign main_bankmachine3_pipe_valid_sink_payload_we = main_bankmachine3_sink_sink_payload_we;
	assign main_bankmachine3_pipe_valid_sink_payload_addr = main_bankmachine3_sink_sink_payload_addr;
	assign main_bankmachine3_source_source_valid = main_bankmachine3_pipe_valid_source_valid;
	assign main_bankmachine3_pipe_valid_source_ready = main_bankmachine3_source_source_ready;
	assign main_bankmachine3_source_source_first = main_bankmachine3_pipe_valid_source_first;
	assign main_bankmachine3_source_source_last = main_bankmachine3_pipe_valid_source_last;
	assign main_bankmachine3_source_source_payload_we = main_bankmachine3_pipe_valid_source_payload_we;
	assign main_bankmachine3_source_source_payload_addr = main_bankmachine3_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine3_next_state <= 3'd0;
		builder_litedramcore_bankmachine3_next_state <= builder_litedramcore_bankmachine3_state;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				if (main_bankmachine3_twtpcon_ready & main_bankmachine3_trascon_ready) begin
					if (main_bankmachine3_cmd_ready)
						builder_litedramcore_bankmachine3_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine3_twtpcon_ready & main_bankmachine3_trascon_ready)
					builder_litedramcore_bankmachine3_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine3_trccon_ready) begin
					if (main_bankmachine3_cmd_ready)
						builder_litedramcore_bankmachine3_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine3_refresh_req)
					builder_litedramcore_bankmachine3_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine3_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine3_next_state <= 1'd0;
			default:
				if (main_bankmachine3_refresh_req)
					builder_litedramcore_bankmachine3_next_state <= 3'd4;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit) begin
							if (main_bankmachine3_cmd_ready & main_bankmachine3_auto_precharge)
								builder_litedramcore_bankmachine3_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine3_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine3_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine3_trccon_ready)
					main_bankmachine3_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine3_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine3_refresh_req)
					;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit)
							main_bankmachine3_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				if (main_bankmachine3_twtpcon_ready & main_bankmachine3_trascon_ready)
					main_bankmachine3_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine3_trccon_ready)
					main_bankmachine3_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine3_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				if (main_bankmachine3_twtpcon_ready & main_bankmachine3_trascon_ready)
					main_bankmachine3_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine3_refresh_req)
					;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit) begin
							if (main_bankmachine3_source_source_payload_we)
								main_bankmachine3_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				if (main_bankmachine3_twtpcon_ready & main_bankmachine3_trascon_ready)
					main_bankmachine3_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine3_trccon_ready)
					main_bankmachine3_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine3_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine3_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine3_refresh_req)
					;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit) begin
							if (main_bankmachine3_source_source_payload_we)
								;
							else
								main_bankmachine3_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine3_refresh_req)
					;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit) begin
							if (main_bankmachine3_source_source_payload_we)
								main_bankmachine3_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine3_refresh_req)
					;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit) begin
							if (main_bankmachine3_source_source_payload_we)
								main_bankmachine3_req_wdata_ready <= main_bankmachine3_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine3_refresh_req)
					;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit) begin
							if (main_bankmachine3_source_source_payload_we)
								;
							else
								main_bankmachine3_req_rdata_valid <= main_bankmachine3_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine3_twtpcon_ready)
					main_bankmachine3_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine3_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine3_trccon_ready)
					main_bankmachine3_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine3_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1:
				if (main_bankmachine3_twtpcon_ready & main_bankmachine3_trascon_ready)
					main_bankmachine3_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine3_trccon_ready)
					main_bankmachine3_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine3_refresh_req)
					;
				else if (main_bankmachine3_source_source_valid) begin
					if (main_bankmachine3_row_opened) begin
						if (main_bankmachine3_row_hit)
							main_bankmachine3_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine3_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine3_state)
			1'd1: main_bankmachine3_row_close <= 1'd1;
			2'd2: main_bankmachine3_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine3_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	assign main_bankmachine4_sink_valid = main_bankmachine4_req_valid;
	assign main_bankmachine4_req_ready = main_bankmachine4_sink_ready;
	assign main_bankmachine4_sink_payload_we = main_bankmachine4_req_we;
	assign main_bankmachine4_sink_payload_addr = main_bankmachine4_req_addr;
	assign main_bankmachine4_sink_sink_valid = main_bankmachine4_source_valid;
	assign main_bankmachine4_source_ready = main_bankmachine4_sink_sink_ready;
	assign main_bankmachine4_sink_sink_first = main_bankmachine4_source_first;
	assign main_bankmachine4_sink_sink_last = main_bankmachine4_source_last;
	assign main_bankmachine4_sink_sink_payload_we = main_bankmachine4_source_payload_we;
	assign main_bankmachine4_sink_sink_payload_addr = main_bankmachine4_source_payload_addr;
	assign main_bankmachine4_source_source_ready = main_bankmachine4_req_wdata_ready | main_bankmachine4_req_rdata_valid;
	assign main_bankmachine4_req_lock = main_bankmachine4_source_valid | main_bankmachine4_source_source_valid;
	assign main_bankmachine4_row_hit = main_bankmachine4_row == main_bankmachine4_source_source_payload_addr[20:7];
	assign main_bankmachine4_cmd_payload_ba = 3'd4;
	always @(*) begin
		main_bankmachine4_cmd_payload_a <= 14'd0;
		if (main_bankmachine4_row_col_n_addr_sel)
			main_bankmachine4_cmd_payload_a <= main_bankmachine4_source_source_payload_addr[20:7];
		else
			main_bankmachine4_cmd_payload_a <= (main_bankmachine4_auto_precharge <<< 4'd10) | {main_bankmachine4_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine4_twtpcon_valid = (main_bankmachine4_cmd_valid & main_bankmachine4_cmd_ready) & main_bankmachine4_cmd_payload_is_write;
	assign main_bankmachine4_trccon_valid = (main_bankmachine4_cmd_valid & main_bankmachine4_cmd_ready) & main_bankmachine4_row_open;
	assign main_bankmachine4_trascon_valid = (main_bankmachine4_cmd_valid & main_bankmachine4_cmd_ready) & main_bankmachine4_row_open;
	always @(*) begin
		main_bankmachine4_auto_precharge <= 1'd0;
		if (main_bankmachine4_source_valid & main_bankmachine4_source_source_valid) begin
			if (main_bankmachine4_source_payload_addr[20:7] != main_bankmachine4_source_source_payload_addr[20:7])
				main_bankmachine4_auto_precharge <= main_bankmachine4_row_close == 1'd0;
		end
	end
	assign main_bankmachine4_syncfifo4_din = {main_bankmachine4_fifo_in_last, main_bankmachine4_fifo_in_first, main_bankmachine4_fifo_in_payload_addr, main_bankmachine4_fifo_in_payload_we};
	assign {main_bankmachine4_fifo_out_last, main_bankmachine4_fifo_out_first, main_bankmachine4_fifo_out_payload_addr, main_bankmachine4_fifo_out_payload_we} = main_bankmachine4_syncfifo4_dout;
	assign {main_bankmachine4_fifo_out_last, main_bankmachine4_fifo_out_first, main_bankmachine4_fifo_out_payload_addr, main_bankmachine4_fifo_out_payload_we} = main_bankmachine4_syncfifo4_dout;
	assign {main_bankmachine4_fifo_out_last, main_bankmachine4_fifo_out_first, main_bankmachine4_fifo_out_payload_addr, main_bankmachine4_fifo_out_payload_we} = main_bankmachine4_syncfifo4_dout;
	assign {main_bankmachine4_fifo_out_last, main_bankmachine4_fifo_out_first, main_bankmachine4_fifo_out_payload_addr, main_bankmachine4_fifo_out_payload_we} = main_bankmachine4_syncfifo4_dout;
	assign main_bankmachine4_sink_ready = main_bankmachine4_syncfifo4_writable;
	assign main_bankmachine4_syncfifo4_we = main_bankmachine4_sink_valid;
	assign main_bankmachine4_fifo_in_first = main_bankmachine4_sink_first;
	assign main_bankmachine4_fifo_in_last = main_bankmachine4_sink_last;
	assign main_bankmachine4_fifo_in_payload_we = main_bankmachine4_sink_payload_we;
	assign main_bankmachine4_fifo_in_payload_addr = main_bankmachine4_sink_payload_addr;
	assign main_bankmachine4_source_valid = main_bankmachine4_syncfifo4_readable;
	assign main_bankmachine4_source_first = main_bankmachine4_fifo_out_first;
	assign main_bankmachine4_source_last = main_bankmachine4_fifo_out_last;
	assign main_bankmachine4_source_payload_we = main_bankmachine4_fifo_out_payload_we;
	assign main_bankmachine4_source_payload_addr = main_bankmachine4_fifo_out_payload_addr;
	assign main_bankmachine4_syncfifo4_re = main_bankmachine4_source_ready;
	always @(*) begin
		main_bankmachine4_wrport_adr <= 4'd0;
		if (main_bankmachine4_replace)
			main_bankmachine4_wrport_adr <= main_bankmachine4_produce - 1'd1;
		else
			main_bankmachine4_wrport_adr <= main_bankmachine4_produce;
	end
	assign main_bankmachine4_wrport_dat_w = main_bankmachine4_syncfifo4_din;
	assign main_bankmachine4_wrport_we = main_bankmachine4_syncfifo4_we & (main_bankmachine4_syncfifo4_writable | main_bankmachine4_replace);
	assign main_bankmachine4_do_read = main_bankmachine4_syncfifo4_readable & main_bankmachine4_syncfifo4_re;
	assign main_bankmachine4_rdport_adr = main_bankmachine4_consume;
	assign main_bankmachine4_syncfifo4_dout = main_bankmachine4_rdport_dat_r;
	assign main_bankmachine4_syncfifo4_writable = main_bankmachine4_level != 5'd16;
	assign main_bankmachine4_syncfifo4_readable = main_bankmachine4_level != 1'd0;
	assign main_bankmachine4_pipe_valid_sink_ready = ~main_bankmachine4_pipe_valid_source_valid | main_bankmachine4_pipe_valid_source_ready;
	assign main_bankmachine4_pipe_valid_sink_valid = main_bankmachine4_sink_sink_valid;
	assign main_bankmachine4_sink_sink_ready = main_bankmachine4_pipe_valid_sink_ready;
	assign main_bankmachine4_pipe_valid_sink_first = main_bankmachine4_sink_sink_first;
	assign main_bankmachine4_pipe_valid_sink_last = main_bankmachine4_sink_sink_last;
	assign main_bankmachine4_pipe_valid_sink_payload_we = main_bankmachine4_sink_sink_payload_we;
	assign main_bankmachine4_pipe_valid_sink_payload_addr = main_bankmachine4_sink_sink_payload_addr;
	assign main_bankmachine4_source_source_valid = main_bankmachine4_pipe_valid_source_valid;
	assign main_bankmachine4_pipe_valid_source_ready = main_bankmachine4_source_source_ready;
	assign main_bankmachine4_source_source_first = main_bankmachine4_pipe_valid_source_first;
	assign main_bankmachine4_source_source_last = main_bankmachine4_pipe_valid_source_last;
	assign main_bankmachine4_source_source_payload_we = main_bankmachine4_pipe_valid_source_payload_we;
	assign main_bankmachine4_source_source_payload_addr = main_bankmachine4_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine4_next_state <= 3'd0;
		builder_litedramcore_bankmachine4_next_state <= builder_litedramcore_bankmachine4_state;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				if (main_bankmachine4_twtpcon_ready & main_bankmachine4_trascon_ready) begin
					if (main_bankmachine4_cmd_ready)
						builder_litedramcore_bankmachine4_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine4_twtpcon_ready & main_bankmachine4_trascon_ready)
					builder_litedramcore_bankmachine4_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine4_trccon_ready) begin
					if (main_bankmachine4_cmd_ready)
						builder_litedramcore_bankmachine4_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine4_refresh_req)
					builder_litedramcore_bankmachine4_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine4_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine4_next_state <= 1'd0;
			default:
				if (main_bankmachine4_refresh_req)
					builder_litedramcore_bankmachine4_next_state <= 3'd4;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit) begin
							if (main_bankmachine4_cmd_ready & main_bankmachine4_auto_precharge)
								builder_litedramcore_bankmachine4_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine4_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine4_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine4_refresh_req)
					;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit)
							main_bankmachine4_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				if (main_bankmachine4_twtpcon_ready & main_bankmachine4_trascon_ready)
					main_bankmachine4_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine4_trccon_ready)
					main_bankmachine4_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine4_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				if (main_bankmachine4_twtpcon_ready & main_bankmachine4_trascon_ready)
					main_bankmachine4_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine4_refresh_req)
					;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit) begin
							if (main_bankmachine4_source_source_payload_we)
								main_bankmachine4_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				if (main_bankmachine4_twtpcon_ready & main_bankmachine4_trascon_ready)
					main_bankmachine4_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine4_trccon_ready)
					main_bankmachine4_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine4_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine4_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine4_refresh_req)
					;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit) begin
							if (main_bankmachine4_source_source_payload_we)
								;
							else
								main_bankmachine4_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine4_refresh_req)
					;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit) begin
							if (main_bankmachine4_source_source_payload_we)
								main_bankmachine4_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine4_refresh_req)
					;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit) begin
							if (main_bankmachine4_source_source_payload_we)
								main_bankmachine4_req_wdata_ready <= main_bankmachine4_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine4_refresh_req)
					;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit) begin
							if (main_bankmachine4_source_source_payload_we)
								;
							else
								main_bankmachine4_req_rdata_valid <= main_bankmachine4_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine4_twtpcon_ready)
					main_bankmachine4_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine4_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine4_trccon_ready)
					main_bankmachine4_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine4_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				if (main_bankmachine4_twtpcon_ready & main_bankmachine4_trascon_ready)
					main_bankmachine4_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine4_trccon_ready)
					main_bankmachine4_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine4_refresh_req)
					;
				else if (main_bankmachine4_source_source_valid) begin
					if (main_bankmachine4_row_opened) begin
						if (main_bankmachine4_row_hit)
							main_bankmachine4_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine4_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1: main_bankmachine4_row_close <= 1'd1;
			2'd2: main_bankmachine4_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine4_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine4_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine4_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine4_trccon_ready)
					main_bankmachine4_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	assign main_bankmachine5_sink_valid = main_bankmachine5_req_valid;
	assign main_bankmachine5_req_ready = main_bankmachine5_sink_ready;
	assign main_bankmachine5_sink_payload_we = main_bankmachine5_req_we;
	assign main_bankmachine5_sink_payload_addr = main_bankmachine5_req_addr;
	assign main_bankmachine5_sink_sink_valid = main_bankmachine5_source_valid;
	assign main_bankmachine5_source_ready = main_bankmachine5_sink_sink_ready;
	assign main_bankmachine5_sink_sink_first = main_bankmachine5_source_first;
	assign main_bankmachine5_sink_sink_last = main_bankmachine5_source_last;
	assign main_bankmachine5_sink_sink_payload_we = main_bankmachine5_source_payload_we;
	assign main_bankmachine5_sink_sink_payload_addr = main_bankmachine5_source_payload_addr;
	assign main_bankmachine5_source_source_ready = main_bankmachine5_req_wdata_ready | main_bankmachine5_req_rdata_valid;
	assign main_bankmachine5_req_lock = main_bankmachine5_source_valid | main_bankmachine5_source_source_valid;
	assign main_bankmachine5_row_hit = main_bankmachine5_row == main_bankmachine5_source_source_payload_addr[20:7];
	assign main_bankmachine5_cmd_payload_ba = 3'd5;
	always @(*) begin
		main_bankmachine5_cmd_payload_a <= 14'd0;
		if (main_bankmachine5_row_col_n_addr_sel)
			main_bankmachine5_cmd_payload_a <= main_bankmachine5_source_source_payload_addr[20:7];
		else
			main_bankmachine5_cmd_payload_a <= (main_bankmachine5_auto_precharge <<< 4'd10) | {main_bankmachine5_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine5_twtpcon_valid = (main_bankmachine5_cmd_valid & main_bankmachine5_cmd_ready) & main_bankmachine5_cmd_payload_is_write;
	assign main_bankmachine5_trccon_valid = (main_bankmachine5_cmd_valid & main_bankmachine5_cmd_ready) & main_bankmachine5_row_open;
	assign main_bankmachine5_trascon_valid = (main_bankmachine5_cmd_valid & main_bankmachine5_cmd_ready) & main_bankmachine5_row_open;
	always @(*) begin
		main_bankmachine5_auto_precharge <= 1'd0;
		if (main_bankmachine5_source_valid & main_bankmachine5_source_source_valid) begin
			if (main_bankmachine5_source_payload_addr[20:7] != main_bankmachine5_source_source_payload_addr[20:7])
				main_bankmachine5_auto_precharge <= main_bankmachine5_row_close == 1'd0;
		end
	end
	assign main_bankmachine5_syncfifo5_din = {main_bankmachine5_fifo_in_last, main_bankmachine5_fifo_in_first, main_bankmachine5_fifo_in_payload_addr, main_bankmachine5_fifo_in_payload_we};
	assign {main_bankmachine5_fifo_out_last, main_bankmachine5_fifo_out_first, main_bankmachine5_fifo_out_payload_addr, main_bankmachine5_fifo_out_payload_we} = main_bankmachine5_syncfifo5_dout;
	assign {main_bankmachine5_fifo_out_last, main_bankmachine5_fifo_out_first, main_bankmachine5_fifo_out_payload_addr, main_bankmachine5_fifo_out_payload_we} = main_bankmachine5_syncfifo5_dout;
	assign {main_bankmachine5_fifo_out_last, main_bankmachine5_fifo_out_first, main_bankmachine5_fifo_out_payload_addr, main_bankmachine5_fifo_out_payload_we} = main_bankmachine5_syncfifo5_dout;
	assign {main_bankmachine5_fifo_out_last, main_bankmachine5_fifo_out_first, main_bankmachine5_fifo_out_payload_addr, main_bankmachine5_fifo_out_payload_we} = main_bankmachine5_syncfifo5_dout;
	assign main_bankmachine5_sink_ready = main_bankmachine5_syncfifo5_writable;
	assign main_bankmachine5_syncfifo5_we = main_bankmachine5_sink_valid;
	assign main_bankmachine5_fifo_in_first = main_bankmachine5_sink_first;
	assign main_bankmachine5_fifo_in_last = main_bankmachine5_sink_last;
	assign main_bankmachine5_fifo_in_payload_we = main_bankmachine5_sink_payload_we;
	assign main_bankmachine5_fifo_in_payload_addr = main_bankmachine5_sink_payload_addr;
	assign main_bankmachine5_source_valid = main_bankmachine5_syncfifo5_readable;
	assign main_bankmachine5_source_first = main_bankmachine5_fifo_out_first;
	assign main_bankmachine5_source_last = main_bankmachine5_fifo_out_last;
	assign main_bankmachine5_source_payload_we = main_bankmachine5_fifo_out_payload_we;
	assign main_bankmachine5_source_payload_addr = main_bankmachine5_fifo_out_payload_addr;
	assign main_bankmachine5_syncfifo5_re = main_bankmachine5_source_ready;
	always @(*) begin
		main_bankmachine5_wrport_adr <= 4'd0;
		if (main_bankmachine5_replace)
			main_bankmachine5_wrport_adr <= main_bankmachine5_produce - 1'd1;
		else
			main_bankmachine5_wrport_adr <= main_bankmachine5_produce;
	end
	assign main_bankmachine5_wrport_dat_w = main_bankmachine5_syncfifo5_din;
	assign main_bankmachine5_wrport_we = main_bankmachine5_syncfifo5_we & (main_bankmachine5_syncfifo5_writable | main_bankmachine5_replace);
	assign main_bankmachine5_do_read = main_bankmachine5_syncfifo5_readable & main_bankmachine5_syncfifo5_re;
	assign main_bankmachine5_rdport_adr = main_bankmachine5_consume;
	assign main_bankmachine5_syncfifo5_dout = main_bankmachine5_rdport_dat_r;
	assign main_bankmachine5_syncfifo5_writable = main_bankmachine5_level != 5'd16;
	assign main_bankmachine5_syncfifo5_readable = main_bankmachine5_level != 1'd0;
	assign main_bankmachine5_pipe_valid_sink_ready = ~main_bankmachine5_pipe_valid_source_valid | main_bankmachine5_pipe_valid_source_ready;
	assign main_bankmachine5_pipe_valid_sink_valid = main_bankmachine5_sink_sink_valid;
	assign main_bankmachine5_sink_sink_ready = main_bankmachine5_pipe_valid_sink_ready;
	assign main_bankmachine5_pipe_valid_sink_first = main_bankmachine5_sink_sink_first;
	assign main_bankmachine5_pipe_valid_sink_last = main_bankmachine5_sink_sink_last;
	assign main_bankmachine5_pipe_valid_sink_payload_we = main_bankmachine5_sink_sink_payload_we;
	assign main_bankmachine5_pipe_valid_sink_payload_addr = main_bankmachine5_sink_sink_payload_addr;
	assign main_bankmachine5_source_source_valid = main_bankmachine5_pipe_valid_source_valid;
	assign main_bankmachine5_pipe_valid_source_ready = main_bankmachine5_source_source_ready;
	assign main_bankmachine5_source_source_first = main_bankmachine5_pipe_valid_source_first;
	assign main_bankmachine5_source_source_last = main_bankmachine5_pipe_valid_source_last;
	assign main_bankmachine5_source_source_payload_we = main_bankmachine5_pipe_valid_source_payload_we;
	assign main_bankmachine5_source_source_payload_addr = main_bankmachine5_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine5_next_state <= 3'd0;
		builder_litedramcore_bankmachine5_next_state <= builder_litedramcore_bankmachine5_state;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				if (main_bankmachine5_twtpcon_ready & main_bankmachine5_trascon_ready) begin
					if (main_bankmachine5_cmd_ready)
						builder_litedramcore_bankmachine5_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine5_twtpcon_ready & main_bankmachine5_trascon_ready)
					builder_litedramcore_bankmachine5_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine5_trccon_ready) begin
					if (main_bankmachine5_cmd_ready)
						builder_litedramcore_bankmachine5_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine5_refresh_req)
					builder_litedramcore_bankmachine5_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine5_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine5_next_state <= 1'd0;
			default:
				if (main_bankmachine5_refresh_req)
					builder_litedramcore_bankmachine5_next_state <= 3'd4;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit) begin
							if (main_bankmachine5_cmd_ready & main_bankmachine5_auto_precharge)
								builder_litedramcore_bankmachine5_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine5_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine5_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine5_refresh_req)
					;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit) begin
							if (main_bankmachine5_source_source_payload_we)
								;
							else
								main_bankmachine5_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine5_refresh_req)
					;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit) begin
							if (main_bankmachine5_source_source_payload_we)
								main_bankmachine5_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine5_refresh_req)
					;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit) begin
							if (main_bankmachine5_source_source_payload_we)
								main_bankmachine5_req_wdata_ready <= main_bankmachine5_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine5_refresh_req)
					;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit) begin
							if (main_bankmachine5_source_source_payload_we)
								;
							else
								main_bankmachine5_req_rdata_valid <= main_bankmachine5_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine5_twtpcon_ready)
					main_bankmachine5_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine5_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				if (main_bankmachine5_twtpcon_ready & main_bankmachine5_trascon_ready)
					main_bankmachine5_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine5_trccon_ready)
					main_bankmachine5_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine5_refresh_req)
					;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit)
							main_bankmachine5_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1: main_bankmachine5_row_close <= 1'd1;
			2'd2: main_bankmachine5_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine5_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine5_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine5_trccon_ready)
					main_bankmachine5_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine5_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine5_trccon_ready)
					main_bankmachine5_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine5_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine5_refresh_req)
					;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit)
							main_bankmachine5_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				if (main_bankmachine5_twtpcon_ready & main_bankmachine5_trascon_ready)
					main_bankmachine5_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine5_trccon_ready)
					main_bankmachine5_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine5_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				if (main_bankmachine5_twtpcon_ready & main_bankmachine5_trascon_ready)
					main_bankmachine5_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine5_refresh_req)
					;
				else if (main_bankmachine5_source_source_valid) begin
					if (main_bankmachine5_row_opened) begin
						if (main_bankmachine5_row_hit) begin
							if (main_bankmachine5_source_source_payload_we)
								main_bankmachine5_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine5_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine5_state)
			1'd1:
				if (main_bankmachine5_twtpcon_ready & main_bankmachine5_trascon_ready)
					main_bankmachine5_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine5_trccon_ready)
					main_bankmachine5_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine5_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	assign main_bankmachine6_sink_valid = main_bankmachine6_req_valid;
	assign main_bankmachine6_req_ready = main_bankmachine6_sink_ready;
	assign main_bankmachine6_sink_payload_we = main_bankmachine6_req_we;
	assign main_bankmachine6_sink_payload_addr = main_bankmachine6_req_addr;
	assign main_bankmachine6_sink_sink_valid = main_bankmachine6_source_valid;
	assign main_bankmachine6_source_ready = main_bankmachine6_sink_sink_ready;
	assign main_bankmachine6_sink_sink_first = main_bankmachine6_source_first;
	assign main_bankmachine6_sink_sink_last = main_bankmachine6_source_last;
	assign main_bankmachine6_sink_sink_payload_we = main_bankmachine6_source_payload_we;
	assign main_bankmachine6_sink_sink_payload_addr = main_bankmachine6_source_payload_addr;
	assign main_bankmachine6_source_source_ready = main_bankmachine6_req_wdata_ready | main_bankmachine6_req_rdata_valid;
	assign main_bankmachine6_req_lock = main_bankmachine6_source_valid | main_bankmachine6_source_source_valid;
	assign main_bankmachine6_row_hit = main_bankmachine6_row == main_bankmachine6_source_source_payload_addr[20:7];
	assign main_bankmachine6_cmd_payload_ba = 3'd6;
	always @(*) begin
		main_bankmachine6_cmd_payload_a <= 14'd0;
		if (main_bankmachine6_row_col_n_addr_sel)
			main_bankmachine6_cmd_payload_a <= main_bankmachine6_source_source_payload_addr[20:7];
		else
			main_bankmachine6_cmd_payload_a <= (main_bankmachine6_auto_precharge <<< 4'd10) | {main_bankmachine6_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine6_twtpcon_valid = (main_bankmachine6_cmd_valid & main_bankmachine6_cmd_ready) & main_bankmachine6_cmd_payload_is_write;
	assign main_bankmachine6_trccon_valid = (main_bankmachine6_cmd_valid & main_bankmachine6_cmd_ready) & main_bankmachine6_row_open;
	assign main_bankmachine6_trascon_valid = (main_bankmachine6_cmd_valid & main_bankmachine6_cmd_ready) & main_bankmachine6_row_open;
	always @(*) begin
		main_bankmachine6_auto_precharge <= 1'd0;
		if (main_bankmachine6_source_valid & main_bankmachine6_source_source_valid) begin
			if (main_bankmachine6_source_payload_addr[20:7] != main_bankmachine6_source_source_payload_addr[20:7])
				main_bankmachine6_auto_precharge <= main_bankmachine6_row_close == 1'd0;
		end
	end
	assign main_bankmachine6_syncfifo6_din = {main_bankmachine6_fifo_in_last, main_bankmachine6_fifo_in_first, main_bankmachine6_fifo_in_payload_addr, main_bankmachine6_fifo_in_payload_we};
	assign {main_bankmachine6_fifo_out_last, main_bankmachine6_fifo_out_first, main_bankmachine6_fifo_out_payload_addr, main_bankmachine6_fifo_out_payload_we} = main_bankmachine6_syncfifo6_dout;
	assign {main_bankmachine6_fifo_out_last, main_bankmachine6_fifo_out_first, main_bankmachine6_fifo_out_payload_addr, main_bankmachine6_fifo_out_payload_we} = main_bankmachine6_syncfifo6_dout;
	assign {main_bankmachine6_fifo_out_last, main_bankmachine6_fifo_out_first, main_bankmachine6_fifo_out_payload_addr, main_bankmachine6_fifo_out_payload_we} = main_bankmachine6_syncfifo6_dout;
	assign {main_bankmachine6_fifo_out_last, main_bankmachine6_fifo_out_first, main_bankmachine6_fifo_out_payload_addr, main_bankmachine6_fifo_out_payload_we} = main_bankmachine6_syncfifo6_dout;
	assign main_bankmachine6_sink_ready = main_bankmachine6_syncfifo6_writable;
	assign main_bankmachine6_syncfifo6_we = main_bankmachine6_sink_valid;
	assign main_bankmachine6_fifo_in_first = main_bankmachine6_sink_first;
	assign main_bankmachine6_fifo_in_last = main_bankmachine6_sink_last;
	assign main_bankmachine6_fifo_in_payload_we = main_bankmachine6_sink_payload_we;
	assign main_bankmachine6_fifo_in_payload_addr = main_bankmachine6_sink_payload_addr;
	assign main_bankmachine6_source_valid = main_bankmachine6_syncfifo6_readable;
	assign main_bankmachine6_source_first = main_bankmachine6_fifo_out_first;
	assign main_bankmachine6_source_last = main_bankmachine6_fifo_out_last;
	assign main_bankmachine6_source_payload_we = main_bankmachine6_fifo_out_payload_we;
	assign main_bankmachine6_source_payload_addr = main_bankmachine6_fifo_out_payload_addr;
	assign main_bankmachine6_syncfifo6_re = main_bankmachine6_source_ready;
	always @(*) begin
		main_bankmachine6_wrport_adr <= 4'd0;
		if (main_bankmachine6_replace)
			main_bankmachine6_wrport_adr <= main_bankmachine6_produce - 1'd1;
		else
			main_bankmachine6_wrport_adr <= main_bankmachine6_produce;
	end
	assign main_bankmachine6_wrport_dat_w = main_bankmachine6_syncfifo6_din;
	assign main_bankmachine6_wrport_we = main_bankmachine6_syncfifo6_we & (main_bankmachine6_syncfifo6_writable | main_bankmachine6_replace);
	assign main_bankmachine6_do_read = main_bankmachine6_syncfifo6_readable & main_bankmachine6_syncfifo6_re;
	assign main_bankmachine6_rdport_adr = main_bankmachine6_consume;
	assign main_bankmachine6_syncfifo6_dout = main_bankmachine6_rdport_dat_r;
	assign main_bankmachine6_syncfifo6_writable = main_bankmachine6_level != 5'd16;
	assign main_bankmachine6_syncfifo6_readable = main_bankmachine6_level != 1'd0;
	assign main_bankmachine6_pipe_valid_sink_ready = ~main_bankmachine6_pipe_valid_source_valid | main_bankmachine6_pipe_valid_source_ready;
	assign main_bankmachine6_pipe_valid_sink_valid = main_bankmachine6_sink_sink_valid;
	assign main_bankmachine6_sink_sink_ready = main_bankmachine6_pipe_valid_sink_ready;
	assign main_bankmachine6_pipe_valid_sink_first = main_bankmachine6_sink_sink_first;
	assign main_bankmachine6_pipe_valid_sink_last = main_bankmachine6_sink_sink_last;
	assign main_bankmachine6_pipe_valid_sink_payload_we = main_bankmachine6_sink_sink_payload_we;
	assign main_bankmachine6_pipe_valid_sink_payload_addr = main_bankmachine6_sink_sink_payload_addr;
	assign main_bankmachine6_source_source_valid = main_bankmachine6_pipe_valid_source_valid;
	assign main_bankmachine6_pipe_valid_source_ready = main_bankmachine6_source_source_ready;
	assign main_bankmachine6_source_source_first = main_bankmachine6_pipe_valid_source_first;
	assign main_bankmachine6_source_source_last = main_bankmachine6_pipe_valid_source_last;
	assign main_bankmachine6_source_source_payload_we = main_bankmachine6_pipe_valid_source_payload_we;
	assign main_bankmachine6_source_source_payload_addr = main_bankmachine6_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine6_next_state <= 3'd0;
		builder_litedramcore_bankmachine6_next_state <= builder_litedramcore_bankmachine6_state;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				if (main_bankmachine6_twtpcon_ready & main_bankmachine6_trascon_ready) begin
					if (main_bankmachine6_cmd_ready)
						builder_litedramcore_bankmachine6_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine6_twtpcon_ready & main_bankmachine6_trascon_ready)
					builder_litedramcore_bankmachine6_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine6_trccon_ready) begin
					if (main_bankmachine6_cmd_ready)
						builder_litedramcore_bankmachine6_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine6_refresh_req)
					builder_litedramcore_bankmachine6_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine6_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine6_next_state <= 1'd0;
			default:
				if (main_bankmachine6_refresh_req)
					builder_litedramcore_bankmachine6_next_state <= 3'd4;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit) begin
							if (main_bankmachine6_cmd_ready & main_bankmachine6_auto_precharge)
								builder_litedramcore_bankmachine6_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine6_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine6_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine6_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine6_twtpcon_ready)
					main_bankmachine6_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine6_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine6_trccon_ready)
					main_bankmachine6_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine6_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				if (main_bankmachine6_twtpcon_ready & main_bankmachine6_trascon_ready)
					main_bankmachine6_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine6_trccon_ready)
					main_bankmachine6_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine6_refresh_req)
					;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit)
							main_bankmachine6_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine6_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1: main_bankmachine6_row_close <= 1'd1;
			2'd2: main_bankmachine6_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine6_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine6_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine6_trccon_ready)
					main_bankmachine6_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine6_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine6_refresh_req)
					;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit)
							main_bankmachine6_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine6_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				if (main_bankmachine6_twtpcon_ready & main_bankmachine6_trascon_ready)
					main_bankmachine6_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine6_trccon_ready)
					main_bankmachine6_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine6_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				if (main_bankmachine6_twtpcon_ready & main_bankmachine6_trascon_ready)
					main_bankmachine6_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine6_refresh_req)
					;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit) begin
							if (main_bankmachine6_source_source_payload_we)
								main_bankmachine6_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine6_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				if (main_bankmachine6_twtpcon_ready & main_bankmachine6_trascon_ready)
					main_bankmachine6_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine6_trccon_ready)
					main_bankmachine6_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine6_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine6_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine6_refresh_req)
					;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit) begin
							if (main_bankmachine6_source_source_payload_we)
								;
							else
								main_bankmachine6_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine6_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine6_refresh_req)
					;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit) begin
							if (main_bankmachine6_source_source_payload_we)
								main_bankmachine6_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine6_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine6_refresh_req)
					;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit) begin
							if (main_bankmachine6_source_source_payload_we)
								main_bankmachine6_req_wdata_ready <= main_bankmachine6_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine6_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine6_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine6_refresh_req)
					;
				else if (main_bankmachine6_source_source_valid) begin
					if (main_bankmachine6_row_opened) begin
						if (main_bankmachine6_row_hit) begin
							if (main_bankmachine6_source_source_payload_we)
								;
							else
								main_bankmachine6_req_rdata_valid <= main_bankmachine6_cmd_ready;
						end
					end
				end
		endcase
	end
	assign main_bankmachine7_sink_valid = main_bankmachine7_req_valid;
	assign main_bankmachine7_req_ready = main_bankmachine7_sink_ready;
	assign main_bankmachine7_sink_payload_we = main_bankmachine7_req_we;
	assign main_bankmachine7_sink_payload_addr = main_bankmachine7_req_addr;
	assign main_bankmachine7_sink_sink_valid = main_bankmachine7_source_valid;
	assign main_bankmachine7_source_ready = main_bankmachine7_sink_sink_ready;
	assign main_bankmachine7_sink_sink_first = main_bankmachine7_source_first;
	assign main_bankmachine7_sink_sink_last = main_bankmachine7_source_last;
	assign main_bankmachine7_sink_sink_payload_we = main_bankmachine7_source_payload_we;
	assign main_bankmachine7_sink_sink_payload_addr = main_bankmachine7_source_payload_addr;
	assign main_bankmachine7_source_source_ready = main_bankmachine7_req_wdata_ready | main_bankmachine7_req_rdata_valid;
	assign main_bankmachine7_req_lock = main_bankmachine7_source_valid | main_bankmachine7_source_source_valid;
	assign main_bankmachine7_row_hit = main_bankmachine7_row == main_bankmachine7_source_source_payload_addr[20:7];
	assign main_bankmachine7_cmd_payload_ba = 3'd7;
	always @(*) begin
		main_bankmachine7_cmd_payload_a <= 14'd0;
		if (main_bankmachine7_row_col_n_addr_sel)
			main_bankmachine7_cmd_payload_a <= main_bankmachine7_source_source_payload_addr[20:7];
		else
			main_bankmachine7_cmd_payload_a <= (main_bankmachine7_auto_precharge <<< 4'd10) | {main_bankmachine7_source_source_payload_addr[6:0], {3 {1'd0}}};
	end
	assign main_bankmachine7_twtpcon_valid = (main_bankmachine7_cmd_valid & main_bankmachine7_cmd_ready) & main_bankmachine7_cmd_payload_is_write;
	assign main_bankmachine7_trccon_valid = (main_bankmachine7_cmd_valid & main_bankmachine7_cmd_ready) & main_bankmachine7_row_open;
	assign main_bankmachine7_trascon_valid = (main_bankmachine7_cmd_valid & main_bankmachine7_cmd_ready) & main_bankmachine7_row_open;
	always @(*) begin
		main_bankmachine7_auto_precharge <= 1'd0;
		if (main_bankmachine7_source_valid & main_bankmachine7_source_source_valid) begin
			if (main_bankmachine7_source_payload_addr[20:7] != main_bankmachine7_source_source_payload_addr[20:7])
				main_bankmachine7_auto_precharge <= main_bankmachine7_row_close == 1'd0;
		end
	end
	assign main_bankmachine7_syncfifo7_din = {main_bankmachine7_fifo_in_last, main_bankmachine7_fifo_in_first, main_bankmachine7_fifo_in_payload_addr, main_bankmachine7_fifo_in_payload_we};
	assign {main_bankmachine7_fifo_out_last, main_bankmachine7_fifo_out_first, main_bankmachine7_fifo_out_payload_addr, main_bankmachine7_fifo_out_payload_we} = main_bankmachine7_syncfifo7_dout;
	assign {main_bankmachine7_fifo_out_last, main_bankmachine7_fifo_out_first, main_bankmachine7_fifo_out_payload_addr, main_bankmachine7_fifo_out_payload_we} = main_bankmachine7_syncfifo7_dout;
	assign {main_bankmachine7_fifo_out_last, main_bankmachine7_fifo_out_first, main_bankmachine7_fifo_out_payload_addr, main_bankmachine7_fifo_out_payload_we} = main_bankmachine7_syncfifo7_dout;
	assign {main_bankmachine7_fifo_out_last, main_bankmachine7_fifo_out_first, main_bankmachine7_fifo_out_payload_addr, main_bankmachine7_fifo_out_payload_we} = main_bankmachine7_syncfifo7_dout;
	assign main_bankmachine7_sink_ready = main_bankmachine7_syncfifo7_writable;
	assign main_bankmachine7_syncfifo7_we = main_bankmachine7_sink_valid;
	assign main_bankmachine7_fifo_in_first = main_bankmachine7_sink_first;
	assign main_bankmachine7_fifo_in_last = main_bankmachine7_sink_last;
	assign main_bankmachine7_fifo_in_payload_we = main_bankmachine7_sink_payload_we;
	assign main_bankmachine7_fifo_in_payload_addr = main_bankmachine7_sink_payload_addr;
	assign main_bankmachine7_source_valid = main_bankmachine7_syncfifo7_readable;
	assign main_bankmachine7_source_first = main_bankmachine7_fifo_out_first;
	assign main_bankmachine7_source_last = main_bankmachine7_fifo_out_last;
	assign main_bankmachine7_source_payload_we = main_bankmachine7_fifo_out_payload_we;
	assign main_bankmachine7_source_payload_addr = main_bankmachine7_fifo_out_payload_addr;
	assign main_bankmachine7_syncfifo7_re = main_bankmachine7_source_ready;
	always @(*) begin
		main_bankmachine7_wrport_adr <= 4'd0;
		if (main_bankmachine7_replace)
			main_bankmachine7_wrport_adr <= main_bankmachine7_produce - 1'd1;
		else
			main_bankmachine7_wrport_adr <= main_bankmachine7_produce;
	end
	assign main_bankmachine7_wrport_dat_w = main_bankmachine7_syncfifo7_din;
	assign main_bankmachine7_wrport_we = main_bankmachine7_syncfifo7_we & (main_bankmachine7_syncfifo7_writable | main_bankmachine7_replace);
	assign main_bankmachine7_do_read = main_bankmachine7_syncfifo7_readable & main_bankmachine7_syncfifo7_re;
	assign main_bankmachine7_rdport_adr = main_bankmachine7_consume;
	assign main_bankmachine7_syncfifo7_dout = main_bankmachine7_rdport_dat_r;
	assign main_bankmachine7_syncfifo7_writable = main_bankmachine7_level != 5'd16;
	assign main_bankmachine7_syncfifo7_readable = main_bankmachine7_level != 1'd0;
	assign main_bankmachine7_pipe_valid_sink_ready = ~main_bankmachine7_pipe_valid_source_valid | main_bankmachine7_pipe_valid_source_ready;
	assign main_bankmachine7_pipe_valid_sink_valid = main_bankmachine7_sink_sink_valid;
	assign main_bankmachine7_sink_sink_ready = main_bankmachine7_pipe_valid_sink_ready;
	assign main_bankmachine7_pipe_valid_sink_first = main_bankmachine7_sink_sink_first;
	assign main_bankmachine7_pipe_valid_sink_last = main_bankmachine7_sink_sink_last;
	assign main_bankmachine7_pipe_valid_sink_payload_we = main_bankmachine7_sink_sink_payload_we;
	assign main_bankmachine7_pipe_valid_sink_payload_addr = main_bankmachine7_sink_sink_payload_addr;
	assign main_bankmachine7_source_source_valid = main_bankmachine7_pipe_valid_source_valid;
	assign main_bankmachine7_pipe_valid_source_ready = main_bankmachine7_source_source_ready;
	assign main_bankmachine7_source_source_first = main_bankmachine7_pipe_valid_source_first;
	assign main_bankmachine7_source_source_last = main_bankmachine7_pipe_valid_source_last;
	assign main_bankmachine7_source_source_payload_we = main_bankmachine7_pipe_valid_source_payload_we;
	assign main_bankmachine7_source_source_payload_addr = main_bankmachine7_pipe_valid_source_payload_addr;
	always @(*) begin
		builder_litedramcore_bankmachine7_next_state <= 3'd0;
		builder_litedramcore_bankmachine7_next_state <= builder_litedramcore_bankmachine7_state;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				if (main_bankmachine7_twtpcon_ready & main_bankmachine7_trascon_ready) begin
					if (main_bankmachine7_cmd_ready)
						builder_litedramcore_bankmachine7_next_state <= 3'd5;
				end
			2'd2:
				if (main_bankmachine7_twtpcon_ready & main_bankmachine7_trascon_ready)
					builder_litedramcore_bankmachine7_next_state <= 3'd5;
			2'd3:
				if (main_bankmachine7_trccon_ready) begin
					if (main_bankmachine7_cmd_ready)
						builder_litedramcore_bankmachine7_next_state <= 3'd6;
				end
			3'd4:
				if (~main_bankmachine7_refresh_req)
					builder_litedramcore_bankmachine7_next_state <= 1'd0;
			3'd5: builder_litedramcore_bankmachine7_next_state <= 2'd3;
			3'd6: builder_litedramcore_bankmachine7_next_state <= 1'd0;
			default:
				if (main_bankmachine7_refresh_req)
					builder_litedramcore_bankmachine7_next_state <= 3'd4;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit) begin
							if (main_bankmachine7_cmd_ready & main_bankmachine7_auto_precharge)
								builder_litedramcore_bankmachine7_next_state <= 2'd2;
						end
						else
							builder_litedramcore_bankmachine7_next_state <= 1'd1;
					end
					else
						builder_litedramcore_bankmachine7_next_state <= 2'd3;
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_row_col_n_addr_sel <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine7_trccon_ready)
					main_bankmachine7_row_col_n_addr_sel <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine7_cmd_payload_cas <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine7_refresh_req)
					;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit)
							main_bankmachine7_cmd_payload_cas <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_cmd_payload_ras <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				if (main_bankmachine7_twtpcon_ready & main_bankmachine7_trascon_ready)
					main_bankmachine7_cmd_payload_ras <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine7_trccon_ready)
					main_bankmachine7_cmd_payload_ras <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine7_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				if (main_bankmachine7_twtpcon_ready & main_bankmachine7_trascon_ready)
					main_bankmachine7_cmd_payload_we <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine7_refresh_req)
					;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit) begin
							if (main_bankmachine7_source_source_payload_we)
								main_bankmachine7_cmd_payload_we <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_cmd_payload_is_cmd <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				if (main_bankmachine7_twtpcon_ready & main_bankmachine7_trascon_ready)
					main_bankmachine7_cmd_payload_is_cmd <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine7_trccon_ready)
					main_bankmachine7_cmd_payload_is_cmd <= 1'd1;
			3'd4: main_bankmachine7_cmd_payload_is_cmd <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine7_cmd_payload_is_read <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine7_refresh_req)
					;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit) begin
							if (main_bankmachine7_source_source_payload_we)
								;
							else
								main_bankmachine7_cmd_payload_is_read <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_cmd_payload_is_write <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine7_refresh_req)
					;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit) begin
							if (main_bankmachine7_source_source_payload_we)
								main_bankmachine7_cmd_payload_is_write <= 1'd1;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_req_wdata_ready <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine7_refresh_req)
					;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit) begin
							if (main_bankmachine7_source_source_payload_we)
								main_bankmachine7_req_wdata_ready <= main_bankmachine7_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_req_rdata_valid <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine7_refresh_req)
					;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit) begin
							if (main_bankmachine7_source_source_payload_we)
								;
							else
								main_bankmachine7_req_rdata_valid <= main_bankmachine7_cmd_ready;
						end
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_refresh_gnt <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				if (main_bankmachine7_twtpcon_ready)
					main_bankmachine7_refresh_gnt <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine7_row_open <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				if (main_bankmachine7_trccon_ready)
					main_bankmachine7_row_open <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_bankmachine7_cmd_valid <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1:
				if (main_bankmachine7_twtpcon_ready & main_bankmachine7_trascon_ready)
					main_bankmachine7_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				if (main_bankmachine7_trccon_ready)
					main_bankmachine7_cmd_valid <= 1'd1;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			default:
				if (main_bankmachine7_refresh_req)
					;
				else if (main_bankmachine7_source_source_valid) begin
					if (main_bankmachine7_row_opened) begin
						if (main_bankmachine7_row_hit)
							main_bankmachine7_cmd_valid <= 1'd1;
					end
				end
		endcase
	end
	always @(*) begin
		main_bankmachine7_row_close <= 1'd0;
		case (builder_litedramcore_bankmachine7_state)
			1'd1: main_bankmachine7_row_close <= 1'd1;
			2'd2: main_bankmachine7_row_close <= 1'd1;
			2'd3:
				;
			3'd4: main_bankmachine7_row_close <= 1'd1;
			3'd5:
				;
			3'd6:
				;
			default:
				;
		endcase
	end
	assign main_rdcmdphase = main_a7ddrphy_rdphase_storage - 1'd1;
	assign main_wrcmdphase = main_a7ddrphy_wrphase_storage - 1'd1;
	assign main_trrdcon_valid = (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & ((main_choose_cmd_cmd_payload_ras & ~main_choose_cmd_cmd_payload_cas) & ~main_choose_cmd_cmd_payload_we);
	assign main_tfawcon_valid = (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & ((main_choose_cmd_cmd_payload_ras & ~main_choose_cmd_cmd_payload_cas) & ~main_choose_cmd_cmd_payload_we);
	assign main_ras_allowed = main_trrdcon_ready & main_tfawcon_ready;
	assign main_tccdcon_valid = (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_cmd_payload_is_write | main_choose_req_cmd_payload_is_read);
	assign main_cas_allowed = main_tccdcon_ready;
	assign main_twtrcon_valid = (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_write;
	assign main_read_available = (((((((main_bankmachine0_cmd_valid & main_bankmachine0_cmd_payload_is_read) | (main_bankmachine1_cmd_valid & main_bankmachine1_cmd_payload_is_read)) | (main_bankmachine2_cmd_valid & main_bankmachine2_cmd_payload_is_read)) | (main_bankmachine3_cmd_valid & main_bankmachine3_cmd_payload_is_read)) | (main_bankmachine4_cmd_valid & main_bankmachine4_cmd_payload_is_read)) | (main_bankmachine5_cmd_valid & main_bankmachine5_cmd_payload_is_read)) | (main_bankmachine6_cmd_valid & main_bankmachine6_cmd_payload_is_read)) | (main_bankmachine7_cmd_valid & main_bankmachine7_cmd_payload_is_read);
	assign main_write_available = (((((((main_bankmachine0_cmd_valid & main_bankmachine0_cmd_payload_is_write) | (main_bankmachine1_cmd_valid & main_bankmachine1_cmd_payload_is_write)) | (main_bankmachine2_cmd_valid & main_bankmachine2_cmd_payload_is_write)) | (main_bankmachine3_cmd_valid & main_bankmachine3_cmd_payload_is_write)) | (main_bankmachine4_cmd_valid & main_bankmachine4_cmd_payload_is_write)) | (main_bankmachine5_cmd_valid & main_bankmachine5_cmd_payload_is_write)) | (main_bankmachine6_cmd_valid & main_bankmachine6_cmd_payload_is_write)) | (main_bankmachine7_cmd_valid & main_bankmachine7_cmd_payload_is_write);
	assign main_max_time0 = main_time0 == 1'd0;
	assign main_max_time1 = main_time1 == 1'd0;
	assign main_bankmachine0_refresh_req = main_cmd_valid;
	assign main_bankmachine1_refresh_req = main_cmd_valid;
	assign main_bankmachine2_refresh_req = main_cmd_valid;
	assign main_bankmachine3_refresh_req = main_cmd_valid;
	assign main_bankmachine4_refresh_req = main_cmd_valid;
	assign main_bankmachine5_refresh_req = main_cmd_valid;
	assign main_bankmachine6_refresh_req = main_cmd_valid;
	assign main_bankmachine7_refresh_req = main_cmd_valid;
	assign main_go_to_refresh = ((((((main_bankmachine0_refresh_gnt & main_bankmachine1_refresh_gnt) & main_bankmachine2_refresh_gnt) & main_bankmachine3_refresh_gnt) & main_bankmachine4_refresh_gnt) & main_bankmachine5_refresh_gnt) & main_bankmachine6_refresh_gnt) & main_bankmachine7_refresh_gnt;
	assign main_interface_rdata = {main_dfi_p3_rddata, main_dfi_p2_rddata, main_dfi_p1_rddata, main_dfi_p0_rddata};
	assign {main_dfi_p3_wrdata, main_dfi_p2_wrdata, main_dfi_p1_wrdata, main_dfi_p0_wrdata} = main_interface_wdata;
	assign {main_dfi_p3_wrdata, main_dfi_p2_wrdata, main_dfi_p1_wrdata, main_dfi_p0_wrdata} = main_interface_wdata;
	assign {main_dfi_p3_wrdata, main_dfi_p2_wrdata, main_dfi_p1_wrdata, main_dfi_p0_wrdata} = main_interface_wdata;
	assign {main_dfi_p3_wrdata, main_dfi_p2_wrdata, main_dfi_p1_wrdata, main_dfi_p0_wrdata} = main_interface_wdata;
	assign {main_dfi_p3_wrdata_mask, main_dfi_p2_wrdata_mask, main_dfi_p1_wrdata_mask, main_dfi_p0_wrdata_mask} = ~main_interface_wdata_we;
	assign {main_dfi_p3_wrdata_mask, main_dfi_p2_wrdata_mask, main_dfi_p1_wrdata_mask, main_dfi_p0_wrdata_mask} = ~main_interface_wdata_we;
	assign {main_dfi_p3_wrdata_mask, main_dfi_p2_wrdata_mask, main_dfi_p1_wrdata_mask, main_dfi_p0_wrdata_mask} = ~main_interface_wdata_we;
	assign {main_dfi_p3_wrdata_mask, main_dfi_p2_wrdata_mask, main_dfi_p1_wrdata_mask, main_dfi_p0_wrdata_mask} = ~main_interface_wdata_we;
	always @(*) begin
		main_choose_cmd_valids <= 8'd0;
		main_choose_cmd_valids[0] <= main_bankmachine0_cmd_valid & (((main_bankmachine0_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine0_cmd_payload_ras & ~main_bankmachine0_cmd_payload_cas) & ~main_bankmachine0_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine0_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine0_cmd_payload_is_write == main_choose_cmd_want_writes)));
		main_choose_cmd_valids[1] <= main_bankmachine1_cmd_valid & (((main_bankmachine1_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine1_cmd_payload_ras & ~main_bankmachine1_cmd_payload_cas) & ~main_bankmachine1_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine1_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine1_cmd_payload_is_write == main_choose_cmd_want_writes)));
		main_choose_cmd_valids[2] <= main_bankmachine2_cmd_valid & (((main_bankmachine2_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine2_cmd_payload_ras & ~main_bankmachine2_cmd_payload_cas) & ~main_bankmachine2_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine2_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine2_cmd_payload_is_write == main_choose_cmd_want_writes)));
		main_choose_cmd_valids[3] <= main_bankmachine3_cmd_valid & (((main_bankmachine3_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine3_cmd_payload_ras & ~main_bankmachine3_cmd_payload_cas) & ~main_bankmachine3_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine3_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine3_cmd_payload_is_write == main_choose_cmd_want_writes)));
		main_choose_cmd_valids[4] <= main_bankmachine4_cmd_valid & (((main_bankmachine4_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine4_cmd_payload_ras & ~main_bankmachine4_cmd_payload_cas) & ~main_bankmachine4_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine4_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine4_cmd_payload_is_write == main_choose_cmd_want_writes)));
		main_choose_cmd_valids[5] <= main_bankmachine5_cmd_valid & (((main_bankmachine5_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine5_cmd_payload_ras & ~main_bankmachine5_cmd_payload_cas) & ~main_bankmachine5_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine5_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine5_cmd_payload_is_write == main_choose_cmd_want_writes)));
		main_choose_cmd_valids[6] <= main_bankmachine6_cmd_valid & (((main_bankmachine6_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine6_cmd_payload_ras & ~main_bankmachine6_cmd_payload_cas) & ~main_bankmachine6_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine6_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine6_cmd_payload_is_write == main_choose_cmd_want_writes)));
		main_choose_cmd_valids[7] <= main_bankmachine7_cmd_valid & (((main_bankmachine7_cmd_payload_is_cmd & main_choose_cmd_want_cmds) & (~((main_bankmachine7_cmd_payload_ras & ~main_bankmachine7_cmd_payload_cas) & ~main_bankmachine7_cmd_payload_we) | main_choose_cmd_want_activates)) | ((main_bankmachine7_cmd_payload_is_read == main_choose_cmd_want_reads) & (main_bankmachine7_cmd_payload_is_write == main_choose_cmd_want_writes)));
	end
	assign main_choose_cmd_request = main_choose_cmd_valids;
	assign main_choose_cmd_cmd_valid = builder_rhs_array_muxed0;
	assign main_choose_cmd_cmd_payload_a = builder_rhs_array_muxed1;
	assign main_choose_cmd_cmd_payload_ba = builder_rhs_array_muxed2;
	assign main_choose_cmd_cmd_payload_is_read = builder_rhs_array_muxed3;
	assign main_choose_cmd_cmd_payload_is_write = builder_rhs_array_muxed4;
	assign main_choose_cmd_cmd_payload_is_cmd = builder_rhs_array_muxed5;
	always @(*) begin
		main_choose_cmd_cmd_payload_cas <= 1'd0;
		if (main_choose_cmd_cmd_valid)
			main_choose_cmd_cmd_payload_cas <= builder_t_array_muxed0;
	end
	always @(*) begin
		main_choose_cmd_cmd_payload_ras <= 1'd0;
		if (main_choose_cmd_cmd_valid)
			main_choose_cmd_cmd_payload_ras <= builder_t_array_muxed1;
	end
	always @(*) begin
		main_choose_cmd_cmd_payload_we <= 1'd0;
		if (main_choose_cmd_cmd_valid)
			main_choose_cmd_cmd_payload_we <= builder_t_array_muxed2;
	end
	always @(*) begin
		main_bankmachine0_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 1'd0))
			main_bankmachine0_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 1'd0))
			main_bankmachine0_cmd_ready <= 1'd1;
	end
	always @(*) begin
		main_bankmachine1_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 1'd1))
			main_bankmachine1_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 1'd1))
			main_bankmachine1_cmd_ready <= 1'd1;
	end
	always @(*) begin
		main_bankmachine2_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 2'd2))
			main_bankmachine2_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 2'd2))
			main_bankmachine2_cmd_ready <= 1'd1;
	end
	always @(*) begin
		main_bankmachine3_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 2'd3))
			main_bankmachine3_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 2'd3))
			main_bankmachine3_cmd_ready <= 1'd1;
	end
	always @(*) begin
		main_bankmachine4_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 3'd4))
			main_bankmachine4_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 3'd4))
			main_bankmachine4_cmd_ready <= 1'd1;
	end
	always @(*) begin
		main_bankmachine5_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 3'd5))
			main_bankmachine5_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 3'd5))
			main_bankmachine5_cmd_ready <= 1'd1;
	end
	always @(*) begin
		main_bankmachine6_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 3'd6))
			main_bankmachine6_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 3'd6))
			main_bankmachine6_cmd_ready <= 1'd1;
	end
	always @(*) begin
		main_bankmachine7_cmd_ready <= 1'd0;
		if ((main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & (main_choose_cmd_grant == 3'd7))
			main_bankmachine7_cmd_ready <= 1'd1;
		if ((main_choose_req_cmd_valid & main_choose_req_cmd_ready) & (main_choose_req_grant == 3'd7))
			main_bankmachine7_cmd_ready <= 1'd1;
	end
	assign main_choose_cmd_ce = main_choose_cmd_cmd_ready | ~main_choose_cmd_cmd_valid;
	always @(*) begin
		main_choose_req_valids <= 8'd0;
		main_choose_req_valids[0] <= main_bankmachine0_cmd_valid & (((main_bankmachine0_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine0_cmd_payload_ras & ~main_bankmachine0_cmd_payload_cas) & ~main_bankmachine0_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine0_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine0_cmd_payload_is_write == main_choose_req_want_writes)));
		main_choose_req_valids[1] <= main_bankmachine1_cmd_valid & (((main_bankmachine1_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine1_cmd_payload_ras & ~main_bankmachine1_cmd_payload_cas) & ~main_bankmachine1_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine1_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine1_cmd_payload_is_write == main_choose_req_want_writes)));
		main_choose_req_valids[2] <= main_bankmachine2_cmd_valid & (((main_bankmachine2_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine2_cmd_payload_ras & ~main_bankmachine2_cmd_payload_cas) & ~main_bankmachine2_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine2_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine2_cmd_payload_is_write == main_choose_req_want_writes)));
		main_choose_req_valids[3] <= main_bankmachine3_cmd_valid & (((main_bankmachine3_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine3_cmd_payload_ras & ~main_bankmachine3_cmd_payload_cas) & ~main_bankmachine3_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine3_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine3_cmd_payload_is_write == main_choose_req_want_writes)));
		main_choose_req_valids[4] <= main_bankmachine4_cmd_valid & (((main_bankmachine4_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine4_cmd_payload_ras & ~main_bankmachine4_cmd_payload_cas) & ~main_bankmachine4_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine4_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine4_cmd_payload_is_write == main_choose_req_want_writes)));
		main_choose_req_valids[5] <= main_bankmachine5_cmd_valid & (((main_bankmachine5_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine5_cmd_payload_ras & ~main_bankmachine5_cmd_payload_cas) & ~main_bankmachine5_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine5_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine5_cmd_payload_is_write == main_choose_req_want_writes)));
		main_choose_req_valids[6] <= main_bankmachine6_cmd_valid & (((main_bankmachine6_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine6_cmd_payload_ras & ~main_bankmachine6_cmd_payload_cas) & ~main_bankmachine6_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine6_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine6_cmd_payload_is_write == main_choose_req_want_writes)));
		main_choose_req_valids[7] <= main_bankmachine7_cmd_valid & (((main_bankmachine7_cmd_payload_is_cmd & main_choose_req_want_cmds) & (~((main_bankmachine7_cmd_payload_ras & ~main_bankmachine7_cmd_payload_cas) & ~main_bankmachine7_cmd_payload_we) | main_choose_req_want_activates)) | ((main_bankmachine7_cmd_payload_is_read == main_choose_req_want_reads) & (main_bankmachine7_cmd_payload_is_write == main_choose_req_want_writes)));
	end
	assign main_choose_req_request = main_choose_req_valids;
	assign main_choose_req_cmd_valid = builder_rhs_array_muxed6;
	assign main_choose_req_cmd_payload_a = builder_rhs_array_muxed7;
	assign main_choose_req_cmd_payload_ba = builder_rhs_array_muxed8;
	assign main_choose_req_cmd_payload_is_read = builder_rhs_array_muxed9;
	assign main_choose_req_cmd_payload_is_write = builder_rhs_array_muxed10;
	assign main_choose_req_cmd_payload_is_cmd = builder_rhs_array_muxed11;
	always @(*) begin
		main_choose_req_cmd_payload_cas <= 1'd0;
		if (main_choose_req_cmd_valid)
			main_choose_req_cmd_payload_cas <= builder_t_array_muxed3;
	end
	always @(*) begin
		main_choose_req_cmd_payload_ras <= 1'd0;
		if (main_choose_req_cmd_valid)
			main_choose_req_cmd_payload_ras <= builder_t_array_muxed4;
	end
	always @(*) begin
		main_choose_req_cmd_payload_we <= 1'd0;
		if (main_choose_req_cmd_valid)
			main_choose_req_cmd_payload_we <= builder_t_array_muxed5;
	end
	assign main_choose_req_ce = main_choose_req_cmd_ready | ~main_choose_req_cmd_valid;
	assign main_dfi_p0_reset_n = 1'd1;
	assign main_dfi_p0_cke = {main_steerer0};
	assign main_dfi_p0_odt = {main_steerer1};
	assign main_dfi_p1_reset_n = 1'd1;
	assign main_dfi_p1_cke = {main_steerer2};
	assign main_dfi_p1_odt = {main_steerer3};
	assign main_dfi_p2_reset_n = 1'd1;
	assign main_dfi_p2_cke = {main_steerer4};
	assign main_dfi_p2_odt = {main_steerer5};
	assign main_dfi_p3_reset_n = 1'd1;
	assign main_dfi_p3_cke = {main_steerer6};
	assign main_dfi_p3_odt = {main_steerer7};
	assign main_tfawcon_count = (main_tfawcon_window[0] + main_tfawcon_window[1]) + main_tfawcon_window[2];
	always @(*) begin
		builder_litedramcore_multiplexer_next_state <= 4'd0;
		builder_litedramcore_multiplexer_next_state <= builder_litedramcore_multiplexer_state;
		case (builder_litedramcore_multiplexer_state)
			1'd1: begin
				if (main_read_available) begin
					if (~main_write_available | main_max_time1)
						builder_litedramcore_multiplexer_next_state <= 2'd3;
				end
				if (main_go_to_refresh)
					builder_litedramcore_multiplexer_next_state <= 2'd2;
			end
			2'd2:
				if (main_cmd_last)
					builder_litedramcore_multiplexer_next_state <= 1'd0;
			2'd3:
				if (main_twtrcon_ready)
					builder_litedramcore_multiplexer_next_state <= 1'd0;
			3'd4: builder_litedramcore_multiplexer_next_state <= 3'd5;
			3'd5: builder_litedramcore_multiplexer_next_state <= 3'd6;
			3'd6: builder_litedramcore_multiplexer_next_state <= 3'd7;
			3'd7: builder_litedramcore_multiplexer_next_state <= 4'd8;
			4'd8: builder_litedramcore_multiplexer_next_state <= 4'd9;
			4'd9: builder_litedramcore_multiplexer_next_state <= 4'd10;
			4'd10: builder_litedramcore_multiplexer_next_state <= 1'd1;
			default: begin
				if (main_write_available) begin
					if (~main_read_available | main_max_time0)
						builder_litedramcore_multiplexer_next_state <= 3'd4;
				end
				if (main_go_to_refresh)
					builder_litedramcore_multiplexer_next_state <= 2'd2;
			end
		endcase
	end
	always @(*) begin
		main_choose_req_cmd_ready <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: main_choose_req_cmd_ready <= main_cas_allowed;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: main_choose_req_cmd_ready <= main_cas_allowed;
		endcase
	end
	always @(*) begin
		main_en1 <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: main_en1 <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_steerer_sel0 <= 2'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: begin
				main_steerer_sel0 <= 1'd0;
				if (main_a7ddrphy_wrphase_storage == 1'd0)
					main_steerer_sel0 <= 2'd2;
				if (main_wrcmdphase == 1'd0)
					main_steerer_sel0 <= 1'd1;
			end
			2'd2: main_steerer_sel0 <= 2'd3;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: begin
				main_steerer_sel0 <= 1'd0;
				if (main_a7ddrphy_rdphase_storage == 1'd0)
					main_steerer_sel0 <= 2'd2;
				if (main_rdcmdphase == 1'd0)
					main_steerer_sel0 <= 1'd1;
			end
		endcase
	end
	always @(*) begin
		main_cmd_ready <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1:
				;
			2'd2: main_cmd_ready <= 1'd1;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_steerer_sel1 <= 2'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: begin
				main_steerer_sel1 <= 1'd0;
				if (main_a7ddrphy_wrphase_storage == 1'd1)
					main_steerer_sel1 <= 2'd2;
				if (main_wrcmdphase == 1'd1)
					main_steerer_sel1 <= 1'd1;
			end
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: begin
				main_steerer_sel1 <= 1'd0;
				if (main_a7ddrphy_rdphase_storage == 1'd1)
					main_steerer_sel1 <= 2'd2;
				if (main_rdcmdphase == 1'd1)
					main_steerer_sel1 <= 1'd1;
			end
		endcase
	end
	always @(*) begin
		main_en0 <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: main_en0 <= 1'd1;
		endcase
	end
	always @(*) begin
		main_steerer_sel2 <= 2'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: begin
				main_steerer_sel2 <= 1'd0;
				if (main_a7ddrphy_wrphase_storage == 2'd2)
					main_steerer_sel2 <= 2'd2;
				if (main_wrcmdphase == 2'd2)
					main_steerer_sel2 <= 1'd1;
			end
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: begin
				main_steerer_sel2 <= 1'd0;
				if (main_a7ddrphy_rdphase_storage == 2'd2)
					main_steerer_sel2 <= 2'd2;
				if (main_rdcmdphase == 2'd2)
					main_steerer_sel2 <= 1'd1;
			end
		endcase
	end
	always @(*) begin
		main_choose_cmd_want_activates <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: main_choose_cmd_want_activates <= main_ras_allowed;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: main_choose_cmd_want_activates <= main_ras_allowed;
		endcase
	end
	always @(*) begin
		main_steerer_sel3 <= 2'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: begin
				main_steerer_sel3 <= 1'd0;
				if (main_a7ddrphy_wrphase_storage == 2'd3)
					main_steerer_sel3 <= 2'd2;
				if (main_wrcmdphase == 2'd3)
					main_steerer_sel3 <= 1'd1;
			end
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: begin
				main_steerer_sel3 <= 1'd0;
				if (main_a7ddrphy_rdphase_storage == 2'd3)
					main_steerer_sel3 <= 2'd2;
				if (main_rdcmdphase == 2'd3)
					main_steerer_sel3 <= 1'd1;
			end
		endcase
	end
	always @(*) begin
		main_choose_cmd_cmd_ready <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: main_choose_cmd_cmd_ready <= ~((main_choose_cmd_cmd_payload_ras & ~main_choose_cmd_cmd_payload_cas) & ~main_choose_cmd_cmd_payload_we) | main_ras_allowed;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: main_choose_cmd_cmd_ready <= ~((main_choose_cmd_cmd_payload_ras & ~main_choose_cmd_cmd_payload_cas) & ~main_choose_cmd_cmd_payload_we) | main_ras_allowed;
		endcase
	end
	always @(*) begin
		main_choose_req_want_reads <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default: main_choose_req_want_reads <= 1'd1;
		endcase
	end
	always @(*) begin
		main_choose_req_want_writes <= 1'd0;
		case (builder_litedramcore_multiplexer_state)
			1'd1: main_choose_req_want_writes <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			3'd4:
				;
			3'd5:
				;
			3'd6:
				;
			3'd7:
				;
			4'd8:
				;
			4'd9:
				;
			4'd10:
				;
			default:
				;
		endcase
	end
	assign builder_litedramcore_roundrobin0_request = {((main_port_cmd_payload_addr[9:7] == 1'd0) & ~(((((((builder_litedramcore_locked0 | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin0_ce = ~main_interface_bank0_valid & ~main_interface_bank0_lock;
	assign main_interface_bank0_addr = builder_rhs_array_muxed12;
	assign main_interface_bank0_we = builder_rhs_array_muxed13;
	assign main_interface_bank0_valid = builder_rhs_array_muxed14;
	assign builder_litedramcore_roundrobin1_request = {((main_port_cmd_payload_addr[9:7] == 1'd1) & ~(((((((builder_litedramcore_locked1 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin1_ce = ~main_interface_bank1_valid & ~main_interface_bank1_lock;
	assign main_interface_bank1_addr = builder_rhs_array_muxed15;
	assign main_interface_bank1_we = builder_rhs_array_muxed16;
	assign main_interface_bank1_valid = builder_rhs_array_muxed17;
	assign builder_litedramcore_roundrobin2_request = {((main_port_cmd_payload_addr[9:7] == 2'd2) & ~(((((((builder_litedramcore_locked2 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin2_ce = ~main_interface_bank2_valid & ~main_interface_bank2_lock;
	assign main_interface_bank2_addr = builder_rhs_array_muxed18;
	assign main_interface_bank2_we = builder_rhs_array_muxed19;
	assign main_interface_bank2_valid = builder_rhs_array_muxed20;
	assign builder_litedramcore_roundrobin3_request = {((main_port_cmd_payload_addr[9:7] == 2'd3) & ~(((((((builder_litedramcore_locked3 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin3_ce = ~main_interface_bank3_valid & ~main_interface_bank3_lock;
	assign main_interface_bank3_addr = builder_rhs_array_muxed21;
	assign main_interface_bank3_we = builder_rhs_array_muxed22;
	assign main_interface_bank3_valid = builder_rhs_array_muxed23;
	assign builder_litedramcore_roundrobin4_request = {((main_port_cmd_payload_addr[9:7] == 3'd4) & ~(((((((builder_litedramcore_locked4 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin4_ce = ~main_interface_bank4_valid & ~main_interface_bank4_lock;
	assign main_interface_bank4_addr = builder_rhs_array_muxed24;
	assign main_interface_bank4_we = builder_rhs_array_muxed25;
	assign main_interface_bank4_valid = builder_rhs_array_muxed26;
	assign builder_litedramcore_roundrobin5_request = {((main_port_cmd_payload_addr[9:7] == 3'd5) & ~(((((((builder_litedramcore_locked5 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin5_ce = ~main_interface_bank5_valid & ~main_interface_bank5_lock;
	assign main_interface_bank5_addr = builder_rhs_array_muxed27;
	assign main_interface_bank5_we = builder_rhs_array_muxed28;
	assign main_interface_bank5_valid = builder_rhs_array_muxed29;
	assign builder_litedramcore_roundrobin6_request = {((main_port_cmd_payload_addr[9:7] == 3'd6) & ~(((((((builder_litedramcore_locked6 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin6_ce = ~main_interface_bank6_valid & ~main_interface_bank6_lock;
	assign main_interface_bank6_addr = builder_rhs_array_muxed30;
	assign main_interface_bank6_we = builder_rhs_array_muxed31;
	assign main_interface_bank6_valid = builder_rhs_array_muxed32;
	assign builder_litedramcore_roundrobin7_request = {((main_port_cmd_payload_addr[9:7] == 3'd7) & ~(((((((builder_litedramcore_locked7 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0)))) & main_port_cmd_valid};
	assign builder_litedramcore_roundrobin7_ce = ~main_interface_bank7_valid & ~main_interface_bank7_lock;
	assign main_interface_bank7_addr = builder_rhs_array_muxed33;
	assign main_interface_bank7_we = builder_rhs_array_muxed34;
	assign main_interface_bank7_valid = builder_rhs_array_muxed35;
	assign main_port_cmd_ready = (((((((1'd0 | (((builder_litedramcore_roundrobin0_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 1'd0) & ~(((((((builder_litedramcore_locked0 | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0))))) & main_interface_bank0_ready)) | (((builder_litedramcore_roundrobin1_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 1'd1) & ~(((((((builder_litedramcore_locked1 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0))))) & main_interface_bank1_ready)) | (((builder_litedramcore_roundrobin2_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 2'd2) & ~(((((((builder_litedramcore_locked2 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0))))) & main_interface_bank2_ready)) | (((builder_litedramcore_roundrobin3_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 2'd3) & ~(((((((builder_litedramcore_locked3 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0))))) & main_interface_bank3_ready)) | (((builder_litedramcore_roundrobin4_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 3'd4) & ~(((((((builder_litedramcore_locked4 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0))))) & main_interface_bank4_ready)) | (((builder_litedramcore_roundrobin5_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 3'd5) & ~(((((((builder_litedramcore_locked5 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0))))) & main_interface_bank5_ready)) | (((builder_litedramcore_roundrobin6_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 3'd6) & ~(((((((builder_litedramcore_locked6 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0))))) & main_interface_bank6_ready)) | (((builder_litedramcore_roundrobin7_grant == 1'd0) & ((main_port_cmd_payload_addr[9:7] == 3'd7) & ~(((((((builder_litedramcore_locked7 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))))) & main_interface_bank7_ready);
	assign main_port_wdata_ready = builder_litedramcore_new_master_wdata_ready1;
	assign main_port_rdata_valid = builder_litedramcore_new_master_rdata_valid8;
	always @(*) begin
		main_interface_wdata <= 128'd0;
		case ({builder_litedramcore_new_master_wdata_ready1})
			1'd1: main_interface_wdata <= main_port_wdata_payload_data;
			default: main_interface_wdata <= 1'd0;
		endcase
	end
	always @(*) begin
		main_interface_wdata_we <= 16'd0;
		case ({builder_litedramcore_new_master_wdata_ready1})
			1'd1: main_interface_wdata_we <= main_port_wdata_payload_we;
			default: main_interface_wdata_we <= 1'd0;
		endcase
	end
	assign main_port_rdata_payload_data = main_interface_rdata;
	assign main_litedramnativeportconverter_cmd_buffer_source_ready = main_litedramnativeportconverter_wdata_finished | main_litedramnativeportconverter_rdata_finished;
	assign main_litedramnativeportconverter_addr_changed = main_litedramnativeportconverter_cmd_addr[25:2] != main_new_port_cmd_payload_addr[25:2];
	assign main_litedramnativeportconverter_rw_collision = (main_litedramnativeportconverter_cmd_we & (main_new_port_cmd_valid & ~main_new_port_cmd_payload_we)) & ~main_litedramnativeportconverter_addr_changed;
	assign main_litedramnativeportconverter_next_cmd = (((main_litedramnativeportconverter_addr_changed | (main_litedramnativeportconverter_cmd_we != main_new_port_cmd_payload_we)) | (main_litedramnativeportconverter_sel == 4'd15)) | main_litedramnativeportconverter_cmd_last) | main_new_port_flush;
	assign main_litedramnativeportconverter_rdata_fifo_sink_valid = main_port_rdata_valid;
	assign main_port_rdata_ready = main_litedramnativeportconverter_rdata_fifo_sink_ready;
	assign main_litedramnativeportconverter_rdata_fifo_sink_first = main_port_rdata_first;
	assign main_litedramnativeportconverter_rdata_fifo_sink_last = main_port_rdata_last;
	assign main_litedramnativeportconverter_rdata_fifo_sink_payload_data = main_port_rdata_payload_data;
	assign main_litedramnativeportconverter_rdata_converter_sink_valid = main_litedramnativeportconverter_rdata_fifo_source_valid;
	assign main_litedramnativeportconverter_rdata_fifo_source_ready = main_litedramnativeportconverter_rdata_converter_sink_ready;
	assign main_litedramnativeportconverter_rdata_converter_sink_first = main_litedramnativeportconverter_rdata_fifo_source_first;
	assign main_litedramnativeportconverter_rdata_converter_sink_last = main_litedramnativeportconverter_rdata_fifo_source_last;
	assign main_litedramnativeportconverter_rdata_converter_sink_payload_data = main_litedramnativeportconverter_rdata_fifo_source_payload_data;
	assign main_litedramnativeportconverter_rdata_chunk_valid = (main_litedramnativeportconverter_cmd_buffer_source_payload_sel & main_litedramnativeportconverter_rdata_chunk) != 1'd0;
	always @(*) begin
		main_new_port_rdata_valid <= 1'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & ~main_litedramnativeportconverter_cmd_buffer_source_payload_we) begin
			if (main_litedramnativeportconverter_rdata_chunk_valid)
				main_new_port_rdata_valid <= main_litedramnativeportconverter_rdata_converter_source_valid;
		end
	end
	always @(*) begin
		main_new_port_rdata_payload_data <= 32'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & ~main_litedramnativeportconverter_cmd_buffer_source_payload_we) begin
			if (main_litedramnativeportconverter_rdata_chunk_valid)
				main_new_port_rdata_payload_data <= main_litedramnativeportconverter_rdata_converter_source_payload_data;
		end
	end
	always @(*) begin
		main_litedramnativeportconverter_rdata_converter_source_ready <= 1'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & ~main_litedramnativeportconverter_cmd_buffer_source_payload_we) begin
			if (main_litedramnativeportconverter_rdata_chunk_valid)
				main_litedramnativeportconverter_rdata_converter_source_ready <= main_new_port_rdata_ready;
			else
				main_litedramnativeportconverter_rdata_converter_source_ready <= 1'd1;
		end
	end
	always @(*) begin
		main_litedramnativeportconverter_rdata_finished <= 1'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & ~main_litedramnativeportconverter_cmd_buffer_source_payload_we)
			main_litedramnativeportconverter_rdata_finished <= (main_litedramnativeportconverter_rdata_converter_source_valid & main_litedramnativeportconverter_rdata_converter_source_ready) & main_litedramnativeportconverter_rdata_chunk[3];
	end
	assign main_litedramnativeportconverter_wdata_fifo_sink_valid = main_new_port_wdata_valid;
	assign main_new_port_wdata_ready = main_litedramnativeportconverter_wdata_fifo_sink_ready;
	assign main_litedramnativeportconverter_wdata_fifo_sink_first = main_new_port_wdata_first;
	assign main_litedramnativeportconverter_wdata_fifo_sink_last = main_new_port_wdata_last;
	assign main_litedramnativeportconverter_wdata_fifo_sink_payload_data = main_new_port_wdata_payload_data;
	assign main_litedramnativeportconverter_wdata_fifo_sink_payload_we = main_new_port_wdata_payload_we;
	assign main_port_wdata_valid = main_litedramnativeportconverter_wdata_buffer_source_source_valid;
	assign main_litedramnativeportconverter_wdata_buffer_source_source_ready = main_port_wdata_ready;
	assign main_port_wdata_first = main_litedramnativeportconverter_wdata_buffer_source_source_first;
	assign main_port_wdata_last = main_litedramnativeportconverter_wdata_buffer_source_source_last;
	assign main_port_wdata_payload_data = main_litedramnativeportconverter_wdata_buffer_source_source_payload_data;
	assign main_port_wdata_payload_we = main_litedramnativeportconverter_wdata_buffer_source_source_payload_we;
	assign main_litedramnativeportconverter_wdata_chunk_valid = (main_litedramnativeportconverter_cmd_buffer_source_payload_sel & main_litedramnativeportconverter_wdata_chunk) != 1'd0;
	always @(*) begin
		main_litedramnativeportconverter_wdata_converter_sink_payload_we <= 4'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & main_litedramnativeportconverter_cmd_buffer_source_payload_we) begin
			if (main_litedramnativeportconverter_wdata_chunk_valid)
				main_litedramnativeportconverter_wdata_converter_sink_payload_we <= main_litedramnativeportconverter_wdata_fifo_source_payload_we;
		end
	end
	always @(*) begin
		main_litedramnativeportconverter_wdata_converter_sink_valid <= 1'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & main_litedramnativeportconverter_cmd_buffer_source_payload_we) begin
			if (main_litedramnativeportconverter_wdata_chunk_valid)
				main_litedramnativeportconverter_wdata_converter_sink_valid <= main_litedramnativeportconverter_wdata_fifo_source_valid;
			else
				main_litedramnativeportconverter_wdata_converter_sink_valid <= 1'd1;
		end
	end
	always @(*) begin
		main_litedramnativeportconverter_wdata_fifo_source_ready <= 1'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & main_litedramnativeportconverter_cmd_buffer_source_payload_we) begin
			if (main_litedramnativeportconverter_wdata_chunk_valid)
				main_litedramnativeportconverter_wdata_fifo_source_ready <= main_litedramnativeportconverter_wdata_converter_sink_ready;
		end
	end
	always @(*) begin
		main_litedramnativeportconverter_wdata_converter_sink_payload_data <= 32'd0;
		if (main_litedramnativeportconverter_cmd_buffer_source_valid & main_litedramnativeportconverter_cmd_buffer_source_payload_we) begin
			if (main_litedramnativeportconverter_wdata_chunk_valid)
				main_litedramnativeportconverter_wdata_converter_sink_payload_data <= main_litedramnativeportconverter_wdata_fifo_source_payload_data;
		end
	end
	assign main_litedramnativeportconverter_wdata_buffer_sink_sink_valid = main_litedramnativeportconverter_wdata_converter_source_valid;
	assign main_litedramnativeportconverter_wdata_buffer_sink_sink_payload_data = main_litedramnativeportconverter_wdata_converter_source_payload_data;
	assign main_litedramnativeportconverter_wdata_buffer_sink_sink_payload_we = main_litedramnativeportconverter_wdata_converter_source_payload_we & main_litedramnativeportconverter_wdata_sel;
	assign main_litedramnativeportconverter_wdata_converter_source_ready = main_litedramnativeportconverter_wdata_buffer_sink_sink_ready;
	assign main_litedramnativeportconverter_wdata_finished = (main_litedramnativeportconverter_wdata_converter_sink_valid & main_litedramnativeportconverter_wdata_converter_sink_ready) & main_litedramnativeportconverter_wdata_chunk[3];
	assign main_litedramnativeportconverter_cmd_buffer_source_valid = main_litedramnativeportconverter_cmd_buffer_sink_valid;
	assign main_litedramnativeportconverter_cmd_buffer_sink_ready = main_litedramnativeportconverter_cmd_buffer_source_ready;
	assign main_litedramnativeportconverter_cmd_buffer_source_first = main_litedramnativeportconverter_cmd_buffer_sink_first;
	assign main_litedramnativeportconverter_cmd_buffer_source_last = main_litedramnativeportconverter_cmd_buffer_sink_last;
	assign main_litedramnativeportconverter_cmd_buffer_source_payload_sel = main_litedramnativeportconverter_cmd_buffer_sink_payload_sel;
	assign main_litedramnativeportconverter_cmd_buffer_source_payload_we = main_litedramnativeportconverter_cmd_buffer_sink_payload_we;
	always @(*) begin
		builder_litedramcore_next_state <= 2'd0;
		builder_litedramcore_next_state <= builder_litedramcore_state;
		case (builder_litedramcore_state)
			1'd1:
				if (main_port_cmd_ready) begin
					if (main_litedramnativeportconverter_cmd_we)
						builder_litedramcore_next_state <= 1'd0;
					else
						builder_litedramcore_next_state <= 2'd2;
				end
			2'd2:
				if (main_litedramnativeportconverter_next_cmd)
					builder_litedramcore_next_state <= 2'd3;
			2'd3:
				if (main_litedramnativeportconverter_cmd_buffer_sink_ready) begin
					if (main_litedramnativeportconverter_cmd_we)
						builder_litedramcore_next_state <= 1'd1;
					else
						builder_litedramcore_next_state <= 1'd0;
				end
			default:
				if (main_new_port_cmd_ready) begin
					if (main_new_port_cmd_payload_we)
						builder_litedramcore_next_state <= 2'd2;
					else
						builder_litedramcore_next_state <= 1'd1;
				end
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_sel_litedramcore_next_value_ce3 <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				if (main_litedramnativeportconverter_next_cmd)
					;
				else if (main_new_port_cmd_valid)
					main_litedramnativeportconverter_sel_litedramcore_next_value_ce3 <= 1'd1;
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_sel_litedramcore_next_value_ce3 <= 1'd1;
		endcase
	end
	always @(*) begin
		main_port_cmd_valid <= 1'd0;
		case (builder_litedramcore_state)
			1'd1: main_port_cmd_valid <= 1'd1;
			2'd2:
				;
			2'd3:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_new_port_cmd_ready <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				if (main_litedramnativeportconverter_next_cmd)
					;
				else
					main_new_port_cmd_ready <= main_new_port_cmd_valid;
			2'd3:
				;
			default: main_new_port_cmd_ready <= main_new_port_cmd_valid & ~main_litedramnativeportconverter_read_lock;
		endcase
	end
	always @(*) begin
		main_port_cmd_payload_we <= 1'd0;
		case (builder_litedramcore_state)
			1'd1: main_port_cmd_payload_we <= main_litedramnativeportconverter_cmd_we;
			2'd2:
				;
			2'd3:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_port_cmd_payload_addr <= 24'd0;
		case (builder_litedramcore_state)
			1'd1: main_port_cmd_payload_addr <= main_litedramnativeportconverter_cmd_addr[25:2];
			2'd2:
				;
			2'd3:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_buffer_sink_valid <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				;
			2'd3: main_litedramnativeportconverter_cmd_buffer_sink_valid <= 1'd1;
			default:
				;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_addr_litedramcore_next_value0 <= 26'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_cmd_addr_litedramcore_next_value0 <= main_new_port_cmd_payload_addr;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_addr_litedramcore_next_value_ce0 <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_cmd_addr_litedramcore_next_value_ce0 <= 1'd1;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_buffer_sink_payload_sel <= 4'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				;
			2'd3: main_litedramnativeportconverter_cmd_buffer_sink_payload_sel <= main_litedramnativeportconverter_sel;
			default:
				;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_we_litedramcore_next_value1 <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_cmd_we_litedramcore_next_value1 <= main_new_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_buffer_sink_payload_we <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				;
			2'd3: main_litedramnativeportconverter_cmd_buffer_sink_payload_we <= main_litedramnativeportconverter_cmd_we;
			default:
				;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_we_litedramcore_next_value_ce1 <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				;
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_cmd_we_litedramcore_next_value_ce1 <= 1'd1;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_last_litedramcore_next_value2 <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				if (main_litedramnativeportconverter_next_cmd)
					;
				else
					main_litedramnativeportconverter_cmd_last_litedramcore_next_value2 <= main_new_port_cmd_last;
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_cmd_last_litedramcore_next_value2 <= main_new_port_cmd_last;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_cmd_last_litedramcore_next_value_ce2 <= 1'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				if (main_litedramnativeportconverter_next_cmd)
					;
				else
					main_litedramnativeportconverter_cmd_last_litedramcore_next_value_ce2 <= 1'd1;
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_cmd_last_litedramcore_next_value_ce2 <= 1'd1;
		endcase
	end
	always @(*) begin
		main_litedramnativeportconverter_sel_litedramcore_next_value3 <= 4'd0;
		case (builder_litedramcore_state)
			1'd1:
				;
			2'd2:
				if (main_litedramnativeportconverter_next_cmd)
					;
				else if (main_new_port_cmd_valid)
					main_litedramnativeportconverter_sel_litedramcore_next_value3 <= main_litedramnativeportconverter_sel | (1'd1 <<< main_new_port_cmd_payload_addr[1:0]);
			2'd3:
				;
			default:
				if (main_new_port_cmd_ready)
					main_litedramnativeportconverter_sel_litedramcore_next_value3 <= 1'd1 <<< main_new_port_cmd_payload_addr[1:0];
		endcase
	end
	assign main_litedramnativeportconverter_rdata_fifo_syncfifo_din = {main_litedramnativeportconverter_rdata_fifo_fifo_in_last, main_litedramnativeportconverter_rdata_fifo_fifo_in_first, main_litedramnativeportconverter_rdata_fifo_fifo_in_payload_data};
	assign {main_litedramnativeportconverter_rdata_fifo_fifo_out_last, main_litedramnativeportconverter_rdata_fifo_fifo_out_first, main_litedramnativeportconverter_rdata_fifo_fifo_out_payload_data} = main_litedramnativeportconverter_rdata_fifo_syncfifo_dout;
	assign {main_litedramnativeportconverter_rdata_fifo_fifo_out_last, main_litedramnativeportconverter_rdata_fifo_fifo_out_first, main_litedramnativeportconverter_rdata_fifo_fifo_out_payload_data} = main_litedramnativeportconverter_rdata_fifo_syncfifo_dout;
	assign {main_litedramnativeportconverter_rdata_fifo_fifo_out_last, main_litedramnativeportconverter_rdata_fifo_fifo_out_first, main_litedramnativeportconverter_rdata_fifo_fifo_out_payload_data} = main_litedramnativeportconverter_rdata_fifo_syncfifo_dout;
	assign main_litedramnativeportconverter_rdata_fifo_sink_ready = main_litedramnativeportconverter_rdata_fifo_syncfifo_writable;
	assign main_litedramnativeportconverter_rdata_fifo_syncfifo_we = main_litedramnativeportconverter_rdata_fifo_sink_valid;
	assign main_litedramnativeportconverter_rdata_fifo_fifo_in_first = main_litedramnativeportconverter_rdata_fifo_sink_first;
	assign main_litedramnativeportconverter_rdata_fifo_fifo_in_last = main_litedramnativeportconverter_rdata_fifo_sink_last;
	assign main_litedramnativeportconverter_rdata_fifo_fifo_in_payload_data = main_litedramnativeportconverter_rdata_fifo_sink_payload_data;
	assign main_litedramnativeportconverter_rdata_fifo_source_valid = main_litedramnativeportconverter_rdata_fifo_syncfifo_readable;
	assign main_litedramnativeportconverter_rdata_fifo_source_first = main_litedramnativeportconverter_rdata_fifo_fifo_out_first;
	assign main_litedramnativeportconverter_rdata_fifo_source_last = main_litedramnativeportconverter_rdata_fifo_fifo_out_last;
	assign main_litedramnativeportconverter_rdata_fifo_source_payload_data = main_litedramnativeportconverter_rdata_fifo_fifo_out_payload_data;
	assign main_litedramnativeportconverter_rdata_fifo_syncfifo_re = main_litedramnativeportconverter_rdata_fifo_source_ready;
	always @(*) begin
		main_litedramnativeportconverter_rdata_fifo_wrport_adr <= 2'd0;
		if (main_litedramnativeportconverter_rdata_fifo_replace)
			main_litedramnativeportconverter_rdata_fifo_wrport_adr <= main_litedramnativeportconverter_rdata_fifo_produce - 1'd1;
		else
			main_litedramnativeportconverter_rdata_fifo_wrport_adr <= main_litedramnativeportconverter_rdata_fifo_produce;
	end
	assign main_litedramnativeportconverter_rdata_fifo_wrport_dat_w = main_litedramnativeportconverter_rdata_fifo_syncfifo_din;
	assign main_litedramnativeportconverter_rdata_fifo_wrport_we = main_litedramnativeportconverter_rdata_fifo_syncfifo_we & (main_litedramnativeportconverter_rdata_fifo_syncfifo_writable | main_litedramnativeportconverter_rdata_fifo_replace);
	assign main_litedramnativeportconverter_rdata_fifo_do_read = main_litedramnativeportconverter_rdata_fifo_syncfifo_readable & main_litedramnativeportconverter_rdata_fifo_syncfifo_re;
	assign main_litedramnativeportconverter_rdata_fifo_rdport_adr = main_litedramnativeportconverter_rdata_fifo_consume;
	assign main_litedramnativeportconverter_rdata_fifo_syncfifo_dout = main_litedramnativeportconverter_rdata_fifo_rdport_dat_r;
	assign main_litedramnativeportconverter_rdata_fifo_syncfifo_writable = main_litedramnativeportconverter_rdata_fifo_level != 2'd3;
	assign main_litedramnativeportconverter_rdata_fifo_syncfifo_readable = main_litedramnativeportconverter_rdata_fifo_level != 1'd0;
	assign main_litedramnativeportconverter_rdata_converter_converter_sink_valid = main_litedramnativeportconverter_rdata_converter_sink_valid;
	assign main_litedramnativeportconverter_rdata_converter_converter_sink_first = main_litedramnativeportconverter_rdata_converter_sink_first;
	assign main_litedramnativeportconverter_rdata_converter_converter_sink_last = main_litedramnativeportconverter_rdata_converter_sink_last;
	assign main_litedramnativeportconverter_rdata_converter_sink_ready = main_litedramnativeportconverter_rdata_converter_converter_sink_ready;
	always @(*) begin
		main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data <= 128'd0;
		main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[31:0] <= main_litedramnativeportconverter_rdata_converter_sink_payload_data[31:0];
		main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[63:32] <= main_litedramnativeportconverter_rdata_converter_sink_payload_data[63:32];
		main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[95:64] <= main_litedramnativeportconverter_rdata_converter_sink_payload_data[95:64];
		main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[127:96] <= main_litedramnativeportconverter_rdata_converter_sink_payload_data[127:96];
	end
	assign main_litedramnativeportconverter_rdata_converter_source_valid = main_litedramnativeportconverter_rdata_converter_source_source_valid;
	assign main_litedramnativeportconverter_rdata_converter_source_first = main_litedramnativeportconverter_rdata_converter_source_source_first;
	assign main_litedramnativeportconverter_rdata_converter_source_last = main_litedramnativeportconverter_rdata_converter_source_source_last;
	assign main_litedramnativeportconverter_rdata_converter_source_source_ready = main_litedramnativeportconverter_rdata_converter_source_ready;
	assign {main_litedramnativeportconverter_rdata_converter_source_payload_data} = main_litedramnativeportconverter_rdata_converter_source_source_payload_data;
	assign main_litedramnativeportconverter_rdata_converter_source_source_valid = main_litedramnativeportconverter_rdata_converter_converter_source_valid;
	assign main_litedramnativeportconverter_rdata_converter_converter_source_ready = main_litedramnativeportconverter_rdata_converter_source_source_ready;
	assign main_litedramnativeportconverter_rdata_converter_source_source_first = main_litedramnativeportconverter_rdata_converter_converter_source_first;
	assign main_litedramnativeportconverter_rdata_converter_source_source_last = main_litedramnativeportconverter_rdata_converter_converter_source_last;
	assign main_litedramnativeportconverter_rdata_converter_source_source_payload_data = main_litedramnativeportconverter_rdata_converter_converter_source_payload_data;
	assign main_litedramnativeportconverter_rdata_converter_converter_first = main_litedramnativeportconverter_rdata_converter_converter_mux == 1'd0;
	assign main_litedramnativeportconverter_rdata_converter_converter_last = main_litedramnativeportconverter_rdata_converter_converter_mux == 2'd3;
	assign main_litedramnativeportconverter_rdata_converter_converter_source_valid = main_litedramnativeportconverter_rdata_converter_converter_sink_valid;
	assign main_litedramnativeportconverter_rdata_converter_converter_source_first = main_litedramnativeportconverter_rdata_converter_converter_sink_first & main_litedramnativeportconverter_rdata_converter_converter_first;
	assign main_litedramnativeportconverter_rdata_converter_converter_source_last = main_litedramnativeportconverter_rdata_converter_converter_sink_last & main_litedramnativeportconverter_rdata_converter_converter_last;
	assign main_litedramnativeportconverter_rdata_converter_converter_sink_ready = main_litedramnativeportconverter_rdata_converter_converter_last & main_litedramnativeportconverter_rdata_converter_converter_source_ready;
	always @(*) begin
		main_litedramnativeportconverter_rdata_converter_converter_source_payload_data <= 32'd0;
		case (main_litedramnativeportconverter_rdata_converter_converter_mux)
			1'd0: main_litedramnativeportconverter_rdata_converter_converter_source_payload_data <= main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[31:0];
			1'd1: main_litedramnativeportconverter_rdata_converter_converter_source_payload_data <= main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[63:32];
			2'd2: main_litedramnativeportconverter_rdata_converter_converter_source_payload_data <= main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[95:64];
			default: main_litedramnativeportconverter_rdata_converter_converter_source_payload_data <= main_litedramnativeportconverter_rdata_converter_converter_sink_payload_data[127:96];
		endcase
	end
	assign main_litedramnativeportconverter_rdata_converter_converter_source_payload_valid_token_count = main_litedramnativeportconverter_rdata_converter_converter_last;
	assign main_litedramnativeportconverter_wdata_converter_converter_sink_valid = main_litedramnativeportconverter_wdata_converter_sink_valid;
	assign main_litedramnativeportconverter_wdata_converter_converter_sink_first = main_litedramnativeportconverter_wdata_converter_sink_first;
	assign main_litedramnativeportconverter_wdata_converter_converter_sink_last = main_litedramnativeportconverter_wdata_converter_sink_last;
	assign main_litedramnativeportconverter_wdata_converter_sink_ready = main_litedramnativeportconverter_wdata_converter_converter_sink_ready;
	assign main_litedramnativeportconverter_wdata_converter_converter_sink_payload_data = {main_litedramnativeportconverter_wdata_converter_sink_payload_we, main_litedramnativeportconverter_wdata_converter_sink_payload_data};
	assign main_litedramnativeportconverter_wdata_converter_source_valid = main_litedramnativeportconverter_wdata_converter_source_source_valid;
	assign main_litedramnativeportconverter_wdata_converter_source_first = main_litedramnativeportconverter_wdata_converter_source_source_first;
	assign main_litedramnativeportconverter_wdata_converter_source_last = main_litedramnativeportconverter_wdata_converter_source_source_last;
	assign main_litedramnativeportconverter_wdata_converter_source_source_ready = main_litedramnativeportconverter_wdata_converter_source_ready;
	always @(*) begin
		main_litedramnativeportconverter_wdata_converter_source_payload_data <= 128'd0;
		main_litedramnativeportconverter_wdata_converter_source_payload_data[31:0] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[31:0];
		main_litedramnativeportconverter_wdata_converter_source_payload_data[63:32] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[67:36];
		main_litedramnativeportconverter_wdata_converter_source_payload_data[95:64] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[103:72];
		main_litedramnativeportconverter_wdata_converter_source_payload_data[127:96] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[139:108];
	end
	always @(*) begin
		main_litedramnativeportconverter_wdata_converter_source_payload_we <= 16'd0;
		main_litedramnativeportconverter_wdata_converter_source_payload_we[3:0] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[35:32];
		main_litedramnativeportconverter_wdata_converter_source_payload_we[7:4] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[71:68];
		main_litedramnativeportconverter_wdata_converter_source_payload_we[11:8] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[107:104];
		main_litedramnativeportconverter_wdata_converter_source_payload_we[15:12] <= main_litedramnativeportconverter_wdata_converter_source_source_payload_data[143:140];
	end
	assign main_litedramnativeportconverter_wdata_converter_source_source_valid = main_litedramnativeportconverter_wdata_converter_converter_source_valid;
	assign main_litedramnativeportconverter_wdata_converter_converter_source_ready = main_litedramnativeportconverter_wdata_converter_source_source_ready;
	assign main_litedramnativeportconverter_wdata_converter_source_source_first = main_litedramnativeportconverter_wdata_converter_converter_source_first;
	assign main_litedramnativeportconverter_wdata_converter_source_source_last = main_litedramnativeportconverter_wdata_converter_converter_source_last;
	assign main_litedramnativeportconverter_wdata_converter_source_source_payload_data = main_litedramnativeportconverter_wdata_converter_converter_source_payload_data;
	assign main_litedramnativeportconverter_wdata_converter_converter_sink_ready = ~main_litedramnativeportconverter_wdata_converter_converter_strobe_all | main_litedramnativeportconverter_wdata_converter_converter_source_ready;
	assign main_litedramnativeportconverter_wdata_converter_converter_source_valid = main_litedramnativeportconverter_wdata_converter_converter_strobe_all;
	assign main_litedramnativeportconverter_wdata_converter_converter_load_part = main_litedramnativeportconverter_wdata_converter_converter_sink_valid & main_litedramnativeportconverter_wdata_converter_converter_sink_ready;
	assign main_litedramnativeportconverter_wdata_fifo_syncfifo_din = {main_litedramnativeportconverter_wdata_fifo_fifo_in_last, main_litedramnativeportconverter_wdata_fifo_fifo_in_first, main_litedramnativeportconverter_wdata_fifo_fifo_in_payload_we, main_litedramnativeportconverter_wdata_fifo_fifo_in_payload_data};
	assign {main_litedramnativeportconverter_wdata_fifo_fifo_out_last, main_litedramnativeportconverter_wdata_fifo_fifo_out_first, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_we, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_data} = main_litedramnativeportconverter_wdata_fifo_syncfifo_dout;
	assign {main_litedramnativeportconverter_wdata_fifo_fifo_out_last, main_litedramnativeportconverter_wdata_fifo_fifo_out_first, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_we, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_data} = main_litedramnativeportconverter_wdata_fifo_syncfifo_dout;
	assign {main_litedramnativeportconverter_wdata_fifo_fifo_out_last, main_litedramnativeportconverter_wdata_fifo_fifo_out_first, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_we, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_data} = main_litedramnativeportconverter_wdata_fifo_syncfifo_dout;
	assign {main_litedramnativeportconverter_wdata_fifo_fifo_out_last, main_litedramnativeportconverter_wdata_fifo_fifo_out_first, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_we, main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_data} = main_litedramnativeportconverter_wdata_fifo_syncfifo_dout;
	assign main_litedramnativeportconverter_wdata_fifo_sink_ready = main_litedramnativeportconverter_wdata_fifo_syncfifo_writable;
	assign main_litedramnativeportconverter_wdata_fifo_syncfifo_we = main_litedramnativeportconverter_wdata_fifo_sink_valid;
	assign main_litedramnativeportconverter_wdata_fifo_fifo_in_first = main_litedramnativeportconverter_wdata_fifo_sink_first;
	assign main_litedramnativeportconverter_wdata_fifo_fifo_in_last = main_litedramnativeportconverter_wdata_fifo_sink_last;
	assign main_litedramnativeportconverter_wdata_fifo_fifo_in_payload_data = main_litedramnativeportconverter_wdata_fifo_sink_payload_data;
	assign main_litedramnativeportconverter_wdata_fifo_fifo_in_payload_we = main_litedramnativeportconverter_wdata_fifo_sink_payload_we;
	assign main_litedramnativeportconverter_wdata_fifo_source_valid = main_litedramnativeportconverter_wdata_fifo_syncfifo_readable;
	assign main_litedramnativeportconverter_wdata_fifo_source_first = main_litedramnativeportconverter_wdata_fifo_fifo_out_first;
	assign main_litedramnativeportconverter_wdata_fifo_source_last = main_litedramnativeportconverter_wdata_fifo_fifo_out_last;
	assign main_litedramnativeportconverter_wdata_fifo_source_payload_data = main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_data;
	assign main_litedramnativeportconverter_wdata_fifo_source_payload_we = main_litedramnativeportconverter_wdata_fifo_fifo_out_payload_we;
	assign main_litedramnativeportconverter_wdata_fifo_syncfifo_re = main_litedramnativeportconverter_wdata_fifo_source_ready;
	always @(*) begin
		main_litedramnativeportconverter_wdata_fifo_wrport_adr <= 2'd0;
		if (main_litedramnativeportconverter_wdata_fifo_replace)
			main_litedramnativeportconverter_wdata_fifo_wrport_adr <= main_litedramnativeportconverter_wdata_fifo_produce - 1'd1;
		else
			main_litedramnativeportconverter_wdata_fifo_wrport_adr <= main_litedramnativeportconverter_wdata_fifo_produce;
	end
	assign main_litedramnativeportconverter_wdata_fifo_wrport_dat_w = main_litedramnativeportconverter_wdata_fifo_syncfifo_din;
	assign main_litedramnativeportconverter_wdata_fifo_wrport_we = main_litedramnativeportconverter_wdata_fifo_syncfifo_we & (main_litedramnativeportconverter_wdata_fifo_syncfifo_writable | main_litedramnativeportconverter_wdata_fifo_replace);
	assign main_litedramnativeportconverter_wdata_fifo_do_read = main_litedramnativeportconverter_wdata_fifo_syncfifo_readable & main_litedramnativeportconverter_wdata_fifo_syncfifo_re;
	assign main_litedramnativeportconverter_wdata_fifo_rdport_adr = main_litedramnativeportconverter_wdata_fifo_consume;
	assign main_litedramnativeportconverter_wdata_fifo_syncfifo_dout = main_litedramnativeportconverter_wdata_fifo_rdport_dat_r;
	assign main_litedramnativeportconverter_wdata_fifo_syncfifo_writable = main_litedramnativeportconverter_wdata_fifo_level != 2'd3;
	assign main_litedramnativeportconverter_wdata_fifo_syncfifo_readable = main_litedramnativeportconverter_wdata_fifo_level != 1'd0;
	assign main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_ready = ~main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_valid | main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_ready;
	assign main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_valid = main_litedramnativeportconverter_wdata_buffer_sink_sink_valid;
	assign main_litedramnativeportconverter_wdata_buffer_sink_sink_ready = main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_ready;
	assign main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_first = main_litedramnativeportconverter_wdata_buffer_sink_sink_first;
	assign main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_last = main_litedramnativeportconverter_wdata_buffer_sink_sink_last;
	assign main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_payload_data = main_litedramnativeportconverter_wdata_buffer_sink_sink_payload_data;
	assign main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_payload_we = main_litedramnativeportconverter_wdata_buffer_sink_sink_payload_we;
	assign main_litedramnativeportconverter_wdata_buffer_source_source_valid = main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_valid;
	assign main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_ready = main_litedramnativeportconverter_wdata_buffer_source_source_ready;
	assign main_litedramnativeportconverter_wdata_buffer_source_source_first = main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_first;
	assign main_litedramnativeportconverter_wdata_buffer_source_source_last = main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_last;
	assign main_litedramnativeportconverter_wdata_buffer_source_source_payload_data = main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_data;
	assign main_litedramnativeportconverter_wdata_buffer_source_source_payload_we = main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_we;
	assign builder_litedramcore_roundrobin0_grant = 1'd0;
	assign builder_litedramcore_roundrobin1_grant = 1'd0;
	assign builder_litedramcore_roundrobin2_grant = 1'd0;
	assign builder_litedramcore_roundrobin3_grant = 1'd0;
	assign builder_litedramcore_roundrobin4_grant = 1'd0;
	assign builder_litedramcore_roundrobin5_grant = 1'd0;
	assign builder_litedramcore_roundrobin6_grant = 1'd0;
	assign builder_litedramcore_roundrobin7_grant = 1'd0;
	assign main_new_port_cmd_payload_addr = main_wb_port_adr - 1'd0;
	assign main_new_port_cmd_payload_we = main_wb_port_we;
	assign main_new_port_cmd_last = ~main_wb_port_we;
	assign main_new_port_flush = 1'd1;
	always @(*) begin
		main_new_port_wdata_valid <= 1'd0;
		main_new_port_wdata_valid <= main_wb_port_stb & main_wb_port_we;
		if (~main_is_ongoing)
			main_new_port_wdata_valid <= 1'd0;
	end
	assign main_new_port_wdata_payload_data = main_wb_port_dat_w;
	assign main_new_port_wdata_payload_we = main_wb_port_sel;
	assign main_new_port_rdata_ready = 1'd1;
	always @(*) begin
		builder_litedramwishbone2native_next_state <= 2'd0;
		builder_litedramwishbone2native_next_state <= builder_litedramwishbone2native_state;
		case (builder_litedramwishbone2native_state)
			1'd1:
				if (main_new_port_wdata_valid & main_new_port_wdata_ready)
					builder_litedramwishbone2native_next_state <= 1'd0;
			2'd2:
				if (main_new_port_rdata_valid)
					builder_litedramwishbone2native_next_state <= 1'd0;
			default: begin
				if ((main_new_port_cmd_valid & main_new_port_cmd_ready) & main_wb_port_we)
					builder_litedramwishbone2native_next_state <= 1'd1;
				if ((main_new_port_cmd_valid & main_new_port_cmd_ready) & ~main_wb_port_we)
					builder_litedramwishbone2native_next_state <= 2'd2;
			end
		endcase
	end
	always @(*) begin
		main_aborted_litedramwishbone2native_next_value <= 1'd0;
		case (builder_litedramwishbone2native_state)
			1'd1: main_aborted_litedramwishbone2native_next_value <= ~main_wb_port_cyc | main_aborted;
			2'd2: main_aborted_litedramwishbone2native_next_value <= ~main_wb_port_cyc | main_aborted;
			default: main_aborted_litedramwishbone2native_next_value <= 1'd0;
		endcase
	end
	always @(*) begin
		main_is_ongoing <= 1'd0;
		case (builder_litedramwishbone2native_state)
			1'd1: main_is_ongoing <= 1'd1;
			2'd2:
				;
			default:
				;
		endcase
	end
	always @(*) begin
		main_aborted_litedramwishbone2native_next_value_ce <= 1'd0;
		case (builder_litedramwishbone2native_state)
			1'd1: main_aborted_litedramwishbone2native_next_value_ce <= 1'd1;
			2'd2: main_aborted_litedramwishbone2native_next_value_ce <= 1'd1;
			default: main_aborted_litedramwishbone2native_next_value_ce <= 1'd1;
		endcase
	end
	always @(*) begin
		main_wb_port_dat_r <= 32'd0;
		case (builder_litedramwishbone2native_state)
			1'd1:
				;
			2'd2:
				if (main_new_port_rdata_valid)
					main_wb_port_dat_r <= main_new_port_rdata_payload_data;
			default:
				;
		endcase
	end
	always @(*) begin
		main_new_port_cmd_valid <= 1'd0;
		case (builder_litedramwishbone2native_state)
			1'd1:
				;
			2'd2:
				;
			default: main_new_port_cmd_valid <= main_wb_port_cyc & main_wb_port_stb;
		endcase
	end
	always @(*) begin
		main_wb_port_ack <= 1'd0;
		case (builder_litedramwishbone2native_state)
			1'd1:
				if (main_new_port_wdata_valid & main_new_port_wdata_ready)
					main_wb_port_ack <= main_wb_port_cyc & ~main_aborted;
			2'd2:
				if (main_new_port_rdata_valid)
					main_wb_port_ack <= main_wb_port_cyc & ~main_aborted;
			default:
				;
		endcase
	end
	always @(*) begin
		builder_wishbone2csr_next_state <= 2'd0;
		builder_wishbone2csr_next_state <= builder_wishbone2csr_state;
		case (builder_wishbone2csr_state)
			1'd1: builder_wishbone2csr_next_state <= 2'd2;
			2'd2: builder_wishbone2csr_next_state <= 1'd0;
			default:
				if (builder_interface0_cyc & builder_interface0_stb)
					builder_wishbone2csr_next_state <= 1'd1;
		endcase
	end
	always @(*) begin
		builder_interface1_adr_wishbone2csr_next_value1 <= 14'd0;
		case (builder_wishbone2csr_state)
			1'd1: builder_interface1_adr_wishbone2csr_next_value1 <= 1'd0;
			2'd2:
				;
			default:
				if (builder_interface0_cyc & builder_interface0_stb)
					builder_interface1_adr_wishbone2csr_next_value1 <= builder_interface0_adr[29:0];
		endcase
	end
	always @(*) begin
		builder_interface1_adr_wishbone2csr_next_value_ce1 <= 1'd0;
		case (builder_wishbone2csr_state)
			1'd1: builder_interface1_adr_wishbone2csr_next_value_ce1 <= 1'd1;
			2'd2:
				;
			default:
				if (builder_interface0_cyc & builder_interface0_stb)
					builder_interface1_adr_wishbone2csr_next_value_ce1 <= 1'd1;
		endcase
	end
	always @(*) begin
		builder_interface1_we_wishbone2csr_next_value2 <= 1'd0;
		case (builder_wishbone2csr_state)
			1'd1: builder_interface1_we_wishbone2csr_next_value2 <= 1'd0;
			2'd2:
				;
			default:
				if (builder_interface0_cyc & builder_interface0_stb)
					builder_interface1_we_wishbone2csr_next_value2 <= builder_interface0_we & (builder_interface0_sel != 1'd0);
		endcase
	end
	always @(*) begin
		builder_interface1_we_wishbone2csr_next_value_ce2 <= 1'd0;
		case (builder_wishbone2csr_state)
			1'd1: builder_interface1_we_wishbone2csr_next_value_ce2 <= 1'd1;
			2'd2:
				;
			default:
				if (builder_interface0_cyc & builder_interface0_stb)
					builder_interface1_we_wishbone2csr_next_value_ce2 <= 1'd1;
		endcase
	end
	always @(*) begin
		builder_interface0_dat_r <= 32'd0;
		case (builder_wishbone2csr_state)
			1'd1:
				;
			2'd2: builder_interface0_dat_r <= builder_interface1_dat_r;
			default:
				;
		endcase
	end
	always @(*) begin
		builder_interface0_ack <= 1'd0;
		case (builder_wishbone2csr_state)
			1'd1:
				;
			2'd2: builder_interface0_ack <= 1'd1;
			default:
				;
		endcase
	end
	always @(*) begin
		builder_interface1_dat_w_wishbone2csr_next_value0 <= 32'd0;
		case (builder_wishbone2csr_state)
			1'd1:
				;
			2'd2:
				;
			default: builder_interface1_dat_w_wishbone2csr_next_value0 <= builder_interface0_dat_w;
		endcase
	end
	always @(*) begin
		builder_interface1_dat_w_wishbone2csr_next_value_ce0 <= 1'd0;
		case (builder_wishbone2csr_state)
			1'd1:
				;
			2'd2:
				;
			default: builder_interface1_dat_w_wishbone2csr_next_value_ce0 <= 1'd1;
		endcase
	end
	assign builder_csrbank0_sel = builder_interface0_bank_bus_adr[13:9] == 1'd0;
	assign builder_csrbank0_init_done0_r = builder_interface0_bank_bus_dat_w[0];
	always @(*) begin
		builder_csrbank0_init_done0_we <= 1'd0;
		if (builder_csrbank0_sel & (builder_interface0_bank_bus_adr[8:0] == 1'd0))
			builder_csrbank0_init_done0_we <= ~builder_interface0_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank0_init_done0_re <= 1'd0;
		if (builder_csrbank0_sel & (builder_interface0_bank_bus_adr[8:0] == 1'd0))
			builder_csrbank0_init_done0_re <= builder_interface0_bank_bus_we;
	end
	assign builder_csrbank0_init_error0_r = builder_interface0_bank_bus_dat_w[0];
	always @(*) begin
		builder_csrbank0_init_error0_re <= 1'd0;
		if (builder_csrbank0_sel & (builder_interface0_bank_bus_adr[8:0] == 1'd1))
			builder_csrbank0_init_error0_re <= builder_interface0_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank0_init_error0_we <= 1'd0;
		if (builder_csrbank0_sel & (builder_interface0_bank_bus_adr[8:0] == 1'd1))
			builder_csrbank0_init_error0_we <= ~builder_interface0_bank_bus_we;
	end
	assign builder_csrbank0_init_done0_w = main_init_done_storage;
	assign builder_csrbank0_init_error0_w = main_init_error_storage;
	assign builder_csrbank1_sel = builder_interface1_bank_bus_adr[13:9] == 1'd1;
	assign builder_csrbank1_rst0_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		builder_csrbank1_rst0_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 1'd0))
			builder_csrbank1_rst0_re <= builder_interface1_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank1_rst0_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 1'd0))
			builder_csrbank1_rst0_we <= ~builder_interface1_bank_bus_we;
	end
	assign builder_csrbank1_dly_sel0_r = builder_interface1_bank_bus_dat_w[1:0];
	always @(*) begin
		builder_csrbank1_dly_sel0_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 1'd1))
			builder_csrbank1_dly_sel0_re <= builder_interface1_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank1_dly_sel0_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 1'd1))
			builder_csrbank1_dly_sel0_we <= ~builder_interface1_bank_bus_we;
	end
	assign builder_csrbank1_half_sys8x_taps0_r = builder_interface1_bank_bus_dat_w[4:0];
	always @(*) begin
		builder_csrbank1_half_sys8x_taps0_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 2'd2))
			builder_csrbank1_half_sys8x_taps0_we <= ~builder_interface1_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank1_half_sys8x_taps0_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 2'd2))
			builder_csrbank1_half_sys8x_taps0_re <= builder_interface1_bank_bus_we;
	end
	assign builder_csrbank1_wlevel_en0_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		builder_csrbank1_wlevel_en0_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 2'd3))
			builder_csrbank1_wlevel_en0_re <= builder_interface1_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank1_wlevel_en0_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 2'd3))
			builder_csrbank1_wlevel_en0_we <= ~builder_interface1_bank_bus_we;
	end
	assign main_a7ddrphy_wlevel_strobe_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		main_a7ddrphy_wlevel_strobe_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd4))
			main_a7ddrphy_wlevel_strobe_re <= builder_interface1_bank_bus_we;
	end
	always @(*) begin
		main_a7ddrphy_wlevel_strobe_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd4))
			main_a7ddrphy_wlevel_strobe_we <= ~builder_interface1_bank_bus_we;
	end
	assign main_a7ddrphy_rdly_dq_rst_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		main_a7ddrphy_rdly_dq_rst_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd5))
			main_a7ddrphy_rdly_dq_rst_re <= builder_interface1_bank_bus_we;
	end
	always @(*) begin
		main_a7ddrphy_rdly_dq_rst_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd5))
			main_a7ddrphy_rdly_dq_rst_we <= ~builder_interface1_bank_bus_we;
	end
	assign main_a7ddrphy_rdly_dq_inc_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		main_a7ddrphy_rdly_dq_inc_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd6))
			main_a7ddrphy_rdly_dq_inc_we <= ~builder_interface1_bank_bus_we;
	end
	always @(*) begin
		main_a7ddrphy_rdly_dq_inc_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd6))
			main_a7ddrphy_rdly_dq_inc_re <= builder_interface1_bank_bus_we;
	end
	assign main_a7ddrphy_rdly_dq_bitslip_rst_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		main_a7ddrphy_rdly_dq_bitslip_rst_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd7))
			main_a7ddrphy_rdly_dq_bitslip_rst_we <= ~builder_interface1_bank_bus_we;
	end
	always @(*) begin
		main_a7ddrphy_rdly_dq_bitslip_rst_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 3'd7))
			main_a7ddrphy_rdly_dq_bitslip_rst_re <= builder_interface1_bank_bus_we;
	end
	assign main_a7ddrphy_rdly_dq_bitslip_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		main_a7ddrphy_rdly_dq_bitslip_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd8))
			main_a7ddrphy_rdly_dq_bitslip_we <= ~builder_interface1_bank_bus_we;
	end
	always @(*) begin
		main_a7ddrphy_rdly_dq_bitslip_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd8))
			main_a7ddrphy_rdly_dq_bitslip_re <= builder_interface1_bank_bus_we;
	end
	assign main_a7ddrphy_wdly_dq_bitslip_rst_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		main_a7ddrphy_wdly_dq_bitslip_rst_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd9))
			main_a7ddrphy_wdly_dq_bitslip_rst_we <= ~builder_interface1_bank_bus_we;
	end
	always @(*) begin
		main_a7ddrphy_wdly_dq_bitslip_rst_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd9))
			main_a7ddrphy_wdly_dq_bitslip_rst_re <= builder_interface1_bank_bus_we;
	end
	assign main_a7ddrphy_wdly_dq_bitslip_r = builder_interface1_bank_bus_dat_w[0];
	always @(*) begin
		main_a7ddrphy_wdly_dq_bitslip_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd10))
			main_a7ddrphy_wdly_dq_bitslip_re <= builder_interface1_bank_bus_we;
	end
	always @(*) begin
		main_a7ddrphy_wdly_dq_bitslip_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd10))
			main_a7ddrphy_wdly_dq_bitslip_we <= ~builder_interface1_bank_bus_we;
	end
	assign builder_csrbank1_rdphase0_r = builder_interface1_bank_bus_dat_w[1:0];
	always @(*) begin
		builder_csrbank1_rdphase0_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd11))
			builder_csrbank1_rdphase0_we <= ~builder_interface1_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank1_rdphase0_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd11))
			builder_csrbank1_rdphase0_re <= builder_interface1_bank_bus_we;
	end
	assign builder_csrbank1_wrphase0_r = builder_interface1_bank_bus_dat_w[1:0];
	always @(*) begin
		builder_csrbank1_wrphase0_we <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd12))
			builder_csrbank1_wrphase0_we <= ~builder_interface1_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank1_wrphase0_re <= 1'd0;
		if (builder_csrbank1_sel & (builder_interface1_bank_bus_adr[8:0] == 4'd12))
			builder_csrbank1_wrphase0_re <= builder_interface1_bank_bus_we;
	end
	assign builder_csrbank1_rst0_w = main_a7ddrphy_rst_storage;
	assign builder_csrbank1_dly_sel0_w = main_a7ddrphy_dly_sel_storage[1:0];
	assign builder_csrbank1_half_sys8x_taps0_w = main_a7ddrphy_half_sys8x_taps_storage[4:0];
	assign builder_csrbank1_wlevel_en0_w = main_a7ddrphy_wlevel_en_storage;
	assign builder_csrbank1_rdphase0_w = main_a7ddrphy_rdphase_storage[1:0];
	assign builder_csrbank1_wrphase0_w = main_a7ddrphy_wrphase_storage[1:0];
	assign builder_csrbank2_sel = builder_interface2_bank_bus_adr[13:9] == 2'd2;
	assign builder_csrbank2_dfii_control0_r = builder_interface2_bank_bus_dat_w[3:0];
	always @(*) begin
		builder_csrbank2_dfii_control0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 1'd0))
			builder_csrbank2_dfii_control0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_control0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 1'd0))
			builder_csrbank2_dfii_control0_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi0_command0_r = builder_interface2_bank_bus_dat_w[7:0];
	always @(*) begin
		builder_csrbank2_dfii_pi0_command0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 1'd1))
			builder_csrbank2_dfii_pi0_command0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi0_command0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 1'd1))
			builder_csrbank2_dfii_pi0_command0_re <= builder_interface2_bank_bus_we;
	end
	assign main_phaseinjector0_command_issue_r = builder_interface2_bank_bus_dat_w[0];
	always @(*) begin
		main_phaseinjector0_command_issue_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 2'd2))
			main_phaseinjector0_command_issue_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		main_phaseinjector0_command_issue_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 2'd2))
			main_phaseinjector0_command_issue_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi0_address0_r = builder_interface2_bank_bus_dat_w[13:0];
	always @(*) begin
		builder_csrbank2_dfii_pi0_address0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 2'd3))
			builder_csrbank2_dfii_pi0_address0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi0_address0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 2'd3))
			builder_csrbank2_dfii_pi0_address0_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi0_baddress0_r = builder_interface2_bank_bus_dat_w[2:0];
	always @(*) begin
		builder_csrbank2_dfii_pi0_baddress0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd4))
			builder_csrbank2_dfii_pi0_baddress0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi0_baddress0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd4))
			builder_csrbank2_dfii_pi0_baddress0_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi0_wrdata0_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi0_wrdata0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd5))
			builder_csrbank2_dfii_pi0_wrdata0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi0_wrdata0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd5))
			builder_csrbank2_dfii_pi0_wrdata0_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi0_rddata_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi0_rddata_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd6))
			builder_csrbank2_dfii_pi0_rddata_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi0_rddata_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd6))
			builder_csrbank2_dfii_pi0_rddata_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi1_command0_r = builder_interface2_bank_bus_dat_w[7:0];
	always @(*) begin
		builder_csrbank2_dfii_pi1_command0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd7))
			builder_csrbank2_dfii_pi1_command0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi1_command0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 3'd7))
			builder_csrbank2_dfii_pi1_command0_re <= builder_interface2_bank_bus_we;
	end
	assign main_phaseinjector1_command_issue_r = builder_interface2_bank_bus_dat_w[0];
	always @(*) begin
		main_phaseinjector1_command_issue_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd8))
			main_phaseinjector1_command_issue_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		main_phaseinjector1_command_issue_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd8))
			main_phaseinjector1_command_issue_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi1_address0_r = builder_interface2_bank_bus_dat_w[13:0];
	always @(*) begin
		builder_csrbank2_dfii_pi1_address0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd9))
			builder_csrbank2_dfii_pi1_address0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi1_address0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd9))
			builder_csrbank2_dfii_pi1_address0_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi1_baddress0_r = builder_interface2_bank_bus_dat_w[2:0];
	always @(*) begin
		builder_csrbank2_dfii_pi1_baddress0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd10))
			builder_csrbank2_dfii_pi1_baddress0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi1_baddress0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd10))
			builder_csrbank2_dfii_pi1_baddress0_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi1_wrdata0_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi1_wrdata0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd11))
			builder_csrbank2_dfii_pi1_wrdata0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi1_wrdata0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd11))
			builder_csrbank2_dfii_pi1_wrdata0_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi1_rddata_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi1_rddata_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd12))
			builder_csrbank2_dfii_pi1_rddata_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi1_rddata_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd12))
			builder_csrbank2_dfii_pi1_rddata_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi2_command0_r = builder_interface2_bank_bus_dat_w[7:0];
	always @(*) begin
		builder_csrbank2_dfii_pi2_command0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd13))
			builder_csrbank2_dfii_pi2_command0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi2_command0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd13))
			builder_csrbank2_dfii_pi2_command0_we <= ~builder_interface2_bank_bus_we;
	end
	assign main_phaseinjector2_command_issue_r = builder_interface2_bank_bus_dat_w[0];
	always @(*) begin
		main_phaseinjector2_command_issue_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd14))
			main_phaseinjector2_command_issue_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		main_phaseinjector2_command_issue_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd14))
			main_phaseinjector2_command_issue_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi2_address0_r = builder_interface2_bank_bus_dat_w[13:0];
	always @(*) begin
		builder_csrbank2_dfii_pi2_address0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd15))
			builder_csrbank2_dfii_pi2_address0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi2_address0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 4'd15))
			builder_csrbank2_dfii_pi2_address0_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi2_baddress0_r = builder_interface2_bank_bus_dat_w[2:0];
	always @(*) begin
		builder_csrbank2_dfii_pi2_baddress0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd16))
			builder_csrbank2_dfii_pi2_baddress0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi2_baddress0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd16))
			builder_csrbank2_dfii_pi2_baddress0_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi2_wrdata0_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi2_wrdata0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd17))
			builder_csrbank2_dfii_pi2_wrdata0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi2_wrdata0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd17))
			builder_csrbank2_dfii_pi2_wrdata0_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi2_rddata_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi2_rddata_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd18))
			builder_csrbank2_dfii_pi2_rddata_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi2_rddata_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd18))
			builder_csrbank2_dfii_pi2_rddata_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi3_command0_r = builder_interface2_bank_bus_dat_w[7:0];
	always @(*) begin
		builder_csrbank2_dfii_pi3_command0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd19))
			builder_csrbank2_dfii_pi3_command0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi3_command0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd19))
			builder_csrbank2_dfii_pi3_command0_we <= ~builder_interface2_bank_bus_we;
	end
	assign main_phaseinjector3_command_issue_r = builder_interface2_bank_bus_dat_w[0];
	always @(*) begin
		main_phaseinjector3_command_issue_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd20))
			main_phaseinjector3_command_issue_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		main_phaseinjector3_command_issue_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd20))
			main_phaseinjector3_command_issue_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi3_address0_r = builder_interface2_bank_bus_dat_w[13:0];
	always @(*) begin
		builder_csrbank2_dfii_pi3_address0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd21))
			builder_csrbank2_dfii_pi3_address0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi3_address0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd21))
			builder_csrbank2_dfii_pi3_address0_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi3_baddress0_r = builder_interface2_bank_bus_dat_w[2:0];
	always @(*) begin
		builder_csrbank2_dfii_pi3_baddress0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd22))
			builder_csrbank2_dfii_pi3_baddress0_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi3_baddress0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd22))
			builder_csrbank2_dfii_pi3_baddress0_re <= builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi3_wrdata0_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi3_wrdata0_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd23))
			builder_csrbank2_dfii_pi3_wrdata0_re <= builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi3_wrdata0_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd23))
			builder_csrbank2_dfii_pi3_wrdata0_we <= ~builder_interface2_bank_bus_we;
	end
	assign builder_csrbank2_dfii_pi3_rddata_r = builder_interface2_bank_bus_dat_w[31:0];
	always @(*) begin
		builder_csrbank2_dfii_pi3_rddata_we <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd24))
			builder_csrbank2_dfii_pi3_rddata_we <= ~builder_interface2_bank_bus_we;
	end
	always @(*) begin
		builder_csrbank2_dfii_pi3_rddata_re <= 1'd0;
		if (builder_csrbank2_sel & (builder_interface2_bank_bus_adr[8:0] == 5'd24))
			builder_csrbank2_dfii_pi3_rddata_re <= builder_interface2_bank_bus_we;
	end
	assign main_sel = main_storage[0];
	assign main_cke = main_storage[1];
	assign main_odt = main_storage[2];
	assign main_reset_n = main_storage[3];
	assign builder_csrbank2_dfii_control0_w = main_storage[3:0];
	assign main_phaseinjector0_csrfield_cs = main_phaseinjector0_command_storage[0];
	assign main_phaseinjector0_csrfield_we = main_phaseinjector0_command_storage[1];
	assign main_phaseinjector0_csrfield_cas = main_phaseinjector0_command_storage[2];
	assign main_phaseinjector0_csrfield_ras = main_phaseinjector0_command_storage[3];
	assign main_phaseinjector0_csrfield_wren = main_phaseinjector0_command_storage[4];
	assign main_phaseinjector0_csrfield_rden = main_phaseinjector0_command_storage[5];
	assign main_phaseinjector0_csrfield_cs_top = main_phaseinjector0_command_storage[6];
	assign main_phaseinjector0_csrfield_cs_bottom = main_phaseinjector0_command_storage[7];
	assign builder_csrbank2_dfii_pi0_command0_w = main_phaseinjector0_command_storage[7:0];
	assign builder_csrbank2_dfii_pi0_address0_w = main_phaseinjector0_address_storage[13:0];
	assign builder_csrbank2_dfii_pi0_baddress0_w = main_phaseinjector0_baddress_storage[2:0];
	assign builder_csrbank2_dfii_pi0_wrdata0_w = main_phaseinjector0_wrdata_storage[31:0];
	assign builder_csrbank2_dfii_pi0_rddata_w = main_phaseinjector0_rddata_status[31:0];
	assign main_phaseinjector0_rddata_we = builder_csrbank2_dfii_pi0_rddata_we;
	assign main_phaseinjector1_csrfield_cs = main_phaseinjector1_command_storage[0];
	assign main_phaseinjector1_csrfield_we = main_phaseinjector1_command_storage[1];
	assign main_phaseinjector1_csrfield_cas = main_phaseinjector1_command_storage[2];
	assign main_phaseinjector1_csrfield_ras = main_phaseinjector1_command_storage[3];
	assign main_phaseinjector1_csrfield_wren = main_phaseinjector1_command_storage[4];
	assign main_phaseinjector1_csrfield_rden = main_phaseinjector1_command_storage[5];
	assign main_phaseinjector1_csrfield_cs_top = main_phaseinjector1_command_storage[6];
	assign main_phaseinjector1_csrfield_cs_bottom = main_phaseinjector1_command_storage[7];
	assign builder_csrbank2_dfii_pi1_command0_w = main_phaseinjector1_command_storage[7:0];
	assign builder_csrbank2_dfii_pi1_address0_w = main_phaseinjector1_address_storage[13:0];
	assign builder_csrbank2_dfii_pi1_baddress0_w = main_phaseinjector1_baddress_storage[2:0];
	assign builder_csrbank2_dfii_pi1_wrdata0_w = main_phaseinjector1_wrdata_storage[31:0];
	assign builder_csrbank2_dfii_pi1_rddata_w = main_phaseinjector1_rddata_status[31:0];
	assign main_phaseinjector1_rddata_we = builder_csrbank2_dfii_pi1_rddata_we;
	assign main_phaseinjector2_csrfield_cs = main_phaseinjector2_command_storage[0];
	assign main_phaseinjector2_csrfield_we = main_phaseinjector2_command_storage[1];
	assign main_phaseinjector2_csrfield_cas = main_phaseinjector2_command_storage[2];
	assign main_phaseinjector2_csrfield_ras = main_phaseinjector2_command_storage[3];
	assign main_phaseinjector2_csrfield_wren = main_phaseinjector2_command_storage[4];
	assign main_phaseinjector2_csrfield_rden = main_phaseinjector2_command_storage[5];
	assign main_phaseinjector2_csrfield_cs_top = main_phaseinjector2_command_storage[6];
	assign main_phaseinjector2_csrfield_cs_bottom = main_phaseinjector2_command_storage[7];
	assign builder_csrbank2_dfii_pi2_command0_w = main_phaseinjector2_command_storage[7:0];
	assign builder_csrbank2_dfii_pi2_address0_w = main_phaseinjector2_address_storage[13:0];
	assign builder_csrbank2_dfii_pi2_baddress0_w = main_phaseinjector2_baddress_storage[2:0];
	assign builder_csrbank2_dfii_pi2_wrdata0_w = main_phaseinjector2_wrdata_storage[31:0];
	assign builder_csrbank2_dfii_pi2_rddata_w = main_phaseinjector2_rddata_status[31:0];
	assign main_phaseinjector2_rddata_we = builder_csrbank2_dfii_pi2_rddata_we;
	assign main_phaseinjector3_csrfield_cs = main_phaseinjector3_command_storage[0];
	assign main_phaseinjector3_csrfield_we = main_phaseinjector3_command_storage[1];
	assign main_phaseinjector3_csrfield_cas = main_phaseinjector3_command_storage[2];
	assign main_phaseinjector3_csrfield_ras = main_phaseinjector3_command_storage[3];
	assign main_phaseinjector3_csrfield_wren = main_phaseinjector3_command_storage[4];
	assign main_phaseinjector3_csrfield_rden = main_phaseinjector3_command_storage[5];
	assign main_phaseinjector3_csrfield_cs_top = main_phaseinjector3_command_storage[6];
	assign main_phaseinjector3_csrfield_cs_bottom = main_phaseinjector3_command_storage[7];
	assign builder_csrbank2_dfii_pi3_command0_w = main_phaseinjector3_command_storage[7:0];
	assign builder_csrbank2_dfii_pi3_address0_w = main_phaseinjector3_address_storage[13:0];
	assign builder_csrbank2_dfii_pi3_baddress0_w = main_phaseinjector3_baddress_storage[2:0];
	assign builder_csrbank2_dfii_pi3_wrdata0_w = main_phaseinjector3_wrdata_storage[31:0];
	assign builder_csrbank2_dfii_pi3_rddata_w = main_phaseinjector3_rddata_status[31:0];
	assign main_phaseinjector3_rddata_we = builder_csrbank2_dfii_pi3_rddata_we;
	assign builder_adr = builder_interface1_adr;
	assign builder_we = builder_interface1_we;
	assign builder_dat_w = builder_interface1_dat_w;
	assign builder_interface1_dat_r = builder_dat_r;
	assign builder_interface0_bank_bus_adr = builder_adr;
	assign builder_interface1_bank_bus_adr = builder_adr;
	assign builder_interface2_bank_bus_adr = builder_adr;
	assign builder_interface0_bank_bus_we = builder_we;
	assign builder_interface1_bank_bus_we = builder_we;
	assign builder_interface2_bank_bus_we = builder_we;
	assign builder_interface0_bank_bus_dat_w = builder_dat_w;
	assign builder_interface1_bank_bus_dat_w = builder_dat_w;
	assign builder_interface2_bank_bus_dat_w = builder_dat_w;
	assign builder_dat_r = (builder_interface0_bank_bus_dat_r | builder_interface1_bank_bus_dat_r) | builder_interface2_bank_bus_dat_r;
	always @(*) begin
		builder_rhs_array_muxed0 <= 1'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_rhs_array_muxed0 <= main_choose_cmd_valids[0];
			1'd1: builder_rhs_array_muxed0 <= main_choose_cmd_valids[1];
			2'd2: builder_rhs_array_muxed0 <= main_choose_cmd_valids[2];
			2'd3: builder_rhs_array_muxed0 <= main_choose_cmd_valids[3];
			3'd4: builder_rhs_array_muxed0 <= main_choose_cmd_valids[4];
			3'd5: builder_rhs_array_muxed0 <= main_choose_cmd_valids[5];
			3'd6: builder_rhs_array_muxed0 <= main_choose_cmd_valids[6];
			default: builder_rhs_array_muxed0 <= main_choose_cmd_valids[7];
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed1 <= 14'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_rhs_array_muxed1 <= main_bankmachine0_cmd_payload_a;
			1'd1: builder_rhs_array_muxed1 <= main_bankmachine1_cmd_payload_a;
			2'd2: builder_rhs_array_muxed1 <= main_bankmachine2_cmd_payload_a;
			2'd3: builder_rhs_array_muxed1 <= main_bankmachine3_cmd_payload_a;
			3'd4: builder_rhs_array_muxed1 <= main_bankmachine4_cmd_payload_a;
			3'd5: builder_rhs_array_muxed1 <= main_bankmachine5_cmd_payload_a;
			3'd6: builder_rhs_array_muxed1 <= main_bankmachine6_cmd_payload_a;
			default: builder_rhs_array_muxed1 <= main_bankmachine7_cmd_payload_a;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed2 <= 3'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_rhs_array_muxed2 <= main_bankmachine0_cmd_payload_ba;
			1'd1: builder_rhs_array_muxed2 <= main_bankmachine1_cmd_payload_ba;
			2'd2: builder_rhs_array_muxed2 <= main_bankmachine2_cmd_payload_ba;
			2'd3: builder_rhs_array_muxed2 <= main_bankmachine3_cmd_payload_ba;
			3'd4: builder_rhs_array_muxed2 <= main_bankmachine4_cmd_payload_ba;
			3'd5: builder_rhs_array_muxed2 <= main_bankmachine5_cmd_payload_ba;
			3'd6: builder_rhs_array_muxed2 <= main_bankmachine6_cmd_payload_ba;
			default: builder_rhs_array_muxed2 <= main_bankmachine7_cmd_payload_ba;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed3 <= 1'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_rhs_array_muxed3 <= main_bankmachine0_cmd_payload_is_read;
			1'd1: builder_rhs_array_muxed3 <= main_bankmachine1_cmd_payload_is_read;
			2'd2: builder_rhs_array_muxed3 <= main_bankmachine2_cmd_payload_is_read;
			2'd3: builder_rhs_array_muxed3 <= main_bankmachine3_cmd_payload_is_read;
			3'd4: builder_rhs_array_muxed3 <= main_bankmachine4_cmd_payload_is_read;
			3'd5: builder_rhs_array_muxed3 <= main_bankmachine5_cmd_payload_is_read;
			3'd6: builder_rhs_array_muxed3 <= main_bankmachine6_cmd_payload_is_read;
			default: builder_rhs_array_muxed3 <= main_bankmachine7_cmd_payload_is_read;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed4 <= 1'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_rhs_array_muxed4 <= main_bankmachine0_cmd_payload_is_write;
			1'd1: builder_rhs_array_muxed4 <= main_bankmachine1_cmd_payload_is_write;
			2'd2: builder_rhs_array_muxed4 <= main_bankmachine2_cmd_payload_is_write;
			2'd3: builder_rhs_array_muxed4 <= main_bankmachine3_cmd_payload_is_write;
			3'd4: builder_rhs_array_muxed4 <= main_bankmachine4_cmd_payload_is_write;
			3'd5: builder_rhs_array_muxed4 <= main_bankmachine5_cmd_payload_is_write;
			3'd6: builder_rhs_array_muxed4 <= main_bankmachine6_cmd_payload_is_write;
			default: builder_rhs_array_muxed4 <= main_bankmachine7_cmd_payload_is_write;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed5 <= 1'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_rhs_array_muxed5 <= main_bankmachine0_cmd_payload_is_cmd;
			1'd1: builder_rhs_array_muxed5 <= main_bankmachine1_cmd_payload_is_cmd;
			2'd2: builder_rhs_array_muxed5 <= main_bankmachine2_cmd_payload_is_cmd;
			2'd3: builder_rhs_array_muxed5 <= main_bankmachine3_cmd_payload_is_cmd;
			3'd4: builder_rhs_array_muxed5 <= main_bankmachine4_cmd_payload_is_cmd;
			3'd5: builder_rhs_array_muxed5 <= main_bankmachine5_cmd_payload_is_cmd;
			3'd6: builder_rhs_array_muxed5 <= main_bankmachine6_cmd_payload_is_cmd;
			default: builder_rhs_array_muxed5 <= main_bankmachine7_cmd_payload_is_cmd;
		endcase
	end
	always @(*) begin
		builder_t_array_muxed0 <= 1'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_t_array_muxed0 <= main_bankmachine0_cmd_payload_cas;
			1'd1: builder_t_array_muxed0 <= main_bankmachine1_cmd_payload_cas;
			2'd2: builder_t_array_muxed0 <= main_bankmachine2_cmd_payload_cas;
			2'd3: builder_t_array_muxed0 <= main_bankmachine3_cmd_payload_cas;
			3'd4: builder_t_array_muxed0 <= main_bankmachine4_cmd_payload_cas;
			3'd5: builder_t_array_muxed0 <= main_bankmachine5_cmd_payload_cas;
			3'd6: builder_t_array_muxed0 <= main_bankmachine6_cmd_payload_cas;
			default: builder_t_array_muxed0 <= main_bankmachine7_cmd_payload_cas;
		endcase
	end
	always @(*) begin
		builder_t_array_muxed1 <= 1'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_t_array_muxed1 <= main_bankmachine0_cmd_payload_ras;
			1'd1: builder_t_array_muxed1 <= main_bankmachine1_cmd_payload_ras;
			2'd2: builder_t_array_muxed1 <= main_bankmachine2_cmd_payload_ras;
			2'd3: builder_t_array_muxed1 <= main_bankmachine3_cmd_payload_ras;
			3'd4: builder_t_array_muxed1 <= main_bankmachine4_cmd_payload_ras;
			3'd5: builder_t_array_muxed1 <= main_bankmachine5_cmd_payload_ras;
			3'd6: builder_t_array_muxed1 <= main_bankmachine6_cmd_payload_ras;
			default: builder_t_array_muxed1 <= main_bankmachine7_cmd_payload_ras;
		endcase
	end
	always @(*) begin
		builder_t_array_muxed2 <= 1'd0;
		case (main_choose_cmd_grant)
			1'd0: builder_t_array_muxed2 <= main_bankmachine0_cmd_payload_we;
			1'd1: builder_t_array_muxed2 <= main_bankmachine1_cmd_payload_we;
			2'd2: builder_t_array_muxed2 <= main_bankmachine2_cmd_payload_we;
			2'd3: builder_t_array_muxed2 <= main_bankmachine3_cmd_payload_we;
			3'd4: builder_t_array_muxed2 <= main_bankmachine4_cmd_payload_we;
			3'd5: builder_t_array_muxed2 <= main_bankmachine5_cmd_payload_we;
			3'd6: builder_t_array_muxed2 <= main_bankmachine6_cmd_payload_we;
			default: builder_t_array_muxed2 <= main_bankmachine7_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed6 <= 1'd0;
		case (main_choose_req_grant)
			1'd0: builder_rhs_array_muxed6 <= main_choose_req_valids[0];
			1'd1: builder_rhs_array_muxed6 <= main_choose_req_valids[1];
			2'd2: builder_rhs_array_muxed6 <= main_choose_req_valids[2];
			2'd3: builder_rhs_array_muxed6 <= main_choose_req_valids[3];
			3'd4: builder_rhs_array_muxed6 <= main_choose_req_valids[4];
			3'd5: builder_rhs_array_muxed6 <= main_choose_req_valids[5];
			3'd6: builder_rhs_array_muxed6 <= main_choose_req_valids[6];
			default: builder_rhs_array_muxed6 <= main_choose_req_valids[7];
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed7 <= 14'd0;
		case (main_choose_req_grant)
			1'd0: builder_rhs_array_muxed7 <= main_bankmachine0_cmd_payload_a;
			1'd1: builder_rhs_array_muxed7 <= main_bankmachine1_cmd_payload_a;
			2'd2: builder_rhs_array_muxed7 <= main_bankmachine2_cmd_payload_a;
			2'd3: builder_rhs_array_muxed7 <= main_bankmachine3_cmd_payload_a;
			3'd4: builder_rhs_array_muxed7 <= main_bankmachine4_cmd_payload_a;
			3'd5: builder_rhs_array_muxed7 <= main_bankmachine5_cmd_payload_a;
			3'd6: builder_rhs_array_muxed7 <= main_bankmachine6_cmd_payload_a;
			default: builder_rhs_array_muxed7 <= main_bankmachine7_cmd_payload_a;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed8 <= 3'd0;
		case (main_choose_req_grant)
			1'd0: builder_rhs_array_muxed8 <= main_bankmachine0_cmd_payload_ba;
			1'd1: builder_rhs_array_muxed8 <= main_bankmachine1_cmd_payload_ba;
			2'd2: builder_rhs_array_muxed8 <= main_bankmachine2_cmd_payload_ba;
			2'd3: builder_rhs_array_muxed8 <= main_bankmachine3_cmd_payload_ba;
			3'd4: builder_rhs_array_muxed8 <= main_bankmachine4_cmd_payload_ba;
			3'd5: builder_rhs_array_muxed8 <= main_bankmachine5_cmd_payload_ba;
			3'd6: builder_rhs_array_muxed8 <= main_bankmachine6_cmd_payload_ba;
			default: builder_rhs_array_muxed8 <= main_bankmachine7_cmd_payload_ba;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed9 <= 1'd0;
		case (main_choose_req_grant)
			1'd0: builder_rhs_array_muxed9 <= main_bankmachine0_cmd_payload_is_read;
			1'd1: builder_rhs_array_muxed9 <= main_bankmachine1_cmd_payload_is_read;
			2'd2: builder_rhs_array_muxed9 <= main_bankmachine2_cmd_payload_is_read;
			2'd3: builder_rhs_array_muxed9 <= main_bankmachine3_cmd_payload_is_read;
			3'd4: builder_rhs_array_muxed9 <= main_bankmachine4_cmd_payload_is_read;
			3'd5: builder_rhs_array_muxed9 <= main_bankmachine5_cmd_payload_is_read;
			3'd6: builder_rhs_array_muxed9 <= main_bankmachine6_cmd_payload_is_read;
			default: builder_rhs_array_muxed9 <= main_bankmachine7_cmd_payload_is_read;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed10 <= 1'd0;
		case (main_choose_req_grant)
			1'd0: builder_rhs_array_muxed10 <= main_bankmachine0_cmd_payload_is_write;
			1'd1: builder_rhs_array_muxed10 <= main_bankmachine1_cmd_payload_is_write;
			2'd2: builder_rhs_array_muxed10 <= main_bankmachine2_cmd_payload_is_write;
			2'd3: builder_rhs_array_muxed10 <= main_bankmachine3_cmd_payload_is_write;
			3'd4: builder_rhs_array_muxed10 <= main_bankmachine4_cmd_payload_is_write;
			3'd5: builder_rhs_array_muxed10 <= main_bankmachine5_cmd_payload_is_write;
			3'd6: builder_rhs_array_muxed10 <= main_bankmachine6_cmd_payload_is_write;
			default: builder_rhs_array_muxed10 <= main_bankmachine7_cmd_payload_is_write;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed11 <= 1'd0;
		case (main_choose_req_grant)
			1'd0: builder_rhs_array_muxed11 <= main_bankmachine0_cmd_payload_is_cmd;
			1'd1: builder_rhs_array_muxed11 <= main_bankmachine1_cmd_payload_is_cmd;
			2'd2: builder_rhs_array_muxed11 <= main_bankmachine2_cmd_payload_is_cmd;
			2'd3: builder_rhs_array_muxed11 <= main_bankmachine3_cmd_payload_is_cmd;
			3'd4: builder_rhs_array_muxed11 <= main_bankmachine4_cmd_payload_is_cmd;
			3'd5: builder_rhs_array_muxed11 <= main_bankmachine5_cmd_payload_is_cmd;
			3'd6: builder_rhs_array_muxed11 <= main_bankmachine6_cmd_payload_is_cmd;
			default: builder_rhs_array_muxed11 <= main_bankmachine7_cmd_payload_is_cmd;
		endcase
	end
	always @(*) begin
		builder_t_array_muxed3 <= 1'd0;
		case (main_choose_req_grant)
			1'd0: builder_t_array_muxed3 <= main_bankmachine0_cmd_payload_cas;
			1'd1: builder_t_array_muxed3 <= main_bankmachine1_cmd_payload_cas;
			2'd2: builder_t_array_muxed3 <= main_bankmachine2_cmd_payload_cas;
			2'd3: builder_t_array_muxed3 <= main_bankmachine3_cmd_payload_cas;
			3'd4: builder_t_array_muxed3 <= main_bankmachine4_cmd_payload_cas;
			3'd5: builder_t_array_muxed3 <= main_bankmachine5_cmd_payload_cas;
			3'd6: builder_t_array_muxed3 <= main_bankmachine6_cmd_payload_cas;
			default: builder_t_array_muxed3 <= main_bankmachine7_cmd_payload_cas;
		endcase
	end
	always @(*) begin
		builder_t_array_muxed4 <= 1'd0;
		case (main_choose_req_grant)
			1'd0: builder_t_array_muxed4 <= main_bankmachine0_cmd_payload_ras;
			1'd1: builder_t_array_muxed4 <= main_bankmachine1_cmd_payload_ras;
			2'd2: builder_t_array_muxed4 <= main_bankmachine2_cmd_payload_ras;
			2'd3: builder_t_array_muxed4 <= main_bankmachine3_cmd_payload_ras;
			3'd4: builder_t_array_muxed4 <= main_bankmachine4_cmd_payload_ras;
			3'd5: builder_t_array_muxed4 <= main_bankmachine5_cmd_payload_ras;
			3'd6: builder_t_array_muxed4 <= main_bankmachine6_cmd_payload_ras;
			default: builder_t_array_muxed4 <= main_bankmachine7_cmd_payload_ras;
		endcase
	end
	always @(*) begin
		builder_t_array_muxed5 <= 1'd0;
		case (main_choose_req_grant)
			1'd0: builder_t_array_muxed5 <= main_bankmachine0_cmd_payload_we;
			1'd1: builder_t_array_muxed5 <= main_bankmachine1_cmd_payload_we;
			2'd2: builder_t_array_muxed5 <= main_bankmachine2_cmd_payload_we;
			2'd3: builder_t_array_muxed5 <= main_bankmachine3_cmd_payload_we;
			3'd4: builder_t_array_muxed5 <= main_bankmachine4_cmd_payload_we;
			3'd5: builder_t_array_muxed5 <= main_bankmachine5_cmd_payload_we;
			3'd6: builder_t_array_muxed5 <= main_bankmachine6_cmd_payload_we;
			default: builder_t_array_muxed5 <= main_bankmachine7_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed12 <= 21'd0;
		case (builder_litedramcore_roundrobin0_grant)
			default: builder_rhs_array_muxed12 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed13 <= 1'd0;
		case (builder_litedramcore_roundrobin0_grant)
			default: builder_rhs_array_muxed13 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed14 <= 1'd0;
		case (builder_litedramcore_roundrobin0_grant)
			default: builder_rhs_array_muxed14 <= ((main_port_cmd_payload_addr[9:7] == 1'd0) & ~(((((((builder_litedramcore_locked0 | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed15 <= 21'd0;
		case (builder_litedramcore_roundrobin1_grant)
			default: builder_rhs_array_muxed15 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed16 <= 1'd0;
		case (builder_litedramcore_roundrobin1_grant)
			default: builder_rhs_array_muxed16 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed17 <= 1'd0;
		case (builder_litedramcore_roundrobin1_grant)
			default: builder_rhs_array_muxed17 <= ((main_port_cmd_payload_addr[9:7] == 1'd1) & ~(((((((builder_litedramcore_locked1 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed18 <= 21'd0;
		case (builder_litedramcore_roundrobin2_grant)
			default: builder_rhs_array_muxed18 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed19 <= 1'd0;
		case (builder_litedramcore_roundrobin2_grant)
			default: builder_rhs_array_muxed19 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed20 <= 1'd0;
		case (builder_litedramcore_roundrobin2_grant)
			default: builder_rhs_array_muxed20 <= ((main_port_cmd_payload_addr[9:7] == 2'd2) & ~(((((((builder_litedramcore_locked2 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed21 <= 21'd0;
		case (builder_litedramcore_roundrobin3_grant)
			default: builder_rhs_array_muxed21 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed22 <= 1'd0;
		case (builder_litedramcore_roundrobin3_grant)
			default: builder_rhs_array_muxed22 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed23 <= 1'd0;
		case (builder_litedramcore_roundrobin3_grant)
			default: builder_rhs_array_muxed23 <= ((main_port_cmd_payload_addr[9:7] == 2'd3) & ~(((((((builder_litedramcore_locked3 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed24 <= 21'd0;
		case (builder_litedramcore_roundrobin4_grant)
			default: builder_rhs_array_muxed24 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed25 <= 1'd0;
		case (builder_litedramcore_roundrobin4_grant)
			default: builder_rhs_array_muxed25 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed26 <= 1'd0;
		case (builder_litedramcore_roundrobin4_grant)
			default: builder_rhs_array_muxed26 <= ((main_port_cmd_payload_addr[9:7] == 3'd4) & ~(((((((builder_litedramcore_locked4 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed27 <= 21'd0;
		case (builder_litedramcore_roundrobin5_grant)
			default: builder_rhs_array_muxed27 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed28 <= 1'd0;
		case (builder_litedramcore_roundrobin5_grant)
			default: builder_rhs_array_muxed28 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed29 <= 1'd0;
		case (builder_litedramcore_roundrobin5_grant)
			default: builder_rhs_array_muxed29 <= ((main_port_cmd_payload_addr[9:7] == 3'd5) & ~(((((((builder_litedramcore_locked5 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed30 <= 21'd0;
		case (builder_litedramcore_roundrobin6_grant)
			default: builder_rhs_array_muxed30 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed31 <= 1'd0;
		case (builder_litedramcore_roundrobin6_grant)
			default: builder_rhs_array_muxed31 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed32 <= 1'd0;
		case (builder_litedramcore_roundrobin6_grant)
			default: builder_rhs_array_muxed32 <= ((main_port_cmd_payload_addr[9:7] == 3'd6) & ~(((((((builder_litedramcore_locked6 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank7_lock & (builder_litedramcore_roundrobin7_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed33 <= 21'd0;
		case (builder_litedramcore_roundrobin7_grant)
			default: builder_rhs_array_muxed33 <= {main_port_cmd_payload_addr[23:10], main_port_cmd_payload_addr[6:0]};
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed34 <= 1'd0;
		case (builder_litedramcore_roundrobin7_grant)
			default: builder_rhs_array_muxed34 <= main_port_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_rhs_array_muxed35 <= 1'd0;
		case (builder_litedramcore_roundrobin7_grant)
			default: builder_rhs_array_muxed35 <= ((main_port_cmd_payload_addr[9:7] == 3'd7) & ~(((((((builder_litedramcore_locked7 | (main_interface_bank0_lock & (builder_litedramcore_roundrobin0_grant == 1'd0))) | (main_interface_bank1_lock & (builder_litedramcore_roundrobin1_grant == 1'd0))) | (main_interface_bank2_lock & (builder_litedramcore_roundrobin2_grant == 1'd0))) | (main_interface_bank3_lock & (builder_litedramcore_roundrobin3_grant == 1'd0))) | (main_interface_bank4_lock & (builder_litedramcore_roundrobin4_grant == 1'd0))) | (main_interface_bank5_lock & (builder_litedramcore_roundrobin5_grant == 1'd0))) | (main_interface_bank6_lock & (builder_litedramcore_roundrobin6_grant == 1'd0)))) & main_port_cmd_valid;
		endcase
	end
	always @(*) begin
		builder_array_muxed0 <= 3'd0;
		case (main_steerer_sel0)
			1'd0: builder_array_muxed0 <= main_nop_ba[2:0];
			1'd1: builder_array_muxed0 <= main_choose_cmd_cmd_payload_ba[2:0];
			2'd2: builder_array_muxed0 <= main_choose_req_cmd_payload_ba[2:0];
			default: builder_array_muxed0 <= main_cmd_payload_ba[2:0];
		endcase
	end
	always @(*) begin
		builder_array_muxed1 <= 14'd0;
		case (main_steerer_sel0)
			1'd0: builder_array_muxed1 <= main_nop_a;
			1'd1: builder_array_muxed1 <= main_choose_cmd_cmd_payload_a;
			2'd2: builder_array_muxed1 <= main_choose_req_cmd_payload_a;
			default: builder_array_muxed1 <= main_cmd_payload_a;
		endcase
	end
	always @(*) begin
		builder_array_muxed2 <= 1'd0;
		case (main_steerer_sel0)
			1'd0: builder_array_muxed2 <= 1'd0;
			1'd1: builder_array_muxed2 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_cas;
			2'd2: builder_array_muxed2 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_cas;
			default: builder_array_muxed2 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_cas;
		endcase
	end
	always @(*) begin
		builder_array_muxed3 <= 1'd0;
		case (main_steerer_sel0)
			1'd0: builder_array_muxed3 <= 1'd0;
			1'd1: builder_array_muxed3 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_ras;
			2'd2: builder_array_muxed3 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_ras;
			default: builder_array_muxed3 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_ras;
		endcase
	end
	always @(*) begin
		builder_array_muxed4 <= 1'd0;
		case (main_steerer_sel0)
			1'd0: builder_array_muxed4 <= 1'd0;
			1'd1: builder_array_muxed4 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_we;
			2'd2: builder_array_muxed4 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_we;
			default: builder_array_muxed4 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_array_muxed5 <= 1'd0;
		case (main_steerer_sel0)
			1'd0: builder_array_muxed5 <= 1'd0;
			1'd1: builder_array_muxed5 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_read;
			2'd2: builder_array_muxed5 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_read;
			default: builder_array_muxed5 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_read;
		endcase
	end
	always @(*) begin
		builder_array_muxed6 <= 1'd0;
		case (main_steerer_sel0)
			1'd0: builder_array_muxed6 <= 1'd0;
			1'd1: builder_array_muxed6 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_write;
			2'd2: builder_array_muxed6 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_write;
			default: builder_array_muxed6 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_write;
		endcase
	end
	always @(*) begin
		builder_array_muxed7 <= 3'd0;
		case (main_steerer_sel1)
			1'd0: builder_array_muxed7 <= main_nop_ba[2:0];
			1'd1: builder_array_muxed7 <= main_choose_cmd_cmd_payload_ba[2:0];
			2'd2: builder_array_muxed7 <= main_choose_req_cmd_payload_ba[2:0];
			default: builder_array_muxed7 <= main_cmd_payload_ba[2:0];
		endcase
	end
	always @(*) begin
		builder_array_muxed8 <= 14'd0;
		case (main_steerer_sel1)
			1'd0: builder_array_muxed8 <= main_nop_a;
			1'd1: builder_array_muxed8 <= main_choose_cmd_cmd_payload_a;
			2'd2: builder_array_muxed8 <= main_choose_req_cmd_payload_a;
			default: builder_array_muxed8 <= main_cmd_payload_a;
		endcase
	end
	always @(*) begin
		builder_array_muxed9 <= 1'd0;
		case (main_steerer_sel1)
			1'd0: builder_array_muxed9 <= 1'd0;
			1'd1: builder_array_muxed9 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_cas;
			2'd2: builder_array_muxed9 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_cas;
			default: builder_array_muxed9 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_cas;
		endcase
	end
	always @(*) begin
		builder_array_muxed10 <= 1'd0;
		case (main_steerer_sel1)
			1'd0: builder_array_muxed10 <= 1'd0;
			1'd1: builder_array_muxed10 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_ras;
			2'd2: builder_array_muxed10 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_ras;
			default: builder_array_muxed10 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_ras;
		endcase
	end
	always @(*) begin
		builder_array_muxed11 <= 1'd0;
		case (main_steerer_sel1)
			1'd0: builder_array_muxed11 <= 1'd0;
			1'd1: builder_array_muxed11 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_we;
			2'd2: builder_array_muxed11 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_we;
			default: builder_array_muxed11 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_array_muxed12 <= 1'd0;
		case (main_steerer_sel1)
			1'd0: builder_array_muxed12 <= 1'd0;
			1'd1: builder_array_muxed12 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_read;
			2'd2: builder_array_muxed12 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_read;
			default: builder_array_muxed12 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_read;
		endcase
	end
	always @(*) begin
		builder_array_muxed13 <= 1'd0;
		case (main_steerer_sel1)
			1'd0: builder_array_muxed13 <= 1'd0;
			1'd1: builder_array_muxed13 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_write;
			2'd2: builder_array_muxed13 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_write;
			default: builder_array_muxed13 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_write;
		endcase
	end
	always @(*) begin
		builder_array_muxed14 <= 3'd0;
		case (main_steerer_sel2)
			1'd0: builder_array_muxed14 <= main_nop_ba[2:0];
			1'd1: builder_array_muxed14 <= main_choose_cmd_cmd_payload_ba[2:0];
			2'd2: builder_array_muxed14 <= main_choose_req_cmd_payload_ba[2:0];
			default: builder_array_muxed14 <= main_cmd_payload_ba[2:0];
		endcase
	end
	always @(*) begin
		builder_array_muxed15 <= 14'd0;
		case (main_steerer_sel2)
			1'd0: builder_array_muxed15 <= main_nop_a;
			1'd1: builder_array_muxed15 <= main_choose_cmd_cmd_payload_a;
			2'd2: builder_array_muxed15 <= main_choose_req_cmd_payload_a;
			default: builder_array_muxed15 <= main_cmd_payload_a;
		endcase
	end
	always @(*) begin
		builder_array_muxed16 <= 1'd0;
		case (main_steerer_sel2)
			1'd0: builder_array_muxed16 <= 1'd0;
			1'd1: builder_array_muxed16 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_cas;
			2'd2: builder_array_muxed16 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_cas;
			default: builder_array_muxed16 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_cas;
		endcase
	end
	always @(*) begin
		builder_array_muxed17 <= 1'd0;
		case (main_steerer_sel2)
			1'd0: builder_array_muxed17 <= 1'd0;
			1'd1: builder_array_muxed17 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_ras;
			2'd2: builder_array_muxed17 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_ras;
			default: builder_array_muxed17 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_ras;
		endcase
	end
	always @(*) begin
		builder_array_muxed18 <= 1'd0;
		case (main_steerer_sel2)
			1'd0: builder_array_muxed18 <= 1'd0;
			1'd1: builder_array_muxed18 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_we;
			2'd2: builder_array_muxed18 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_we;
			default: builder_array_muxed18 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_array_muxed19 <= 1'd0;
		case (main_steerer_sel2)
			1'd0: builder_array_muxed19 <= 1'd0;
			1'd1: builder_array_muxed19 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_read;
			2'd2: builder_array_muxed19 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_read;
			default: builder_array_muxed19 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_read;
		endcase
	end
	always @(*) begin
		builder_array_muxed20 <= 1'd0;
		case (main_steerer_sel2)
			1'd0: builder_array_muxed20 <= 1'd0;
			1'd1: builder_array_muxed20 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_write;
			2'd2: builder_array_muxed20 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_write;
			default: builder_array_muxed20 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_write;
		endcase
	end
	always @(*) begin
		builder_array_muxed21 <= 3'd0;
		case (main_steerer_sel3)
			1'd0: builder_array_muxed21 <= main_nop_ba[2:0];
			1'd1: builder_array_muxed21 <= main_choose_cmd_cmd_payload_ba[2:0];
			2'd2: builder_array_muxed21 <= main_choose_req_cmd_payload_ba[2:0];
			default: builder_array_muxed21 <= main_cmd_payload_ba[2:0];
		endcase
	end
	always @(*) begin
		builder_array_muxed22 <= 14'd0;
		case (main_steerer_sel3)
			1'd0: builder_array_muxed22 <= main_nop_a;
			1'd1: builder_array_muxed22 <= main_choose_cmd_cmd_payload_a;
			2'd2: builder_array_muxed22 <= main_choose_req_cmd_payload_a;
			default: builder_array_muxed22 <= main_cmd_payload_a;
		endcase
	end
	always @(*) begin
		builder_array_muxed23 <= 1'd0;
		case (main_steerer_sel3)
			1'd0: builder_array_muxed23 <= 1'd0;
			1'd1: builder_array_muxed23 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_cas;
			2'd2: builder_array_muxed23 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_cas;
			default: builder_array_muxed23 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_cas;
		endcase
	end
	always @(*) begin
		builder_array_muxed24 <= 1'd0;
		case (main_steerer_sel3)
			1'd0: builder_array_muxed24 <= 1'd0;
			1'd1: builder_array_muxed24 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_ras;
			2'd2: builder_array_muxed24 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_ras;
			default: builder_array_muxed24 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_ras;
		endcase
	end
	always @(*) begin
		builder_array_muxed25 <= 1'd0;
		case (main_steerer_sel3)
			1'd0: builder_array_muxed25 <= 1'd0;
			1'd1: builder_array_muxed25 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_we;
			2'd2: builder_array_muxed25 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_we;
			default: builder_array_muxed25 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_we;
		endcase
	end
	always @(*) begin
		builder_array_muxed26 <= 1'd0;
		case (main_steerer_sel3)
			1'd0: builder_array_muxed26 <= 1'd0;
			1'd1: builder_array_muxed26 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_read;
			2'd2: builder_array_muxed26 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_read;
			default: builder_array_muxed26 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_read;
		endcase
	end
	always @(*) begin
		builder_array_muxed27 <= 1'd0;
		case (main_steerer_sel3)
			1'd0: builder_array_muxed27 <= 1'd0;
			1'd1: builder_array_muxed27 <= (main_choose_cmd_cmd_valid & main_choose_cmd_cmd_ready) & main_choose_cmd_cmd_payload_is_write;
			2'd2: builder_array_muxed27 <= (main_choose_req_cmd_valid & main_choose_req_cmd_ready) & main_choose_req_cmd_payload_is_write;
			default: builder_array_muxed27 <= (main_cmd_valid & main_cmd_ready) & main_cmd_payload_is_write;
		endcase
	end
	assign builder_xilinxasyncresetsynchronizerimpl0 = ~main_locked;
	assign builder_xilinxasyncresetsynchronizerimpl1 = ~main_locked;
	assign builder_xilinxasyncresetsynchronizerimpl2 = ~main_locked;
	assign builder_xilinxasyncresetsynchronizerimpl3 = ~main_locked;
	assign builder_xilinxasyncresetsynchronizerimpl4 = ~main_locked;
	always @(posedge iodelay_clk) begin
		if (main_reset_counter != 1'd0)
			main_reset_counter <= main_reset_counter - 1'd1;
		else
			main_ic_reset <= 1'd0;
		if (iodelay_rst) begin
			main_reset_counter <= 4'd15;
			main_ic_reset <= 1'd1;
		end
	end
	always @(posedge sys_clk) begin
		main_user_enable <= main_init_done_storage & ~main_init_error_storage;
		main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline0 <= main_a7ddrphy_dqs_oe_delay_tappeddelayline;
		main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline1 <= main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline0;
		main_a7ddrphy_dqspattern_o1 <= main_a7ddrphy_dqspattern_o0;
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip0_value0 <= main_a7ddrphy_bitslip0_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip0_value0 <= 3'd7;
		main_a7ddrphy_bitslip0_r0 <= {main_a7ddrphy_dqspattern_o1, main_a7ddrphy_bitslip0_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip1_value0 <= main_a7ddrphy_bitslip1_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip1_value0 <= 3'd7;
		main_a7ddrphy_bitslip1_r0 <= {main_a7ddrphy_dqspattern_o1, main_a7ddrphy_bitslip1_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip0_value1 <= main_a7ddrphy_bitslip0_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip0_value1 <= 3'd7;
		main_a7ddrphy_bitslip0_r1 <= {main_a7ddrphy_dfi_p3_wrdata_mask[2], main_a7ddrphy_dfi_p3_wrdata_mask[0], main_a7ddrphy_dfi_p2_wrdata_mask[2], main_a7ddrphy_dfi_p2_wrdata_mask[0], main_a7ddrphy_dfi_p1_wrdata_mask[2], main_a7ddrphy_dfi_p1_wrdata_mask[0], main_a7ddrphy_dfi_p0_wrdata_mask[2], main_a7ddrphy_dfi_p0_wrdata_mask[0], main_a7ddrphy_bitslip0_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip1_value1 <= main_a7ddrphy_bitslip1_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip1_value1 <= 3'd7;
		main_a7ddrphy_bitslip1_r1 <= {main_a7ddrphy_dfi_p3_wrdata_mask[3], main_a7ddrphy_dfi_p3_wrdata_mask[1], main_a7ddrphy_dfi_p2_wrdata_mask[3], main_a7ddrphy_dfi_p2_wrdata_mask[1], main_a7ddrphy_dfi_p1_wrdata_mask[3], main_a7ddrphy_dfi_p1_wrdata_mask[1], main_a7ddrphy_dfi_p0_wrdata_mask[3], main_a7ddrphy_dfi_p0_wrdata_mask[1], main_a7ddrphy_bitslip1_r1[15:8]};
		main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline0 <= main_a7ddrphy_dq_oe_delay_tappeddelayline;
		main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1 <= main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline0;
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip0_value2 <= main_a7ddrphy_bitslip0_value2 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip0_value2 <= 3'd7;
		main_a7ddrphy_bitslip0_r2 <= {main_a7ddrphy_dfi_p3_wrdata[16], main_a7ddrphy_dfi_p3_wrdata[0], main_a7ddrphy_dfi_p2_wrdata[16], main_a7ddrphy_dfi_p2_wrdata[0], main_a7ddrphy_dfi_p1_wrdata[16], main_a7ddrphy_dfi_p1_wrdata[0], main_a7ddrphy_dfi_p0_wrdata[16], main_a7ddrphy_dfi_p0_wrdata[0], main_a7ddrphy_bitslip0_r2[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip0_value3 <= main_a7ddrphy_bitslip0_value3 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip0_value3 <= 3'd7;
		main_a7ddrphy_bitslip0_r3 <= {main_a7ddrphy_bitslip03, main_a7ddrphy_bitslip0_r3[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip1_value2 <= main_a7ddrphy_bitslip1_value2 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip1_value2 <= 3'd7;
		main_a7ddrphy_bitslip1_r2 <= {main_a7ddrphy_dfi_p3_wrdata[17], main_a7ddrphy_dfi_p3_wrdata[1], main_a7ddrphy_dfi_p2_wrdata[17], main_a7ddrphy_dfi_p2_wrdata[1], main_a7ddrphy_dfi_p1_wrdata[17], main_a7ddrphy_dfi_p1_wrdata[1], main_a7ddrphy_dfi_p0_wrdata[17], main_a7ddrphy_dfi_p0_wrdata[1], main_a7ddrphy_bitslip1_r2[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip1_value3 <= main_a7ddrphy_bitslip1_value3 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip1_value3 <= 3'd7;
		main_a7ddrphy_bitslip1_r3 <= {main_a7ddrphy_bitslip13, main_a7ddrphy_bitslip1_r3[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip2_value0 <= main_a7ddrphy_bitslip2_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip2_value0 <= 3'd7;
		main_a7ddrphy_bitslip2_r0 <= {main_a7ddrphy_dfi_p3_wrdata[18], main_a7ddrphy_dfi_p3_wrdata[2], main_a7ddrphy_dfi_p2_wrdata[18], main_a7ddrphy_dfi_p2_wrdata[2], main_a7ddrphy_dfi_p1_wrdata[18], main_a7ddrphy_dfi_p1_wrdata[2], main_a7ddrphy_dfi_p0_wrdata[18], main_a7ddrphy_dfi_p0_wrdata[2], main_a7ddrphy_bitslip2_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip2_value1 <= main_a7ddrphy_bitslip2_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip2_value1 <= 3'd7;
		main_a7ddrphy_bitslip2_r1 <= {main_a7ddrphy_bitslip21, main_a7ddrphy_bitslip2_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip3_value0 <= main_a7ddrphy_bitslip3_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip3_value0 <= 3'd7;
		main_a7ddrphy_bitslip3_r0 <= {main_a7ddrphy_dfi_p3_wrdata[19], main_a7ddrphy_dfi_p3_wrdata[3], main_a7ddrphy_dfi_p2_wrdata[19], main_a7ddrphy_dfi_p2_wrdata[3], main_a7ddrphy_dfi_p1_wrdata[19], main_a7ddrphy_dfi_p1_wrdata[3], main_a7ddrphy_dfi_p0_wrdata[19], main_a7ddrphy_dfi_p0_wrdata[3], main_a7ddrphy_bitslip3_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip3_value1 <= main_a7ddrphy_bitslip3_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip3_value1 <= 3'd7;
		main_a7ddrphy_bitslip3_r1 <= {main_a7ddrphy_bitslip31, main_a7ddrphy_bitslip3_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip4_value0 <= main_a7ddrphy_bitslip4_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip4_value0 <= 3'd7;
		main_a7ddrphy_bitslip4_r0 <= {main_a7ddrphy_dfi_p3_wrdata[20], main_a7ddrphy_dfi_p3_wrdata[4], main_a7ddrphy_dfi_p2_wrdata[20], main_a7ddrphy_dfi_p2_wrdata[4], main_a7ddrphy_dfi_p1_wrdata[20], main_a7ddrphy_dfi_p1_wrdata[4], main_a7ddrphy_dfi_p0_wrdata[20], main_a7ddrphy_dfi_p0_wrdata[4], main_a7ddrphy_bitslip4_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip4_value1 <= main_a7ddrphy_bitslip4_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip4_value1 <= 3'd7;
		main_a7ddrphy_bitslip4_r1 <= {main_a7ddrphy_bitslip41, main_a7ddrphy_bitslip4_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip5_value0 <= main_a7ddrphy_bitslip5_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip5_value0 <= 3'd7;
		main_a7ddrphy_bitslip5_r0 <= {main_a7ddrphy_dfi_p3_wrdata[21], main_a7ddrphy_dfi_p3_wrdata[5], main_a7ddrphy_dfi_p2_wrdata[21], main_a7ddrphy_dfi_p2_wrdata[5], main_a7ddrphy_dfi_p1_wrdata[21], main_a7ddrphy_dfi_p1_wrdata[5], main_a7ddrphy_dfi_p0_wrdata[21], main_a7ddrphy_dfi_p0_wrdata[5], main_a7ddrphy_bitslip5_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip5_value1 <= main_a7ddrphy_bitslip5_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip5_value1 <= 3'd7;
		main_a7ddrphy_bitslip5_r1 <= {main_a7ddrphy_bitslip51, main_a7ddrphy_bitslip5_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip6_value0 <= main_a7ddrphy_bitslip6_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip6_value0 <= 3'd7;
		main_a7ddrphy_bitslip6_r0 <= {main_a7ddrphy_dfi_p3_wrdata[22], main_a7ddrphy_dfi_p3_wrdata[6], main_a7ddrphy_dfi_p2_wrdata[22], main_a7ddrphy_dfi_p2_wrdata[6], main_a7ddrphy_dfi_p1_wrdata[22], main_a7ddrphy_dfi_p1_wrdata[6], main_a7ddrphy_dfi_p0_wrdata[22], main_a7ddrphy_dfi_p0_wrdata[6], main_a7ddrphy_bitslip6_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip6_value1 <= main_a7ddrphy_bitslip6_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip6_value1 <= 3'd7;
		main_a7ddrphy_bitslip6_r1 <= {main_a7ddrphy_bitslip61, main_a7ddrphy_bitslip6_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip7_value0 <= main_a7ddrphy_bitslip7_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip7_value0 <= 3'd7;
		main_a7ddrphy_bitslip7_r0 <= {main_a7ddrphy_dfi_p3_wrdata[23], main_a7ddrphy_dfi_p3_wrdata[7], main_a7ddrphy_dfi_p2_wrdata[23], main_a7ddrphy_dfi_p2_wrdata[7], main_a7ddrphy_dfi_p1_wrdata[23], main_a7ddrphy_dfi_p1_wrdata[7], main_a7ddrphy_dfi_p0_wrdata[23], main_a7ddrphy_dfi_p0_wrdata[7], main_a7ddrphy_bitslip7_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip7_value1 <= main_a7ddrphy_bitslip7_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip7_value1 <= 3'd7;
		main_a7ddrphy_bitslip7_r1 <= {main_a7ddrphy_bitslip71, main_a7ddrphy_bitslip7_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip8_value0 <= main_a7ddrphy_bitslip8_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip8_value0 <= 3'd7;
		main_a7ddrphy_bitslip8_r0 <= {main_a7ddrphy_dfi_p3_wrdata[24], main_a7ddrphy_dfi_p3_wrdata[8], main_a7ddrphy_dfi_p2_wrdata[24], main_a7ddrphy_dfi_p2_wrdata[8], main_a7ddrphy_dfi_p1_wrdata[24], main_a7ddrphy_dfi_p1_wrdata[8], main_a7ddrphy_dfi_p0_wrdata[24], main_a7ddrphy_dfi_p0_wrdata[8], main_a7ddrphy_bitslip8_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip8_value1 <= main_a7ddrphy_bitslip8_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip8_value1 <= 3'd7;
		main_a7ddrphy_bitslip8_r1 <= {main_a7ddrphy_bitslip81, main_a7ddrphy_bitslip8_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip9_value0 <= main_a7ddrphy_bitslip9_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip9_value0 <= 3'd7;
		main_a7ddrphy_bitslip9_r0 <= {main_a7ddrphy_dfi_p3_wrdata[25], main_a7ddrphy_dfi_p3_wrdata[9], main_a7ddrphy_dfi_p2_wrdata[25], main_a7ddrphy_dfi_p2_wrdata[9], main_a7ddrphy_dfi_p1_wrdata[25], main_a7ddrphy_dfi_p1_wrdata[9], main_a7ddrphy_dfi_p0_wrdata[25], main_a7ddrphy_dfi_p0_wrdata[9], main_a7ddrphy_bitslip9_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip9_value1 <= main_a7ddrphy_bitslip9_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip9_value1 <= 3'd7;
		main_a7ddrphy_bitslip9_r1 <= {main_a7ddrphy_bitslip91, main_a7ddrphy_bitslip9_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip10_value0 <= main_a7ddrphy_bitslip10_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip10_value0 <= 3'd7;
		main_a7ddrphy_bitslip10_r0 <= {main_a7ddrphy_dfi_p3_wrdata[26], main_a7ddrphy_dfi_p3_wrdata[10], main_a7ddrphy_dfi_p2_wrdata[26], main_a7ddrphy_dfi_p2_wrdata[10], main_a7ddrphy_dfi_p1_wrdata[26], main_a7ddrphy_dfi_p1_wrdata[10], main_a7ddrphy_dfi_p0_wrdata[26], main_a7ddrphy_dfi_p0_wrdata[10], main_a7ddrphy_bitslip10_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip10_value1 <= main_a7ddrphy_bitslip10_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip10_value1 <= 3'd7;
		main_a7ddrphy_bitslip10_r1 <= {main_a7ddrphy_bitslip101, main_a7ddrphy_bitslip10_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip11_value0 <= main_a7ddrphy_bitslip11_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip11_value0 <= 3'd7;
		main_a7ddrphy_bitslip11_r0 <= {main_a7ddrphy_dfi_p3_wrdata[27], main_a7ddrphy_dfi_p3_wrdata[11], main_a7ddrphy_dfi_p2_wrdata[27], main_a7ddrphy_dfi_p2_wrdata[11], main_a7ddrphy_dfi_p1_wrdata[27], main_a7ddrphy_dfi_p1_wrdata[11], main_a7ddrphy_dfi_p0_wrdata[27], main_a7ddrphy_dfi_p0_wrdata[11], main_a7ddrphy_bitslip11_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip11_value1 <= main_a7ddrphy_bitslip11_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip11_value1 <= 3'd7;
		main_a7ddrphy_bitslip11_r1 <= {main_a7ddrphy_bitslip111, main_a7ddrphy_bitslip11_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip12_value0 <= main_a7ddrphy_bitslip12_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip12_value0 <= 3'd7;
		main_a7ddrphy_bitslip12_r0 <= {main_a7ddrphy_dfi_p3_wrdata[28], main_a7ddrphy_dfi_p3_wrdata[12], main_a7ddrphy_dfi_p2_wrdata[28], main_a7ddrphy_dfi_p2_wrdata[12], main_a7ddrphy_dfi_p1_wrdata[28], main_a7ddrphy_dfi_p1_wrdata[12], main_a7ddrphy_dfi_p0_wrdata[28], main_a7ddrphy_dfi_p0_wrdata[12], main_a7ddrphy_bitslip12_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip12_value1 <= main_a7ddrphy_bitslip12_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip12_value1 <= 3'd7;
		main_a7ddrphy_bitslip12_r1 <= {main_a7ddrphy_bitslip121, main_a7ddrphy_bitslip12_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip13_value0 <= main_a7ddrphy_bitslip13_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip13_value0 <= 3'd7;
		main_a7ddrphy_bitslip13_r0 <= {main_a7ddrphy_dfi_p3_wrdata[29], main_a7ddrphy_dfi_p3_wrdata[13], main_a7ddrphy_dfi_p2_wrdata[29], main_a7ddrphy_dfi_p2_wrdata[13], main_a7ddrphy_dfi_p1_wrdata[29], main_a7ddrphy_dfi_p1_wrdata[13], main_a7ddrphy_dfi_p0_wrdata[29], main_a7ddrphy_dfi_p0_wrdata[13], main_a7ddrphy_bitslip13_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip13_value1 <= main_a7ddrphy_bitslip13_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip13_value1 <= 3'd7;
		main_a7ddrphy_bitslip13_r1 <= {main_a7ddrphy_bitslip131, main_a7ddrphy_bitslip13_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip14_value0 <= main_a7ddrphy_bitslip14_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip14_value0 <= 3'd7;
		main_a7ddrphy_bitslip14_r0 <= {main_a7ddrphy_dfi_p3_wrdata[30], main_a7ddrphy_dfi_p3_wrdata[14], main_a7ddrphy_dfi_p2_wrdata[30], main_a7ddrphy_dfi_p2_wrdata[14], main_a7ddrphy_dfi_p1_wrdata[30], main_a7ddrphy_dfi_p1_wrdata[14], main_a7ddrphy_dfi_p0_wrdata[30], main_a7ddrphy_dfi_p0_wrdata[14], main_a7ddrphy_bitslip14_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip14_value1 <= main_a7ddrphy_bitslip14_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip14_value1 <= 3'd7;
		main_a7ddrphy_bitslip14_r1 <= {main_a7ddrphy_bitslip141, main_a7ddrphy_bitslip14_r1[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_re)
			main_a7ddrphy_bitslip15_value0 <= main_a7ddrphy_bitslip15_value0 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_wdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip15_value0 <= 3'd7;
		main_a7ddrphy_bitslip15_r0 <= {main_a7ddrphy_dfi_p3_wrdata[31], main_a7ddrphy_dfi_p3_wrdata[15], main_a7ddrphy_dfi_p2_wrdata[31], main_a7ddrphy_dfi_p2_wrdata[15], main_a7ddrphy_dfi_p1_wrdata[31], main_a7ddrphy_dfi_p1_wrdata[15], main_a7ddrphy_dfi_p0_wrdata[31], main_a7ddrphy_dfi_p0_wrdata[15], main_a7ddrphy_bitslip15_r0[15:8]};
		if (main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_re)
			main_a7ddrphy_bitslip15_value1 <= main_a7ddrphy_bitslip15_value1 + 1'd1;
		if ((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_bitslip_rst_re) | main_a7ddrphy_rst_storage)
			main_a7ddrphy_bitslip15_value1 <= 3'd7;
		main_a7ddrphy_bitslip15_r1 <= {main_a7ddrphy_bitslip151, main_a7ddrphy_bitslip15_r1[15:8]};
		main_a7ddrphy_rddata_en_tappeddelayline0 <= ((main_a7ddrphy_dfi_p0_rddata_en | main_a7ddrphy_dfi_p1_rddata_en) | main_a7ddrphy_dfi_p2_rddata_en) | main_a7ddrphy_dfi_p3_rddata_en;
		main_a7ddrphy_rddata_en_tappeddelayline1 <= main_a7ddrphy_rddata_en_tappeddelayline0;
		main_a7ddrphy_rddata_en_tappeddelayline2 <= main_a7ddrphy_rddata_en_tappeddelayline1;
		main_a7ddrphy_rddata_en_tappeddelayline3 <= main_a7ddrphy_rddata_en_tappeddelayline2;
		main_a7ddrphy_rddata_en_tappeddelayline4 <= main_a7ddrphy_rddata_en_tappeddelayline3;
		main_a7ddrphy_rddata_en_tappeddelayline5 <= main_a7ddrphy_rddata_en_tappeddelayline4;
		main_a7ddrphy_rddata_en_tappeddelayline6 <= main_a7ddrphy_rddata_en_tappeddelayline5;
		main_a7ddrphy_rddata_en_tappeddelayline7 <= main_a7ddrphy_rddata_en_tappeddelayline6;
		main_a7ddrphy_wrdata_en_tappeddelayline0 <= ((main_a7ddrphy_dfi_p0_wrdata_en | main_a7ddrphy_dfi_p1_wrdata_en) | main_a7ddrphy_dfi_p2_wrdata_en) | main_a7ddrphy_dfi_p3_wrdata_en;
		main_a7ddrphy_wrdata_en_tappeddelayline1 <= main_a7ddrphy_wrdata_en_tappeddelayline0;
		main_a7ddrphy_wrdata_en_tappeddelayline2 <= main_a7ddrphy_wrdata_en_tappeddelayline1;
		if (main_csr_dfi_p0_rddata_valid)
			main_phaseinjector0_rddata_status <= main_csr_dfi_p0_rddata;
		if (main_csr_dfi_p1_rddata_valid)
			main_phaseinjector1_rddata_status <= main_csr_dfi_p1_rddata;
		if (main_csr_dfi_p2_rddata_valid)
			main_phaseinjector2_rddata_status <= main_csr_dfi_p2_rddata;
		if (main_csr_dfi_p3_rddata_valid)
			main_phaseinjector3_rddata_status <= main_csr_dfi_p3_rddata;
		if (main_timer_wait & ~main_timer_done0)
			main_timer_count1 <= main_timer_count1 - 1'd1;
		else
			main_timer_count1 <= 9'd390;
		main_postponer_req_o <= 1'd0;
		if (main_postponer_req_i) begin
			main_postponer_count <= main_postponer_count - 1'd1;
			if (main_postponer_count == 1'd0) begin
				main_postponer_count <= 1'd0;
				main_postponer_req_o <= 1'd1;
			end
		end
		if (main_sequencer_start0)
			main_sequencer_count <= 1'd0;
		else if (main_sequencer_done1) begin
			if (main_sequencer_count != 1'd0)
				main_sequencer_count <= main_sequencer_count - 1'd1;
		end
		main_cmd_payload_a <= 1'd0;
		main_cmd_payload_ba <= 1'd0;
		main_cmd_payload_cas <= 1'd0;
		main_cmd_payload_ras <= 1'd0;
		main_cmd_payload_we <= 1'd0;
		main_sequencer_done1 <= 1'd0;
		if (main_sequencer_start1 & (main_sequencer_counter == 1'd0)) begin
			main_cmd_payload_a <= 11'd1024;
			main_cmd_payload_ba <= 1'd0;
			main_cmd_payload_cas <= 1'd0;
			main_cmd_payload_ras <= 1'd1;
			main_cmd_payload_we <= 1'd1;
		end
		if (main_sequencer_counter == 2'd2) begin
			main_cmd_payload_a <= 11'd1024;
			main_cmd_payload_ba <= 1'd0;
			main_cmd_payload_cas <= 1'd1;
			main_cmd_payload_ras <= 1'd1;
			main_cmd_payload_we <= 1'd0;
		end
		if (main_sequencer_counter == 6'd34) begin
			main_cmd_payload_a <= 1'd0;
			main_cmd_payload_ba <= 1'd0;
			main_cmd_payload_cas <= 1'd0;
			main_cmd_payload_ras <= 1'd0;
			main_cmd_payload_we <= 1'd0;
			main_sequencer_done1 <= 1'd1;
		end
		if (main_sequencer_counter == 6'd34)
			main_sequencer_counter <= 1'd0;
		else if (main_sequencer_counter != 1'd0)
			main_sequencer_counter <= main_sequencer_counter + 1'd1;
		else if (main_sequencer_start1)
			main_sequencer_counter <= 1'd1;
		if (main_zqcs_timer_wait & ~main_zqcs_timer_done0)
			main_zqcs_timer_count1 <= main_zqcs_timer_count1 - 1'd1;
		else
			main_zqcs_timer_count1 <= 26'd49999999;
		main_zqcs_executer_done <= 1'd0;
		if (main_zqcs_executer_start & (main_zqcs_executer_counter == 1'd0)) begin
			main_cmd_payload_a <= 11'd1024;
			main_cmd_payload_ba <= 1'd0;
			main_cmd_payload_cas <= 1'd0;
			main_cmd_payload_ras <= 1'd1;
			main_cmd_payload_we <= 1'd1;
		end
		if (main_zqcs_executer_counter == 2'd2) begin
			main_cmd_payload_a <= 1'd0;
			main_cmd_payload_ba <= 1'd0;
			main_cmd_payload_cas <= 1'd0;
			main_cmd_payload_ras <= 1'd0;
			main_cmd_payload_we <= 1'd1;
		end
		if (main_zqcs_executer_counter == 5'd18) begin
			main_cmd_payload_a <= 1'd0;
			main_cmd_payload_ba <= 1'd0;
			main_cmd_payload_cas <= 1'd0;
			main_cmd_payload_ras <= 1'd0;
			main_cmd_payload_we <= 1'd0;
			main_zqcs_executer_done <= 1'd1;
		end
		if (main_zqcs_executer_counter == 5'd18)
			main_zqcs_executer_counter <= 1'd0;
		else if (main_zqcs_executer_counter != 1'd0)
			main_zqcs_executer_counter <= main_zqcs_executer_counter + 1'd1;
		else if (main_zqcs_executer_start)
			main_zqcs_executer_counter <= 1'd1;
		builder_litedramcore_refresher_state <= builder_litedramcore_refresher_next_state;
		if (main_bankmachine0_row_close)
			main_bankmachine0_row_opened <= 1'd0;
		else if (main_bankmachine0_row_open) begin
			main_bankmachine0_row_opened <= 1'd1;
			main_bankmachine0_row <= main_bankmachine0_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine0_syncfifo0_we & main_bankmachine0_syncfifo0_writable) & ~main_bankmachine0_replace)
			main_bankmachine0_produce <= main_bankmachine0_produce + 1'd1;
		if (main_bankmachine0_do_read)
			main_bankmachine0_consume <= main_bankmachine0_consume + 1'd1;
		if ((main_bankmachine0_syncfifo0_we & main_bankmachine0_syncfifo0_writable) & ~main_bankmachine0_replace) begin
			if (~main_bankmachine0_do_read)
				main_bankmachine0_level <= main_bankmachine0_level + 1'd1;
		end
		else if (main_bankmachine0_do_read)
			main_bankmachine0_level <= main_bankmachine0_level - 1'd1;
		if (~main_bankmachine0_pipe_valid_source_valid | main_bankmachine0_pipe_valid_source_ready) begin
			main_bankmachine0_pipe_valid_source_valid <= main_bankmachine0_pipe_valid_sink_valid;
			main_bankmachine0_pipe_valid_source_first <= main_bankmachine0_pipe_valid_sink_first;
			main_bankmachine0_pipe_valid_source_last <= main_bankmachine0_pipe_valid_sink_last;
			main_bankmachine0_pipe_valid_source_payload_we <= main_bankmachine0_pipe_valid_sink_payload_we;
			main_bankmachine0_pipe_valid_source_payload_addr <= main_bankmachine0_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine0_twtpcon_valid) begin
			main_bankmachine0_twtpcon_count <= 3'd4;
			main_bankmachine0_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine0_twtpcon_ready) begin
			main_bankmachine0_twtpcon_count <= main_bankmachine0_twtpcon_count - 1'd1;
			if (main_bankmachine0_twtpcon_count == 1'd1)
				main_bankmachine0_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine0_trccon_valid) begin
			main_bankmachine0_trccon_count <= 2'd3;
			main_bankmachine0_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine0_trccon_ready) begin
			main_bankmachine0_trccon_count <= main_bankmachine0_trccon_count - 1'd1;
			if (main_bankmachine0_trccon_count == 1'd1)
				main_bankmachine0_trccon_ready <= 1'd1;
		end
		if (main_bankmachine0_trascon_valid) begin
			main_bankmachine0_trascon_count <= 2'd2;
			main_bankmachine0_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine0_trascon_ready) begin
			main_bankmachine0_trascon_count <= main_bankmachine0_trascon_count - 1'd1;
			if (main_bankmachine0_trascon_count == 1'd1)
				main_bankmachine0_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine0_state <= builder_litedramcore_bankmachine0_next_state;
		if (main_bankmachine1_row_close)
			main_bankmachine1_row_opened <= 1'd0;
		else if (main_bankmachine1_row_open) begin
			main_bankmachine1_row_opened <= 1'd1;
			main_bankmachine1_row <= main_bankmachine1_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine1_syncfifo1_we & main_bankmachine1_syncfifo1_writable) & ~main_bankmachine1_replace)
			main_bankmachine1_produce <= main_bankmachine1_produce + 1'd1;
		if (main_bankmachine1_do_read)
			main_bankmachine1_consume <= main_bankmachine1_consume + 1'd1;
		if ((main_bankmachine1_syncfifo1_we & main_bankmachine1_syncfifo1_writable) & ~main_bankmachine1_replace) begin
			if (~main_bankmachine1_do_read)
				main_bankmachine1_level <= main_bankmachine1_level + 1'd1;
		end
		else if (main_bankmachine1_do_read)
			main_bankmachine1_level <= main_bankmachine1_level - 1'd1;
		if (~main_bankmachine1_pipe_valid_source_valid | main_bankmachine1_pipe_valid_source_ready) begin
			main_bankmachine1_pipe_valid_source_valid <= main_bankmachine1_pipe_valid_sink_valid;
			main_bankmachine1_pipe_valid_source_first <= main_bankmachine1_pipe_valid_sink_first;
			main_bankmachine1_pipe_valid_source_last <= main_bankmachine1_pipe_valid_sink_last;
			main_bankmachine1_pipe_valid_source_payload_we <= main_bankmachine1_pipe_valid_sink_payload_we;
			main_bankmachine1_pipe_valid_source_payload_addr <= main_bankmachine1_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine1_twtpcon_valid) begin
			main_bankmachine1_twtpcon_count <= 3'd4;
			main_bankmachine1_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine1_twtpcon_ready) begin
			main_bankmachine1_twtpcon_count <= main_bankmachine1_twtpcon_count - 1'd1;
			if (main_bankmachine1_twtpcon_count == 1'd1)
				main_bankmachine1_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine1_trccon_valid) begin
			main_bankmachine1_trccon_count <= 2'd3;
			main_bankmachine1_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine1_trccon_ready) begin
			main_bankmachine1_trccon_count <= main_bankmachine1_trccon_count - 1'd1;
			if (main_bankmachine1_trccon_count == 1'd1)
				main_bankmachine1_trccon_ready <= 1'd1;
		end
		if (main_bankmachine1_trascon_valid) begin
			main_bankmachine1_trascon_count <= 2'd2;
			main_bankmachine1_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine1_trascon_ready) begin
			main_bankmachine1_trascon_count <= main_bankmachine1_trascon_count - 1'd1;
			if (main_bankmachine1_trascon_count == 1'd1)
				main_bankmachine1_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine1_state <= builder_litedramcore_bankmachine1_next_state;
		if (main_bankmachine2_row_close)
			main_bankmachine2_row_opened <= 1'd0;
		else if (main_bankmachine2_row_open) begin
			main_bankmachine2_row_opened <= 1'd1;
			main_bankmachine2_row <= main_bankmachine2_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine2_syncfifo2_we & main_bankmachine2_syncfifo2_writable) & ~main_bankmachine2_replace)
			main_bankmachine2_produce <= main_bankmachine2_produce + 1'd1;
		if (main_bankmachine2_do_read)
			main_bankmachine2_consume <= main_bankmachine2_consume + 1'd1;
		if ((main_bankmachine2_syncfifo2_we & main_bankmachine2_syncfifo2_writable) & ~main_bankmachine2_replace) begin
			if (~main_bankmachine2_do_read)
				main_bankmachine2_level <= main_bankmachine2_level + 1'd1;
		end
		else if (main_bankmachine2_do_read)
			main_bankmachine2_level <= main_bankmachine2_level - 1'd1;
		if (~main_bankmachine2_pipe_valid_source_valid | main_bankmachine2_pipe_valid_source_ready) begin
			main_bankmachine2_pipe_valid_source_valid <= main_bankmachine2_pipe_valid_sink_valid;
			main_bankmachine2_pipe_valid_source_first <= main_bankmachine2_pipe_valid_sink_first;
			main_bankmachine2_pipe_valid_source_last <= main_bankmachine2_pipe_valid_sink_last;
			main_bankmachine2_pipe_valid_source_payload_we <= main_bankmachine2_pipe_valid_sink_payload_we;
			main_bankmachine2_pipe_valid_source_payload_addr <= main_bankmachine2_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine2_twtpcon_valid) begin
			main_bankmachine2_twtpcon_count <= 3'd4;
			main_bankmachine2_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine2_twtpcon_ready) begin
			main_bankmachine2_twtpcon_count <= main_bankmachine2_twtpcon_count - 1'd1;
			if (main_bankmachine2_twtpcon_count == 1'd1)
				main_bankmachine2_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine2_trccon_valid) begin
			main_bankmachine2_trccon_count <= 2'd3;
			main_bankmachine2_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine2_trccon_ready) begin
			main_bankmachine2_trccon_count <= main_bankmachine2_trccon_count - 1'd1;
			if (main_bankmachine2_trccon_count == 1'd1)
				main_bankmachine2_trccon_ready <= 1'd1;
		end
		if (main_bankmachine2_trascon_valid) begin
			main_bankmachine2_trascon_count <= 2'd2;
			main_bankmachine2_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine2_trascon_ready) begin
			main_bankmachine2_trascon_count <= main_bankmachine2_trascon_count - 1'd1;
			if (main_bankmachine2_trascon_count == 1'd1)
				main_bankmachine2_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine2_state <= builder_litedramcore_bankmachine2_next_state;
		if (main_bankmachine3_row_close)
			main_bankmachine3_row_opened <= 1'd0;
		else if (main_bankmachine3_row_open) begin
			main_bankmachine3_row_opened <= 1'd1;
			main_bankmachine3_row <= main_bankmachine3_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine3_syncfifo3_we & main_bankmachine3_syncfifo3_writable) & ~main_bankmachine3_replace)
			main_bankmachine3_produce <= main_bankmachine3_produce + 1'd1;
		if (main_bankmachine3_do_read)
			main_bankmachine3_consume <= main_bankmachine3_consume + 1'd1;
		if ((main_bankmachine3_syncfifo3_we & main_bankmachine3_syncfifo3_writable) & ~main_bankmachine3_replace) begin
			if (~main_bankmachine3_do_read)
				main_bankmachine3_level <= main_bankmachine3_level + 1'd1;
		end
		else if (main_bankmachine3_do_read)
			main_bankmachine3_level <= main_bankmachine3_level - 1'd1;
		if (~main_bankmachine3_pipe_valid_source_valid | main_bankmachine3_pipe_valid_source_ready) begin
			main_bankmachine3_pipe_valid_source_valid <= main_bankmachine3_pipe_valid_sink_valid;
			main_bankmachine3_pipe_valid_source_first <= main_bankmachine3_pipe_valid_sink_first;
			main_bankmachine3_pipe_valid_source_last <= main_bankmachine3_pipe_valid_sink_last;
			main_bankmachine3_pipe_valid_source_payload_we <= main_bankmachine3_pipe_valid_sink_payload_we;
			main_bankmachine3_pipe_valid_source_payload_addr <= main_bankmachine3_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine3_twtpcon_valid) begin
			main_bankmachine3_twtpcon_count <= 3'd4;
			main_bankmachine3_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine3_twtpcon_ready) begin
			main_bankmachine3_twtpcon_count <= main_bankmachine3_twtpcon_count - 1'd1;
			if (main_bankmachine3_twtpcon_count == 1'd1)
				main_bankmachine3_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine3_trccon_valid) begin
			main_bankmachine3_trccon_count <= 2'd3;
			main_bankmachine3_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine3_trccon_ready) begin
			main_bankmachine3_trccon_count <= main_bankmachine3_trccon_count - 1'd1;
			if (main_bankmachine3_trccon_count == 1'd1)
				main_bankmachine3_trccon_ready <= 1'd1;
		end
		if (main_bankmachine3_trascon_valid) begin
			main_bankmachine3_trascon_count <= 2'd2;
			main_bankmachine3_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine3_trascon_ready) begin
			main_bankmachine3_trascon_count <= main_bankmachine3_trascon_count - 1'd1;
			if (main_bankmachine3_trascon_count == 1'd1)
				main_bankmachine3_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine3_state <= builder_litedramcore_bankmachine3_next_state;
		if (main_bankmachine4_row_close)
			main_bankmachine4_row_opened <= 1'd0;
		else if (main_bankmachine4_row_open) begin
			main_bankmachine4_row_opened <= 1'd1;
			main_bankmachine4_row <= main_bankmachine4_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine4_syncfifo4_we & main_bankmachine4_syncfifo4_writable) & ~main_bankmachine4_replace)
			main_bankmachine4_produce <= main_bankmachine4_produce + 1'd1;
		if (main_bankmachine4_do_read)
			main_bankmachine4_consume <= main_bankmachine4_consume + 1'd1;
		if ((main_bankmachine4_syncfifo4_we & main_bankmachine4_syncfifo4_writable) & ~main_bankmachine4_replace) begin
			if (~main_bankmachine4_do_read)
				main_bankmachine4_level <= main_bankmachine4_level + 1'd1;
		end
		else if (main_bankmachine4_do_read)
			main_bankmachine4_level <= main_bankmachine4_level - 1'd1;
		if (~main_bankmachine4_pipe_valid_source_valid | main_bankmachine4_pipe_valid_source_ready) begin
			main_bankmachine4_pipe_valid_source_valid <= main_bankmachine4_pipe_valid_sink_valid;
			main_bankmachine4_pipe_valid_source_first <= main_bankmachine4_pipe_valid_sink_first;
			main_bankmachine4_pipe_valid_source_last <= main_bankmachine4_pipe_valid_sink_last;
			main_bankmachine4_pipe_valid_source_payload_we <= main_bankmachine4_pipe_valid_sink_payload_we;
			main_bankmachine4_pipe_valid_source_payload_addr <= main_bankmachine4_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine4_twtpcon_valid) begin
			main_bankmachine4_twtpcon_count <= 3'd4;
			main_bankmachine4_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine4_twtpcon_ready) begin
			main_bankmachine4_twtpcon_count <= main_bankmachine4_twtpcon_count - 1'd1;
			if (main_bankmachine4_twtpcon_count == 1'd1)
				main_bankmachine4_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine4_trccon_valid) begin
			main_bankmachine4_trccon_count <= 2'd3;
			main_bankmachine4_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine4_trccon_ready) begin
			main_bankmachine4_trccon_count <= main_bankmachine4_trccon_count - 1'd1;
			if (main_bankmachine4_trccon_count == 1'd1)
				main_bankmachine4_trccon_ready <= 1'd1;
		end
		if (main_bankmachine4_trascon_valid) begin
			main_bankmachine4_trascon_count <= 2'd2;
			main_bankmachine4_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine4_trascon_ready) begin
			main_bankmachine4_trascon_count <= main_bankmachine4_trascon_count - 1'd1;
			if (main_bankmachine4_trascon_count == 1'd1)
				main_bankmachine4_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine4_state <= builder_litedramcore_bankmachine4_next_state;
		if (main_bankmachine5_row_close)
			main_bankmachine5_row_opened <= 1'd0;
		else if (main_bankmachine5_row_open) begin
			main_bankmachine5_row_opened <= 1'd1;
			main_bankmachine5_row <= main_bankmachine5_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine5_syncfifo5_we & main_bankmachine5_syncfifo5_writable) & ~main_bankmachine5_replace)
			main_bankmachine5_produce <= main_bankmachine5_produce + 1'd1;
		if (main_bankmachine5_do_read)
			main_bankmachine5_consume <= main_bankmachine5_consume + 1'd1;
		if ((main_bankmachine5_syncfifo5_we & main_bankmachine5_syncfifo5_writable) & ~main_bankmachine5_replace) begin
			if (~main_bankmachine5_do_read)
				main_bankmachine5_level <= main_bankmachine5_level + 1'd1;
		end
		else if (main_bankmachine5_do_read)
			main_bankmachine5_level <= main_bankmachine5_level - 1'd1;
		if (~main_bankmachine5_pipe_valid_source_valid | main_bankmachine5_pipe_valid_source_ready) begin
			main_bankmachine5_pipe_valid_source_valid <= main_bankmachine5_pipe_valid_sink_valid;
			main_bankmachine5_pipe_valid_source_first <= main_bankmachine5_pipe_valid_sink_first;
			main_bankmachine5_pipe_valid_source_last <= main_bankmachine5_pipe_valid_sink_last;
			main_bankmachine5_pipe_valid_source_payload_we <= main_bankmachine5_pipe_valid_sink_payload_we;
			main_bankmachine5_pipe_valid_source_payload_addr <= main_bankmachine5_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine5_twtpcon_valid) begin
			main_bankmachine5_twtpcon_count <= 3'd4;
			main_bankmachine5_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine5_twtpcon_ready) begin
			main_bankmachine5_twtpcon_count <= main_bankmachine5_twtpcon_count - 1'd1;
			if (main_bankmachine5_twtpcon_count == 1'd1)
				main_bankmachine5_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine5_trccon_valid) begin
			main_bankmachine5_trccon_count <= 2'd3;
			main_bankmachine5_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine5_trccon_ready) begin
			main_bankmachine5_trccon_count <= main_bankmachine5_trccon_count - 1'd1;
			if (main_bankmachine5_trccon_count == 1'd1)
				main_bankmachine5_trccon_ready <= 1'd1;
		end
		if (main_bankmachine5_trascon_valid) begin
			main_bankmachine5_trascon_count <= 2'd2;
			main_bankmachine5_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine5_trascon_ready) begin
			main_bankmachine5_trascon_count <= main_bankmachine5_trascon_count - 1'd1;
			if (main_bankmachine5_trascon_count == 1'd1)
				main_bankmachine5_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine5_state <= builder_litedramcore_bankmachine5_next_state;
		if (main_bankmachine6_row_close)
			main_bankmachine6_row_opened <= 1'd0;
		else if (main_bankmachine6_row_open) begin
			main_bankmachine6_row_opened <= 1'd1;
			main_bankmachine6_row <= main_bankmachine6_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine6_syncfifo6_we & main_bankmachine6_syncfifo6_writable) & ~main_bankmachine6_replace)
			main_bankmachine6_produce <= main_bankmachine6_produce + 1'd1;
		if (main_bankmachine6_do_read)
			main_bankmachine6_consume <= main_bankmachine6_consume + 1'd1;
		if ((main_bankmachine6_syncfifo6_we & main_bankmachine6_syncfifo6_writable) & ~main_bankmachine6_replace) begin
			if (~main_bankmachine6_do_read)
				main_bankmachine6_level <= main_bankmachine6_level + 1'd1;
		end
		else if (main_bankmachine6_do_read)
			main_bankmachine6_level <= main_bankmachine6_level - 1'd1;
		if (~main_bankmachine6_pipe_valid_source_valid | main_bankmachine6_pipe_valid_source_ready) begin
			main_bankmachine6_pipe_valid_source_valid <= main_bankmachine6_pipe_valid_sink_valid;
			main_bankmachine6_pipe_valid_source_first <= main_bankmachine6_pipe_valid_sink_first;
			main_bankmachine6_pipe_valid_source_last <= main_bankmachine6_pipe_valid_sink_last;
			main_bankmachine6_pipe_valid_source_payload_we <= main_bankmachine6_pipe_valid_sink_payload_we;
			main_bankmachine6_pipe_valid_source_payload_addr <= main_bankmachine6_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine6_twtpcon_valid) begin
			main_bankmachine6_twtpcon_count <= 3'd4;
			main_bankmachine6_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine6_twtpcon_ready) begin
			main_bankmachine6_twtpcon_count <= main_bankmachine6_twtpcon_count - 1'd1;
			if (main_bankmachine6_twtpcon_count == 1'd1)
				main_bankmachine6_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine6_trccon_valid) begin
			main_bankmachine6_trccon_count <= 2'd3;
			main_bankmachine6_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine6_trccon_ready) begin
			main_bankmachine6_trccon_count <= main_bankmachine6_trccon_count - 1'd1;
			if (main_bankmachine6_trccon_count == 1'd1)
				main_bankmachine6_trccon_ready <= 1'd1;
		end
		if (main_bankmachine6_trascon_valid) begin
			main_bankmachine6_trascon_count <= 2'd2;
			main_bankmachine6_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine6_trascon_ready) begin
			main_bankmachine6_trascon_count <= main_bankmachine6_trascon_count - 1'd1;
			if (main_bankmachine6_trascon_count == 1'd1)
				main_bankmachine6_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine6_state <= builder_litedramcore_bankmachine6_next_state;
		if (main_bankmachine7_row_close)
			main_bankmachine7_row_opened <= 1'd0;
		else if (main_bankmachine7_row_open) begin
			main_bankmachine7_row_opened <= 1'd1;
			main_bankmachine7_row <= main_bankmachine7_source_source_payload_addr[20:7];
		end
		if ((main_bankmachine7_syncfifo7_we & main_bankmachine7_syncfifo7_writable) & ~main_bankmachine7_replace)
			main_bankmachine7_produce <= main_bankmachine7_produce + 1'd1;
		if (main_bankmachine7_do_read)
			main_bankmachine7_consume <= main_bankmachine7_consume + 1'd1;
		if ((main_bankmachine7_syncfifo7_we & main_bankmachine7_syncfifo7_writable) & ~main_bankmachine7_replace) begin
			if (~main_bankmachine7_do_read)
				main_bankmachine7_level <= main_bankmachine7_level + 1'd1;
		end
		else if (main_bankmachine7_do_read)
			main_bankmachine7_level <= main_bankmachine7_level - 1'd1;
		if (~main_bankmachine7_pipe_valid_source_valid | main_bankmachine7_pipe_valid_source_ready) begin
			main_bankmachine7_pipe_valid_source_valid <= main_bankmachine7_pipe_valid_sink_valid;
			main_bankmachine7_pipe_valid_source_first <= main_bankmachine7_pipe_valid_sink_first;
			main_bankmachine7_pipe_valid_source_last <= main_bankmachine7_pipe_valid_sink_last;
			main_bankmachine7_pipe_valid_source_payload_we <= main_bankmachine7_pipe_valid_sink_payload_we;
			main_bankmachine7_pipe_valid_source_payload_addr <= main_bankmachine7_pipe_valid_sink_payload_addr;
		end
		if (main_bankmachine7_twtpcon_valid) begin
			main_bankmachine7_twtpcon_count <= 3'd4;
			main_bankmachine7_twtpcon_ready <= 1'd0;
		end
		else if (~main_bankmachine7_twtpcon_ready) begin
			main_bankmachine7_twtpcon_count <= main_bankmachine7_twtpcon_count - 1'd1;
			if (main_bankmachine7_twtpcon_count == 1'd1)
				main_bankmachine7_twtpcon_ready <= 1'd1;
		end
		if (main_bankmachine7_trccon_valid) begin
			main_bankmachine7_trccon_count <= 2'd3;
			main_bankmachine7_trccon_ready <= 1'd0;
		end
		else if (~main_bankmachine7_trccon_ready) begin
			main_bankmachine7_trccon_count <= main_bankmachine7_trccon_count - 1'd1;
			if (main_bankmachine7_trccon_count == 1'd1)
				main_bankmachine7_trccon_ready <= 1'd1;
		end
		if (main_bankmachine7_trascon_valid) begin
			main_bankmachine7_trascon_count <= 2'd2;
			main_bankmachine7_trascon_ready <= 1'd0;
		end
		else if (~main_bankmachine7_trascon_ready) begin
			main_bankmachine7_trascon_count <= main_bankmachine7_trascon_count - 1'd1;
			if (main_bankmachine7_trascon_count == 1'd1)
				main_bankmachine7_trascon_ready <= 1'd1;
		end
		builder_litedramcore_bankmachine7_state <= builder_litedramcore_bankmachine7_next_state;
		if (~main_en0)
			main_time0 <= 5'd31;
		else if (~main_max_time0)
			main_time0 <= main_time0 - 1'd1;
		if (~main_en1)
			main_time1 <= 4'd15;
		else if (~main_max_time1)
			main_time1 <= main_time1 - 1'd1;
		if (main_choose_cmd_ce)
			case (main_choose_cmd_grant)
				1'd0:
					if (main_choose_cmd_request[1])
						main_choose_cmd_grant <= 1'd1;
					else if (main_choose_cmd_request[2])
						main_choose_cmd_grant <= 2'd2;
					else if (main_choose_cmd_request[3])
						main_choose_cmd_grant <= 2'd3;
					else if (main_choose_cmd_request[4])
						main_choose_cmd_grant <= 3'd4;
					else if (main_choose_cmd_request[5])
						main_choose_cmd_grant <= 3'd5;
					else if (main_choose_cmd_request[6])
						main_choose_cmd_grant <= 3'd6;
					else if (main_choose_cmd_request[7])
						main_choose_cmd_grant <= 3'd7;
				1'd1:
					if (main_choose_cmd_request[2])
						main_choose_cmd_grant <= 2'd2;
					else if (main_choose_cmd_request[3])
						main_choose_cmd_grant <= 2'd3;
					else if (main_choose_cmd_request[4])
						main_choose_cmd_grant <= 3'd4;
					else if (main_choose_cmd_request[5])
						main_choose_cmd_grant <= 3'd5;
					else if (main_choose_cmd_request[6])
						main_choose_cmd_grant <= 3'd6;
					else if (main_choose_cmd_request[7])
						main_choose_cmd_grant <= 3'd7;
					else if (main_choose_cmd_request[0])
						main_choose_cmd_grant <= 1'd0;
				2'd2:
					if (main_choose_cmd_request[3])
						main_choose_cmd_grant <= 2'd3;
					else if (main_choose_cmd_request[4])
						main_choose_cmd_grant <= 3'd4;
					else if (main_choose_cmd_request[5])
						main_choose_cmd_grant <= 3'd5;
					else if (main_choose_cmd_request[6])
						main_choose_cmd_grant <= 3'd6;
					else if (main_choose_cmd_request[7])
						main_choose_cmd_grant <= 3'd7;
					else if (main_choose_cmd_request[0])
						main_choose_cmd_grant <= 1'd0;
					else if (main_choose_cmd_request[1])
						main_choose_cmd_grant <= 1'd1;
				2'd3:
					if (main_choose_cmd_request[4])
						main_choose_cmd_grant <= 3'd4;
					else if (main_choose_cmd_request[5])
						main_choose_cmd_grant <= 3'd5;
					else if (main_choose_cmd_request[6])
						main_choose_cmd_grant <= 3'd6;
					else if (main_choose_cmd_request[7])
						main_choose_cmd_grant <= 3'd7;
					else if (main_choose_cmd_request[0])
						main_choose_cmd_grant <= 1'd0;
					else if (main_choose_cmd_request[1])
						main_choose_cmd_grant <= 1'd1;
					else if (main_choose_cmd_request[2])
						main_choose_cmd_grant <= 2'd2;
				3'd4:
					if (main_choose_cmd_request[5])
						main_choose_cmd_grant <= 3'd5;
					else if (main_choose_cmd_request[6])
						main_choose_cmd_grant <= 3'd6;
					else if (main_choose_cmd_request[7])
						main_choose_cmd_grant <= 3'd7;
					else if (main_choose_cmd_request[0])
						main_choose_cmd_grant <= 1'd0;
					else if (main_choose_cmd_request[1])
						main_choose_cmd_grant <= 1'd1;
					else if (main_choose_cmd_request[2])
						main_choose_cmd_grant <= 2'd2;
					else if (main_choose_cmd_request[3])
						main_choose_cmd_grant <= 2'd3;
				3'd5:
					if (main_choose_cmd_request[6])
						main_choose_cmd_grant <= 3'd6;
					else if (main_choose_cmd_request[7])
						main_choose_cmd_grant <= 3'd7;
					else if (main_choose_cmd_request[0])
						main_choose_cmd_grant <= 1'd0;
					else if (main_choose_cmd_request[1])
						main_choose_cmd_grant <= 1'd1;
					else if (main_choose_cmd_request[2])
						main_choose_cmd_grant <= 2'd2;
					else if (main_choose_cmd_request[3])
						main_choose_cmd_grant <= 2'd3;
					else if (main_choose_cmd_request[4])
						main_choose_cmd_grant <= 3'd4;
				3'd6:
					if (main_choose_cmd_request[7])
						main_choose_cmd_grant <= 3'd7;
					else if (main_choose_cmd_request[0])
						main_choose_cmd_grant <= 1'd0;
					else if (main_choose_cmd_request[1])
						main_choose_cmd_grant <= 1'd1;
					else if (main_choose_cmd_request[2])
						main_choose_cmd_grant <= 2'd2;
					else if (main_choose_cmd_request[3])
						main_choose_cmd_grant <= 2'd3;
					else if (main_choose_cmd_request[4])
						main_choose_cmd_grant <= 3'd4;
					else if (main_choose_cmd_request[5])
						main_choose_cmd_grant <= 3'd5;
				3'd7:
					if (main_choose_cmd_request[0])
						main_choose_cmd_grant <= 1'd0;
					else if (main_choose_cmd_request[1])
						main_choose_cmd_grant <= 1'd1;
					else if (main_choose_cmd_request[2])
						main_choose_cmd_grant <= 2'd2;
					else if (main_choose_cmd_request[3])
						main_choose_cmd_grant <= 2'd3;
					else if (main_choose_cmd_request[4])
						main_choose_cmd_grant <= 3'd4;
					else if (main_choose_cmd_request[5])
						main_choose_cmd_grant <= 3'd5;
					else if (main_choose_cmd_request[6])
						main_choose_cmd_grant <= 3'd6;
			endcase
		if (main_choose_req_ce)
			case (main_choose_req_grant)
				1'd0:
					if (main_choose_req_request[1])
						main_choose_req_grant <= 1'd1;
					else if (main_choose_req_request[2])
						main_choose_req_grant <= 2'd2;
					else if (main_choose_req_request[3])
						main_choose_req_grant <= 2'd3;
					else if (main_choose_req_request[4])
						main_choose_req_grant <= 3'd4;
					else if (main_choose_req_request[5])
						main_choose_req_grant <= 3'd5;
					else if (main_choose_req_request[6])
						main_choose_req_grant <= 3'd6;
					else if (main_choose_req_request[7])
						main_choose_req_grant <= 3'd7;
				1'd1:
					if (main_choose_req_request[2])
						main_choose_req_grant <= 2'd2;
					else if (main_choose_req_request[3])
						main_choose_req_grant <= 2'd3;
					else if (main_choose_req_request[4])
						main_choose_req_grant <= 3'd4;
					else if (main_choose_req_request[5])
						main_choose_req_grant <= 3'd5;
					else if (main_choose_req_request[6])
						main_choose_req_grant <= 3'd6;
					else if (main_choose_req_request[7])
						main_choose_req_grant <= 3'd7;
					else if (main_choose_req_request[0])
						main_choose_req_grant <= 1'd0;
				2'd2:
					if (main_choose_req_request[3])
						main_choose_req_grant <= 2'd3;
					else if (main_choose_req_request[4])
						main_choose_req_grant <= 3'd4;
					else if (main_choose_req_request[5])
						main_choose_req_grant <= 3'd5;
					else if (main_choose_req_request[6])
						main_choose_req_grant <= 3'd6;
					else if (main_choose_req_request[7])
						main_choose_req_grant <= 3'd7;
					else if (main_choose_req_request[0])
						main_choose_req_grant <= 1'd0;
					else if (main_choose_req_request[1])
						main_choose_req_grant <= 1'd1;
				2'd3:
					if (main_choose_req_request[4])
						main_choose_req_grant <= 3'd4;
					else if (main_choose_req_request[5])
						main_choose_req_grant <= 3'd5;
					else if (main_choose_req_request[6])
						main_choose_req_grant <= 3'd6;
					else if (main_choose_req_request[7])
						main_choose_req_grant <= 3'd7;
					else if (main_choose_req_request[0])
						main_choose_req_grant <= 1'd0;
					else if (main_choose_req_request[1])
						main_choose_req_grant <= 1'd1;
					else if (main_choose_req_request[2])
						main_choose_req_grant <= 2'd2;
				3'd4:
					if (main_choose_req_request[5])
						main_choose_req_grant <= 3'd5;
					else if (main_choose_req_request[6])
						main_choose_req_grant <= 3'd6;
					else if (main_choose_req_request[7])
						main_choose_req_grant <= 3'd7;
					else if (main_choose_req_request[0])
						main_choose_req_grant <= 1'd0;
					else if (main_choose_req_request[1])
						main_choose_req_grant <= 1'd1;
					else if (main_choose_req_request[2])
						main_choose_req_grant <= 2'd2;
					else if (main_choose_req_request[3])
						main_choose_req_grant <= 2'd3;
				3'd5:
					if (main_choose_req_request[6])
						main_choose_req_grant <= 3'd6;
					else if (main_choose_req_request[7])
						main_choose_req_grant <= 3'd7;
					else if (main_choose_req_request[0])
						main_choose_req_grant <= 1'd0;
					else if (main_choose_req_request[1])
						main_choose_req_grant <= 1'd1;
					else if (main_choose_req_request[2])
						main_choose_req_grant <= 2'd2;
					else if (main_choose_req_request[3])
						main_choose_req_grant <= 2'd3;
					else if (main_choose_req_request[4])
						main_choose_req_grant <= 3'd4;
				3'd6:
					if (main_choose_req_request[7])
						main_choose_req_grant <= 3'd7;
					else if (main_choose_req_request[0])
						main_choose_req_grant <= 1'd0;
					else if (main_choose_req_request[1])
						main_choose_req_grant <= 1'd1;
					else if (main_choose_req_request[2])
						main_choose_req_grant <= 2'd2;
					else if (main_choose_req_request[3])
						main_choose_req_grant <= 2'd3;
					else if (main_choose_req_request[4])
						main_choose_req_grant <= 3'd4;
					else if (main_choose_req_request[5])
						main_choose_req_grant <= 3'd5;
				3'd7:
					if (main_choose_req_request[0])
						main_choose_req_grant <= 1'd0;
					else if (main_choose_req_request[1])
						main_choose_req_grant <= 1'd1;
					else if (main_choose_req_request[2])
						main_choose_req_grant <= 2'd2;
					else if (main_choose_req_request[3])
						main_choose_req_grant <= 2'd3;
					else if (main_choose_req_request[4])
						main_choose_req_grant <= 3'd4;
					else if (main_choose_req_request[5])
						main_choose_req_grant <= 3'd5;
					else if (main_choose_req_request[6])
						main_choose_req_grant <= 3'd6;
			endcase
		main_dfi_p0_cs_n <= 1'd0;
		main_dfi_p0_bank <= builder_array_muxed0;
		main_dfi_p0_address <= builder_array_muxed1;
		main_dfi_p0_cas_n <= ~builder_array_muxed2;
		main_dfi_p0_ras_n <= ~builder_array_muxed3;
		main_dfi_p0_we_n <= ~builder_array_muxed4;
		main_dfi_p0_rddata_en <= builder_array_muxed5;
		main_dfi_p0_wrdata_en <= builder_array_muxed6;
		main_dfi_p1_cs_n <= 1'd0;
		main_dfi_p1_bank <= builder_array_muxed7;
		main_dfi_p1_address <= builder_array_muxed8;
		main_dfi_p1_cas_n <= ~builder_array_muxed9;
		main_dfi_p1_ras_n <= ~builder_array_muxed10;
		main_dfi_p1_we_n <= ~builder_array_muxed11;
		main_dfi_p1_rddata_en <= builder_array_muxed12;
		main_dfi_p1_wrdata_en <= builder_array_muxed13;
		main_dfi_p2_cs_n <= 1'd0;
		main_dfi_p2_bank <= builder_array_muxed14;
		main_dfi_p2_address <= builder_array_muxed15;
		main_dfi_p2_cas_n <= ~builder_array_muxed16;
		main_dfi_p2_ras_n <= ~builder_array_muxed17;
		main_dfi_p2_we_n <= ~builder_array_muxed18;
		main_dfi_p2_rddata_en <= builder_array_muxed19;
		main_dfi_p2_wrdata_en <= builder_array_muxed20;
		main_dfi_p3_cs_n <= 1'd0;
		main_dfi_p3_bank <= builder_array_muxed21;
		main_dfi_p3_address <= builder_array_muxed22;
		main_dfi_p3_cas_n <= ~builder_array_muxed23;
		main_dfi_p3_ras_n <= ~builder_array_muxed24;
		main_dfi_p3_we_n <= ~builder_array_muxed25;
		main_dfi_p3_rddata_en <= builder_array_muxed26;
		main_dfi_p3_wrdata_en <= builder_array_muxed27;
		if (main_trrdcon_valid) begin
			main_trrdcon_count <= 1'd1;
			main_trrdcon_ready <= 1'd0;
		end
		else if (~main_trrdcon_ready) begin
			main_trrdcon_count <= main_trrdcon_count - 1'd1;
			if (main_trrdcon_count == 1'd1)
				main_trrdcon_ready <= 1'd1;
		end
		main_tfawcon_window <= {main_tfawcon_window, main_tfawcon_valid};
		if (main_tfawcon_count < 3'd4) begin
			if (main_tfawcon_count == 2'd3)
				main_tfawcon_ready <= ~main_tfawcon_valid;
			else
				main_tfawcon_ready <= 1'd1;
		end
		if (main_tccdcon_valid) begin
			main_tccdcon_count <= 1'd0;
			main_tccdcon_ready <= 1'd1;
		end
		else if (~main_tccdcon_ready) begin
			main_tccdcon_count <= main_tccdcon_count - 1'd1;
			if (main_tccdcon_count == 1'd1)
				main_tccdcon_ready <= 1'd1;
		end
		if (main_twtrcon_valid) begin
			main_twtrcon_count <= 3'd4;
			main_twtrcon_ready <= 1'd0;
		end
		else if (~main_twtrcon_ready) begin
			main_twtrcon_count <= main_twtrcon_count - 1'd1;
			if (main_twtrcon_count == 1'd1)
				main_twtrcon_ready <= 1'd1;
		end
		builder_litedramcore_multiplexer_state <= builder_litedramcore_multiplexer_next_state;
		builder_litedramcore_new_master_wdata_ready0 <= (((((((1'd0 | ((builder_litedramcore_roundrobin0_grant == 1'd0) & main_interface_bank0_wdata_ready)) | ((builder_litedramcore_roundrobin1_grant == 1'd0) & main_interface_bank1_wdata_ready)) | ((builder_litedramcore_roundrobin2_grant == 1'd0) & main_interface_bank2_wdata_ready)) | ((builder_litedramcore_roundrobin3_grant == 1'd0) & main_interface_bank3_wdata_ready)) | ((builder_litedramcore_roundrobin4_grant == 1'd0) & main_interface_bank4_wdata_ready)) | ((builder_litedramcore_roundrobin5_grant == 1'd0) & main_interface_bank5_wdata_ready)) | ((builder_litedramcore_roundrobin6_grant == 1'd0) & main_interface_bank6_wdata_ready)) | ((builder_litedramcore_roundrobin7_grant == 1'd0) & main_interface_bank7_wdata_ready);
		builder_litedramcore_new_master_wdata_ready1 <= builder_litedramcore_new_master_wdata_ready0;
		builder_litedramcore_new_master_rdata_valid0 <= (((((((1'd0 | ((builder_litedramcore_roundrobin0_grant == 1'd0) & main_interface_bank0_rdata_valid)) | ((builder_litedramcore_roundrobin1_grant == 1'd0) & main_interface_bank1_rdata_valid)) | ((builder_litedramcore_roundrobin2_grant == 1'd0) & main_interface_bank2_rdata_valid)) | ((builder_litedramcore_roundrobin3_grant == 1'd0) & main_interface_bank3_rdata_valid)) | ((builder_litedramcore_roundrobin4_grant == 1'd0) & main_interface_bank4_rdata_valid)) | ((builder_litedramcore_roundrobin5_grant == 1'd0) & main_interface_bank5_rdata_valid)) | ((builder_litedramcore_roundrobin6_grant == 1'd0) & main_interface_bank6_rdata_valid)) | ((builder_litedramcore_roundrobin7_grant == 1'd0) & main_interface_bank7_rdata_valid);
		builder_litedramcore_new_master_rdata_valid1 <= builder_litedramcore_new_master_rdata_valid0;
		builder_litedramcore_new_master_rdata_valid2 <= builder_litedramcore_new_master_rdata_valid1;
		builder_litedramcore_new_master_rdata_valid3 <= builder_litedramcore_new_master_rdata_valid2;
		builder_litedramcore_new_master_rdata_valid4 <= builder_litedramcore_new_master_rdata_valid3;
		builder_litedramcore_new_master_rdata_valid5 <= builder_litedramcore_new_master_rdata_valid4;
		builder_litedramcore_new_master_rdata_valid6 <= builder_litedramcore_new_master_rdata_valid5;
		builder_litedramcore_new_master_rdata_valid7 <= builder_litedramcore_new_master_rdata_valid6;
		builder_litedramcore_new_master_rdata_valid8 <= builder_litedramcore_new_master_rdata_valid7;
		if (main_litedramnativeportconverter_wdata_finished) begin
			main_litedramnativeportconverter_read_lock <= 1'd0;
			main_litedramnativeportconverter_read_unlocked <= 1'd1;
		end
		else if ((main_litedramnativeportconverter_rw_collision & ~main_port_cmd_valid) & ~main_litedramnativeportconverter_read_unlocked)
			main_litedramnativeportconverter_read_lock <= 1'd1;
		if (main_new_port_cmd_valid & main_new_port_cmd_ready)
			main_litedramnativeportconverter_read_unlocked <= 1'd0;
		if (main_litedramnativeportconverter_rdata_converter_source_valid & main_litedramnativeportconverter_rdata_converter_source_ready)
			main_litedramnativeportconverter_rdata_chunk <= {main_litedramnativeportconverter_rdata_chunk[2:0], main_litedramnativeportconverter_rdata_chunk[3]};
		if (main_litedramnativeportconverter_wdata_converter_sink_valid & main_litedramnativeportconverter_wdata_converter_sink_ready)
			main_litedramnativeportconverter_wdata_chunk <= {main_litedramnativeportconverter_wdata_chunk[2:0], main_litedramnativeportconverter_wdata_chunk[3]};
		if ((main_litedramnativeportconverter_cmd_buffer_source_valid & main_litedramnativeportconverter_cmd_buffer_source_payload_we) & main_litedramnativeportconverter_wdata_chunk[3])
			main_litedramnativeportconverter_wdata_sel <= {{4 {main_litedramnativeportconverter_cmd_buffer_source_payload_sel[3]}}, {4 {main_litedramnativeportconverter_cmd_buffer_source_payload_sel[2]}}, {4 {main_litedramnativeportconverter_cmd_buffer_source_payload_sel[1]}}, {4 {main_litedramnativeportconverter_cmd_buffer_source_payload_sel[0]}}};
		builder_litedramcore_state <= builder_litedramcore_next_state;
		if (main_litedramnativeportconverter_cmd_addr_litedramcore_next_value_ce0)
			main_litedramnativeportconverter_cmd_addr <= main_litedramnativeportconverter_cmd_addr_litedramcore_next_value0;
		if (main_litedramnativeportconverter_cmd_we_litedramcore_next_value_ce1)
			main_litedramnativeportconverter_cmd_we <= main_litedramnativeportconverter_cmd_we_litedramcore_next_value1;
		if (main_litedramnativeportconverter_cmd_last_litedramcore_next_value_ce2)
			main_litedramnativeportconverter_cmd_last <= main_litedramnativeportconverter_cmd_last_litedramcore_next_value2;
		if (main_litedramnativeportconverter_sel_litedramcore_next_value_ce3)
			main_litedramnativeportconverter_sel <= main_litedramnativeportconverter_sel_litedramcore_next_value3;
		if ((main_litedramnativeportconverter_rdata_fifo_syncfifo_we & main_litedramnativeportconverter_rdata_fifo_syncfifo_writable) & ~main_litedramnativeportconverter_rdata_fifo_replace) begin
			if (main_litedramnativeportconverter_rdata_fifo_produce == 2'd2)
				main_litedramnativeportconverter_rdata_fifo_produce <= 1'd0;
			else
				main_litedramnativeportconverter_rdata_fifo_produce <= main_litedramnativeportconverter_rdata_fifo_produce + 1'd1;
		end
		if (main_litedramnativeportconverter_rdata_fifo_do_read) begin
			if (main_litedramnativeportconverter_rdata_fifo_consume == 2'd2)
				main_litedramnativeportconverter_rdata_fifo_consume <= 1'd0;
			else
				main_litedramnativeportconverter_rdata_fifo_consume <= main_litedramnativeportconverter_rdata_fifo_consume + 1'd1;
		end
		if ((main_litedramnativeportconverter_rdata_fifo_syncfifo_we & main_litedramnativeportconverter_rdata_fifo_syncfifo_writable) & ~main_litedramnativeportconverter_rdata_fifo_replace) begin
			if (~main_litedramnativeportconverter_rdata_fifo_do_read)
				main_litedramnativeportconverter_rdata_fifo_level <= main_litedramnativeportconverter_rdata_fifo_level + 1'd1;
		end
		else if (main_litedramnativeportconverter_rdata_fifo_do_read)
			main_litedramnativeportconverter_rdata_fifo_level <= main_litedramnativeportconverter_rdata_fifo_level - 1'd1;
		if (main_litedramnativeportconverter_rdata_converter_converter_source_valid & main_litedramnativeportconverter_rdata_converter_converter_source_ready) begin
			if (main_litedramnativeportconverter_rdata_converter_converter_last)
				main_litedramnativeportconverter_rdata_converter_converter_mux <= 1'd0;
			else
				main_litedramnativeportconverter_rdata_converter_converter_mux <= main_litedramnativeportconverter_rdata_converter_converter_mux + 1'd1;
		end
		if (main_litedramnativeportconverter_wdata_converter_converter_source_ready)
			main_litedramnativeportconverter_wdata_converter_converter_strobe_all <= 1'd0;
		if (main_litedramnativeportconverter_wdata_converter_converter_load_part) begin
			if ((main_litedramnativeportconverter_wdata_converter_converter_demux == 2'd3) | main_litedramnativeportconverter_wdata_converter_converter_sink_last) begin
				main_litedramnativeportconverter_wdata_converter_converter_demux <= 1'd0;
				main_litedramnativeportconverter_wdata_converter_converter_strobe_all <= 1'd1;
			end
			else
				main_litedramnativeportconverter_wdata_converter_converter_demux <= main_litedramnativeportconverter_wdata_converter_converter_demux + 1'd1;
		end
		if (main_litedramnativeportconverter_wdata_converter_converter_source_valid & main_litedramnativeportconverter_wdata_converter_converter_source_ready) begin
			if (main_litedramnativeportconverter_wdata_converter_converter_sink_valid & main_litedramnativeportconverter_wdata_converter_converter_sink_ready) begin
				main_litedramnativeportconverter_wdata_converter_converter_source_first <= main_litedramnativeportconverter_wdata_converter_converter_sink_first;
				main_litedramnativeportconverter_wdata_converter_converter_source_last <= main_litedramnativeportconverter_wdata_converter_converter_sink_last;
			end
			else begin
				main_litedramnativeportconverter_wdata_converter_converter_source_first <= 1'd0;
				main_litedramnativeportconverter_wdata_converter_converter_source_last <= 1'd0;
			end
		end
		else if (main_litedramnativeportconverter_wdata_converter_converter_sink_valid & main_litedramnativeportconverter_wdata_converter_converter_sink_ready) begin
			main_litedramnativeportconverter_wdata_converter_converter_source_first <= main_litedramnativeportconverter_wdata_converter_converter_sink_first | main_litedramnativeportconverter_wdata_converter_converter_source_first;
			main_litedramnativeportconverter_wdata_converter_converter_source_last <= main_litedramnativeportconverter_wdata_converter_converter_sink_last | main_litedramnativeportconverter_wdata_converter_converter_source_last;
		end
		if (main_litedramnativeportconverter_wdata_converter_converter_load_part)
			case (main_litedramnativeportconverter_wdata_converter_converter_demux)
				1'd0: main_litedramnativeportconverter_wdata_converter_converter_source_payload_data[35:0] <= main_litedramnativeportconverter_wdata_converter_converter_sink_payload_data;
				1'd1: main_litedramnativeportconverter_wdata_converter_converter_source_payload_data[71:36] <= main_litedramnativeportconverter_wdata_converter_converter_sink_payload_data;
				2'd2: main_litedramnativeportconverter_wdata_converter_converter_source_payload_data[107:72] <= main_litedramnativeportconverter_wdata_converter_converter_sink_payload_data;
				2'd3: main_litedramnativeportconverter_wdata_converter_converter_source_payload_data[143:108] <= main_litedramnativeportconverter_wdata_converter_converter_sink_payload_data;
			endcase
		if (main_litedramnativeportconverter_wdata_converter_converter_load_part)
			main_litedramnativeportconverter_wdata_converter_converter_source_payload_valid_token_count <= main_litedramnativeportconverter_wdata_converter_converter_demux + 1'd1;
		if ((main_litedramnativeportconverter_wdata_fifo_syncfifo_we & main_litedramnativeportconverter_wdata_fifo_syncfifo_writable) & ~main_litedramnativeportconverter_wdata_fifo_replace) begin
			if (main_litedramnativeportconverter_wdata_fifo_produce == 2'd2)
				main_litedramnativeportconverter_wdata_fifo_produce <= 1'd0;
			else
				main_litedramnativeportconverter_wdata_fifo_produce <= main_litedramnativeportconverter_wdata_fifo_produce + 1'd1;
		end
		if (main_litedramnativeportconverter_wdata_fifo_do_read) begin
			if (main_litedramnativeportconverter_wdata_fifo_consume == 2'd2)
				main_litedramnativeportconverter_wdata_fifo_consume <= 1'd0;
			else
				main_litedramnativeportconverter_wdata_fifo_consume <= main_litedramnativeportconverter_wdata_fifo_consume + 1'd1;
		end
		if ((main_litedramnativeportconverter_wdata_fifo_syncfifo_we & main_litedramnativeportconverter_wdata_fifo_syncfifo_writable) & ~main_litedramnativeportconverter_wdata_fifo_replace) begin
			if (~main_litedramnativeportconverter_wdata_fifo_do_read)
				main_litedramnativeportconverter_wdata_fifo_level <= main_litedramnativeportconverter_wdata_fifo_level + 1'd1;
		end
		else if (main_litedramnativeportconverter_wdata_fifo_do_read)
			main_litedramnativeportconverter_wdata_fifo_level <= main_litedramnativeportconverter_wdata_fifo_level - 1'd1;
		if (~main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_valid | main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_ready) begin
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_valid <= main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_valid;
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_first <= main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_first;
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_last <= main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_last;
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_data <= main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_payload_data;
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_we <= main_litedramnativeportconverter_wdata_buffer_pipe_valid_sink_payload_we;
		end
		builder_litedramwishbone2native_state <= builder_litedramwishbone2native_next_state;
		if (main_aborted_litedramwishbone2native_next_value_ce)
			main_aborted <= main_aborted_litedramwishbone2native_next_value;
		builder_wishbone2csr_state <= builder_wishbone2csr_next_state;
		if (builder_interface1_dat_w_wishbone2csr_next_value_ce0)
			builder_interface1_dat_w <= builder_interface1_dat_w_wishbone2csr_next_value0;
		if (builder_interface1_adr_wishbone2csr_next_value_ce1)
			builder_interface1_adr <= builder_interface1_adr_wishbone2csr_next_value1;
		if (builder_interface1_we_wishbone2csr_next_value_ce2)
			builder_interface1_we <= builder_interface1_we_wishbone2csr_next_value2;
		builder_interface0_bank_bus_dat_r <= 1'd0;
		if (builder_csrbank0_sel)
			case (builder_interface0_bank_bus_adr[8:0])
				1'd0: builder_interface0_bank_bus_dat_r <= builder_csrbank0_init_done0_w;
				1'd1: builder_interface0_bank_bus_dat_r <= builder_csrbank0_init_error0_w;
			endcase
		if (builder_csrbank0_init_done0_re)
			main_init_done_storage <= builder_csrbank0_init_done0_r;
		main_init_done_re <= builder_csrbank0_init_done0_re;
		if (builder_csrbank0_init_error0_re)
			main_init_error_storage <= builder_csrbank0_init_error0_r;
		main_init_error_re <= builder_csrbank0_init_error0_re;
		builder_interface1_bank_bus_dat_r <= 1'd0;
		if (builder_csrbank1_sel)
			case (builder_interface1_bank_bus_adr[8:0])
				1'd0: builder_interface1_bank_bus_dat_r <= builder_csrbank1_rst0_w;
				1'd1: builder_interface1_bank_bus_dat_r <= builder_csrbank1_dly_sel0_w;
				2'd2: builder_interface1_bank_bus_dat_r <= builder_csrbank1_half_sys8x_taps0_w;
				2'd3: builder_interface1_bank_bus_dat_r <= builder_csrbank1_wlevel_en0_w;
				3'd4: builder_interface1_bank_bus_dat_r <= main_a7ddrphy_wlevel_strobe_w;
				3'd5: builder_interface1_bank_bus_dat_r <= main_a7ddrphy_rdly_dq_rst_w;
				3'd6: builder_interface1_bank_bus_dat_r <= main_a7ddrphy_rdly_dq_inc_w;
				3'd7: builder_interface1_bank_bus_dat_r <= main_a7ddrphy_rdly_dq_bitslip_rst_w;
				4'd8: builder_interface1_bank_bus_dat_r <= main_a7ddrphy_rdly_dq_bitslip_w;
				4'd9: builder_interface1_bank_bus_dat_r <= main_a7ddrphy_wdly_dq_bitslip_rst_w;
				4'd10: builder_interface1_bank_bus_dat_r <= main_a7ddrphy_wdly_dq_bitslip_w;
				4'd11: builder_interface1_bank_bus_dat_r <= builder_csrbank1_rdphase0_w;
				4'd12: builder_interface1_bank_bus_dat_r <= builder_csrbank1_wrphase0_w;
			endcase
		if (builder_csrbank1_rst0_re)
			main_a7ddrphy_rst_storage <= builder_csrbank1_rst0_r;
		main_a7ddrphy_rst_re <= builder_csrbank1_rst0_re;
		if (builder_csrbank1_dly_sel0_re)
			main_a7ddrphy_dly_sel_storage[1:0] <= builder_csrbank1_dly_sel0_r;
		main_a7ddrphy_dly_sel_re <= builder_csrbank1_dly_sel0_re;
		if (builder_csrbank1_half_sys8x_taps0_re)
			main_a7ddrphy_half_sys8x_taps_storage[4:0] <= builder_csrbank1_half_sys8x_taps0_r;
		main_a7ddrphy_half_sys8x_taps_re <= builder_csrbank1_half_sys8x_taps0_re;
		if (builder_csrbank1_wlevel_en0_re)
			main_a7ddrphy_wlevel_en_storage <= builder_csrbank1_wlevel_en0_r;
		main_a7ddrphy_wlevel_en_re <= builder_csrbank1_wlevel_en0_re;
		if (builder_csrbank1_rdphase0_re)
			main_a7ddrphy_rdphase_storage[1:0] <= builder_csrbank1_rdphase0_r;
		main_a7ddrphy_rdphase_re <= builder_csrbank1_rdphase0_re;
		if (builder_csrbank1_wrphase0_re)
			main_a7ddrphy_wrphase_storage[1:0] <= builder_csrbank1_wrphase0_r;
		main_a7ddrphy_wrphase_re <= builder_csrbank1_wrphase0_re;
		builder_interface2_bank_bus_dat_r <= 1'd0;
		if (builder_csrbank2_sel)
			case (builder_interface2_bank_bus_adr[8:0])
				1'd0: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_control0_w;
				1'd1: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi0_command0_w;
				2'd2: builder_interface2_bank_bus_dat_r <= main_phaseinjector0_command_issue_w;
				2'd3: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi0_address0_w;
				3'd4: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi0_baddress0_w;
				3'd5: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi0_wrdata0_w;
				3'd6: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi0_rddata_w;
				3'd7: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi1_command0_w;
				4'd8: builder_interface2_bank_bus_dat_r <= main_phaseinjector1_command_issue_w;
				4'd9: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi1_address0_w;
				4'd10: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi1_baddress0_w;
				4'd11: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi1_wrdata0_w;
				4'd12: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi1_rddata_w;
				4'd13: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi2_command0_w;
				4'd14: builder_interface2_bank_bus_dat_r <= main_phaseinjector2_command_issue_w;
				4'd15: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi2_address0_w;
				5'd16: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi2_baddress0_w;
				5'd17: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi2_wrdata0_w;
				5'd18: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi2_rddata_w;
				5'd19: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi3_command0_w;
				5'd20: builder_interface2_bank_bus_dat_r <= main_phaseinjector3_command_issue_w;
				5'd21: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi3_address0_w;
				5'd22: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi3_baddress0_w;
				5'd23: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi3_wrdata0_w;
				5'd24: builder_interface2_bank_bus_dat_r <= builder_csrbank2_dfii_pi3_rddata_w;
			endcase
		if (builder_csrbank2_dfii_control0_re)
			main_storage[3:0] <= builder_csrbank2_dfii_control0_r;
		main_re <= builder_csrbank2_dfii_control0_re;
		if (builder_csrbank2_dfii_pi0_command0_re)
			main_phaseinjector0_command_storage[7:0] <= builder_csrbank2_dfii_pi0_command0_r;
		main_phaseinjector0_command_re <= builder_csrbank2_dfii_pi0_command0_re;
		if (builder_csrbank2_dfii_pi0_address0_re)
			main_phaseinjector0_address_storage[13:0] <= builder_csrbank2_dfii_pi0_address0_r;
		main_phaseinjector0_address_re <= builder_csrbank2_dfii_pi0_address0_re;
		if (builder_csrbank2_dfii_pi0_baddress0_re)
			main_phaseinjector0_baddress_storage[2:0] <= builder_csrbank2_dfii_pi0_baddress0_r;
		main_phaseinjector0_baddress_re <= builder_csrbank2_dfii_pi0_baddress0_re;
		if (builder_csrbank2_dfii_pi0_wrdata0_re)
			main_phaseinjector0_wrdata_storage[31:0] <= builder_csrbank2_dfii_pi0_wrdata0_r;
		main_phaseinjector0_wrdata_re <= builder_csrbank2_dfii_pi0_wrdata0_re;
		main_phaseinjector0_rddata_re <= builder_csrbank2_dfii_pi0_rddata_re;
		if (builder_csrbank2_dfii_pi1_command0_re)
			main_phaseinjector1_command_storage[7:0] <= builder_csrbank2_dfii_pi1_command0_r;
		main_phaseinjector1_command_re <= builder_csrbank2_dfii_pi1_command0_re;
		if (builder_csrbank2_dfii_pi1_address0_re)
			main_phaseinjector1_address_storage[13:0] <= builder_csrbank2_dfii_pi1_address0_r;
		main_phaseinjector1_address_re <= builder_csrbank2_dfii_pi1_address0_re;
		if (builder_csrbank2_dfii_pi1_baddress0_re)
			main_phaseinjector1_baddress_storage[2:0] <= builder_csrbank2_dfii_pi1_baddress0_r;
		main_phaseinjector1_baddress_re <= builder_csrbank2_dfii_pi1_baddress0_re;
		if (builder_csrbank2_dfii_pi1_wrdata0_re)
			main_phaseinjector1_wrdata_storage[31:0] <= builder_csrbank2_dfii_pi1_wrdata0_r;
		main_phaseinjector1_wrdata_re <= builder_csrbank2_dfii_pi1_wrdata0_re;
		main_phaseinjector1_rddata_re <= builder_csrbank2_dfii_pi1_rddata_re;
		if (builder_csrbank2_dfii_pi2_command0_re)
			main_phaseinjector2_command_storage[7:0] <= builder_csrbank2_dfii_pi2_command0_r;
		main_phaseinjector2_command_re <= builder_csrbank2_dfii_pi2_command0_re;
		if (builder_csrbank2_dfii_pi2_address0_re)
			main_phaseinjector2_address_storage[13:0] <= builder_csrbank2_dfii_pi2_address0_r;
		main_phaseinjector2_address_re <= builder_csrbank2_dfii_pi2_address0_re;
		if (builder_csrbank2_dfii_pi2_baddress0_re)
			main_phaseinjector2_baddress_storage[2:0] <= builder_csrbank2_dfii_pi2_baddress0_r;
		main_phaseinjector2_baddress_re <= builder_csrbank2_dfii_pi2_baddress0_re;
		if (builder_csrbank2_dfii_pi2_wrdata0_re)
			main_phaseinjector2_wrdata_storage[31:0] <= builder_csrbank2_dfii_pi2_wrdata0_r;
		main_phaseinjector2_wrdata_re <= builder_csrbank2_dfii_pi2_wrdata0_re;
		main_phaseinjector2_rddata_re <= builder_csrbank2_dfii_pi2_rddata_re;
		if (builder_csrbank2_dfii_pi3_command0_re)
			main_phaseinjector3_command_storage[7:0] <= builder_csrbank2_dfii_pi3_command0_r;
		main_phaseinjector3_command_re <= builder_csrbank2_dfii_pi3_command0_re;
		if (builder_csrbank2_dfii_pi3_address0_re)
			main_phaseinjector3_address_storage[13:0] <= builder_csrbank2_dfii_pi3_address0_r;
		main_phaseinjector3_address_re <= builder_csrbank2_dfii_pi3_address0_re;
		if (builder_csrbank2_dfii_pi3_baddress0_re)
			main_phaseinjector3_baddress_storage[2:0] <= builder_csrbank2_dfii_pi3_baddress0_r;
		main_phaseinjector3_baddress_re <= builder_csrbank2_dfii_pi3_baddress0_re;
		if (builder_csrbank2_dfii_pi3_wrdata0_re)
			main_phaseinjector3_wrdata_storage[31:0] <= builder_csrbank2_dfii_pi3_wrdata0_r;
		main_phaseinjector3_wrdata_re <= builder_csrbank2_dfii_pi3_wrdata0_re;
		main_phaseinjector3_rddata_re <= builder_csrbank2_dfii_pi3_rddata_re;
		if (sys_rst) begin
			main_a7ddrphy_rst_storage <= 1'd0;
			main_a7ddrphy_rst_re <= 1'd0;
			main_a7ddrphy_dly_sel_storage <= 2'd0;
			main_a7ddrphy_dly_sel_re <= 1'd0;
			main_a7ddrphy_half_sys8x_taps_storage <= 5'd16;
			main_a7ddrphy_half_sys8x_taps_re <= 1'd0;
			main_a7ddrphy_wlevel_en_storage <= 1'd0;
			main_a7ddrphy_wlevel_en_re <= 1'd0;
			main_a7ddrphy_rdphase_storage <= 2'd2;
			main_a7ddrphy_rdphase_re <= 1'd0;
			main_a7ddrphy_wrphase_storage <= 2'd3;
			main_a7ddrphy_wrphase_re <= 1'd0;
			main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline0 <= 1'd0;
			main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline1 <= 1'd0;
			main_a7ddrphy_dqspattern_o1 <= 8'd0;
			main_a7ddrphy_bitslip0_value0 <= 3'd7;
			main_a7ddrphy_bitslip1_value0 <= 3'd7;
			main_a7ddrphy_bitslip0_value1 <= 3'd7;
			main_a7ddrphy_bitslip1_value1 <= 3'd7;
			main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline0 <= 1'd0;
			main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1 <= 1'd0;
			main_a7ddrphy_bitslip0_value2 <= 3'd7;
			main_a7ddrphy_bitslip0_value3 <= 3'd7;
			main_a7ddrphy_bitslip1_value2 <= 3'd7;
			main_a7ddrphy_bitslip1_value3 <= 3'd7;
			main_a7ddrphy_bitslip2_value0 <= 3'd7;
			main_a7ddrphy_bitslip2_value1 <= 3'd7;
			main_a7ddrphy_bitslip3_value0 <= 3'd7;
			main_a7ddrphy_bitslip3_value1 <= 3'd7;
			main_a7ddrphy_bitslip4_value0 <= 3'd7;
			main_a7ddrphy_bitslip4_value1 <= 3'd7;
			main_a7ddrphy_bitslip5_value0 <= 3'd7;
			main_a7ddrphy_bitslip5_value1 <= 3'd7;
			main_a7ddrphy_bitslip6_value0 <= 3'd7;
			main_a7ddrphy_bitslip6_value1 <= 3'd7;
			main_a7ddrphy_bitslip7_value0 <= 3'd7;
			main_a7ddrphy_bitslip7_value1 <= 3'd7;
			main_a7ddrphy_bitslip8_value0 <= 3'd7;
			main_a7ddrphy_bitslip8_value1 <= 3'd7;
			main_a7ddrphy_bitslip9_value0 <= 3'd7;
			main_a7ddrphy_bitslip9_value1 <= 3'd7;
			main_a7ddrphy_bitslip10_value0 <= 3'd7;
			main_a7ddrphy_bitslip10_value1 <= 3'd7;
			main_a7ddrphy_bitslip11_value0 <= 3'd7;
			main_a7ddrphy_bitslip11_value1 <= 3'd7;
			main_a7ddrphy_bitslip12_value0 <= 3'd7;
			main_a7ddrphy_bitslip12_value1 <= 3'd7;
			main_a7ddrphy_bitslip13_value0 <= 3'd7;
			main_a7ddrphy_bitslip13_value1 <= 3'd7;
			main_a7ddrphy_bitslip14_value0 <= 3'd7;
			main_a7ddrphy_bitslip14_value1 <= 3'd7;
			main_a7ddrphy_bitslip15_value0 <= 3'd7;
			main_a7ddrphy_bitslip15_value1 <= 3'd7;
			main_a7ddrphy_rddata_en_tappeddelayline0 <= 1'd0;
			main_a7ddrphy_rddata_en_tappeddelayline1 <= 1'd0;
			main_a7ddrphy_rddata_en_tappeddelayline2 <= 1'd0;
			main_a7ddrphy_rddata_en_tappeddelayline3 <= 1'd0;
			main_a7ddrphy_rddata_en_tappeddelayline4 <= 1'd0;
			main_a7ddrphy_rddata_en_tappeddelayline5 <= 1'd0;
			main_a7ddrphy_rddata_en_tappeddelayline6 <= 1'd0;
			main_a7ddrphy_rddata_en_tappeddelayline7 <= 1'd0;
			main_a7ddrphy_wrdata_en_tappeddelayline0 <= 1'd0;
			main_a7ddrphy_wrdata_en_tappeddelayline1 <= 1'd0;
			main_a7ddrphy_wrdata_en_tappeddelayline2 <= 1'd0;
			main_storage <= 4'd1;
			main_re <= 1'd0;
			main_phaseinjector0_command_storage <= 8'd0;
			main_phaseinjector0_command_re <= 1'd0;
			main_phaseinjector0_address_re <= 1'd0;
			main_phaseinjector0_baddress_re <= 1'd0;
			main_phaseinjector0_wrdata_re <= 1'd0;
			main_phaseinjector0_rddata_status <= 32'd0;
			main_phaseinjector0_rddata_re <= 1'd0;
			main_phaseinjector1_command_storage <= 8'd0;
			main_phaseinjector1_command_re <= 1'd0;
			main_phaseinjector1_address_re <= 1'd0;
			main_phaseinjector1_baddress_re <= 1'd0;
			main_phaseinjector1_wrdata_re <= 1'd0;
			main_phaseinjector1_rddata_status <= 32'd0;
			main_phaseinjector1_rddata_re <= 1'd0;
			main_phaseinjector2_command_storage <= 8'd0;
			main_phaseinjector2_command_re <= 1'd0;
			main_phaseinjector2_address_re <= 1'd0;
			main_phaseinjector2_baddress_re <= 1'd0;
			main_phaseinjector2_wrdata_re <= 1'd0;
			main_phaseinjector2_rddata_status <= 32'd0;
			main_phaseinjector2_rddata_re <= 1'd0;
			main_phaseinjector3_command_storage <= 8'd0;
			main_phaseinjector3_command_re <= 1'd0;
			main_phaseinjector3_address_re <= 1'd0;
			main_phaseinjector3_baddress_re <= 1'd0;
			main_phaseinjector3_wrdata_re <= 1'd0;
			main_phaseinjector3_rddata_status <= 32'd0;
			main_phaseinjector3_rddata_re <= 1'd0;
			main_dfi_p0_address <= 14'd0;
			main_dfi_p0_bank <= 3'd0;
			main_dfi_p0_cas_n <= 1'd1;
			main_dfi_p0_cs_n <= 1'd1;
			main_dfi_p0_ras_n <= 1'd1;
			main_dfi_p0_we_n <= 1'd1;
			main_dfi_p0_wrdata_en <= 1'd0;
			main_dfi_p0_rddata_en <= 1'd0;
			main_dfi_p1_address <= 14'd0;
			main_dfi_p1_bank <= 3'd0;
			main_dfi_p1_cas_n <= 1'd1;
			main_dfi_p1_cs_n <= 1'd1;
			main_dfi_p1_ras_n <= 1'd1;
			main_dfi_p1_we_n <= 1'd1;
			main_dfi_p1_wrdata_en <= 1'd0;
			main_dfi_p1_rddata_en <= 1'd0;
			main_dfi_p2_address <= 14'd0;
			main_dfi_p2_bank <= 3'd0;
			main_dfi_p2_cas_n <= 1'd1;
			main_dfi_p2_cs_n <= 1'd1;
			main_dfi_p2_ras_n <= 1'd1;
			main_dfi_p2_we_n <= 1'd1;
			main_dfi_p2_wrdata_en <= 1'd0;
			main_dfi_p2_rddata_en <= 1'd0;
			main_dfi_p3_address <= 14'd0;
			main_dfi_p3_bank <= 3'd0;
			main_dfi_p3_cas_n <= 1'd1;
			main_dfi_p3_cs_n <= 1'd1;
			main_dfi_p3_ras_n <= 1'd1;
			main_dfi_p3_we_n <= 1'd1;
			main_dfi_p3_wrdata_en <= 1'd0;
			main_dfi_p3_rddata_en <= 1'd0;
			main_cmd_payload_a <= 14'd0;
			main_cmd_payload_ba <= 3'd0;
			main_cmd_payload_cas <= 1'd0;
			main_cmd_payload_ras <= 1'd0;
			main_cmd_payload_we <= 1'd0;
			main_timer_count1 <= 9'd390;
			main_postponer_req_o <= 1'd0;
			main_postponer_count <= 1'd0;
			main_sequencer_done1 <= 1'd0;
			main_sequencer_counter <= 6'd0;
			main_sequencer_count <= 1'd0;
			main_zqcs_timer_count1 <= 26'd49999999;
			main_zqcs_executer_done <= 1'd0;
			main_zqcs_executer_counter <= 5'd0;
			main_bankmachine0_level <= 5'd0;
			main_bankmachine0_produce <= 4'd0;
			main_bankmachine0_consume <= 4'd0;
			main_bankmachine0_pipe_valid_source_valid <= 1'd0;
			main_bankmachine0_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine0_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine0_row <= 14'd0;
			main_bankmachine0_row_opened <= 1'd0;
			main_bankmachine0_twtpcon_ready <= 1'd0;
			main_bankmachine0_twtpcon_count <= 3'd0;
			main_bankmachine0_trccon_ready <= 1'd0;
			main_bankmachine0_trccon_count <= 2'd0;
			main_bankmachine0_trascon_ready <= 1'd0;
			main_bankmachine0_trascon_count <= 2'd0;
			main_bankmachine1_level <= 5'd0;
			main_bankmachine1_produce <= 4'd0;
			main_bankmachine1_consume <= 4'd0;
			main_bankmachine1_pipe_valid_source_valid <= 1'd0;
			main_bankmachine1_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine1_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine1_row <= 14'd0;
			main_bankmachine1_row_opened <= 1'd0;
			main_bankmachine1_twtpcon_ready <= 1'd0;
			main_bankmachine1_twtpcon_count <= 3'd0;
			main_bankmachine1_trccon_ready <= 1'd0;
			main_bankmachine1_trccon_count <= 2'd0;
			main_bankmachine1_trascon_ready <= 1'd0;
			main_bankmachine1_trascon_count <= 2'd0;
			main_bankmachine2_level <= 5'd0;
			main_bankmachine2_produce <= 4'd0;
			main_bankmachine2_consume <= 4'd0;
			main_bankmachine2_pipe_valid_source_valid <= 1'd0;
			main_bankmachine2_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine2_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine2_row <= 14'd0;
			main_bankmachine2_row_opened <= 1'd0;
			main_bankmachine2_twtpcon_ready <= 1'd0;
			main_bankmachine2_twtpcon_count <= 3'd0;
			main_bankmachine2_trccon_ready <= 1'd0;
			main_bankmachine2_trccon_count <= 2'd0;
			main_bankmachine2_trascon_ready <= 1'd0;
			main_bankmachine2_trascon_count <= 2'd0;
			main_bankmachine3_level <= 5'd0;
			main_bankmachine3_produce <= 4'd0;
			main_bankmachine3_consume <= 4'd0;
			main_bankmachine3_pipe_valid_source_valid <= 1'd0;
			main_bankmachine3_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine3_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine3_row <= 14'd0;
			main_bankmachine3_row_opened <= 1'd0;
			main_bankmachine3_twtpcon_ready <= 1'd0;
			main_bankmachine3_twtpcon_count <= 3'd0;
			main_bankmachine3_trccon_ready <= 1'd0;
			main_bankmachine3_trccon_count <= 2'd0;
			main_bankmachine3_trascon_ready <= 1'd0;
			main_bankmachine3_trascon_count <= 2'd0;
			main_bankmachine4_level <= 5'd0;
			main_bankmachine4_produce <= 4'd0;
			main_bankmachine4_consume <= 4'd0;
			main_bankmachine4_pipe_valid_source_valid <= 1'd0;
			main_bankmachine4_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine4_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine4_row <= 14'd0;
			main_bankmachine4_row_opened <= 1'd0;
			main_bankmachine4_twtpcon_ready <= 1'd0;
			main_bankmachine4_twtpcon_count <= 3'd0;
			main_bankmachine4_trccon_ready <= 1'd0;
			main_bankmachine4_trccon_count <= 2'd0;
			main_bankmachine4_trascon_ready <= 1'd0;
			main_bankmachine4_trascon_count <= 2'd0;
			main_bankmachine5_level <= 5'd0;
			main_bankmachine5_produce <= 4'd0;
			main_bankmachine5_consume <= 4'd0;
			main_bankmachine5_pipe_valid_source_valid <= 1'd0;
			main_bankmachine5_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine5_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine5_row <= 14'd0;
			main_bankmachine5_row_opened <= 1'd0;
			main_bankmachine5_twtpcon_ready <= 1'd0;
			main_bankmachine5_twtpcon_count <= 3'd0;
			main_bankmachine5_trccon_ready <= 1'd0;
			main_bankmachine5_trccon_count <= 2'd0;
			main_bankmachine5_trascon_ready <= 1'd0;
			main_bankmachine5_trascon_count <= 2'd0;
			main_bankmachine6_level <= 5'd0;
			main_bankmachine6_produce <= 4'd0;
			main_bankmachine6_consume <= 4'd0;
			main_bankmachine6_pipe_valid_source_valid <= 1'd0;
			main_bankmachine6_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine6_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine6_row <= 14'd0;
			main_bankmachine6_row_opened <= 1'd0;
			main_bankmachine6_twtpcon_ready <= 1'd0;
			main_bankmachine6_twtpcon_count <= 3'd0;
			main_bankmachine6_trccon_ready <= 1'd0;
			main_bankmachine6_trccon_count <= 2'd0;
			main_bankmachine6_trascon_ready <= 1'd0;
			main_bankmachine6_trascon_count <= 2'd0;
			main_bankmachine7_level <= 5'd0;
			main_bankmachine7_produce <= 4'd0;
			main_bankmachine7_consume <= 4'd0;
			main_bankmachine7_pipe_valid_source_valid <= 1'd0;
			main_bankmachine7_pipe_valid_source_payload_we <= 1'd0;
			main_bankmachine7_pipe_valid_source_payload_addr <= 21'd0;
			main_bankmachine7_row <= 14'd0;
			main_bankmachine7_row_opened <= 1'd0;
			main_bankmachine7_twtpcon_ready <= 1'd0;
			main_bankmachine7_twtpcon_count <= 3'd0;
			main_bankmachine7_trccon_ready <= 1'd0;
			main_bankmachine7_trccon_count <= 2'd0;
			main_bankmachine7_trascon_ready <= 1'd0;
			main_bankmachine7_trascon_count <= 2'd0;
			main_choose_cmd_grant <= 3'd0;
			main_choose_req_grant <= 3'd0;
			main_trrdcon_ready <= 1'd0;
			main_trrdcon_count <= 1'd0;
			main_tfawcon_ready <= 1'd1;
			main_tfawcon_window <= 3'd0;
			main_tccdcon_ready <= 1'd0;
			main_tccdcon_count <= 1'd0;
			main_twtrcon_ready <= 1'd0;
			main_twtrcon_count <= 3'd0;
			main_time0 <= 5'd0;
			main_time1 <= 4'd0;
			main_init_done_storage <= 1'd0;
			main_init_done_re <= 1'd0;
			main_init_error_storage <= 1'd0;
			main_init_error_re <= 1'd0;
			main_user_enable <= 1'd0;
			main_litedramnativeportconverter_sel <= 4'd0;
			main_litedramnativeportconverter_cmd_addr <= 26'd0;
			main_litedramnativeportconverter_cmd_we <= 1'd0;
			main_litedramnativeportconverter_cmd_last <= 1'd0;
			main_litedramnativeportconverter_read_lock <= 1'd0;
			main_litedramnativeportconverter_read_unlocked <= 1'd0;
			main_litedramnativeportconverter_rdata_fifo_level <= 2'd0;
			main_litedramnativeportconverter_rdata_fifo_produce <= 2'd0;
			main_litedramnativeportconverter_rdata_fifo_consume <= 2'd0;
			main_litedramnativeportconverter_rdata_converter_converter_mux <= 2'd0;
			main_litedramnativeportconverter_rdata_chunk <= 4'd1;
			main_litedramnativeportconverter_wdata_fifo_level <= 2'd0;
			main_litedramnativeportconverter_wdata_fifo_produce <= 2'd0;
			main_litedramnativeportconverter_wdata_fifo_consume <= 2'd0;
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_valid <= 1'd0;
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_data <= 128'd0;
			main_litedramnativeportconverter_wdata_buffer_pipe_valid_source_payload_we <= 16'd0;
			main_litedramnativeportconverter_wdata_converter_converter_source_payload_data <= 144'd0;
			main_litedramnativeportconverter_wdata_converter_converter_source_payload_valid_token_count <= 3'd0;
			main_litedramnativeportconverter_wdata_converter_converter_demux <= 2'd0;
			main_litedramnativeportconverter_wdata_converter_converter_strobe_all <= 1'd0;
			main_litedramnativeportconverter_wdata_chunk <= 4'd1;
			main_litedramnativeportconverter_wdata_sel <= 16'd0;
			main_aborted <= 1'd0;
			builder_interface1_we <= 1'd0;
			builder_litedramcore_refresher_state <= 2'd0;
			builder_litedramcore_bankmachine0_state <= 3'd0;
			builder_litedramcore_bankmachine1_state <= 3'd0;
			builder_litedramcore_bankmachine2_state <= 3'd0;
			builder_litedramcore_bankmachine3_state <= 3'd0;
			builder_litedramcore_bankmachine4_state <= 3'd0;
			builder_litedramcore_bankmachine5_state <= 3'd0;
			builder_litedramcore_bankmachine6_state <= 3'd0;
			builder_litedramcore_bankmachine7_state <= 3'd0;
			builder_litedramcore_multiplexer_state <= 4'd0;
			builder_litedramcore_state <= 2'd0;
			builder_litedramcore_new_master_wdata_ready0 <= 1'd0;
			builder_litedramcore_new_master_wdata_ready1 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid0 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid1 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid2 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid3 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid4 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid5 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid6 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid7 <= 1'd0;
			builder_litedramcore_new_master_rdata_valid8 <= 1'd0;
			builder_litedramwishbone2native_state <= 2'd0;
			builder_wishbone2csr_state <= 2'd0;
		end
	end
	BUFG BUFG(
		.I(main_clkout0),
		.O(main_clkout_buf0)
	);
	BUFG BUFG_1(
		.I(main_clkout1),
		.O(main_clkout_buf1)
	);
	BUFG BUFG_2(
		.I(main_clkout2),
		.O(main_clkout_buf2)
	);
	BUFG BUFG_3(
		.I(main_clkout3),
		.O(main_clkout_buf3)
	);
	BUFG BUFG_4(
		.I(main_clkout4),
		.O(main_clkout_buf4)
	);
	IDELAYCTRL IDELAYCTRL(
		.REFCLK(iodelay_clk),
		.RST(main_ic_reset)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(1'd0),
		.D2(1'd1),
		.D3(1'd0),
		.D4(1'd1),
		.D5(1'd0),
		.D6(1'd1),
		.D7(1'd0),
		.D8(1'd1),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(main_a7ddrphy_sd_clk_se_nodelay)
	);
	OBUFDS OBUFDS(
		.I(main_a7ddrphy_sd_clk_se_nodelay),
		.O(ddram_clk_p),
		.OB(ddram_clk_n)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_1(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_reset_n),
		.D2(main_a7ddrphy_dfi_p0_reset_n),
		.D3(main_a7ddrphy_dfi_p1_reset_n),
		.D4(main_a7ddrphy_dfi_p1_reset_n),
		.D5(main_a7ddrphy_dfi_p2_reset_n),
		.D6(main_a7ddrphy_dfi_p2_reset_n),
		.D7(main_a7ddrphy_dfi_p3_reset_n),
		.D8(main_a7ddrphy_dfi_p3_reset_n),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_reset_n)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_2(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_cs_n),
		.D2(main_a7ddrphy_dfi_p0_cs_n),
		.D3(main_a7ddrphy_dfi_p1_cs_n),
		.D4(main_a7ddrphy_dfi_p1_cs_n),
		.D5(main_a7ddrphy_dfi_p2_cs_n),
		.D6(main_a7ddrphy_dfi_p2_cs_n),
		.D7(main_a7ddrphy_dfi_p3_cs_n),
		.D8(main_a7ddrphy_dfi_p3_cs_n),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_cs_n)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_3(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[0]),
		.D2(main_a7ddrphy_dfi_p0_address[0]),
		.D3(main_a7ddrphy_dfi_p1_address[0]),
		.D4(main_a7ddrphy_dfi_p1_address[0]),
		.D5(main_a7ddrphy_dfi_p2_address[0]),
		.D6(main_a7ddrphy_dfi_p2_address[0]),
		.D7(main_a7ddrphy_dfi_p3_address[0]),
		.D8(main_a7ddrphy_dfi_p3_address[0]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[0])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_4(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[1]),
		.D2(main_a7ddrphy_dfi_p0_address[1]),
		.D3(main_a7ddrphy_dfi_p1_address[1]),
		.D4(main_a7ddrphy_dfi_p1_address[1]),
		.D5(main_a7ddrphy_dfi_p2_address[1]),
		.D6(main_a7ddrphy_dfi_p2_address[1]),
		.D7(main_a7ddrphy_dfi_p3_address[1]),
		.D8(main_a7ddrphy_dfi_p3_address[1]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[1])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_5(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[2]),
		.D2(main_a7ddrphy_dfi_p0_address[2]),
		.D3(main_a7ddrphy_dfi_p1_address[2]),
		.D4(main_a7ddrphy_dfi_p1_address[2]),
		.D5(main_a7ddrphy_dfi_p2_address[2]),
		.D6(main_a7ddrphy_dfi_p2_address[2]),
		.D7(main_a7ddrphy_dfi_p3_address[2]),
		.D8(main_a7ddrphy_dfi_p3_address[2]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[2])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_6(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[3]),
		.D2(main_a7ddrphy_dfi_p0_address[3]),
		.D3(main_a7ddrphy_dfi_p1_address[3]),
		.D4(main_a7ddrphy_dfi_p1_address[3]),
		.D5(main_a7ddrphy_dfi_p2_address[3]),
		.D6(main_a7ddrphy_dfi_p2_address[3]),
		.D7(main_a7ddrphy_dfi_p3_address[3]),
		.D8(main_a7ddrphy_dfi_p3_address[3]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[3])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_7(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[4]),
		.D2(main_a7ddrphy_dfi_p0_address[4]),
		.D3(main_a7ddrphy_dfi_p1_address[4]),
		.D4(main_a7ddrphy_dfi_p1_address[4]),
		.D5(main_a7ddrphy_dfi_p2_address[4]),
		.D6(main_a7ddrphy_dfi_p2_address[4]),
		.D7(main_a7ddrphy_dfi_p3_address[4]),
		.D8(main_a7ddrphy_dfi_p3_address[4]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[4])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_8(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[5]),
		.D2(main_a7ddrphy_dfi_p0_address[5]),
		.D3(main_a7ddrphy_dfi_p1_address[5]),
		.D4(main_a7ddrphy_dfi_p1_address[5]),
		.D5(main_a7ddrphy_dfi_p2_address[5]),
		.D6(main_a7ddrphy_dfi_p2_address[5]),
		.D7(main_a7ddrphy_dfi_p3_address[5]),
		.D8(main_a7ddrphy_dfi_p3_address[5]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[5])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_9(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[6]),
		.D2(main_a7ddrphy_dfi_p0_address[6]),
		.D3(main_a7ddrphy_dfi_p1_address[6]),
		.D4(main_a7ddrphy_dfi_p1_address[6]),
		.D5(main_a7ddrphy_dfi_p2_address[6]),
		.D6(main_a7ddrphy_dfi_p2_address[6]),
		.D7(main_a7ddrphy_dfi_p3_address[6]),
		.D8(main_a7ddrphy_dfi_p3_address[6]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[6])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_10(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[7]),
		.D2(main_a7ddrphy_dfi_p0_address[7]),
		.D3(main_a7ddrphy_dfi_p1_address[7]),
		.D4(main_a7ddrphy_dfi_p1_address[7]),
		.D5(main_a7ddrphy_dfi_p2_address[7]),
		.D6(main_a7ddrphy_dfi_p2_address[7]),
		.D7(main_a7ddrphy_dfi_p3_address[7]),
		.D8(main_a7ddrphy_dfi_p3_address[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[7])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_11(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[8]),
		.D2(main_a7ddrphy_dfi_p0_address[8]),
		.D3(main_a7ddrphy_dfi_p1_address[8]),
		.D4(main_a7ddrphy_dfi_p1_address[8]),
		.D5(main_a7ddrphy_dfi_p2_address[8]),
		.D6(main_a7ddrphy_dfi_p2_address[8]),
		.D7(main_a7ddrphy_dfi_p3_address[8]),
		.D8(main_a7ddrphy_dfi_p3_address[8]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[8])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_12(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[9]),
		.D2(main_a7ddrphy_dfi_p0_address[9]),
		.D3(main_a7ddrphy_dfi_p1_address[9]),
		.D4(main_a7ddrphy_dfi_p1_address[9]),
		.D5(main_a7ddrphy_dfi_p2_address[9]),
		.D6(main_a7ddrphy_dfi_p2_address[9]),
		.D7(main_a7ddrphy_dfi_p3_address[9]),
		.D8(main_a7ddrphy_dfi_p3_address[9]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[9])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_13(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[10]),
		.D2(main_a7ddrphy_dfi_p0_address[10]),
		.D3(main_a7ddrphy_dfi_p1_address[10]),
		.D4(main_a7ddrphy_dfi_p1_address[10]),
		.D5(main_a7ddrphy_dfi_p2_address[10]),
		.D6(main_a7ddrphy_dfi_p2_address[10]),
		.D7(main_a7ddrphy_dfi_p3_address[10]),
		.D8(main_a7ddrphy_dfi_p3_address[10]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[10])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_14(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[11]),
		.D2(main_a7ddrphy_dfi_p0_address[11]),
		.D3(main_a7ddrphy_dfi_p1_address[11]),
		.D4(main_a7ddrphy_dfi_p1_address[11]),
		.D5(main_a7ddrphy_dfi_p2_address[11]),
		.D6(main_a7ddrphy_dfi_p2_address[11]),
		.D7(main_a7ddrphy_dfi_p3_address[11]),
		.D8(main_a7ddrphy_dfi_p3_address[11]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[11])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_15(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[12]),
		.D2(main_a7ddrphy_dfi_p0_address[12]),
		.D3(main_a7ddrphy_dfi_p1_address[12]),
		.D4(main_a7ddrphy_dfi_p1_address[12]),
		.D5(main_a7ddrphy_dfi_p2_address[12]),
		.D6(main_a7ddrphy_dfi_p2_address[12]),
		.D7(main_a7ddrphy_dfi_p3_address[12]),
		.D8(main_a7ddrphy_dfi_p3_address[12]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[12])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_16(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_address[13]),
		.D2(main_a7ddrphy_dfi_p0_address[13]),
		.D3(main_a7ddrphy_dfi_p1_address[13]),
		.D4(main_a7ddrphy_dfi_p1_address[13]),
		.D5(main_a7ddrphy_dfi_p2_address[13]),
		.D6(main_a7ddrphy_dfi_p2_address[13]),
		.D7(main_a7ddrphy_dfi_p3_address[13]),
		.D8(main_a7ddrphy_dfi_p3_address[13]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_a[13])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_17(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_bank[0]),
		.D2(main_a7ddrphy_dfi_p0_bank[0]),
		.D3(main_a7ddrphy_dfi_p1_bank[0]),
		.D4(main_a7ddrphy_dfi_p1_bank[0]),
		.D5(main_a7ddrphy_dfi_p2_bank[0]),
		.D6(main_a7ddrphy_dfi_p2_bank[0]),
		.D7(main_a7ddrphy_dfi_p3_bank[0]),
		.D8(main_a7ddrphy_dfi_p3_bank[0]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(main_a7ddrphy_pads_ba[0])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_18(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_bank[1]),
		.D2(main_a7ddrphy_dfi_p0_bank[1]),
		.D3(main_a7ddrphy_dfi_p1_bank[1]),
		.D4(main_a7ddrphy_dfi_p1_bank[1]),
		.D5(main_a7ddrphy_dfi_p2_bank[1]),
		.D6(main_a7ddrphy_dfi_p2_bank[1]),
		.D7(main_a7ddrphy_dfi_p3_bank[1]),
		.D8(main_a7ddrphy_dfi_p3_bank[1]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(main_a7ddrphy_pads_ba[1])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_19(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_bank[2]),
		.D2(main_a7ddrphy_dfi_p0_bank[2]),
		.D3(main_a7ddrphy_dfi_p1_bank[2]),
		.D4(main_a7ddrphy_dfi_p1_bank[2]),
		.D5(main_a7ddrphy_dfi_p2_bank[2]),
		.D6(main_a7ddrphy_dfi_p2_bank[2]),
		.D7(main_a7ddrphy_dfi_p3_bank[2]),
		.D8(main_a7ddrphy_dfi_p3_bank[2]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(main_a7ddrphy_pads_ba[2])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_20(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_ras_n),
		.D2(main_a7ddrphy_dfi_p0_ras_n),
		.D3(main_a7ddrphy_dfi_p1_ras_n),
		.D4(main_a7ddrphy_dfi_p1_ras_n),
		.D5(main_a7ddrphy_dfi_p2_ras_n),
		.D6(main_a7ddrphy_dfi_p2_ras_n),
		.D7(main_a7ddrphy_dfi_p3_ras_n),
		.D8(main_a7ddrphy_dfi_p3_ras_n),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_ras_n)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_21(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_cas_n),
		.D2(main_a7ddrphy_dfi_p0_cas_n),
		.D3(main_a7ddrphy_dfi_p1_cas_n),
		.D4(main_a7ddrphy_dfi_p1_cas_n),
		.D5(main_a7ddrphy_dfi_p2_cas_n),
		.D6(main_a7ddrphy_dfi_p2_cas_n),
		.D7(main_a7ddrphy_dfi_p3_cas_n),
		.D8(main_a7ddrphy_dfi_p3_cas_n),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_cas_n)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_22(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_we_n),
		.D2(main_a7ddrphy_dfi_p0_we_n),
		.D3(main_a7ddrphy_dfi_p1_we_n),
		.D4(main_a7ddrphy_dfi_p1_we_n),
		.D5(main_a7ddrphy_dfi_p2_we_n),
		.D6(main_a7ddrphy_dfi_p2_we_n),
		.D7(main_a7ddrphy_dfi_p3_we_n),
		.D8(main_a7ddrphy_dfi_p3_we_n),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_we_n)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_23(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_cke),
		.D2(main_a7ddrphy_dfi_p0_cke),
		.D3(main_a7ddrphy_dfi_p1_cke),
		.D4(main_a7ddrphy_dfi_p1_cke),
		.D5(main_a7ddrphy_dfi_p2_cke),
		.D6(main_a7ddrphy_dfi_p2_cke),
		.D7(main_a7ddrphy_dfi_p3_cke),
		.D8(main_a7ddrphy_dfi_p3_cke),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_cke)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_24(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_dfi_p0_odt),
		.D2(main_a7ddrphy_dfi_p0_odt),
		.D3(main_a7ddrphy_dfi_p1_odt),
		.D4(main_a7ddrphy_dfi_p1_odt),
		.D5(main_a7ddrphy_dfi_p2_odt),
		.D6(main_a7ddrphy_dfi_p2_odt),
		.D7(main_a7ddrphy_dfi_p3_odt),
		.D8(main_a7ddrphy_dfi_p3_odt),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_odt)
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_25(
		.CLK(sys4x_dqs_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip00[0]),
		.D2(main_a7ddrphy_bitslip00[1]),
		.D3(main_a7ddrphy_bitslip00[2]),
		.D4(main_a7ddrphy_bitslip00[3]),
		.D5(main_a7ddrphy_bitslip00[4]),
		.D6(main_a7ddrphy_bitslip00[5]),
		.D7(main_a7ddrphy_bitslip00[6]),
		.D8(main_a7ddrphy_bitslip00[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OFB(main_a7ddrphy0),
		.OQ(main_a7ddrphy_dqs_o_no_delay0),
		.TQ(main_a7ddrphy_dqs_t0)
	);
	IOBUFDS IOBUFDS(
		.I(main_a7ddrphy_dqs_o_no_delay0),
		.T(main_a7ddrphy_dqs_t0),
		.IO(ddram_dqs_p[0]),
		.IOB(ddram_dqs_n[0])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_26(
		.CLK(sys4x_dqs_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip10[0]),
		.D2(main_a7ddrphy_bitslip10[1]),
		.D3(main_a7ddrphy_bitslip10[2]),
		.D4(main_a7ddrphy_bitslip10[3]),
		.D5(main_a7ddrphy_bitslip10[4]),
		.D6(main_a7ddrphy_bitslip10[5]),
		.D7(main_a7ddrphy_bitslip10[6]),
		.D8(main_a7ddrphy_bitslip10[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dqs_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OFB(main_a7ddrphy1),
		.OQ(main_a7ddrphy_dqs_o_no_delay1),
		.TQ(main_a7ddrphy_dqs_t1)
	);
	IOBUFDS IOBUFDS_1(
		.I(main_a7ddrphy_dqs_o_no_delay1),
		.T(main_a7ddrphy_dqs_t1),
		.IO(ddram_dqs_p[1]),
		.IOB(ddram_dqs_n[1])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_27(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip01[0]),
		.D2(main_a7ddrphy_bitslip01[1]),
		.D3(main_a7ddrphy_bitslip01[2]),
		.D4(main_a7ddrphy_bitslip01[3]),
		.D5(main_a7ddrphy_bitslip01[4]),
		.D6(main_a7ddrphy_bitslip01[5]),
		.D7(main_a7ddrphy_bitslip01[6]),
		.D8(main_a7ddrphy_bitslip01[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_dm[0])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_28(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip11[0]),
		.D2(main_a7ddrphy_bitslip11[1]),
		.D3(main_a7ddrphy_bitslip11[2]),
		.D4(main_a7ddrphy_bitslip11[3]),
		.D5(main_a7ddrphy_bitslip11[4]),
		.D6(main_a7ddrphy_bitslip11[5]),
		.D7(main_a7ddrphy_bitslip11[6]),
		.D8(main_a7ddrphy_bitslip11[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.OQ(ddram_dm[1])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_29(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip02[0]),
		.D2(main_a7ddrphy_bitslip02[1]),
		.D3(main_a7ddrphy_bitslip02[2]),
		.D4(main_a7ddrphy_bitslip02[3]),
		.D5(main_a7ddrphy_bitslip02[4]),
		.D6(main_a7ddrphy_bitslip02[5]),
		.D7(main_a7ddrphy_bitslip02[6]),
		.D8(main_a7ddrphy_bitslip02[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay0),
		.TQ(main_a7ddrphy_dq_t0)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed0),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip03[7]),
		.Q2(main_a7ddrphy_bitslip03[6]),
		.Q3(main_a7ddrphy_bitslip03[5]),
		.Q4(main_a7ddrphy_bitslip03[4]),
		.Q5(main_a7ddrphy_bitslip03[3]),
		.Q6(main_a7ddrphy_bitslip03[2]),
		.Q7(main_a7ddrphy_bitslip03[1]),
		.Q8(main_a7ddrphy_bitslip03[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay0),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed0)
	);
	IOBUF IOBUF(
		.I(main_a7ddrphy_dq_o_nodelay0),
		.T(main_a7ddrphy_dq_t0),
		.O(main_a7ddrphy_dq_i_nodelay0),
		.IO(ddram_dq[0])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_30(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip12[0]),
		.D2(main_a7ddrphy_bitslip12[1]),
		.D3(main_a7ddrphy_bitslip12[2]),
		.D4(main_a7ddrphy_bitslip12[3]),
		.D5(main_a7ddrphy_bitslip12[4]),
		.D6(main_a7ddrphy_bitslip12[5]),
		.D7(main_a7ddrphy_bitslip12[6]),
		.D8(main_a7ddrphy_bitslip12[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay1),
		.TQ(main_a7ddrphy_dq_t1)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_1(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip13[7]),
		.Q2(main_a7ddrphy_bitslip13[6]),
		.Q3(main_a7ddrphy_bitslip13[5]),
		.Q4(main_a7ddrphy_bitslip13[4]),
		.Q5(main_a7ddrphy_bitslip13[3]),
		.Q6(main_a7ddrphy_bitslip13[2]),
		.Q7(main_a7ddrphy_bitslip13[1]),
		.Q8(main_a7ddrphy_bitslip13[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_1(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay1),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed1)
	);
	IOBUF IOBUF_1(
		.I(main_a7ddrphy_dq_o_nodelay1),
		.T(main_a7ddrphy_dq_t1),
		.O(main_a7ddrphy_dq_i_nodelay1),
		.IO(ddram_dq[1])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_31(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip20[0]),
		.D2(main_a7ddrphy_bitslip20[1]),
		.D3(main_a7ddrphy_bitslip20[2]),
		.D4(main_a7ddrphy_bitslip20[3]),
		.D5(main_a7ddrphy_bitslip20[4]),
		.D6(main_a7ddrphy_bitslip20[5]),
		.D7(main_a7ddrphy_bitslip20[6]),
		.D8(main_a7ddrphy_bitslip20[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay2),
		.TQ(main_a7ddrphy_dq_t2)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_2(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed2),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip21[7]),
		.Q2(main_a7ddrphy_bitslip21[6]),
		.Q3(main_a7ddrphy_bitslip21[5]),
		.Q4(main_a7ddrphy_bitslip21[4]),
		.Q5(main_a7ddrphy_bitslip21[3]),
		.Q6(main_a7ddrphy_bitslip21[2]),
		.Q7(main_a7ddrphy_bitslip21[1]),
		.Q8(main_a7ddrphy_bitslip21[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_2(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay2),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed2)
	);
	IOBUF IOBUF_2(
		.I(main_a7ddrphy_dq_o_nodelay2),
		.T(main_a7ddrphy_dq_t2),
		.O(main_a7ddrphy_dq_i_nodelay2),
		.IO(ddram_dq[2])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_32(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip30[0]),
		.D2(main_a7ddrphy_bitslip30[1]),
		.D3(main_a7ddrphy_bitslip30[2]),
		.D4(main_a7ddrphy_bitslip30[3]),
		.D5(main_a7ddrphy_bitslip30[4]),
		.D6(main_a7ddrphy_bitslip30[5]),
		.D7(main_a7ddrphy_bitslip30[6]),
		.D8(main_a7ddrphy_bitslip30[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay3),
		.TQ(main_a7ddrphy_dq_t3)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_3(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed3),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip31[7]),
		.Q2(main_a7ddrphy_bitslip31[6]),
		.Q3(main_a7ddrphy_bitslip31[5]),
		.Q4(main_a7ddrphy_bitslip31[4]),
		.Q5(main_a7ddrphy_bitslip31[3]),
		.Q6(main_a7ddrphy_bitslip31[2]),
		.Q7(main_a7ddrphy_bitslip31[1]),
		.Q8(main_a7ddrphy_bitslip31[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_3(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay3),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed3)
	);
	IOBUF IOBUF_3(
		.I(main_a7ddrphy_dq_o_nodelay3),
		.T(main_a7ddrphy_dq_t3),
		.O(main_a7ddrphy_dq_i_nodelay3),
		.IO(ddram_dq[3])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_33(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip40[0]),
		.D2(main_a7ddrphy_bitslip40[1]),
		.D3(main_a7ddrphy_bitslip40[2]),
		.D4(main_a7ddrphy_bitslip40[3]),
		.D5(main_a7ddrphy_bitslip40[4]),
		.D6(main_a7ddrphy_bitslip40[5]),
		.D7(main_a7ddrphy_bitslip40[6]),
		.D8(main_a7ddrphy_bitslip40[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay4),
		.TQ(main_a7ddrphy_dq_t4)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_4(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed4),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip41[7]),
		.Q2(main_a7ddrphy_bitslip41[6]),
		.Q3(main_a7ddrphy_bitslip41[5]),
		.Q4(main_a7ddrphy_bitslip41[4]),
		.Q5(main_a7ddrphy_bitslip41[3]),
		.Q6(main_a7ddrphy_bitslip41[2]),
		.Q7(main_a7ddrphy_bitslip41[1]),
		.Q8(main_a7ddrphy_bitslip41[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_4(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay4),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed4)
	);
	IOBUF IOBUF_4(
		.I(main_a7ddrphy_dq_o_nodelay4),
		.T(main_a7ddrphy_dq_t4),
		.O(main_a7ddrphy_dq_i_nodelay4),
		.IO(ddram_dq[4])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_34(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip50[0]),
		.D2(main_a7ddrphy_bitslip50[1]),
		.D3(main_a7ddrphy_bitslip50[2]),
		.D4(main_a7ddrphy_bitslip50[3]),
		.D5(main_a7ddrphy_bitslip50[4]),
		.D6(main_a7ddrphy_bitslip50[5]),
		.D7(main_a7ddrphy_bitslip50[6]),
		.D8(main_a7ddrphy_bitslip50[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay5),
		.TQ(main_a7ddrphy_dq_t5)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_5(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed5),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip51[7]),
		.Q2(main_a7ddrphy_bitslip51[6]),
		.Q3(main_a7ddrphy_bitslip51[5]),
		.Q4(main_a7ddrphy_bitslip51[4]),
		.Q5(main_a7ddrphy_bitslip51[3]),
		.Q6(main_a7ddrphy_bitslip51[2]),
		.Q7(main_a7ddrphy_bitslip51[1]),
		.Q8(main_a7ddrphy_bitslip51[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_5(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay5),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed5)
	);
	IOBUF IOBUF_5(
		.I(main_a7ddrphy_dq_o_nodelay5),
		.T(main_a7ddrphy_dq_t5),
		.O(main_a7ddrphy_dq_i_nodelay5),
		.IO(ddram_dq[5])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_35(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip60[0]),
		.D2(main_a7ddrphy_bitslip60[1]),
		.D3(main_a7ddrphy_bitslip60[2]),
		.D4(main_a7ddrphy_bitslip60[3]),
		.D5(main_a7ddrphy_bitslip60[4]),
		.D6(main_a7ddrphy_bitslip60[5]),
		.D7(main_a7ddrphy_bitslip60[6]),
		.D8(main_a7ddrphy_bitslip60[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay6),
		.TQ(main_a7ddrphy_dq_t6)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_6(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed6),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip61[7]),
		.Q2(main_a7ddrphy_bitslip61[6]),
		.Q3(main_a7ddrphy_bitslip61[5]),
		.Q4(main_a7ddrphy_bitslip61[4]),
		.Q5(main_a7ddrphy_bitslip61[3]),
		.Q6(main_a7ddrphy_bitslip61[2]),
		.Q7(main_a7ddrphy_bitslip61[1]),
		.Q8(main_a7ddrphy_bitslip61[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_6(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay6),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed6)
	);
	IOBUF IOBUF_6(
		.I(main_a7ddrphy_dq_o_nodelay6),
		.T(main_a7ddrphy_dq_t6),
		.O(main_a7ddrphy_dq_i_nodelay6),
		.IO(ddram_dq[6])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_36(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip70[0]),
		.D2(main_a7ddrphy_bitslip70[1]),
		.D3(main_a7ddrphy_bitslip70[2]),
		.D4(main_a7ddrphy_bitslip70[3]),
		.D5(main_a7ddrphy_bitslip70[4]),
		.D6(main_a7ddrphy_bitslip70[5]),
		.D7(main_a7ddrphy_bitslip70[6]),
		.D8(main_a7ddrphy_bitslip70[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay7),
		.TQ(main_a7ddrphy_dq_t7)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_7(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed7),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip71[7]),
		.Q2(main_a7ddrphy_bitslip71[6]),
		.Q3(main_a7ddrphy_bitslip71[5]),
		.Q4(main_a7ddrphy_bitslip71[4]),
		.Q5(main_a7ddrphy_bitslip71[3]),
		.Q6(main_a7ddrphy_bitslip71[2]),
		.Q7(main_a7ddrphy_bitslip71[1]),
		.Q8(main_a7ddrphy_bitslip71[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_7(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay7),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[0] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed7)
	);
	IOBUF IOBUF_7(
		.I(main_a7ddrphy_dq_o_nodelay7),
		.T(main_a7ddrphy_dq_t7),
		.O(main_a7ddrphy_dq_i_nodelay7),
		.IO(ddram_dq[7])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_37(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip80[0]),
		.D2(main_a7ddrphy_bitslip80[1]),
		.D3(main_a7ddrphy_bitslip80[2]),
		.D4(main_a7ddrphy_bitslip80[3]),
		.D5(main_a7ddrphy_bitslip80[4]),
		.D6(main_a7ddrphy_bitslip80[5]),
		.D7(main_a7ddrphy_bitslip80[6]),
		.D8(main_a7ddrphy_bitslip80[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay8),
		.TQ(main_a7ddrphy_dq_t8)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_8(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed8),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip81[7]),
		.Q2(main_a7ddrphy_bitslip81[6]),
		.Q3(main_a7ddrphy_bitslip81[5]),
		.Q4(main_a7ddrphy_bitslip81[4]),
		.Q5(main_a7ddrphy_bitslip81[3]),
		.Q6(main_a7ddrphy_bitslip81[2]),
		.Q7(main_a7ddrphy_bitslip81[1]),
		.Q8(main_a7ddrphy_bitslip81[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_8(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay8),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed8)
	);
	IOBUF IOBUF_8(
		.I(main_a7ddrphy_dq_o_nodelay8),
		.T(main_a7ddrphy_dq_t8),
		.O(main_a7ddrphy_dq_i_nodelay8),
		.IO(ddram_dq[8])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_38(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip90[0]),
		.D2(main_a7ddrphy_bitslip90[1]),
		.D3(main_a7ddrphy_bitslip90[2]),
		.D4(main_a7ddrphy_bitslip90[3]),
		.D5(main_a7ddrphy_bitslip90[4]),
		.D6(main_a7ddrphy_bitslip90[5]),
		.D7(main_a7ddrphy_bitslip90[6]),
		.D8(main_a7ddrphy_bitslip90[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay9),
		.TQ(main_a7ddrphy_dq_t9)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_9(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed9),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip91[7]),
		.Q2(main_a7ddrphy_bitslip91[6]),
		.Q3(main_a7ddrphy_bitslip91[5]),
		.Q4(main_a7ddrphy_bitslip91[4]),
		.Q5(main_a7ddrphy_bitslip91[3]),
		.Q6(main_a7ddrphy_bitslip91[2]),
		.Q7(main_a7ddrphy_bitslip91[1]),
		.Q8(main_a7ddrphy_bitslip91[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_9(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay9),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed9)
	);
	IOBUF IOBUF_9(
		.I(main_a7ddrphy_dq_o_nodelay9),
		.T(main_a7ddrphy_dq_t9),
		.O(main_a7ddrphy_dq_i_nodelay9),
		.IO(ddram_dq[9])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_39(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip100[0]),
		.D2(main_a7ddrphy_bitslip100[1]),
		.D3(main_a7ddrphy_bitslip100[2]),
		.D4(main_a7ddrphy_bitslip100[3]),
		.D5(main_a7ddrphy_bitslip100[4]),
		.D6(main_a7ddrphy_bitslip100[5]),
		.D7(main_a7ddrphy_bitslip100[6]),
		.D8(main_a7ddrphy_bitslip100[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay10),
		.TQ(main_a7ddrphy_dq_t10)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_10(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed10),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip101[7]),
		.Q2(main_a7ddrphy_bitslip101[6]),
		.Q3(main_a7ddrphy_bitslip101[5]),
		.Q4(main_a7ddrphy_bitslip101[4]),
		.Q5(main_a7ddrphy_bitslip101[3]),
		.Q6(main_a7ddrphy_bitslip101[2]),
		.Q7(main_a7ddrphy_bitslip101[1]),
		.Q8(main_a7ddrphy_bitslip101[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_10(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay10),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed10)
	);
	IOBUF IOBUF_10(
		.I(main_a7ddrphy_dq_o_nodelay10),
		.T(main_a7ddrphy_dq_t10),
		.O(main_a7ddrphy_dq_i_nodelay10),
		.IO(ddram_dq[10])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_40(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip110[0]),
		.D2(main_a7ddrphy_bitslip110[1]),
		.D3(main_a7ddrphy_bitslip110[2]),
		.D4(main_a7ddrphy_bitslip110[3]),
		.D5(main_a7ddrphy_bitslip110[4]),
		.D6(main_a7ddrphy_bitslip110[5]),
		.D7(main_a7ddrphy_bitslip110[6]),
		.D8(main_a7ddrphy_bitslip110[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay11),
		.TQ(main_a7ddrphy_dq_t11)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_11(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed11),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip111[7]),
		.Q2(main_a7ddrphy_bitslip111[6]),
		.Q3(main_a7ddrphy_bitslip111[5]),
		.Q4(main_a7ddrphy_bitslip111[4]),
		.Q5(main_a7ddrphy_bitslip111[3]),
		.Q6(main_a7ddrphy_bitslip111[2]),
		.Q7(main_a7ddrphy_bitslip111[1]),
		.Q8(main_a7ddrphy_bitslip111[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_11(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay11),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed11)
	);
	IOBUF IOBUF_11(
		.I(main_a7ddrphy_dq_o_nodelay11),
		.T(main_a7ddrphy_dq_t11),
		.O(main_a7ddrphy_dq_i_nodelay11),
		.IO(ddram_dq[11])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_41(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip120[0]),
		.D2(main_a7ddrphy_bitslip120[1]),
		.D3(main_a7ddrphy_bitslip120[2]),
		.D4(main_a7ddrphy_bitslip120[3]),
		.D5(main_a7ddrphy_bitslip120[4]),
		.D6(main_a7ddrphy_bitslip120[5]),
		.D7(main_a7ddrphy_bitslip120[6]),
		.D8(main_a7ddrphy_bitslip120[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay12),
		.TQ(main_a7ddrphy_dq_t12)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_12(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed12),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip121[7]),
		.Q2(main_a7ddrphy_bitslip121[6]),
		.Q3(main_a7ddrphy_bitslip121[5]),
		.Q4(main_a7ddrphy_bitslip121[4]),
		.Q5(main_a7ddrphy_bitslip121[3]),
		.Q6(main_a7ddrphy_bitslip121[2]),
		.Q7(main_a7ddrphy_bitslip121[1]),
		.Q8(main_a7ddrphy_bitslip121[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_12(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay12),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed12)
	);
	IOBUF IOBUF_12(
		.I(main_a7ddrphy_dq_o_nodelay12),
		.T(main_a7ddrphy_dq_t12),
		.O(main_a7ddrphy_dq_i_nodelay12),
		.IO(ddram_dq[12])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_42(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip130[0]),
		.D2(main_a7ddrphy_bitslip130[1]),
		.D3(main_a7ddrphy_bitslip130[2]),
		.D4(main_a7ddrphy_bitslip130[3]),
		.D5(main_a7ddrphy_bitslip130[4]),
		.D6(main_a7ddrphy_bitslip130[5]),
		.D7(main_a7ddrphy_bitslip130[6]),
		.D8(main_a7ddrphy_bitslip130[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay13),
		.TQ(main_a7ddrphy_dq_t13)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_13(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed13),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip131[7]),
		.Q2(main_a7ddrphy_bitslip131[6]),
		.Q3(main_a7ddrphy_bitslip131[5]),
		.Q4(main_a7ddrphy_bitslip131[4]),
		.Q5(main_a7ddrphy_bitslip131[3]),
		.Q6(main_a7ddrphy_bitslip131[2]),
		.Q7(main_a7ddrphy_bitslip131[1]),
		.Q8(main_a7ddrphy_bitslip131[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_13(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay13),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed13)
	);
	IOBUF IOBUF_13(
		.I(main_a7ddrphy_dq_o_nodelay13),
		.T(main_a7ddrphy_dq_t13),
		.O(main_a7ddrphy_dq_i_nodelay13),
		.IO(ddram_dq[13])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_43(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip140[0]),
		.D2(main_a7ddrphy_bitslip140[1]),
		.D3(main_a7ddrphy_bitslip140[2]),
		.D4(main_a7ddrphy_bitslip140[3]),
		.D5(main_a7ddrphy_bitslip140[4]),
		.D6(main_a7ddrphy_bitslip140[5]),
		.D7(main_a7ddrphy_bitslip140[6]),
		.D8(main_a7ddrphy_bitslip140[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay14),
		.TQ(main_a7ddrphy_dq_t14)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_14(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed14),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip141[7]),
		.Q2(main_a7ddrphy_bitslip141[6]),
		.Q3(main_a7ddrphy_bitslip141[5]),
		.Q4(main_a7ddrphy_bitslip141[4]),
		.Q5(main_a7ddrphy_bitslip141[3]),
		.Q6(main_a7ddrphy_bitslip141[2]),
		.Q7(main_a7ddrphy_bitslip141[1]),
		.Q8(main_a7ddrphy_bitslip141[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_14(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay14),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed14)
	);
	IOBUF IOBUF_14(
		.I(main_a7ddrphy_dq_o_nodelay14),
		.T(main_a7ddrphy_dq_t14),
		.O(main_a7ddrphy_dq_i_nodelay14),
		.IO(ddram_dq[14])
	);
	OSERDESE2 #(
		.DATA_RATE_OQ("DDR"),
		.DATA_RATE_TQ("BUF"),
		.DATA_WIDTH(4'd8),
		.SERDES_MODE("MASTER"),
		.TRISTATE_WIDTH(1'd1)
	) OSERDESE2_44(
		.CLK(sys4x_clk),
		.CLKDIV(sys_clk),
		.D1(main_a7ddrphy_bitslip150[0]),
		.D2(main_a7ddrphy_bitslip150[1]),
		.D3(main_a7ddrphy_bitslip150[2]),
		.D4(main_a7ddrphy_bitslip150[3]),
		.D5(main_a7ddrphy_bitslip150[4]),
		.D6(main_a7ddrphy_bitslip150[5]),
		.D7(main_a7ddrphy_bitslip150[6]),
		.D8(main_a7ddrphy_bitslip150[7]),
		.OCE(1'd1),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.T1(~main_a7ddrphy_dq_oe_delay_tappeddelayline_tappeddelayline1),
		.TCE(1'd1),
		.OQ(main_a7ddrphy_dq_o_nodelay15),
		.TQ(main_a7ddrphy_dq_t15)
	);
	ISERDESE2 #(
		.DATA_RATE("DDR"),
		.DATA_WIDTH(4'd8),
		.INTERFACE_TYPE("NETWORKING"),
		.IOBDELAY("IFD"),
		.NUM_CE(1'd1),
		.SERDES_MODE("MASTER")
	) ISERDESE2_15(
		.BITSLIP(1'd0),
		.CE1(1'd1),
		.CLK(sys4x_clk),
		.CLKB(~sys4x_clk),
		.CLKDIV(sys_clk),
		.DDLY(main_a7ddrphy_dq_i_delayed15),
		.RST(sys_rst | main_a7ddrphy_rst_storage),
		.Q1(main_a7ddrphy_bitslip151[7]),
		.Q2(main_a7ddrphy_bitslip151[6]),
		.Q3(main_a7ddrphy_bitslip151[5]),
		.Q4(main_a7ddrphy_bitslip151[4]),
		.Q5(main_a7ddrphy_bitslip151[3]),
		.Q6(main_a7ddrphy_bitslip151[2]),
		.Q7(main_a7ddrphy_bitslip151[1]),
		.Q8(main_a7ddrphy_bitslip151[0])
	);
	IDELAYE2 #(
		.CINVCTRL_SEL("FALSE"),
		.DELAY_SRC("IDATAIN"),
		.HIGH_PERFORMANCE_MODE("TRUE"),
		.IDELAY_TYPE("VARIABLE"),
		.IDELAY_VALUE(1'd0),
		.PIPE_SEL("FALSE"),
		.REFCLK_FREQUENCY(200.0),
		.SIGNAL_PATTERN("DATA")
	) IDELAYE2_15(
		.C(sys_clk),
		.CE(main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_inc_re),
		.IDATAIN(main_a7ddrphy_dq_i_nodelay15),
		.INC(1'd1),
		.LD((main_a7ddrphy_dly_sel_storage[1] & main_a7ddrphy_rdly_dq_rst_re) | main_a7ddrphy_rst_storage),
		.LDPIPEEN(1'd0),
		.DATAOUT(main_a7ddrphy_dq_i_delayed15)
	);
	IOBUF IOBUF_15(
		.I(main_a7ddrphy_dq_o_nodelay15),
		.T(main_a7ddrphy_dq_t15),
		.O(main_a7ddrphy_dq_i_nodelay15),
		.IO(ddram_dq[15])
	);
	reg [23:0] storage [0:15];
	reg [23:0] storage_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine0_wrport_we)
			storage[main_bankmachine0_wrport_adr] <= main_bankmachine0_wrport_dat_w;
		storage_dat0 <= storage[main_bankmachine0_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine0_wrport_dat_r = storage_dat0;
	assign main_bankmachine0_rdport_dat_r = storage[main_bankmachine0_rdport_adr];
	reg [23:0] storage_1 [0:15];
	reg [23:0] storage_1_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine1_wrport_we)
			storage_1[main_bankmachine1_wrport_adr] <= main_bankmachine1_wrport_dat_w;
		storage_1_dat0 <= storage_1[main_bankmachine1_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine1_wrport_dat_r = storage_1_dat0;
	assign main_bankmachine1_rdport_dat_r = storage_1[main_bankmachine1_rdport_adr];
	reg [23:0] storage_2 [0:15];
	reg [23:0] storage_2_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine2_wrport_we)
			storage_2[main_bankmachine2_wrport_adr] <= main_bankmachine2_wrport_dat_w;
		storage_2_dat0 <= storage_2[main_bankmachine2_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine2_wrport_dat_r = storage_2_dat0;
	assign main_bankmachine2_rdport_dat_r = storage_2[main_bankmachine2_rdport_adr];
	reg [23:0] storage_3 [0:15];
	reg [23:0] storage_3_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine3_wrport_we)
			storage_3[main_bankmachine3_wrport_adr] <= main_bankmachine3_wrport_dat_w;
		storage_3_dat0 <= storage_3[main_bankmachine3_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine3_wrport_dat_r = storage_3_dat0;
	assign main_bankmachine3_rdport_dat_r = storage_3[main_bankmachine3_rdport_adr];
	reg [23:0] storage_4 [0:15];
	reg [23:0] storage_4_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine4_wrport_we)
			storage_4[main_bankmachine4_wrport_adr] <= main_bankmachine4_wrport_dat_w;
		storage_4_dat0 <= storage_4[main_bankmachine4_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine4_wrport_dat_r = storage_4_dat0;
	assign main_bankmachine4_rdport_dat_r = storage_4[main_bankmachine4_rdport_adr];
	reg [23:0] storage_5 [0:15];
	reg [23:0] storage_5_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine5_wrport_we)
			storage_5[main_bankmachine5_wrport_adr] <= main_bankmachine5_wrport_dat_w;
		storage_5_dat0 <= storage_5[main_bankmachine5_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine5_wrport_dat_r = storage_5_dat0;
	assign main_bankmachine5_rdport_dat_r = storage_5[main_bankmachine5_rdport_adr];
	reg [23:0] storage_6 [0:15];
	reg [23:0] storage_6_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine6_wrport_we)
			storage_6[main_bankmachine6_wrport_adr] <= main_bankmachine6_wrport_dat_w;
		storage_6_dat0 <= storage_6[main_bankmachine6_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine6_wrport_dat_r = storage_6_dat0;
	assign main_bankmachine6_rdport_dat_r = storage_6[main_bankmachine6_rdport_adr];
	reg [23:0] storage_7 [0:15];
	reg [23:0] storage_7_dat0;
	always @(posedge sys_clk) begin
		if (main_bankmachine7_wrport_we)
			storage_7[main_bankmachine7_wrport_adr] <= main_bankmachine7_wrport_dat_w;
		storage_7_dat0 <= storage_7[main_bankmachine7_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_bankmachine7_wrport_dat_r = storage_7_dat0;
	assign main_bankmachine7_rdport_dat_r = storage_7[main_bankmachine7_rdport_adr];
	reg [129:0] storage_8 [0:2];
	reg [129:0] storage_8_dat0;
	always @(posedge sys_clk) begin
		if (main_litedramnativeportconverter_rdata_fifo_wrport_we)
			storage_8[main_litedramnativeportconverter_rdata_fifo_wrport_adr] <= main_litedramnativeportconverter_rdata_fifo_wrport_dat_w;
		storage_8_dat0 <= storage_8[main_litedramnativeportconverter_rdata_fifo_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_litedramnativeportconverter_rdata_fifo_wrport_dat_r = storage_8_dat0;
	assign main_litedramnativeportconverter_rdata_fifo_rdport_dat_r = storage_8[main_litedramnativeportconverter_rdata_fifo_rdport_adr];
	reg [37:0] storage_9 [0:2];
	reg [37:0] storage_9_dat0;
	always @(posedge sys_clk) begin
		if (main_litedramnativeportconverter_wdata_fifo_wrport_we)
			storage_9[main_litedramnativeportconverter_wdata_fifo_wrport_adr] <= main_litedramnativeportconverter_wdata_fifo_wrport_dat_w;
		storage_9_dat0 <= storage_9[main_litedramnativeportconverter_wdata_fifo_wrport_adr];
	end
	always @(posedge sys_clk)
		;
	assign main_litedramnativeportconverter_wdata_fifo_wrport_dat_r = storage_9_dat0;
	assign main_litedramnativeportconverter_wdata_fifo_rdport_dat_r = storage_9[main_litedramnativeportconverter_wdata_fifo_rdport_adr];
	FDCE FDCE(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(main_reset),
		.Q(builder_reset0)
	);
	FDCE FDCE_1(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(builder_reset0),
		.Q(builder_reset1)
	);
	FDCE FDCE_2(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(builder_reset1),
		.Q(builder_reset2)
	);
	FDCE FDCE_3(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(builder_reset2),
		.Q(builder_reset3)
	);
	FDCE FDCE_4(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(builder_reset3),
		.Q(builder_reset4)
	);
	FDCE FDCE_5(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(builder_reset4),
		.Q(builder_reset5)
	);
	FDCE FDCE_6(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(builder_reset5),
		.Q(builder_reset6)
	);
	FDCE FDCE_7(
		.C(main_clkin),
		.CE(1'd1),
		.CLR(1'd0),
		.D(builder_reset6),
		.Q(builder_reset7)
	);
	PLLE2_ADV #(
		.CLKFBOUT_MULT(5'd16),
		.CLKIN1_PERIOD(10.0),
		.CLKOUT0_DIVIDE(4'd8),
		.CLKOUT0_PHASE(1'd0),
		.CLKOUT1_DIVIDE(6'd32),
		.CLKOUT1_PHASE(1'd0),
		.CLKOUT2_DIVIDE(5'd16),
		.CLKOUT2_PHASE(1'd0),
		.CLKOUT3_DIVIDE(4'd8),
		.CLKOUT3_PHASE(1'd0),
		.CLKOUT4_DIVIDE(4'd8),
		.CLKOUT4_PHASE(7'd90),
		.DIVCLK_DIVIDE(1'd1),
		.REF_JITTER1(0.01),
		.STARTUP_WAIT("FALSE")
	) PLLE2_ADV(
		.CLKFBIN(builder_pll_fb),
		.CLKIN1(main_clkin),
		.PWRDWN(main_power_down),
		.RST(builder_reset7),
		.CLKFBOUT(builder_pll_fb),
		.CLKOUT0(main_clkout0),
		.CLKOUT1(main_clkout1),
		.CLKOUT2(main_clkout2),
		.CLKOUT3(main_clkout3),
		.CLKOUT4(main_clkout4),
		.LOCKED(main_locked)
	);
	(* ars_ff1 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE(
		.C(iodelay_clk),
		.CE(1'd1),
		.D(1'd0),
		.PRE(builder_xilinxasyncresetsynchronizerimpl0),
		.Q(builder_xilinxasyncresetsynchronizerimpl0_rst_meta)
	);
	(* ars_ff2 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_1(
		.C(iodelay_clk),
		.CE(1'd1),
		.D(builder_xilinxasyncresetsynchronizerimpl0_rst_meta),
		.PRE(builder_xilinxasyncresetsynchronizerimpl0),
		.Q(iodelay_rst)
	);
	(* ars_ff1 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_2(
		.C(sys_clk),
		.CE(1'd1),
		.D(1'd0),
		.PRE(builder_xilinxasyncresetsynchronizerimpl1),
		.Q(builder_xilinxasyncresetsynchronizerimpl1_rst_meta)
	);
	(* ars_ff2 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_3(
		.C(sys_clk),
		.CE(1'd1),
		.D(builder_xilinxasyncresetsynchronizerimpl1_rst_meta),
		.PRE(builder_xilinxasyncresetsynchronizerimpl1),
		.Q(sys_rst)
	);
	(* ars_ff1 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_4(
		.C(sys2x_clk),
		.CE(1'd1),
		.D(1'd0),
		.PRE(builder_xilinxasyncresetsynchronizerimpl2),
		.Q(builder_xilinxasyncresetsynchronizerimpl2_rst_meta)
	);
	(* ars_ff2 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_5(
		.C(sys2x_clk),
		.CE(1'd1),
		.D(builder_xilinxasyncresetsynchronizerimpl2_rst_meta),
		.PRE(builder_xilinxasyncresetsynchronizerimpl2),
		.Q(builder_xilinxasyncresetsynchronizerimpl2_expr)
	);
	(* ars_ff1 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_6(
		.C(sys4x_clk),
		.CE(1'd1),
		.D(1'd0),
		.PRE(builder_xilinxasyncresetsynchronizerimpl3),
		.Q(builder_xilinxasyncresetsynchronizerimpl3_rst_meta)
	);
	(* ars_ff2 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_7(
		.C(sys4x_clk),
		.CE(1'd1),
		.D(builder_xilinxasyncresetsynchronizerimpl3_rst_meta),
		.PRE(builder_xilinxasyncresetsynchronizerimpl3),
		.Q(builder_xilinxasyncresetsynchronizerimpl3_expr)
	);
	(* ars_ff1 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_8(
		.C(sys4x_dqs_clk),
		.CE(1'd1),
		.D(1'd0),
		.PRE(builder_xilinxasyncresetsynchronizerimpl4),
		.Q(builder_xilinxasyncresetsynchronizerimpl4_rst_meta)
	);
	(* ars_ff2 = "true", async_reg = "true" *) FDPE #(.INIT(1'd1)) FDPE_9(
		.C(sys4x_dqs_clk),
		.CE(1'd1),
		.D(builder_xilinxasyncresetsynchronizerimpl4_rst_meta),
		.PRE(builder_xilinxasyncresetsynchronizerimpl4),
		.Q(builder_xilinxasyncresetsynchronizerimpl4_expr)
	);
endmodule
