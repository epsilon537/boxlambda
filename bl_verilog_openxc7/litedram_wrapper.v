`default_nettype none
module litedram_wrapper (
	clk,
	rst,
	sys_clk,
	sys_clkx2,
	sys_rst,
	pll_locked,
	ddram_a,
	ddram_ba,
	ddram_ras_n,
	ddram_cas_n,
	ddram_we_n,
	ddram_cs_n,
	ddram_dm,
	ddram_dq,
	ddram_dqs_p,
	ddram_dqs_n,
	ddram_clk_p,
	ddram_clk_n,
	ddram_cke,
	ddram_odt,
	ddram_reset_n,
	init_done,
	init_error,
	wb_ctrl_adr,
	wb_ctrl_dat_w,
	wb_ctrl_dat_r,
	wb_ctrl_sel,
	wb_ctrl_stall,
	wb_ctrl_cyc,
	wb_ctrl_stb,
	wb_ctrl_ack,
	wb_ctrl_we,
	wb_ctrl_err,
	user_port_wishbone_p_0_adr,
	user_port_wishbone_p_0_dat_w,
	user_port_wishbone_p_0_dat_r,
	user_port_wishbone_p_0_sel,
	user_port_wishbone_p_0_stall,
	user_port_wishbone_p_0_cyc,
	user_port_wishbone_p_0_stb,
	user_port_wishbone_p_0_ack,
	user_port_wishbone_p_0_we,
	user_port_wishbone_p_0_err
);
	input wire clk;
	input wire rst;
	output wire sys_clk;
	output wire sys_clkx2;
	output wire sys_rst;
	output wire pll_locked;
	output wire [13:0] ddram_a;
	output wire [2:0] ddram_ba;
	output wire ddram_ras_n;
	output wire ddram_cas_n;
	output wire ddram_we_n;
	output wire ddram_cs_n;
	output wire [1:0] ddram_dm;
	inout wire [15:0] ddram_dq;
	inout wire [1:0] ddram_dqs_p;
	inout wire [1:0] ddram_dqs_n;
	output wire ddram_clk_p;
	output wire ddram_clk_n;
	output wire ddram_cke;
	output wire ddram_odt;
	output wire ddram_reset_n;
	output wire init_done;
	output wire init_error;
	input wire [27:0] wb_ctrl_adr;
	input wire [31:0] wb_ctrl_dat_w;
	output wire [31:0] wb_ctrl_dat_r;
	input wire [3:0] wb_ctrl_sel;
	output wire wb_ctrl_stall;
	input wire wb_ctrl_cyc;
	input wire wb_ctrl_stb;
	output wire wb_ctrl_ack;
	input wire wb_ctrl_we;
	output wire wb_ctrl_err;
	input wire [27:0] user_port_wishbone_p_0_adr;
	input wire [31:0] user_port_wishbone_p_0_dat_w;
	output wire [31:0] user_port_wishbone_p_0_dat_r;
	input wire [3:0] user_port_wishbone_p_0_sel;
	output wire user_port_wishbone_p_0_stall;
	input wire user_port_wishbone_p_0_cyc;
	input wire user_port_wishbone_p_0_stb;
	output wire user_port_wishbone_p_0_ack;
	input wire user_port_wishbone_p_0_we;
	output wire user_port_wishbone_p_0_err;
	wire [25:0] user_port_wishbone_c_0_adr;
	wire [31:0] user_port_wishbone_c_0_dat_w;
	wire [31:0] user_port_wishbone_c_0_dat_r;
	wire [3:0] user_port_wishbone_c_0_sel;
	wire user_port_wishbone_c_0_cyc;
	wire user_port_wishbone_c_0_stb;
	wire user_port_wishbone_c_0_ack;
	wire user_port_wishbone_c_0_we;
	wire user_port_wishbone_c_0_err;
	assign user_port_wishbone_p_0_dat_r = user_port_wishbone_c_0_dat_r;
	assign user_port_wishbone_p_0_ack = user_port_wishbone_c_0_ack;
	assign user_port_wishbone_p_0_err = user_port_wishbone_c_0_err;
	assign user_port_wishbone_p_0_stall = (!user_port_wishbone_p_0_cyc ? 1'b0 : !user_port_wishbone_c_0_ack);
	assign user_port_wishbone_c_0_stb = user_port_wishbone_p_0_stb;
	assign user_port_wishbone_c_0_adr = user_port_wishbone_p_0_adr[25:0];
	assign user_port_wishbone_c_0_we = user_port_wishbone_p_0_we;
	assign user_port_wishbone_c_0_dat_w = user_port_wishbone_p_0_dat_w;
	assign user_port_wishbone_c_0_sel = user_port_wishbone_p_0_sel;
	assign user_port_wishbone_c_0_cyc = user_port_wishbone_p_0_cyc;
	wire [29:0] ctrl_port_wishbone_c_adr;
	wire [31:0] ctrl_port_wishbone_c_dat_w;
	wire [31:0] ctrl_port_wishbone_c_dat_r;
	wire [3:0] ctrl_port_wishbone_c_sel;
	wire ctrl_port_wishbone_c_cyc;
	wire ctrl_port_wishbone_c_stb;
	wire ctrl_port_wishbone_c_ack;
	wire ctrl_port_wishbone_c_we;
	wire ctrl_port_wishbone_c_err;
	assign wb_ctrl_dat_r = ctrl_port_wishbone_c_dat_r;
	assign wb_ctrl_ack = ctrl_port_wishbone_c_ack;
	assign wb_ctrl_err = ctrl_port_wishbone_c_err;
	assign wb_ctrl_stall = (!wb_ctrl_cyc ? 1'b0 : !ctrl_port_wishbone_c_ack);
	assign ctrl_port_wishbone_c_stb = wb_ctrl_stb;
	assign ctrl_port_wishbone_c_adr = {2'b00, wb_ctrl_adr};
	assign ctrl_port_wishbone_c_we = wb_ctrl_we;
	assign ctrl_port_wishbone_c_dat_w = wb_ctrl_dat_w;
	assign ctrl_port_wishbone_c_sel = wb_ctrl_sel;
	assign ctrl_port_wishbone_c_cyc = wb_ctrl_cyc;
	litedram litedram_inst(
		.clk(clk),
		.rst(rst),
		.pll_locked(pll_locked),
		.ddram_a(ddram_a),
		.ddram_ba(ddram_ba),
		.ddram_ras_n(ddram_ras_n),
		.ddram_cas_n(ddram_cas_n),
		.ddram_we_n(ddram_we_n),
		.ddram_cs_n(ddram_cs_n),
		.ddram_dm(ddram_dm),
		.ddram_dq(ddram_dq),
		.ddram_dqs_p(ddram_dqs_p),
		.ddram_dqs_n(ddram_dqs_n),
		.ddram_clk_p(ddram_clk_p),
		.ddram_clk_n(ddram_clk_n),
		.ddram_cke(ddram_cke),
		.ddram_odt(ddram_odt),
		.ddram_reset_n(ddram_reset_n),
		.init_done(init_done),
		.init_error(init_error),
		.wb_ctrl_adr(ctrl_port_wishbone_c_adr),
		.wb_ctrl_dat_w(ctrl_port_wishbone_c_dat_w),
		.wb_ctrl_dat_r(ctrl_port_wishbone_c_dat_r),
		.wb_ctrl_sel(ctrl_port_wishbone_c_sel),
		.wb_ctrl_cyc(ctrl_port_wishbone_c_cyc),
		.wb_ctrl_stb(ctrl_port_wishbone_c_stb),
		.wb_ctrl_ack(ctrl_port_wishbone_c_ack),
		.wb_ctrl_we(ctrl_port_wishbone_c_we),
		.wb_ctrl_cti(3'b000),
		.wb_ctrl_bte(2'b00),
		.wb_ctrl_err(ctrl_port_wishbone_c_err),
		.user_clkx2(sys_clkx2),
		.user_clk(sys_clk),
		.user_rst(sys_rst),
		.user_port_wishbone_0_adr(user_port_wishbone_c_0_adr),
		.user_port_wishbone_0_dat_w(user_port_wishbone_c_0_dat_w),
		.user_port_wishbone_0_dat_r(user_port_wishbone_c_0_dat_r),
		.user_port_wishbone_0_sel(user_port_wishbone_c_0_sel),
		.user_port_wishbone_0_cyc(user_port_wishbone_c_0_cyc),
		.user_port_wishbone_0_stb(user_port_wishbone_c_0_stb),
		.user_port_wishbone_0_ack(user_port_wishbone_c_0_ack),
		.user_port_wishbone_0_we(user_port_wishbone_c_0_we),
		.user_port_wishbone_0_err(user_port_wishbone_c_0_err)
	);
endmodule
