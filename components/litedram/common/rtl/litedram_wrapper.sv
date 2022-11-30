module litedram_wrapper (
	input wire         clk,
	output wire        init_done,
	output wire        init_error,
	input wire [31:0]  wb_ctrl_adr,
	input wire [31:0]  wb_ctrl_dat_w,
	output wire [31:0] wb_ctrl_dat_r,
	input wire [3:0]   wb_ctrl_sel,
    output wire        wb_ctrl_stall,
	input wire         wb_ctrl_cyc,
	input wire         wb_ctrl_stb,
	output wire        wb_ctrl_ack,
	input wire         wb_ctrl_we,
	output wire        wb_ctrl_err,
    input wire         user_port_wishbone_p_0_rst,
	input wire [31:0]  user_port_wishbone_p_0_adr,
	input wire [31:0]  user_port_wishbone_p_0_dat_w,
	output wire [31:0] user_port_wishbone_p_0_dat_r,
	input wire [3:0]   user_port_wishbone_p_0_sel,
	output wire        user_port_wishbone_p_0_stall,
	input wire         user_port_wishbone_p_0_cyc,
	input wire         user_port_wishbone_p_0_stb,
	output wire        user_port_wishbone_p_0_ack,
	input wire         user_port_wishbone_p_0_we,
	output wire        user_port_wishbone_p_0_err,
    input wire         user_port_wishbone_p_1_rst,
	input wire [31:0]  user_port_wishbone_p_1_adr,
	input wire [31:0]  user_port_wishbone_p_1_dat_w,
	output wire [31:0] user_port_wishbone_p_1_dat_r,
	input wire [3:0]   user_port_wishbone_p_1_sel,
	output wire        user_port_wishbone_p_1_stall,
	input wire         user_port_wishbone_p_1_cyc,
	input wire         user_port_wishbone_p_1_stb,
	output wire        user_port_wishbone_p_1_ack,
	input wire         user_port_wishbone_p_1_we,
	output wire        user_port_wishbone_p_1_err
                         );

   logic [23:0] user_port_wishbone_c_0_adr;
   logic [127:0] user_port_wishbone_c_0_dat_w;
   logic [127:0] user_port_wishbone_c_0_dat_r;
   logic [15:0]  user_port_wishbone_c_0_sel;
   logic        user_port_wishbone_c_0_cyc;
   logic        user_port_wishbone_c_0_stb;
   logic        user_port_wishbone_c_0_ack;
   logic        user_port_wishbone_c_0_we;
   logic        user_port_wishbone_c_0_err;

   logic [23:0] user_port_wishbone_c_1_adr;
   logic [127:0] user_port_wishbone_c_1_dat_w;
   logic [127:0] user_port_wishbone_c_1_dat_r;
   logic [15:0]  user_port_wishbone_c_1_sel;
   logic        user_port_wishbone_c_1_cyc;
   logic        user_port_wishbone_c_1_stb;
   logic        user_port_wishbone_c_1_ack;
   logic        user_port_wishbone_c_1_we;
   logic        user_port_wishbone_c_1_err;

   initial user_port_wishbone_c_0_stb = 0;

   always_ff @(posedge clk, posedge user_port_wishbone_p_0_stb)
     if (user_port_wishbone_p_0_rst)
       user_port_wishbone_c_0_stb <= 1'b0;
     else if (user_port_wishbone_p_0_stb)
       user_port_wishbone_c_0_stb <= 1'b1;
     else if (user_port_wishbone_c_0_ack || user_port_wishbone_c_0_err)
       user_port_wishbone_c_0_stb <= 1'b0;

   assign user_port_wishbone_p_0_stall = user_port_wishbone_c_0_stb & ~(user_port_wishbone_c_0_ack|user_port_wishbone_c_0_err);
   assign user_port_wishbone_p_0_dat_r = user_port_wishbone_c_0_dat_r[31:0];
   assign user_port_wishbone_p_0_ack = user_port_wishbone_c_0_ack;
   assign user_port_wishbone_p_0_err = user_port_wishbone_c_0_err;

   assign user_port_wishbone_c_0_adr = user_port_wishbone_p_0_adr[23:0];
   assign user_port_wishbone_c_0_dat_w = {96'b0,user_port_wishbone_p_0_dat_w};
   assign user_port_wishbone_c_0_sel = {12'hfff, user_port_wishbone_p_0_sel};
   assign user_port_wishbone_c_0_cyc = user_port_wishbone_p_0_cyc;
   assign user_port_wishbone_c_0_we = user_port_wishbone_p_0_we;

   initial user_port_wishbone_c_1_stb = 0;

   always_ff @(posedge clk, posedge user_port_wishbone_p_1_stb)
     if (user_port_wishbone_p_1_rst)
       user_port_wishbone_c_1_stb <= 1'b0;
     else if (user_port_wishbone_p_1_stb)
       user_port_wishbone_c_1_stb <= 1'b1;
     else if (user_port_wishbone_c_1_ack || user_port_wishbone_c_1_err)
       user_port_wishbone_c_1_stb <= 1'b0;

   assign user_port_wishbone_p_1_stall = user_port_wishbone_c_1_stb & ~(user_port_wishbone_c_1_ack|user_port_wishbone_c_1_err);
   assign user_port_wishbone_p_1_dat_r = user_port_wishbone_c_1_dat_r[31:0];
   assign user_port_wishbone_p_1_ack = user_port_wishbone_c_1_ack;
   assign user_port_wishbone_p_1_err = user_port_wishbone_c_1_err;

   assign user_port_wishbone_c_1_adr = user_port_wishbone_p_1_adr[23:0];
   assign user_port_wishbone_c_1_dat_w = {96'b0, user_port_wishbone_p_1_dat_w};
   assign user_port_wishbone_c_1_sel = {12'hfff, user_port_wishbone_p_1_sel};
   assign user_port_wishbone_c_1_cyc = user_port_wishbone_p_1_cyc;
   assign user_port_wishbone_c_1_we = user_port_wishbone_p_1_we;

   //We could also instantiated a pipeline->classic bridge for the ctrl port but I think that's overkill.
   assign wb_ctrl_stall = 1'b0;

   litedram litedram_inst (
	.sim_trace(1'b0),
	.clk(clk),
	.init_done(init_done),
	.init_error(init_error),
	.wb_ctrl_adr(wb_ctrl_adr[29:0]),
	.wb_ctrl_dat_w(wb_ctrl_dat_w),
	.wb_ctrl_dat_r(wb_ctrl_dat_r),
	.wb_ctrl_sel(wb_ctrl_sel),
	.wb_ctrl_cyc(wb_ctrl_cyc),
	.wb_ctrl_stb(wb_ctrl_stb),
	.wb_ctrl_ack(wb_ctrl_ack),
	.wb_ctrl_we(wb_ctrl_we),
	.wb_ctrl_cti(3'b0),
	.wb_ctrl_bte(2'b0),
	.wb_ctrl_err(wb_ctrl_err),
	.user_clk(),
	.user_rst(),
	.user_port_wishbone_0_adr(user_port_wishbone_c_0_adr),
	.user_port_wishbone_0_dat_w(user_port_wishbone_c_0_dat_w),
	.user_port_wishbone_0_dat_r(user_port_wishbone_c_0_dat_r),
	.user_port_wishbone_0_sel(user_port_wishbone_c_0_sel),
	.user_port_wishbone_0_cyc(user_port_wishbone_c_0_cyc),
	.user_port_wishbone_0_stb(user_port_wishbone_c_0_stb),
	.user_port_wishbone_0_ack(user_port_wishbone_c_0_ack),
	.user_port_wishbone_0_we(user_port_wishbone_c_0_we),
	.user_port_wishbone_0_err(user_port_wishbone_c_0_err),
	.user_port_wishbone_1_adr(user_port_wishbone_c_1_adr),
	.user_port_wishbone_1_dat_w(user_port_wishbone_c_1_dat_w),
	.user_port_wishbone_1_dat_r(user_port_wishbone_c_1_dat_r),
	.user_port_wishbone_1_sel(user_port_wishbone_c_1_sel),
	.user_port_wishbone_1_cyc(user_port_wishbone_c_1_cyc),
	.user_port_wishbone_1_stb(user_port_wishbone_c_1_stb),
	.user_port_wishbone_1_ack(user_port_wishbone_c_1_ack),
	.user_port_wishbone_1_we(user_port_wishbone_c_1_we),
	.user_port_wishbone_1_err(user_port_wishbone_c_1_err)
				           );
   endmodule
