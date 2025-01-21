`default_nettype wire
module YM2149_PSG_system_wb (
	clk,
	clk_i2s,
	rst,
	wb_adr,
	wb_dat_w,
	wb_dat_r,
	wb_sel,
	wb_stall,
	wb_cyc,
	wb_stb,
	wb_ack,
	wb_we,
	wb_err,
	i2s_sclk,
	i2s_lrclk,
	i2s_data,
	sound,
	sound_right
);
	parameter CLK_IN_HZ = 100000000;
	parameter CLK_I2S_IN_HZ = 200000000;
	parameter CLK_PSG_HZ = 1789000;
	parameter I2S_DAC_HZ = 48000;
	parameter YM2149_DAC_BITS = 8;
	parameter MIXER_DAC_BITS = 16;
	input wire clk;
	input wire clk_i2s;
	input wire rst;
	input wire [7:0] wb_adr;
	input wire [31:0] wb_dat_w;
	output wire [31:0] wb_dat_r;
	input wire [3:0] wb_sel;
	output wire wb_stall;
	input wire wb_cyc;
	input wire wb_stb;
	output wire wb_ack;
	input wire wb_we;
	output wire wb_err;
	output wire i2s_sclk;
	output wire i2s_lrclk;
	output wire i2s_data;
	output wire signed [MIXER_DAC_BITS - 1:0] sound;
	output wire signed [MIXER_DAC_BITS - 1:0] sound_right;
	wire [7:0] ym_sys_addr;
	wire [7:0] ym_sys_data;
	wire ym_sys_wr_n;
	wire [7:0] ym_sys_dout;
	reg do_ack;
	assign ym_sys_addr = wb_adr;
	assign ym_sys_data = wb_dat_w[7:0];
	assign wb_dat_r = {24'b000000000000000000000000, ym_sys_dout};
	assign wb_err = 1'b0;
	assign ym_sys_wr_n = ~((wb_cyc && wb_stb) && wb_we);
	always @(posedge clk) begin
		do_ack <= 1'b0;
		if (wb_stb)
			do_ack <= 1'b1;
	end
	assign wb_ack = do_ack & wb_cyc;
	assign wb_stall = (!wb_cyc ? 1'b0 : !wb_ack);
	YM2149_PSG_system #(
		.CLK_IN_HZ(CLK_IN_HZ),
		.CLK_I2S_IN_HZ(CLK_I2S_IN_HZ),
		.CLK_PSG_HZ(CLK_PSG_HZ),
		.I2S_DAC_HZ(I2S_DAC_HZ),
		.YM2149_DAC_BITS(YM2149_DAC_BITS),
		.MIXER_DAC_BITS(MIXER_DAC_BITS)
	) ym2149_psg_sys_inst(
		.clk(clk),
		.clk_i2s(clk_i2s),
		.reset_n(~rst),
		.addr(ym_sys_addr),
		.data(ym_sys_data),
		.wr_n(ym_sys_wr_n),
		.dout(ym_sys_dout),
		.i2s_sclk(i2s_sclk),
		.i2s_lrclk(i2s_lrclk),
		.i2s_data(i2s_data),
		.sound(sound),
		.sound_right(sound_right)
	);
endmodule
