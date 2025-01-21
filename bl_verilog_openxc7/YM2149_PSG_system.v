`default_nettype wire
module YM2149_PSG_system (
	clk,
	clk_i2s,
	reset_n,
	addr,
	data,
	wr_n,
	dout,
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
	input wire reset_n;
	input wire [7:0] addr;
	input wire [7:0] data;
	input wire wr_n;
	output reg [7:0] dout;
	output wire i2s_sclk;
	output wire i2s_lrclk;
	output wire i2s_data;
	output wire signed [MIXER_DAC_BITS - 1:0] sound;
	output wire signed [MIXER_DAC_BITS - 1:0] sound_right;
	wire i2s_clk;
	wire i2s_stb;
	wire p_div;
	wire p_stb;
	wire sample_stb;
	wire [YM2149_DAC_BITS - 1:0] sound_A;
	wire [YM2149_DAC_BITS - 1:0] sound_B;
	wire [YM2149_DAC_BITS - 1:0] sound_C;
	wire [YM2149_DAC_BITS + 1:0] sound_mix;
	wire signed [MIXER_DAC_BITS - 1:0] sound_left;
	wire [7:0] rr_psg_a;
	wire [7:0] rr_psg_b;
	wire [7:0] rr_fmix_l;
	wire [7:0] rr_fmix_r;
	initial dout = 8'd0;
	always @(posedge clk) dout <= (addr[7:4] == 4'b1001 ? rr_fmix_r : (addr[7:4] == 4'b1000 ? rr_fmix_l : (addr[7:4] == 4'b0001 ? rr_psg_b : (addr[7:4] == 4'b0000 ? rr_psg_a : 8'd0))));
	BHG_FP_clk_divider #(
		.INPUT_CLK_HZ(CLK_IN_HZ),
		.OUTPUT_CLK_HZ(CLK_PSG_HZ)
	) fdiv_psg(
		.clk_in(clk),
		.rst_in(1'b0),
		.clk_out(p_div),
		.clk_p0(p_stb),
		.clk_p180()
	);
	BHG_jt49 #(.DAC_BITS(YM2149_DAC_BITS)) PSG(
		.rst_n(reset_n),
		.clk(clk),
		.clk_en(p_stb),
		.addr(addr[3:0]),
		.cs_n(1'b0),
		.wr_n(wr_n || (addr[7:4] != 4'b0000)),
		.din(data),
		.sel(1'b1),
		.dout(rr_psg_a),
		.sound(sound_mix),
		.A(sound_A),
		.B(sound_B),
		.C(sound_C),
		.sample(sample_stb),
		.IOA_in(),
		.IOA_out(),
		.IOB_in(),
		.IOB_out()
	);
	wire [YM2149_DAC_BITS - 1:0] sound_D;
	wire [YM2149_DAC_BITS - 1:0] sound_E;
	wire [YM2149_DAC_BITS - 1:0] sound_F;
	BHG_jt49 #(.DAC_BITS(YM2149_DAC_BITS)) PSG_b(
		.rst_n(reset_n),
		.clk(clk),
		.clk_en(p_stb),
		.addr(addr[3:0]),
		.cs_n(1'b0),
		.wr_n(wr_n || (addr[7:4] != 4'b0001)),
		.din(data),
		.sel(1'b1),
		.dout(rr_psg_b),
		.sound(),
		.A(sound_D),
		.B(sound_E),
		.C(sound_F),
		.sample(),
		.IOA_in(),
		.IOA_out(),
		.IOB_in(),
		.IOB_out()
	);
	BHG_audio_filter_mixer #(
		.IN_BITS(YM2149_DAC_BITS),
		.OUT_BITS(MIXER_DAC_BITS)
	) FMIX_LEFT(
		.rst(~reset_n),
		.clk(clk),
		.clk_en(sample_stb),
		.c_addr(addr[3:0]),
		.c_wr(~(wr_n || (addr[7:4] != 4'b1000))),
		.c_din(data),
		.c_dout(rr_fmix_l),
		.s_in({sound_A, sound_B, sound_C, sound_D, sound_E, sound_F}),
		.s_out(sound_left)
	);
	assign sound_right = sound_left;
	assign sound = sound_left;
	assign rr_fmix_r = 8'd0;
	assign i2s_sclk = 0;
	assign i2s_lrclk = 0;
	assign i2s_data = 0;
endmodule
