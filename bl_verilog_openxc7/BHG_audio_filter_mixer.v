`default_nettype wire
module BHG_audio_filter_mixer (
	rst,
	clk,
	clk_en,
	c_addr,
	c_wr,
	c_din,
	c_dout,
	s_in,
	s_out
);
	reg _sv2v_0;
	parameter signed [31:0] IN_BITS = 8;
	parameter [0:5] IN_REP = 6'h00;
	parameter signed [31:0] OUT_BITS = 12;
	parameter [79:0] RST_REGS = 296'h40000000400000004000000040000000400000004000000080000000001900000080;
	input rst;
	input clk;
	input clk_en;
	input [3:0] c_addr;
	input c_wr;
	input [7:0] c_din;
	output reg [7:0] c_dout;
	input [(6 * IN_BITS) - 1:0] s_in;
	output wire signed [OUT_BITS - 1:0] s_out;
	initial c_dout = 8'd0;
	reg [4:0] seq = 0;
	reg signed [15:0] audio_out = 0;
	function automatic signed [OUT_BITS - 1:0] sv2v_cast_CEC20_signed;
		input reg signed [OUT_BITS - 1:0] inp;
		sv2v_cast_CEC20_signed = inp;
	endfunction
	assign s_out = sv2v_cast_CEC20_signed(audio_out <<< (OUT_BITS - 14));
	reg [79:0] cregs = {10 {8'd0}};
	always @(posedge clk)
		if (rst)
			cregs <= RST_REGS;
		else if (c_wr)
			cregs[(9 - c_addr) * 8+:8] <= c_din;
	always @(posedge clk) c_dout <= cregs[(9 - c_addr) * 8+:8];
	reg signed [15:0] ps_in [0:5];
	reg signed [15:0] sound_in [0:5];
	always @(*) begin : sv2v_autoblock_1
		reg signed [31:0] i;
		if (_sv2v_0)
			;
		for (i = 0; i < 6; i = i + 1)
			ps_in[i] = (s_in[(5 - i) * IN_BITS+:IN_BITS] ^ (!IN_REP[i] << (IN_BITS - 1))) << (OUT_BITS - IN_BITS);
	end
	always @(*) begin : sv2v_autoblock_2
		reg signed [31:0] i;
		if (_sv2v_0)
			;
		for (i = 0; i < 6; i = i + 1)
			sound_in[i] = ps_in[i] >>> 2;
	end
	reg signed [8:0] volume [0:5];
	always @(*) begin : sv2v_autoblock_3
		reg signed [31:0] i;
		if (_sv2v_0)
			;
		for (i = 0; i < 6; i = i + 1)
			volume[i] = {cregs[16 + i], (cregs[16 + i] ? cregs[(9 - i) * 8+:8] ^ 8'd255 : cregs[(9 - i) * 8+:8])};
	end
	reg signed [15:0] m_audio_in = 0;
	reg signed [8:0] m_audio_gain = 0;
	reg signed [23:0] m_audio_out = 0;
	always @(posedge clk) m_audio_out <= m_audio_in * m_audio_gain;
	reg signed [15:0] a_audio_in = 0;
	reg signed [15:0] a_audio_out = 0;
	always @(posedge clk)
		if (seq == 0)
			a_audio_out <= 0;
		else
			a_audio_out <= a_audio_out + a_audio_in;
	localparam signed [15:0] clip_pos = 8191;
	reg signed [23:0] dc_offset = 0;
	reg signed [15:0] hf_offset = 0;
	reg signed [15:0] lf_offset_adj = 0;
	reg signed [15:0] hf_offset_adj = 0;
	reg dcc = 0;
	wire [7:0] bass = cregs[8+:8];
	wire [7:0] treb = 8'd255 ^ cregs[0+:8];
	wire [7:0] vol = cregs[24+:8];
	function automatic signed [15:0] sv2v_cast_16_signed;
		input reg signed [15:0] inp;
		sv2v_cast_16_signed = inp;
	endfunction
	always @(posedge clk)
		if (rst) begin
			dc_offset <= 0;
			seq <= 0;
		end
		else if (clk_en)
			seq <= 0;
		else begin
			if (seq != 23)
				seq <= seq + 1'b1;
			if (seq <= 5) begin
				m_audio_in <= sound_in[seq];
				m_audio_gain <= volume[seq];
			end
			a_audio_in <= 0;
			if ((seq >= 2) && (seq <= 7))
				a_audio_in <= sv2v_cast_16_signed(m_audio_out >>> 8);
			else if (seq == 9) begin
				if ((a_audio_out + (dc_offset >>> 8)) > clip_pos) begin
					dcc <= 1;
					dc_offset <= (clip_pos - a_audio_out) <<< 8;
				end
				else if ((a_audio_out + (dc_offset >>> 8)) < -clip_pos) begin
					dcc <= 1;
					dc_offset <= -(clip_pos + a_audio_out) <<< 8;
				end
				else begin
					dcc <= 0;
					lf_offset_adj <= sv2v_cast_16_signed((a_audio_out + (dc_offset >>> 8)) >>> 8);
				end
			end
			else if ((seq == 10) && !dcc) begin
				m_audio_in <= lf_offset_adj;
				m_audio_gain <= bass;
			end
			else if ((seq == 12) && !dcc)
				dc_offset <= dc_offset - m_audio_out;
			else if (seq == 13)
				a_audio_in <= sv2v_cast_16_signed(dc_offset >>> 8);
			else if (seq == 15)
				hf_offset_adj <= hf_offset - a_audio_out;
			else if (seq == 16) begin
				m_audio_in <= hf_offset_adj;
				m_audio_gain <= treb;
			end
			else if (seq == 18)
				a_audio_in <= sv2v_cast_16_signed(m_audio_out >>> 8);
			else if (seq == 20) begin
				hf_offset <= a_audio_out;
				m_audio_in <= a_audio_out;
				m_audio_gain <= vol;
			end
			else if (seq == 22) begin
				if ((m_audio_out >>> 7) > clip_pos)
					audio_out <= (clip_pos >>> 32'sd1) + (audio_out >>> 32'sd1);
				else if ((m_audio_out >>> 7) < -clip_pos)
					audio_out <= (-clip_pos >>> 32'sd1) + (audio_out >>> 32'sd1);
				else
					audio_out <= sv2v_cast_16_signed((m_audio_out >>> 8) + (audio_out >>> 1));
			end
		end
	initial _sv2v_0 = 0;
endmodule
