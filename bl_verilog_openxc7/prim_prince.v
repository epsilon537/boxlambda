module prim_prince (
	clk_i,
	rst_ni,
	valid_i,
	data_i,
	key_i,
	dec_i,
	valid_o,
	data_o
);
	reg _sv2v_0;
	parameter signed [31:0] DataWidth = 64;
	parameter signed [31:0] KeyWidth = 128;
	parameter signed [31:0] NumRoundsHalf = 5;
	parameter [0:0] UseOldKeySched = 1'b0;
	parameter [0:0] HalfwayDataReg = 1'b0;
	parameter [0:0] HalfwayKeyReg = 1'b0;
	input clk_i;
	input rst_ni;
	input valid_i;
	input [DataWidth - 1:0] data_i;
	input [KeyWidth - 1:0] key_i;
	input dec_i;
	output wire valid_o;
	output reg [DataWidth - 1:0] data_o;
	reg [DataWidth - 1:0] k0;
	reg [DataWidth - 1:0] k0_prime_d;
	reg [DataWidth - 1:0] k1_d;
	reg [DataWidth - 1:0] k0_new_d;
	reg [DataWidth - 1:0] k0_prime_q;
	reg [DataWidth - 1:0] k1_q;
	reg [DataWidth - 1:0] k0_new_q;
	localparam [63:0] prim_cipher_pkg_PRINCE_ALPHA_CONST = 64'hc0ac29b7c97c50dd;
	always @(*) begin : p_key_expansion
		if (_sv2v_0)
			;
		k0 = key_i[(2 * DataWidth) - 1:DataWidth];
		k0_prime_d = {k0[0], k0[DataWidth - 1:2], k0[DataWidth - 1] ^ k0[1]};
		k1_d = key_i[DataWidth - 1:0];
		if (dec_i) begin
			k0 = k0_prime_d;
			k0_prime_d = key_i[(2 * DataWidth) - 1:DataWidth];
			k1_d = k1_d ^ prim_cipher_pkg_PRINCE_ALPHA_CONST[DataWidth - 1:0];
		end
	end
	generate
		if (UseOldKeySched) begin : gen_legacy_keyschedule
			wire [DataWidth:1] sv2v_tmp_003C5;
			assign sv2v_tmp_003C5 = k1_d;
			always @(*) k0_new_d = sv2v_tmp_003C5;
		end
		else begin : gen_new_keyschedule
			always @(*) begin : p_new_keyschedule_k0_alpha
				if (_sv2v_0)
					;
				k0_new_d = key_i[(2 * DataWidth) - 1:DataWidth];
				if (dec_i)
					k0_new_d = k0_new_d ^ prim_cipher_pkg_PRINCE_ALPHA_CONST[DataWidth - 1:0];
			end
		end
		if (HalfwayKeyReg) begin : gen_key_reg
			always @(posedge clk_i or negedge rst_ni) begin : p_key_reg
				if (!rst_ni) begin
					k1_q <= 1'sb0;
					k0_prime_q <= 1'sb0;
					k0_new_q <= 1'sb0;
				end
				else if (valid_i) begin
					k1_q <= k1_d;
					k0_prime_q <= k0_prime_d;
					k0_new_q <= k0_new_d;
				end
			end
		end
		else begin : gen_no_key_reg
			wire [DataWidth:1] sv2v_tmp_6E066;
			assign sv2v_tmp_6E066 = k1_d;
			always @(*) k1_q = sv2v_tmp_6E066;
			wire [DataWidth:1] sv2v_tmp_BD8CA;
			assign sv2v_tmp_BD8CA = k0_prime_d;
			always @(*) k0_prime_q = sv2v_tmp_BD8CA;
			wire [DataWidth:1] sv2v_tmp_A995E;
			assign sv2v_tmp_A995E = k0_new_d;
			always @(*) k0_new_q = sv2v_tmp_A995E;
		end
	endgenerate
	reg [(NumRoundsHalf >= 0 ? ((NumRoundsHalf + 1) * DataWidth) - 1 : ((1 - NumRoundsHalf) * DataWidth) + ((NumRoundsHalf * DataWidth) - 1)):(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf * DataWidth)] data_state_lo;
	reg [(NumRoundsHalf >= 0 ? ((NumRoundsHalf + 1) * DataWidth) - 1 : ((1 - NumRoundsHalf) * DataWidth) + ((NumRoundsHalf * DataWidth) - 1)):(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf * DataWidth)] data_state_hi;
	localparam [767:0] prim_cipher_pkg_PRINCE_ROUND_CONST = 768'hc0ac29b7c97c50ddd3b5a399ca0c239964a51195e0e3610dc882d32f25323c5485840851f1ac43aa7ef84f78fd955cb1be5466cf34e90c6c452821e638d01377082efa98ec4e6c89a4093822299f31d013198a2e037073440000000000000000;
	always @(*) begin : p_pre_round_xor
		if (_sv2v_0)
			;
		data_state_lo[(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf) * DataWidth+:DataWidth] = data_i ^ k0;
		data_state_lo[(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf) * DataWidth+:DataWidth] = data_state_lo[(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf) * DataWidth+:DataWidth] ^ k1_d;
		data_state_lo[(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf) * DataWidth+:DataWidth] = data_state_lo[(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf) * DataWidth+:DataWidth] ^ prim_cipher_pkg_PRINCE_ROUND_CONST[DataWidth - 1-:DataWidth];
	end
	genvar _gv_k_2;
	localparam [63:0] prim_cipher_pkg_PRINCE_SBOX4 = 64'h4d5e087619ca23fb;
	localparam [63:0] prim_cipher_pkg_PRINCE_SHIFT_ROWS64 = 64'hfa50b61c72d83e94;
	localparam [15:0] prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST0 = 16'h7bde;
	localparam [15:0] prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST1 = 16'hbde7;
	localparam [15:0] prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST2 = 16'hde7b;
	localparam [15:0] prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST3 = 16'he7bd;
	function automatic [3:0] prim_cipher_pkg_prince_nibble_red16;
		input reg [15:0] vect;
		prim_cipher_pkg_prince_nibble_red16 = ((vect[0+:4] ^ vect[4+:4]) ^ vect[8+:4]) ^ vect[12+:4];
	endfunction
	function automatic [31:0] prim_cipher_pkg_prince_mult_prime_32bit;
		input reg [31:0] state_in;
		reg [31:0] state_out;
		begin
			state_out[0+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST3);
			state_out[4+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST2);
			state_out[8+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST1);
			state_out[12+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST0);
			state_out[16+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST0);
			state_out[20+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST3);
			state_out[24+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST2);
			state_out[28+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST1);
			prim_cipher_pkg_prince_mult_prime_32bit = state_out;
		end
	endfunction
	function automatic [63:0] prim_cipher_pkg_prince_mult_prime_64bit;
		input reg [63:0] state_in;
		reg [63:0] state_out;
		begin
			state_out[0+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST3);
			state_out[4+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST2);
			state_out[8+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST1);
			state_out[12+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[0+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST0);
			state_out[16+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST0);
			state_out[20+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST3);
			state_out[24+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST2);
			state_out[28+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[16+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST1);
			state_out[32+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[32+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST0);
			state_out[36+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[32+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST3);
			state_out[40+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[32+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST2);
			state_out[44+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[32+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST1);
			state_out[48+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[48+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST3);
			state_out[52+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[48+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST2);
			state_out[56+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[48+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST1);
			state_out[60+:4] = prim_cipher_pkg_prince_nibble_red16(state_in[48+:16] & prim_cipher_pkg_PRINCE_SHIFT_ROWS_CONST0);
			prim_cipher_pkg_prince_mult_prime_64bit = state_out;
		end
	endfunction
	function automatic [31:0] prim_cipher_pkg_prince_shiftrows_32bit;
		input reg [31:0] state_in;
		input reg [63:0] shifts;
		reg [31:0] state_out;
		begin
			begin : sv2v_autoblock_1
				reg signed [31:0] k;
				for (k = 0; k < 16; k = k + 1)
					state_out[k * 2+:2] = state_in[shifts[k * 4+:4] * 2+:2];
			end
			prim_cipher_pkg_prince_shiftrows_32bit = state_out;
		end
	endfunction
	function automatic [63:0] prim_cipher_pkg_prince_shiftrows_64bit;
		input reg [63:0] state_in;
		input reg [63:0] shifts;
		reg [63:0] state_out;
		begin
			begin : sv2v_autoblock_2
				reg signed [31:0] k;
				for (k = 0; k < 16; k = k + 1)
					state_out[k * 4+:4] = state_in[shifts[k * 4+:4] * 4+:4];
			end
			prim_cipher_pkg_prince_shiftrows_64bit = state_out;
		end
	endfunction
	function automatic [7:0] prim_cipher_pkg_sbox4_8bit;
		input reg [7:0] state_in;
		input reg [63:0] sbox4;
		reg [7:0] state_out;
		begin
			begin : sv2v_autoblock_3
				reg signed [31:0] k;
				for (k = 0; k < 2; k = k + 1)
					state_out[k * 4+:4] = sbox4[state_in[k * 4+:4] * 4+:4];
			end
			prim_cipher_pkg_sbox4_8bit = state_out;
		end
	endfunction
	function automatic [31:0] prim_cipher_pkg_sbox4_32bit;
		input reg [31:0] state_in;
		input reg [63:0] sbox4;
		reg [31:0] state_out;
		begin
			begin : sv2v_autoblock_4
				reg signed [31:0] k;
				for (k = 0; k < 4; k = k + 1)
					state_out[k * 8+:8] = prim_cipher_pkg_sbox4_8bit(state_in[k * 8+:8], sbox4);
			end
			prim_cipher_pkg_sbox4_32bit = state_out;
		end
	endfunction
	function automatic [63:0] prim_cipher_pkg_sbox4_64bit;
		input reg [63:0] state_in;
		input reg [63:0] sbox4;
		reg [63:0] state_out;
		begin
			begin : sv2v_autoblock_5
				reg signed [31:0] k;
				for (k = 0; k < 8; k = k + 1)
					state_out[k * 8+:8] = prim_cipher_pkg_sbox4_8bit(state_in[k * 8+:8], sbox4);
			end
			prim_cipher_pkg_sbox4_64bit = state_out;
		end
	endfunction
	generate
		for (_gv_k_2 = 1; _gv_k_2 <= NumRoundsHalf; _gv_k_2 = _gv_k_2 + 1) begin : gen_fwd_pass
			localparam k = _gv_k_2;
			reg [DataWidth - 1:0] data_state_round;
			if (DataWidth == 64) begin : gen_fwd_d64
				always @(*) begin : p_fwd_d64
					if (_sv2v_0)
						;
					data_state_round = prim_cipher_pkg_sbox4_64bit(data_state_lo[(NumRoundsHalf >= 0 ? k - 1 : NumRoundsHalf - (k - 1)) * DataWidth+:DataWidth], prim_cipher_pkg_PRINCE_SBOX4);
					data_state_round = prim_cipher_pkg_prince_mult_prime_64bit(data_state_round);
					data_state_round = prim_cipher_pkg_prince_shiftrows_64bit(data_state_round, prim_cipher_pkg_PRINCE_SHIFT_ROWS64);
				end
			end
			else begin : gen_fwd_d32
				always @(*) begin : p_fwd_d32
					if (_sv2v_0)
						;
					data_state_round = prim_cipher_pkg_sbox4_32bit(data_state_lo[(NumRoundsHalf >= 0 ? k - 1 : NumRoundsHalf - (k - 1)) * DataWidth+:DataWidth], prim_cipher_pkg_PRINCE_SBOX4);
					data_state_round = prim_cipher_pkg_prince_mult_prime_32bit(data_state_round);
					data_state_round = prim_cipher_pkg_prince_shiftrows_32bit(data_state_round, prim_cipher_pkg_PRINCE_SHIFT_ROWS64);
				end
			end
			wire [DataWidth - 1:0] data_state_xor;
			assign data_state_xor = data_state_round ^ prim_cipher_pkg_PRINCE_ROUND_CONST[(k * 64) + (DataWidth - 1)-:DataWidth];
			if ((k % 2) == 1) begin : gen_fwd_key_odd
				wire [DataWidth * 1:1] sv2v_tmp_6624A;
				assign sv2v_tmp_6624A = data_state_xor ^ k0_new_d;
				always @(*) data_state_lo[(NumRoundsHalf >= 0 ? k : NumRoundsHalf - k) * DataWidth+:DataWidth] = sv2v_tmp_6624A;
			end
			else begin : gen_fwd_key_even
				wire [DataWidth * 1:1] sv2v_tmp_BC4CC;
				assign sv2v_tmp_BC4CC = data_state_xor ^ k1_d;
				always @(*) data_state_lo[(NumRoundsHalf >= 0 ? k : NumRoundsHalf - k) * DataWidth+:DataWidth] = sv2v_tmp_BC4CC;
			end
		end
	endgenerate
	reg [DataWidth - 1:0] data_state_middle_d;
	reg [DataWidth - 1:0] data_state_middle_q;
	reg [DataWidth - 1:0] data_state_middle;
	localparam [63:0] prim_cipher_pkg_PRINCE_SBOX4_INV = 64'h1ce5046a98df237b;
	generate
		if (DataWidth == 64) begin : gen_middle_d64
			always @(*) begin : p_middle_d64
				if (_sv2v_0)
					;
				data_state_middle_d = prim_cipher_pkg_sbox4_64bit(data_state_lo[(NumRoundsHalf >= 0 ? NumRoundsHalf : NumRoundsHalf - NumRoundsHalf) * DataWidth+:DataWidth], prim_cipher_pkg_PRINCE_SBOX4);
				data_state_middle = prim_cipher_pkg_prince_mult_prime_64bit(data_state_middle_q);
				data_state_middle = prim_cipher_pkg_sbox4_64bit(data_state_middle, prim_cipher_pkg_PRINCE_SBOX4_INV);
			end
		end
		else begin : gen_middle_d32
			always @(*) begin : p_middle_d32
				if (_sv2v_0)
					;
				data_state_middle_d = prim_cipher_pkg_sbox4_32bit(data_state_middle[NumRoundsHalf], prim_cipher_pkg_PRINCE_SBOX4);
				data_state_middle = prim_cipher_pkg_prince_mult_prime_32bit(data_state_middle_q);
				data_state_middle = prim_cipher_pkg_sbox4_32bit(data_state_middle, prim_cipher_pkg_PRINCE_SBOX4_INV);
			end
		end
		if (HalfwayDataReg) begin : gen_data_reg
			reg valid_q;
			always @(posedge clk_i or negedge rst_ni) begin : p_data_reg
				if (!rst_ni) begin
					valid_q <= 1'b0;
					data_state_middle_q <= 1'sb0;
				end
				else begin
					valid_q <= valid_i;
					if (valid_i)
						data_state_middle_q <= data_state_middle_d;
				end
			end
			assign valid_o = valid_q;
		end
		else begin : gen_no_data_reg
			wire [DataWidth:1] sv2v_tmp_F3D64;
			assign sv2v_tmp_F3D64 = data_state_middle_d;
			always @(*) data_state_middle_q = sv2v_tmp_F3D64;
			assign valid_o = valid_i;
		end
	endgenerate
	wire [DataWidth * 1:1] sv2v_tmp_AD9CC;
	assign sv2v_tmp_AD9CC = data_state_middle;
	always @(*) data_state_hi[(NumRoundsHalf >= 0 ? 0 : NumRoundsHalf) * DataWidth+:DataWidth] = sv2v_tmp_AD9CC;
	genvar _gv_k_3;
	localparam [63:0] prim_cipher_pkg_PRINCE_SHIFT_ROWS64_INV = 64'hf258be147ad0369c;
	generate
		for (_gv_k_3 = 1; _gv_k_3 <= NumRoundsHalf; _gv_k_3 = _gv_k_3 + 1) begin : gen_bwd_pass
			localparam k = _gv_k_3;
			wire [DataWidth - 1:0] data_state_xor0;
			wire [DataWidth - 1:0] data_state_xor1;
			if ((((NumRoundsHalf + k) + 1) % 2) == 1) begin : gen_bkwd_key_odd
				assign data_state_xor0 = data_state_hi[(NumRoundsHalf >= 0 ? k - 1 : NumRoundsHalf - (k - 1)) * DataWidth+:DataWidth] ^ k0_new_q;
			end
			else begin : gen_bkwd_key_even
				assign data_state_xor0 = data_state_hi[(NumRoundsHalf >= 0 ? k - 1 : NumRoundsHalf - (k - 1)) * DataWidth+:DataWidth] ^ k1_q;
			end
			assign data_state_xor1 = data_state_xor0 ^ prim_cipher_pkg_PRINCE_ROUND_CONST[(((10 - NumRoundsHalf) + k) * 64) + (DataWidth - 1)-:DataWidth];
			reg [DataWidth - 1:0] data_state_bwd;
			if (DataWidth == 64) begin : gen_bwd_d64
				always @(*) begin : p_bwd_d64
					if (_sv2v_0)
						;
					data_state_bwd = prim_cipher_pkg_prince_shiftrows_64bit(data_state_xor1, prim_cipher_pkg_PRINCE_SHIFT_ROWS64_INV);
					data_state_bwd = prim_cipher_pkg_prince_mult_prime_64bit(data_state_bwd);
					data_state_hi[(NumRoundsHalf >= 0 ? k : NumRoundsHalf - k) * DataWidth+:DataWidth] = prim_cipher_pkg_sbox4_64bit(data_state_bwd, prim_cipher_pkg_PRINCE_SBOX4_INV);
				end
			end
			else begin : gen_bwd_d32
				always @(*) begin : p_bwd_d32
					if (_sv2v_0)
						;
					data_state_bwd = prim_cipher_pkg_prince_shiftrows_32bit(data_state_xor1, prim_cipher_pkg_PRINCE_SHIFT_ROWS64_INV);
					data_state_bwd = prim_cipher_pkg_prince_mult_prime_32bit(data_state_bwd);
					data_state_hi[(NumRoundsHalf >= 0 ? k : NumRoundsHalf - k) * DataWidth+:DataWidth] = prim_cipher_pkg_sbox4_32bit(data_state_bwd, prim_cipher_pkg_PRINCE_SBOX4_INV);
				end
			end
		end
	endgenerate
	always @(*) begin : p_post_round_xor
		if (_sv2v_0)
			;
		data_o = data_state_hi[(NumRoundsHalf >= 0 ? NumRoundsHalf : NumRoundsHalf - NumRoundsHalf) * DataWidth+:DataWidth] ^ prim_cipher_pkg_PRINCE_ROUND_CONST[DataWidth + 703-:DataWidth];
		data_o = data_o ^ k1_q;
		data_o = data_o ^ k0_prime_q;
	end
	initial _sv2v_0 = 0;
endmodule
