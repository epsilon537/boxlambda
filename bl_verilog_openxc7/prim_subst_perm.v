module prim_subst_perm (
	data_i,
	key_i,
	data_o
);
	reg _sv2v_0;
	parameter signed [31:0] DataWidth = 64;
	parameter signed [31:0] NumRounds = 31;
	parameter [0:0] Decrypt = 0;
	input [DataWidth - 1:0] data_i;
	input [DataWidth - 1:0] key_i;
	output wire [DataWidth - 1:0] data_o;
	reg [(NumRounds >= 0 ? ((NumRounds + 1) * DataWidth) - 1 : ((1 - NumRounds) * DataWidth) + ((NumRounds * DataWidth) - 1)):(NumRounds >= 0 ? 0 : NumRounds * DataWidth)] data_state;
	wire [DataWidth * 1:1] sv2v_tmp_BB140;
	assign sv2v_tmp_BB140 = data_i;
	always @(*) data_state[(NumRounds >= 0 ? 0 : NumRounds) * DataWidth+:DataWidth] = sv2v_tmp_BB140;
	genvar _gv_r_1;
	localparam [63:0] prim_cipher_pkg_PRESENT_SBOX4 = 64'h21748fe3da09b65c;
	localparam [63:0] prim_cipher_pkg_PRESENT_SBOX4_INV = 64'ha970364bd21c8fe5;
	generate
		for (_gv_r_1 = 0; _gv_r_1 < NumRounds; _gv_r_1 = _gv_r_1 + 1) begin : gen_round
			localparam r = _gv_r_1;
			reg [DataWidth - 1:0] data_state_sbox;
			reg [DataWidth - 1:0] data_state_flipped;
			if (Decrypt) begin : gen_dec
				always @(*) begin : p_dec
					if (_sv2v_0)
						;
					data_state_sbox = data_state[(NumRounds >= 0 ? r : NumRounds - r) * DataWidth+:DataWidth] ^ key_i;
					data_state_flipped = data_state_sbox;
					begin : sv2v_autoblock_1
						reg signed [31:0] k;
						for (k = 0; k < (DataWidth / 2); k = k + 1)
							begin
								data_state_flipped[k * 2] = data_state_sbox[k];
								data_state_flipped[(k * 2) + 1] = data_state_sbox[k + (DataWidth / 2)];
							end
					end
					begin : sv2v_autoblock_2
						reg signed [31:0] k;
						for (k = 0; k < DataWidth; k = k + 1)
							data_state_sbox[(DataWidth - 1) - k] = data_state_flipped[k];
					end
					begin : sv2v_autoblock_3
						reg signed [31:0] k;
						for (k = 0; k < (DataWidth / 4); k = k + 1)
							data_state_sbox[k * 4+:4] = prim_cipher_pkg_PRESENT_SBOX4_INV[data_state_sbox[k * 4+:4] * 4+:4];
					end
					data_state[(NumRounds >= 0 ? r + 1 : NumRounds - (r + 1)) * DataWidth+:DataWidth] = data_state_sbox;
				end
			end
			else begin : gen_enc
				always @(*) begin : p_enc
					if (_sv2v_0)
						;
					data_state_sbox = data_state[(NumRounds >= 0 ? r : NumRounds - r) * DataWidth+:DataWidth] ^ key_i;
					begin : sv2v_autoblock_4
						reg signed [31:0] k;
						for (k = 0; k < (DataWidth / 4); k = k + 1)
							data_state_sbox[k * 4+:4] = prim_cipher_pkg_PRESENT_SBOX4[data_state_sbox[k * 4+:4] * 4+:4];
					end
					begin : sv2v_autoblock_5
						reg signed [31:0] k;
						for (k = 0; k < DataWidth; k = k + 1)
							data_state_flipped[(DataWidth - 1) - k] = data_state_sbox[k];
					end
					data_state_sbox = data_state_flipped;
					begin : sv2v_autoblock_6
						reg signed [31:0] k;
						for (k = 0; k < (DataWidth / 2); k = k + 1)
							begin
								data_state_sbox[k] = data_state_flipped[k * 2];
								data_state_sbox[k + (DataWidth / 2)] = data_state_flipped[(k * 2) + 1];
							end
					end
					data_state[(NumRounds >= 0 ? r + 1 : NumRounds - (r + 1)) * DataWidth+:DataWidth] = data_state_sbox;
				end
			end
		end
	endgenerate
	assign data_o = data_state[(NumRounds >= 0 ? NumRounds : NumRounds - NumRounds) * DataWidth+:DataWidth] ^ key_i;
	initial _sv2v_0 = 0;
endmodule
