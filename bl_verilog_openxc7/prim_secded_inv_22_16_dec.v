module prim_secded_inv_22_16_dec (
	data_i,
	data_o,
	syndrome_o,
	err_o
);
	reg _sv2v_0;
	input [21:0] data_i;
	output reg [15:0] data_o;
	output reg [5:0] syndrome_o;
	output reg [1:0] err_o;
	always @(*) begin : p_encode
		if (_sv2v_0)
			;
		syndrome_o[0] = ^((data_i ^ 22'h2a0000) & 22'h01496e);
		syndrome_o[1] = ^((data_i ^ 22'h2a0000) & 22'h02f20b);
		syndrome_o[2] = ^((data_i ^ 22'h2a0000) & 22'h048ed8);
		syndrome_o[3] = ^((data_i ^ 22'h2a0000) & 22'h087714);
		syndrome_o[4] = ^((data_i ^ 22'h2a0000) & 22'h10aca5);
		syndrome_o[5] = ^((data_i ^ 22'h2a0000) & 22'h2011f3);
		data_o[0] = (syndrome_o == 6'h32) ^ data_i[0];
		data_o[1] = (syndrome_o == 6'h23) ^ data_i[1];
		data_o[2] = (syndrome_o == 6'h19) ^ data_i[2];
		data_o[3] = (syndrome_o == 6'h07) ^ data_i[3];
		data_o[4] = (syndrome_o == 6'h2c) ^ data_i[4];
		data_o[5] = (syndrome_o == 6'h31) ^ data_i[5];
		data_o[6] = (syndrome_o == 6'h25) ^ data_i[6];
		data_o[7] = (syndrome_o == 6'h34) ^ data_i[7];
		data_o[8] = (syndrome_o == 6'h29) ^ data_i[8];
		data_o[9] = (syndrome_o == 6'h0e) ^ data_i[9];
		data_o[10] = (syndrome_o == 6'h1c) ^ data_i[10];
		data_o[11] = (syndrome_o == 6'h15) ^ data_i[11];
		data_o[12] = (syndrome_o == 6'h2a) ^ data_i[12];
		data_o[13] = (syndrome_o == 6'h1a) ^ data_i[13];
		data_o[14] = (syndrome_o == 6'h0b) ^ data_i[14];
		data_o[15] = (syndrome_o == 6'h16) ^ data_i[15];
		err_o[0] = ^syndrome_o;
		err_o[1] = ~err_o[0] & |syndrome_o;
	end
	initial _sv2v_0 = 0;
endmodule
