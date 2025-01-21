module prim_secded_inv_28_22_dec (
	data_i,
	data_o,
	syndrome_o,
	err_o
);
	reg _sv2v_0;
	input [27:0] data_i;
	output reg [21:0] data_o;
	output reg [5:0] syndrome_o;
	output reg [1:0] err_o;
	always @(*) begin : p_encode
		if (_sv2v_0)
			;
		syndrome_o[0] = ^((data_i ^ 28'ha800000) & 28'h07003ff);
		syndrome_o[1] = ^((data_i ^ 28'ha800000) & 28'h090fc0f);
		syndrome_o[2] = ^((data_i ^ 28'ha800000) & 28'h1271c71);
		syndrome_o[3] = ^((data_i ^ 28'ha800000) & 28'h23b6592);
		syndrome_o[4] = ^((data_i ^ 28'ha800000) & 28'h43daaa4);
		syndrome_o[5] = ^((data_i ^ 28'ha800000) & 28'h83ed348);
		data_o[0] = (syndrome_o == 6'h07) ^ data_i[0];
		data_o[1] = (syndrome_o == 6'h0b) ^ data_i[1];
		data_o[2] = (syndrome_o == 6'h13) ^ data_i[2];
		data_o[3] = (syndrome_o == 6'h23) ^ data_i[3];
		data_o[4] = (syndrome_o == 6'h0d) ^ data_i[4];
		data_o[5] = (syndrome_o == 6'h15) ^ data_i[5];
		data_o[6] = (syndrome_o == 6'h25) ^ data_i[6];
		data_o[7] = (syndrome_o == 6'h19) ^ data_i[7];
		data_o[8] = (syndrome_o == 6'h29) ^ data_i[8];
		data_o[9] = (syndrome_o == 6'h31) ^ data_i[9];
		data_o[10] = (syndrome_o == 6'h0e) ^ data_i[10];
		data_o[11] = (syndrome_o == 6'h16) ^ data_i[11];
		data_o[12] = (syndrome_o == 6'h26) ^ data_i[12];
		data_o[13] = (syndrome_o == 6'h1a) ^ data_i[13];
		data_o[14] = (syndrome_o == 6'h2a) ^ data_i[14];
		data_o[15] = (syndrome_o == 6'h32) ^ data_i[15];
		data_o[16] = (syndrome_o == 6'h1c) ^ data_i[16];
		data_o[17] = (syndrome_o == 6'h2c) ^ data_i[17];
		data_o[18] = (syndrome_o == 6'h34) ^ data_i[18];
		data_o[19] = (syndrome_o == 6'h38) ^ data_i[19];
		data_o[20] = (syndrome_o == 6'h3b) ^ data_i[20];
		data_o[21] = (syndrome_o == 6'h3d) ^ data_i[21];
		err_o[0] = ^syndrome_o;
		err_o[1] = ~err_o[0] & |syndrome_o;
	end
	initial _sv2v_0 = 0;
endmodule
