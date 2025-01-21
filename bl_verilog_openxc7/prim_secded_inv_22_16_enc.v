module prim_secded_inv_22_16_enc (
	data_i,
	data_o
);
	reg _sv2v_0;
	input [15:0] data_i;
	output reg [21:0] data_o;
	function automatic [21:0] sv2v_cast_22;
		input reg [21:0] inp;
		sv2v_cast_22 = inp;
	endfunction
	always @(*) begin : p_encode
		if (_sv2v_0)
			;
		data_o = sv2v_cast_22(data_i);
		data_o[16] = ^(data_o & 22'h00496e);
		data_o[17] = ^(data_o & 22'h00f20b);
		data_o[18] = ^(data_o & 22'h008ed8);
		data_o[19] = ^(data_o & 22'h007714);
		data_o[20] = ^(data_o & 22'h00aca5);
		data_o[21] = ^(data_o & 22'h0011f3);
		data_o = data_o ^ 22'h2a0000;
	end
	initial _sv2v_0 = 0;
endmodule
