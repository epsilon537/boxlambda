module prim_secded_inv_39_32_enc (
	data_i,
	data_o
);
	reg _sv2v_0;
	input [31:0] data_i;
	output reg [38:0] data_o;
	function automatic [38:0] sv2v_cast_39;
		input reg [38:0] inp;
		sv2v_cast_39 = inp;
	endfunction
	always @(*) begin : p_encode
		if (_sv2v_0)
			;
		data_o = sv2v_cast_39(data_i);
		data_o[32] = ^(data_o & 39'h002606bd25);
		data_o[33] = ^(data_o & 39'h00deba8050);
		data_o[34] = ^(data_o & 39'h00413d89aa);
		data_o[35] = ^(data_o & 39'h0031234ed1);
		data_o[36] = ^(data_o & 39'h00c2c1323b);
		data_o[37] = ^(data_o & 39'h002dcc624c);
		data_o[38] = ^(data_o & 39'h0098505586);
		data_o = data_o ^ 39'h2a00000000;
	end
	initial _sv2v_0 = 0;
endmodule
