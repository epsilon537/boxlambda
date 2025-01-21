module prim_secded_inv_28_22_enc (
	data_i,
	data_o
);
	reg _sv2v_0;
	input [21:0] data_i;
	output reg [27:0] data_o;
	function automatic [27:0] sv2v_cast_28;
		input reg [27:0] inp;
		sv2v_cast_28 = inp;
	endfunction
	always @(*) begin : p_encode
		if (_sv2v_0)
			;
		data_o = sv2v_cast_28(data_i);
		data_o[22] = ^(data_o & 28'h03003ff);
		data_o[23] = ^(data_o & 28'h010fc0f);
		data_o[24] = ^(data_o & 28'h0271c71);
		data_o[25] = ^(data_o & 28'h03b6592);
		data_o[26] = ^(data_o & 28'h03daaa4);
		data_o[27] = ^(data_o & 28'h03ed348);
		data_o = data_o ^ 28'ha800000;
	end
	initial _sv2v_0 = 0;
endmodule
