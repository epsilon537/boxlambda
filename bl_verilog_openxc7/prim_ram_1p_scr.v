module prim_ram_1p_scr (
	clk_i,
	rst_ni,
	key_valid_i,
	key_i,
	nonce_i,
	req_i,
	gnt_o,
	write_i,
	addr_i,
	wdata_i,
	wmask_i,
	intg_error_i,
	rdata_o,
	rvalid_o,
	rerror_o,
	raddr_o,
	cfg_i,
	wr_collision_o,
	write_pending_o,
	alert_o
);
	reg _sv2v_0;
	parameter signed [31:0] Depth = 16384;
	parameter signed [31:0] Width = 32;
	parameter signed [31:0] DataBitsPerMask = 8;
	parameter [0:0] EnableParity = 1;
	parameter signed [31:0] NumPrinceRoundsHalf = 3;
	parameter signed [31:0] NumDiffRounds = 0;
	parameter signed [31:0] DiffWidth = DataBitsPerMask;
	parameter signed [31:0] NumAddrScrRounds = 2;
	parameter [0:0] ReplicateKeyStream = 1'b0;
	function automatic integer prim_util_pkg_vbits;
		input integer value;
		prim_util_pkg_vbits = (value == 1 ? 1 : $clog2(value));
	endfunction
	localparam signed [31:0] AddrWidth = prim_util_pkg_vbits(Depth);
	localparam signed [31:0] NumParScr = (ReplicateKeyStream ? 1 : (Width + 63) / 64);
	localparam signed [31:0] NumParKeystr = (ReplicateKeyStream ? (Width + 63) / 64 : 1);
	localparam signed [31:0] DataKeyWidth = 128;
	localparam signed [31:0] NonceWidth = 64 * NumParScr;
	input clk_i;
	input rst_ni;
	input key_valid_i;
	input [127:0] key_i;
	input [NonceWidth - 1:0] nonce_i;
	input req_i;
	output wire gnt_o;
	input write_i;
	input [AddrWidth - 1:0] addr_i;
	input [Width - 1:0] wdata_i;
	input [Width - 1:0] wmask_i;
	input intg_error_i;
	output reg [Width - 1:0] rdata_o;
	output reg rvalid_o;
	output wire [1:0] rerror_o;
	output wire [31:0] raddr_o;
	input wire [9:0] cfg_i;
	output wire wr_collision_o;
	output wire write_pending_o;
	output wire alert_o;
	localparam signed [31:0] prim_mubi_pkg_MuBi4Width = 4;
	wire [3:0] read_en;
	wire [3:0] read_en_buf;
	wire read_en_b;
	wire [3:0] write_en_d;
	wire [3:0] write_en_buf_d;
	wire [3:0] write_en_q;
	wire write_en_b;
	wire [3:0] read_en_b_buf;
	wire [3:0] write_en_buf_b_d;
	assign gnt_o = req_i & key_valid_i;
	function automatic [3:0] sv2v_cast_EECFA;
		input reg [3:0] inp;
		sv2v_cast_EECFA = inp;
	endfunction
	function automatic [3:0] prim_mubi_pkg_mubi4_bool_to_mubi;
		input reg val;
		prim_mubi_pkg_mubi4_bool_to_mubi = (val ? sv2v_cast_EECFA(4'h6) : sv2v_cast_EECFA(4'h9));
	endfunction
	assign read_en = prim_mubi_pkg_mubi4_bool_to_mubi(gnt_o & ~write_i);
	assign write_en_d = prim_mubi_pkg_mubi4_bool_to_mubi(gnt_o & write_i);
	prim_buf #(.Width(prim_mubi_pkg_MuBi4Width)) u_read_en_buf(
		.in_i(read_en),
		.out_o(read_en_b_buf)
	);
	assign read_en_buf = sv2v_cast_EECFA(read_en_b_buf);
	prim_buf #(.Width(prim_mubi_pkg_MuBi4Width)) u_write_en_d_buf(
		.in_i(write_en_d),
		.out_o(write_en_buf_b_d)
	);
	assign write_en_buf_d = sv2v_cast_EECFA(write_en_buf_b_d);
	wire [3:0] write_pending_q;
	wire [3:0] addr_collision_d;
	wire [3:0] addr_collision_q;
	wire [AddrWidth - 1:0] addr_scr;
	reg [AddrWidth - 1:0] waddr_scr_q;
	wire [3:0] addr_match;
	wire [3:0] addr_match_buf;
	assign addr_match = (addr_scr == waddr_scr_q ? sv2v_cast_EECFA(4'h6) : sv2v_cast_EECFA(4'h9));
	prim_buf #(.Width(prim_mubi_pkg_MuBi4Width)) u_addr_match_buf(
		.in_i(addr_match),
		.out_o(addr_match_buf)
	);
	function automatic [3:0] prim_mubi_pkg_mubi4_and;
		input reg [3:0] a;
		input reg [3:0] b;
		input reg [3:0] act;
		reg [3:0] a_in;
		reg [3:0] b_in;
		reg [3:0] act_in;
		reg [3:0] out;
		begin
			a_in = a;
			b_in = b;
			act_in = act;
			begin : sv2v_autoblock_1
				reg signed [31:0] k;
				for (k = 0; k < prim_mubi_pkg_MuBi4Width; k = k + 1)
					if (act_in[k])
						out[k] = a_in[k] && b_in[k];
					else
						out[k] = a_in[k] || b_in[k];
			end
			prim_mubi_pkg_mubi4_and = sv2v_cast_EECFA(out);
		end
	endfunction
	function automatic [3:0] prim_mubi_pkg_mubi4_and_hi;
		input reg [3:0] a;
		input reg [3:0] b;
		prim_mubi_pkg_mubi4_and_hi = prim_mubi_pkg_mubi4_and(a, b, sv2v_cast_EECFA(4'h6));
	endfunction
	function automatic [3:0] prim_mubi_pkg_mubi4_or;
		input reg [3:0] a;
		input reg [3:0] b;
		input reg [3:0] act;
		reg [3:0] a_in;
		reg [3:0] b_in;
		reg [3:0] act_in;
		reg [3:0] out;
		begin
			a_in = a;
			b_in = b;
			act_in = act;
			begin : sv2v_autoblock_2
				reg signed [31:0] k;
				for (k = 0; k < prim_mubi_pkg_MuBi4Width; k = k + 1)
					if (act_in[k])
						out[k] = a_in[k] || b_in[k];
					else
						out[k] = a_in[k] && b_in[k];
			end
			prim_mubi_pkg_mubi4_or = sv2v_cast_EECFA(out);
		end
	endfunction
	function automatic [3:0] prim_mubi_pkg_mubi4_or_hi;
		input reg [3:0] a;
		input reg [3:0] b;
		prim_mubi_pkg_mubi4_or_hi = prim_mubi_pkg_mubi4_or(a, b, sv2v_cast_EECFA(4'h6));
	endfunction
	assign addr_collision_d = prim_mubi_pkg_mubi4_and_hi(prim_mubi_pkg_mubi4_and_hi(prim_mubi_pkg_mubi4_or_hi(write_en_q, write_pending_q), read_en_buf), sv2v_cast_EECFA(addr_match_buf));
	wire intg_error_buf;
	reg intg_error_w_q;
	prim_buf u_intg_error(
		.in_i(intg_error_i),
		.out_o(intg_error_buf)
	);
	wire macro_req;
	function automatic prim_mubi_pkg_mubi4_test_true_loose;
		input reg [3:0] val;
		prim_mubi_pkg_mubi4_test_true_loose = sv2v_cast_EECFA(4'h9) != val;
	endfunction
	assign macro_req = (~intg_error_w_q & ~intg_error_buf) & prim_mubi_pkg_mubi4_test_true_loose(prim_mubi_pkg_mubi4_or_hi(prim_mubi_pkg_mubi4_or_hi(read_en_buf, write_en_q), write_pending_q));
	wire macro_write;
	assign macro_write = (prim_mubi_pkg_mubi4_test_true_loose(prim_mubi_pkg_mubi4_or_hi(write_en_q, write_pending_q)) & ~prim_mubi_pkg_mubi4_test_true_loose(read_en_buf)) & ~intg_error_w_q;
	wire rw_collision;
	assign rw_collision = prim_mubi_pkg_mubi4_test_true_loose(prim_mubi_pkg_mubi4_and_hi(write_en_q, read_en_buf));
	assign write_pending_o = macro_write | prim_mubi_pkg_mubi4_test_true_loose(write_en_buf_d);
	assign wr_collision_o = prim_mubi_pkg_mubi4_test_true_loose(addr_collision_q);
	wire [AddrWidth - 1:0] addr_mux;
	assign addr_mux = (prim_mubi_pkg_mubi4_test_true_loose(read_en_buf) ? addr_scr : waddr_scr_q);
	generate
		if (NumAddrScrRounds > 0) begin : gen_addr_scr
			wire [AddrWidth - 1:0] addr_scr_nonce;
			assign addr_scr_nonce = nonce_i[NonceWidth - AddrWidth+:AddrWidth];
			prim_subst_perm #(
				.DataWidth(AddrWidth),
				.NumRounds(NumAddrScrRounds),
				.Decrypt(0)
			) u_prim_subst_perm(
				.data_i(addr_i),
				.key_i(addr_scr_nonce),
				.data_o(addr_scr)
			);
		end
		else begin : gen_no_addr_scr
			assign addr_scr = addr_i;
		end
	endgenerate
	reg [AddrWidth - 1:0] raddr_q;
	function automatic [31:0] sv2v_cast_32;
		input reg [31:0] inp;
		sv2v_cast_32 = inp;
	endfunction
	assign raddr_o = sv2v_cast_32(raddr_q);
	localparam signed [31:0] DataNonceWidth = 64 - AddrWidth;
	wire [(NumParScr * 64) - 1:0] keystream;
	wire [(NumParScr * DataNonceWidth) - 1:0] data_scr_nonce;
	genvar _gv_k_34;
	generate
		for (_gv_k_34 = 0; _gv_k_34 < NumParScr; _gv_k_34 = _gv_k_34 + 1) begin : gen_par_scr
			localparam k = _gv_k_34;
			assign data_scr_nonce[k * DataNonceWidth+:DataNonceWidth] = nonce_i[k * DataNonceWidth+:DataNonceWidth];
			prim_prince #(
				.DataWidth(64),
				.KeyWidth(128),
				.NumRoundsHalf(NumPrinceRoundsHalf),
				.UseOldKeySched(1'b0),
				.HalfwayDataReg(1'b1),
				.HalfwayKeyReg(1'b0)
			) u_prim_prince(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.valid_i(gnt_o),
				.data_i({data_scr_nonce[k * DataNonceWidth+:DataNonceWidth], addr_i}),
				.key_i(key_i),
				.dec_i(1'b0),
				.data_o(keystream[k * 64+:64]),
				.valid_o()
			);
			if ((k == (NumParKeystr - 1)) && ((Width % 64) > 0)) begin : gen_unread_last
				localparam signed [31:0] UnusedWidth = 64 - (Width % 64);
				wire [UnusedWidth - 1:0] unused_keystream;
				assign unused_keystream = keystream[((k + 1) * 64) - 1-:UnusedWidth];
			end
		end
	endgenerate
	wire [Width - 1:0] keystream_repl;
	function automatic [Width - 1:0] sv2v_cast_62596;
		input reg [Width - 1:0] inp;
		sv2v_cast_62596 = inp;
	endfunction
	assign keystream_repl = sv2v_cast_62596({NumParKeystr {keystream}});
	wire [Width - 1:0] rdata_scr;
	wire [Width - 1:0] rdata;
	wire [Width - 1:0] wdata_scr_d;
	reg [Width - 1:0] wdata_scr_q;
	reg [Width - 1:0] wdata_q;
	genvar _gv_k_35;
	function automatic signed [31:0] sv2v_cast_32_signed;
		input reg signed [31:0] inp;
		sv2v_cast_32_signed = inp;
	endfunction
	generate
		for (_gv_k_35 = 0; _gv_k_35 < (((Width + DiffWidth) - 1) / DiffWidth); _gv_k_35 = _gv_k_35 + 1) begin : gen_diffuse_data
			localparam k = _gv_k_35;
			localparam signed [31:0] LocalWidth = ((Width - (k * DiffWidth)) >= DiffWidth ? DiffWidth : Width - (k * DiffWidth));
			wire [LocalWidth - 1:0] wdata_xor;
			assign wdata_xor = wdata_q[k * DiffWidth+:LocalWidth] ^ keystream_repl[k * DiffWidth+:LocalWidth];
			localparam signed [31:0] sv2v_uu_u_prim_subst_perm_enc_DataWidth = LocalWidth;
			localparam [sv2v_cast_32_signed(((Width - (_gv_k_35 * DiffWidth)) >= DiffWidth ? DiffWidth : Width - (_gv_k_35 * DiffWidth))) - 1:0] sv2v_uu_u_prim_subst_perm_enc_ext_key_i_0 = 1'sb0;
			prim_subst_perm #(
				.DataWidth(LocalWidth),
				.NumRounds(NumDiffRounds),
				.Decrypt(0)
			) u_prim_subst_perm_enc(
				.data_i(wdata_xor),
				.key_i(sv2v_uu_u_prim_subst_perm_enc_ext_key_i_0),
				.data_o(wdata_scr_d[k * DiffWidth+:LocalWidth])
			);
			wire [LocalWidth - 1:0] rdata_xor;
			localparam signed [31:0] sv2v_uu_u_prim_subst_perm_dec_DataWidth = LocalWidth;
			localparam [sv2v_cast_32_signed(((Width - (_gv_k_35 * DiffWidth)) >= DiffWidth ? DiffWidth : Width - (_gv_k_35 * DiffWidth))) - 1:0] sv2v_uu_u_prim_subst_perm_dec_ext_key_i_0 = 1'sb0;
			prim_subst_perm #(
				.DataWidth(LocalWidth),
				.NumRounds(NumDiffRounds),
				.Decrypt(1)
			) u_prim_subst_perm_dec(
				.data_i(rdata_scr[k * DiffWidth+:LocalWidth]),
				.key_i(sv2v_uu_u_prim_subst_perm_dec_ext_key_i_0),
				.data_o(rdata_xor)
			);
			assign rdata[k * DiffWidth+:LocalWidth] = rdata_xor ^ keystream_repl[k * DiffWidth+:LocalWidth];
		end
	endgenerate
	wire [3:0] write_scr_pending_d;
	assign write_scr_pending_d = (macro_write ? sv2v_cast_EECFA(4'h9) : (rw_collision ? sv2v_cast_EECFA(4'h6) : write_pending_q));
	wire [Width - 1:0] wdata_scr;
	assign wdata_scr = (prim_mubi_pkg_mubi4_test_true_loose(write_pending_q) ? wdata_scr_q : wdata_scr_d);
	wire [3:0] rvalid_q;
	reg intg_error_r_q;
	reg [Width - 1:0] wmask_q;
	always @(*) begin : p_forward_mux
		if (_sv2v_0)
			;
		rdata_o = 1'sb0;
		rvalid_o = 1'b0;
		if (!intg_error_r_q && prim_mubi_pkg_mubi4_test_true_loose(rvalid_q)) begin
			rvalid_o = 1'b1;
			if (prim_mubi_pkg_mubi4_test_true_loose(addr_collision_q)) begin : sv2v_autoblock_3
				reg signed [31:0] k;
				for (k = 0; k < Width; k = k + 1)
					if (wmask_q[k])
						rdata_o[k] = wdata_q[k];
					else
						rdata_o[k] = rdata[k];
			end
			else
				rdata_o = rdata;
		end
	end
	wire ram_alert;
	function automatic prim_mubi_pkg_mubi4_test_invalid;
		input reg [3:0] val;
		prim_mubi_pkg_mubi4_test_invalid = ~(|{((sv2v_cast_EECFA(4'h6) ^ (val ^ val)) === (val ^ (sv2v_cast_EECFA(4'h6) ^ sv2v_cast_EECFA(4'h6)))) & ((((val ^ val) ^ (sv2v_cast_EECFA(4'h6) ^ sv2v_cast_EECFA(4'h6))) === (sv2v_cast_EECFA(4'h6) ^ sv2v_cast_EECFA(4'h6))) | 1'bx), ((sv2v_cast_EECFA(4'h9) ^ (val ^ val)) === (val ^ (sv2v_cast_EECFA(4'h9) ^ sv2v_cast_EECFA(4'h9)))) & ((((val ^ val) ^ (sv2v_cast_EECFA(4'h9) ^ sv2v_cast_EECFA(4'h9))) === (sv2v_cast_EECFA(4'h9) ^ sv2v_cast_EECFA(4'h9))) | 1'bx)});
	endfunction
	assign alert_o = (((prim_mubi_pkg_mubi4_test_invalid(write_en_q) | prim_mubi_pkg_mubi4_test_invalid(addr_collision_q)) | prim_mubi_pkg_mubi4_test_invalid(write_pending_q)) | prim_mubi_pkg_mubi4_test_invalid(rvalid_q)) | ram_alert;
	function automatic [3:0] sv2v_cast_4;
		input reg [3:0] inp;
		sv2v_cast_4 = inp;
	endfunction
	prim_flop #(
		.Width(prim_mubi_pkg_MuBi4Width),
		.ResetValue(sv2v_cast_4(sv2v_cast_EECFA(4'h9)))
	) u_write_en_flop(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.d_i(sv2v_cast_4(write_en_buf_d)),
		.q_o({write_en_q})
	);
	prim_flop #(
		.Width(prim_mubi_pkg_MuBi4Width),
		.ResetValue(sv2v_cast_4(sv2v_cast_EECFA(4'h9)))
	) u_addr_collision_flop(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.d_i(sv2v_cast_4(addr_collision_d)),
		.q_o({addr_collision_q})
	);
	prim_flop #(
		.Width(prim_mubi_pkg_MuBi4Width),
		.ResetValue(sv2v_cast_4(sv2v_cast_EECFA(4'h9)))
	) u_write_pending_flop(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.d_i(sv2v_cast_4(write_scr_pending_d)),
		.q_o({write_pending_q})
	);
	prim_flop #(
		.Width(prim_mubi_pkg_MuBi4Width),
		.ResetValue(sv2v_cast_4(sv2v_cast_EECFA(4'h9)))
	) u_rvalid_flop(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.d_i(sv2v_cast_4(read_en_buf)),
		.q_o({rvalid_q})
	);
	assign read_en_b = prim_mubi_pkg_mubi4_test_true_loose(read_en_buf);
	assign write_en_b = prim_mubi_pkg_mubi4_test_true_loose(write_en_buf_d);
	always @(posedge clk_i or negedge rst_ni) begin : p_wdata_buf
		if (!rst_ni) begin
			intg_error_r_q <= 1'b0;
			intg_error_w_q <= 1'b0;
			raddr_q <= 1'sb0;
			waddr_scr_q <= 1'sb0;
			wmask_q <= 1'sb0;
			wdata_q <= 1'sb0;
			wdata_scr_q <= 1'sb0;
		end
		else begin
			intg_error_r_q <= intg_error_buf;
			if (read_en_b)
				raddr_q <= addr_i;
			if (write_en_b) begin
				waddr_scr_q <= addr_scr;
				wmask_q <= wmask_i;
				wdata_q <= wdata_i;
				intg_error_w_q <= intg_error_buf;
			end
			if (rw_collision)
				wdata_scr_q <= wdata_scr_d;
		end
	end
	prim_ram_1p_adv #(
		.Depth(Depth),
		.Width(Width),
		.DataBitsPerMask(DataBitsPerMask),
		.EnableECC(1'b0),
		.EnableParity(EnableParity),
		.EnableInputPipeline(1'b0),
		.EnableOutputPipeline(1'b0)
	) u_prim_ram_1p_adv(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.req_i(macro_req),
		.write_i(macro_write),
		.addr_i(addr_mux),
		.wdata_i(wdata_scr),
		.wmask_i(wmask_q),
		.rdata_o(rdata_scr),
		.rvalid_o(),
		.rerror_o(rerror_o),
		.cfg_i(cfg_i),
		.alert_o(ram_alert)
	);
	initial _sv2v_0 = 0;
endmodule
