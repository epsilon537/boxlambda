module prim_ram_1p_adv (
	clk_i,
	rst_ni,
	req_i,
	write_i,
	addr_i,
	wdata_i,
	wmask_i,
	rdata_o,
	rvalid_o,
	rerror_o,
	cfg_i,
	alert_o
);
	reg _sv2v_0;
	parameter signed [31:0] Depth = 512;
	parameter signed [31:0] Width = 32;
	parameter signed [31:0] DataBitsPerMask = 1;
	parameter MemInitFile = "";
	parameter [0:0] EnableECC = 0;
	parameter [0:0] EnableParity = 0;
	parameter [0:0] EnableInputPipeline = 0;
	parameter [0:0] EnableOutputPipeline = 0;
	parameter [0:0] HammingECC = 0;
	function automatic integer prim_util_pkg_vbits;
		input integer value;
		prim_util_pkg_vbits = (value == 1 ? 1 : $clog2(value));
	endfunction
	localparam signed [31:0] Aw = prim_util_pkg_vbits(Depth);
	input clk_i;
	input rst_ni;
	input req_i;
	input write_i;
	input [Aw - 1:0] addr_i;
	input [Width - 1:0] wdata_i;
	input [Width - 1:0] wmask_i;
	output wire [Width - 1:0] rdata_o;
	output wire rvalid_o;
	output wire [1:0] rerror_o;
	input wire [9:0] cfg_i;
	output wire alert_o;
	localparam signed [31:0] ParWidth = (EnableParity ? Width / 8 : (!EnableECC ? 0 : (Width <= 4 ? 4 : (Width <= 11 ? 5 : (Width <= 26 ? 6 : (Width <= 57 ? 7 : (Width <= 120 ? 8 : 8)))))));
	localparam signed [31:0] TotalWidth = Width + ParWidth;
	localparam signed [31:0] LocalDataBitsPerMask = (EnableParity ? 9 : (EnableECC ? TotalWidth : DataBitsPerMask));
	localparam signed [31:0] prim_mubi_pkg_MuBi4Width = 4;
	reg [3:0] req_q;
	wire [3:0] req_d;
	wire [3:0] req_buf_d;
	wire [3:0] req_buf_b_d;
	wire req_q_b;
	reg [3:0] write_q;
	wire [3:0] write_d;
	wire [3:0] write_buf_d;
	wire [3:0] write_buf_b_d;
	wire write_q_b;
	reg [Aw - 1:0] addr_q;
	wire [Aw - 1:0] addr_d;
	reg [TotalWidth - 1:0] wdata_q;
	reg [TotalWidth - 1:0] wdata_d;
	reg [TotalWidth - 1:0] wmask_q;
	reg [TotalWidth - 1:0] wmask_d;
	reg [3:0] rvalid_q;
	wire [3:0] rvalid_d;
	reg [3:0] rvalid_sram_q;
	wire [3:0] rvalid_sram_d;
	reg [Width - 1:0] rdata_q;
	reg [Width - 1:0] rdata_d;
	wire [TotalWidth - 1:0] rdata_sram;
	reg [1:0] rerror_q;
	reg [1:0] rerror_d;
	function automatic [3:0] sv2v_cast_EECFA;
		input reg [3:0] inp;
		sv2v_cast_EECFA = inp;
	endfunction
	function automatic prim_mubi_pkg_mubi4_test_true_loose;
		input reg [3:0] val;
		prim_mubi_pkg_mubi4_test_true_loose = sv2v_cast_EECFA(4'h9) != val;
	endfunction
	assign req_q_b = prim_mubi_pkg_mubi4_test_true_loose(req_q);
	assign write_q_b = prim_mubi_pkg_mubi4_test_true_loose(write_q);
	prim_ram_1p #(
		.MemInitFile(MemInitFile),
		.Width(TotalWidth),
		.Depth(Depth),
		.DataBitsPerMask(LocalDataBitsPerMask)
	) u_mem(
		.clk_i(clk_i),
		.req_i(req_q_b),
		.write_i(write_q_b),
		.addr_i(addr_q),
		.wdata_i(wdata_q),
		.wmask_i(wmask_q),
		.rdata_o(rdata_sram),
		.cfg_i(cfg_i)
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
	assign rvalid_sram_d = prim_mubi_pkg_mubi4_and_hi(req_q, sv2v_cast_EECFA(~write_q));
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			rvalid_sram_q <= sv2v_cast_EECFA(4'h9);
		else
			rvalid_sram_q <= rvalid_sram_d;
	function automatic [3:0] prim_mubi_pkg_mubi4_bool_to_mubi;
		input reg val;
		prim_mubi_pkg_mubi4_bool_to_mubi = (val ? sv2v_cast_EECFA(4'h6) : sv2v_cast_EECFA(4'h9));
	endfunction
	assign req_d = prim_mubi_pkg_mubi4_bool_to_mubi(req_i);
	assign write_d = prim_mubi_pkg_mubi4_bool_to_mubi(write_i);
	assign addr_d = addr_i;
	assign rvalid_o = prim_mubi_pkg_mubi4_test_true_loose(rvalid_q);
	assign rdata_o = rdata_q;
	assign rerror_o = rerror_q;
	prim_buf #(.Width(prim_mubi_pkg_MuBi4Width)) u_req_d_buf(
		.in_i(req_d),
		.out_o(req_buf_b_d)
	);
	assign req_buf_d = sv2v_cast_EECFA(req_buf_b_d);
	prim_buf #(.Width(prim_mubi_pkg_MuBi4Width)) u_write_d_buf(
		.in_i(write_d),
		.out_o(write_buf_b_d)
	);
	assign write_buf_d = sv2v_cast_EECFA(write_buf_b_d);
	generate
		if ((EnableParity == 0) && EnableECC) begin : gen_secded
			wire unused_wmask;
			assign unused_wmask = ^wmask_i;
			wire [TotalWidth:1] sv2v_tmp_93EFC;
			assign sv2v_tmp_93EFC = {TotalWidth {1'b1}};
			always @(*) wmask_d = sv2v_tmp_93EFC;
			if (Width == 16) begin : gen_secded_22_16
				if (HammingECC) begin : gen_hamming
					wire [TotalWidth:1] sv2v_tmp_u_enc_data_o;
					always @(*) wdata_d = sv2v_tmp_u_enc_data_o;
					prim_secded_inv_hamming_22_16_enc u_enc(
						.data_i(wdata_i),
						.data_o(sv2v_tmp_u_enc_data_o)
					);
					wire [Width * 1:1] sv2v_tmp_u_dec_data_o;
					always @(*) rdata_d[0+:Width] = sv2v_tmp_u_dec_data_o;
					wire [2:1] sv2v_tmp_u_dec_err_o;
					always @(*) rerror_d = sv2v_tmp_u_dec_err_o;
					prim_secded_inv_hamming_22_16_dec u_dec(
						.data_i(rdata_sram),
						.data_o(sv2v_tmp_u_dec_data_o),
						.syndrome_o(),
						.err_o(sv2v_tmp_u_dec_err_o)
					);
				end
				else begin : gen_hsiao
					wire [TotalWidth:1] sv2v_tmp_u_enc_data_o;
					always @(*) wdata_d = sv2v_tmp_u_enc_data_o;
					prim_secded_inv_22_16_enc u_enc(
						.data_i(wdata_i),
						.data_o(sv2v_tmp_u_enc_data_o)
					);
					wire [Width * 1:1] sv2v_tmp_u_dec_data_o;
					always @(*) rdata_d[0+:Width] = sv2v_tmp_u_dec_data_o;
					wire [2:1] sv2v_tmp_u_dec_err_o;
					always @(*) rerror_d = sv2v_tmp_u_dec_err_o;
					prim_secded_inv_22_16_dec u_dec(
						.data_i(rdata_sram),
						.data_o(sv2v_tmp_u_dec_data_o),
						.syndrome_o(),
						.err_o(sv2v_tmp_u_dec_err_o)
					);
				end
			end
			else if (Width == 32) begin : gen_secded_39_32
				if (HammingECC) begin : gen_hamming
					wire [TotalWidth:1] sv2v_tmp_u_enc_data_o;
					always @(*) wdata_d = sv2v_tmp_u_enc_data_o;
					prim_secded_inv_hamming_39_32_enc u_enc(
						.data_i(wdata_i),
						.data_o(sv2v_tmp_u_enc_data_o)
					);
					wire [Width * 1:1] sv2v_tmp_u_dec_data_o;
					always @(*) rdata_d[0+:Width] = sv2v_tmp_u_dec_data_o;
					wire [2:1] sv2v_tmp_u_dec_err_o;
					always @(*) rerror_d = sv2v_tmp_u_dec_err_o;
					prim_secded_inv_hamming_39_32_dec u_dec(
						.data_i(rdata_sram),
						.data_o(sv2v_tmp_u_dec_data_o),
						.syndrome_o(),
						.err_o(sv2v_tmp_u_dec_err_o)
					);
				end
				else begin : gen_hsiao
					wire [TotalWidth:1] sv2v_tmp_u_enc_data_o;
					always @(*) wdata_d = sv2v_tmp_u_enc_data_o;
					prim_secded_inv_39_32_enc u_enc(
						.data_i(wdata_i),
						.data_o(sv2v_tmp_u_enc_data_o)
					);
					wire [Width * 1:1] sv2v_tmp_u_dec_data_o;
					always @(*) rdata_d[0+:Width] = sv2v_tmp_u_dec_data_o;
					wire [2:1] sv2v_tmp_u_dec_err_o;
					always @(*) rerror_d = sv2v_tmp_u_dec_err_o;
					prim_secded_inv_39_32_dec u_dec(
						.data_i(rdata_sram),
						.data_o(sv2v_tmp_u_dec_data_o),
						.syndrome_o(),
						.err_o(sv2v_tmp_u_dec_err_o)
					);
				end
			end
		end
		else if (EnableParity) begin : gen_byte_parity
			always @(*) begin : p_parity
				if (_sv2v_0)
					;
				rerror_d = 1'sb0;
				begin : sv2v_autoblock_2
					reg signed [31:0] i;
					for (i = 0; i < (Width / 8); i = i + 1)
						begin
							wmask_d[i * 9+:8] = wmask_i[i * 8+:8];
							wdata_d[i * 9+:8] = wdata_i[i * 8+:8];
							rdata_d[i * 8+:8] = rdata_sram[i * 9+:8];
							wdata_d[(i * 9) + 8] = ~(^wdata_i[i * 8+:8]);
							wmask_d[(i * 9) + 8] = &wmask_i[i * 8+:8];
							rerror_d[1] = rerror_d[1] | ~(^{rdata_sram[i * 9+:8], rdata_sram[(i * 9) + 8]});
						end
				end
			end
		end
		else begin : gen_nosecded_noparity
			wire [TotalWidth:1] sv2v_tmp_37F44;
			assign sv2v_tmp_37F44 = wmask_i;
			always @(*) wmask_d = sv2v_tmp_37F44;
			wire [TotalWidth:1] sv2v_tmp_1BDC4;
			assign sv2v_tmp_1BDC4 = wdata_i;
			always @(*) wdata_d = sv2v_tmp_1BDC4;
			wire [Width:1] sv2v_tmp_97DCE;
			assign sv2v_tmp_97DCE = rdata_sram[0+:Width];
			always @(*) rdata_d = sv2v_tmp_97DCE;
			wire [2:1] sv2v_tmp_D120E;
			assign sv2v_tmp_D120E = 1'sb0;
			always @(*) rerror_d = sv2v_tmp_D120E;
		end
	endgenerate
	assign rvalid_d = rvalid_sram_q;
	function automatic [3:0] sv2v_cast_4;
		input reg [3:0] inp;
		sv2v_cast_4 = inp;
	endfunction
	generate
		if (EnableInputPipeline) begin : gen_regslice_input
			if (EnableECC || EnableParity) begin : gen_prim_flop
				wire [4:1] sv2v_tmp_u_write_flop_q_o;
				always @(*) {write_q} = sv2v_tmp_u_write_flop_q_o;
				prim_flop #(
					.Width(prim_mubi_pkg_MuBi4Width),
					.ResetValue(sv2v_cast_4(sv2v_cast_EECFA(4'h9)))
				) u_write_flop(
					.clk_i(clk_i),
					.rst_ni(rst_ni),
					.d_i(sv2v_cast_4(write_buf_d)),
					.q_o(sv2v_tmp_u_write_flop_q_o)
				);
				wire [4:1] sv2v_tmp_u_req_flop_q_o;
				always @(*) {req_q} = sv2v_tmp_u_req_flop_q_o;
				prim_flop #(
					.Width(prim_mubi_pkg_MuBi4Width),
					.ResetValue(sv2v_cast_4(sv2v_cast_EECFA(4'h9)))
				) u_req_flop(
					.clk_i(clk_i),
					.rst_ni(rst_ni),
					.d_i(sv2v_cast_4(req_buf_d)),
					.q_o(sv2v_tmp_u_req_flop_q_o)
				);
			end
			else begin : gen_no_prim_flop
				always @(posedge clk_i or negedge rst_ni)
					if (!rst_ni) begin
						write_q <= sv2v_cast_EECFA(4'h9);
						req_q <= sv2v_cast_EECFA(4'h9);
					end
					else begin
						write_q <= write_buf_d;
						req_q <= req_buf_d;
					end
			end
			always @(posedge clk_i or negedge rst_ni)
				if (!rst_ni) begin
					addr_q <= 1'sb0;
					wdata_q <= 1'sb0;
					wmask_q <= 1'sb0;
				end
				else begin
					addr_q <= addr_d;
					wdata_q <= wdata_d;
					wmask_q <= wmask_d;
				end
		end
		else begin : gen_dirconnect_input
			wire [4:1] sv2v_tmp_EB9D2;
			assign sv2v_tmp_EB9D2 = req_buf_d;
			always @(*) req_q = sv2v_tmp_EB9D2;
			wire [4:1] sv2v_tmp_793D6;
			assign sv2v_tmp_793D6 = write_buf_d;
			always @(*) write_q = sv2v_tmp_793D6;
			wire [Aw:1] sv2v_tmp_D98D2;
			assign sv2v_tmp_D98D2 = addr_d;
			always @(*) addr_q = sv2v_tmp_D98D2;
			wire [TotalWidth:1] sv2v_tmp_A9AFC;
			assign sv2v_tmp_A9AFC = wdata_d;
			always @(*) wdata_q = sv2v_tmp_A9AFC;
			wire [TotalWidth:1] sv2v_tmp_7467C;
			assign sv2v_tmp_7467C = wmask_d;
			always @(*) wmask_q = sv2v_tmp_7467C;
		end
		if (EnableOutputPipeline) begin : gen_regslice_output
			if (EnableECC || EnableParity) begin : gen_prim_rvalid_flop
				wire [4:1] sv2v_tmp_u_rvalid_flop_q_o;
				always @(*) {rvalid_q} = sv2v_tmp_u_rvalid_flop_q_o;
				prim_flop #(
					.Width(prim_mubi_pkg_MuBi4Width),
					.ResetValue(sv2v_cast_4(sv2v_cast_EECFA(4'h9)))
				) u_rvalid_flop(
					.clk_i(clk_i),
					.rst_ni(rst_ni),
					.d_i(sv2v_cast_4(rvalid_d)),
					.q_o(sv2v_tmp_u_rvalid_flop_q_o)
				);
			end
			else begin : gen_no_prim_rvalid_flop
				always @(posedge clk_i or negedge rst_ni)
					if (!rst_ni)
						rvalid_q <= sv2v_cast_EECFA(4'h9);
					else
						rvalid_q <= rvalid_d;
			end
			always @(posedge clk_i or negedge rst_ni)
				if (!rst_ni) begin
					rdata_q <= 1'sb0;
					rerror_q <= 1'sb0;
				end
				else begin
					rdata_q <= rdata_d;
					rerror_q <= rerror_d & {2 {prim_mubi_pkg_mubi4_test_true_loose(rvalid_d)}};
				end
		end
		else begin : gen_dirconnect_output
			wire [4:1] sv2v_tmp_970DE;
			assign sv2v_tmp_970DE = rvalid_d;
			always @(*) rvalid_q = sv2v_tmp_970DE;
			wire [Width:1] sv2v_tmp_5ED9C;
			assign sv2v_tmp_5ED9C = rdata_d;
			always @(*) rdata_q = sv2v_tmp_5ED9C;
			wire [2:1] sv2v_tmp_DE00A;
			assign sv2v_tmp_DE00A = rerror_d & {2 {prim_mubi_pkg_mubi4_test_true_loose(rvalid_d)}};
			always @(*) rerror_q = sv2v_tmp_DE00A;
		end
	endgenerate
	function automatic prim_mubi_pkg_mubi4_test_invalid;
		input reg [3:0] val;
		prim_mubi_pkg_mubi4_test_invalid = ~(|{((sv2v_cast_EECFA(4'h6) ^ (val ^ val)) === (val ^ (sv2v_cast_EECFA(4'h6) ^ sv2v_cast_EECFA(4'h6)))) & ((((val ^ val) ^ (sv2v_cast_EECFA(4'h6) ^ sv2v_cast_EECFA(4'h6))) === (sv2v_cast_EECFA(4'h6) ^ sv2v_cast_EECFA(4'h6))) | 1'bx), ((sv2v_cast_EECFA(4'h9) ^ (val ^ val)) === (val ^ (sv2v_cast_EECFA(4'h9) ^ sv2v_cast_EECFA(4'h9)))) & ((((val ^ val) ^ (sv2v_cast_EECFA(4'h9) ^ sv2v_cast_EECFA(4'h9))) === (sv2v_cast_EECFA(4'h9) ^ sv2v_cast_EECFA(4'h9))) | 1'bx)});
	endfunction
	assign alert_o = ((prim_mubi_pkg_mubi4_test_invalid(req_q) | prim_mubi_pkg_mubi4_test_invalid(write_q)) | prim_mubi_pkg_mubi4_test_invalid(rvalid_q)) | prim_mubi_pkg_mubi4_test_invalid(rvalid_sram_q);
	initial _sv2v_0 = 0;
endmodule
