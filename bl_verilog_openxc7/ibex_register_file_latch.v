module ibex_register_file_latch (
	clk_i,
	rst_ni,
	test_en_i,
	dummy_instr_id_i,
	dummy_instr_wb_i,
	raddr_a_i,
	rdata_a_o,
	raddr_b_i,
	rdata_b_o,
	waddr_a_i,
	wdata_a_i,
	we_a_i,
	err_o
);
	reg _sv2v_0;
	parameter [0:0] RV32E = 0;
	parameter [31:0] DataWidth = 32;
	parameter [0:0] DummyInstructions = 0;
	parameter [0:0] WrenCheck = 0;
	parameter [0:0] RdataMuxCheck = 0;
	parameter [DataWidth - 1:0] WordZeroVal = 1'sb0;
	input wire clk_i;
	input wire rst_ni;
	input wire test_en_i;
	input wire dummy_instr_id_i;
	input wire dummy_instr_wb_i;
	input wire [4:0] raddr_a_i;
	output wire [DataWidth - 1:0] rdata_a_o;
	input wire [4:0] raddr_b_i;
	output wire [DataWidth - 1:0] rdata_b_o;
	input wire [4:0] waddr_a_i;
	input wire [DataWidth - 1:0] wdata_a_i;
	input wire we_a_i;
	output wire err_o;
	localparam [31:0] ADDR_WIDTH = (RV32E ? 4 : 5);
	localparam [31:0] NUM_WORDS = 2 ** ADDR_WIDTH;
	reg [(NUM_WORDS * DataWidth) - 1:0] mem;
	reg [NUM_WORDS - 1:0] waddr_onehot_a;
	wire oh_raddr_a_err;
	wire oh_raddr_b_err;
	wire oh_we_err;
	wire [NUM_WORDS - 1:1] mem_clocks;
	reg [DataWidth - 1:0] wdata_a_q;
	wire [ADDR_WIDTH - 1:0] raddr_a_int;
	wire [ADDR_WIDTH - 1:0] raddr_b_int;
	wire [ADDR_WIDTH - 1:0] waddr_a_int;
	assign raddr_a_int = raddr_a_i[ADDR_WIDTH - 1:0];
	assign raddr_b_int = raddr_b_i[ADDR_WIDTH - 1:0];
	assign waddr_a_int = waddr_a_i[ADDR_WIDTH - 1:0];
	wire clk_int;
	assign err_o = (oh_raddr_a_err || oh_raddr_b_err) || oh_we_err;
	generate
		if (RdataMuxCheck) begin : gen_rdata_mux_check
			wire [NUM_WORDS - 1:0] raddr_onehot_a;
			wire [NUM_WORDS - 1:0] raddr_onehot_b;
			wire [NUM_WORDS - 1:0] raddr_onehot_a_buf;
			wire [NUM_WORDS - 1:0] raddr_onehot_b_buf;
			prim_onehot_enc #(.OneHotWidth(NUM_WORDS)) u_prim_onehot_enc_raddr_a(
				.in_i(raddr_a_int),
				.en_i(1'b1),
				.out_o(raddr_onehot_a)
			);
			prim_onehot_enc #(.OneHotWidth(NUM_WORDS)) u_prim_onehot_enc_raddr_b(
				.in_i(raddr_b_int),
				.en_i(1'b1),
				.out_o(raddr_onehot_b)
			);
			prim_buf #(.Width(NUM_WORDS)) u_prim_buf_raddr_a(
				.in_i(raddr_onehot_a),
				.out_o(raddr_onehot_a_buf)
			);
			prim_buf #(.Width(NUM_WORDS)) u_prim_buf_raddr_b(
				.in_i(raddr_onehot_b),
				.out_o(raddr_onehot_b_buf)
			);
			prim_onehot_check #(
				.AddrWidth(ADDR_WIDTH),
				.OneHotWidth(NUM_WORDS),
				.AddrCheck(1),
				.EnableCheck(1)
			) u_prim_onehot_check_raddr_a(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.oh_i(raddr_onehot_a_buf),
				.addr_i(raddr_a_int),
				.en_i(1'b1),
				.err_o(oh_raddr_a_err)
			);
			prim_onehot_check #(
				.AddrWidth(ADDR_WIDTH),
				.OneHotWidth(NUM_WORDS),
				.AddrCheck(1),
				.EnableCheck(1)
			) u_prim_onehot_check_raddr_b(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.oh_i(raddr_onehot_b_buf),
				.addr_i(raddr_b_int),
				.en_i(1'b1),
				.err_o(oh_raddr_b_err)
			);
			prim_onehot_mux #(
				.Width(DataWidth),
				.Inputs(NUM_WORDS)
			) u_rdata_a_mux(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.in_i(mem),
				.sel_i(raddr_onehot_a),
				.out_o(rdata_a_o)
			);
			prim_onehot_mux #(
				.Width(DataWidth),
				.Inputs(NUM_WORDS)
			) u_rdata_b_mux(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.in_i(mem),
				.sel_i(raddr_onehot_b),
				.out_o(rdata_b_o)
			);
		end
		else begin : gen_no_rdata_mux_check
			assign rdata_a_o = mem[((NUM_WORDS - 1) - raddr_a_int) * DataWidth+:DataWidth];
			assign rdata_b_o = mem[((NUM_WORDS - 1) - raddr_b_int) * DataWidth+:DataWidth];
			assign oh_raddr_a_err = 1'b0;
			assign oh_raddr_b_err = 1'b0;
		end
	endgenerate
	prim_clock_gating cg_we_global(
		.clk_i(clk_i),
		.en_i(we_a_i),
		.test_en_i(test_en_i),
		.clk_o(clk_int)
	);
	always @(posedge clk_int or negedge rst_ni) begin : sample_wdata
		if (!rst_ni)
			wdata_a_q <= WordZeroVal;
		else if (we_a_i)
			wdata_a_q <= wdata_a_i;
	end
	function automatic signed [4:0] sv2v_cast_5_signed;
		input reg signed [4:0] inp;
		sv2v_cast_5_signed = inp;
	endfunction
	always @(*) begin : wad
		if (_sv2v_0)
			;
		begin : sv2v_autoblock_1
			reg signed [31:0] i;
			for (i = 0; i < NUM_WORDS; i = i + 1)
				begin : wad_word_iter
					if (we_a_i && (waddr_a_int == sv2v_cast_5_signed(i)))
						waddr_onehot_a[i] = 1'b1;
					else
						waddr_onehot_a[i] = 1'b0;
				end
		end
	end
	generate
		if (WrenCheck) begin : gen_wren_check
			wire [NUM_WORDS - 1:0] waddr_onehot_a_buf;
			prim_buf #(.Width(NUM_WORDS)) u_prim_buf(
				.in_i(waddr_onehot_a),
				.out_o(waddr_onehot_a_buf)
			);
			prim_onehot_check #(
				.AddrWidth(ADDR_WIDTH),
				.AddrCheck(1),
				.EnableCheck(1)
			) u_prim_onehot_check(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.oh_i(waddr_onehot_a_buf),
				.addr_i(waddr_a_i),
				.en_i(we_a_i),
				.err_o(oh_we_err)
			);
		end
		else begin : gen_no_wren_check
			wire unused_strobe;
			assign unused_strobe = waddr_onehot_a[0];
			assign oh_we_err = 1'b0;
		end
	endgenerate
	genvar _gv_x_1;
	generate
		for (_gv_x_1 = 1; _gv_x_1 < NUM_WORDS; _gv_x_1 = _gv_x_1 + 1) begin : gen_cg_word_iter
			localparam x = _gv_x_1;
			prim_clock_gating cg_i(
				.clk_i(clk_int),
				.en_i(waddr_onehot_a[x]),
				.test_en_i(test_en_i),
				.clk_o(mem_clocks[x])
			);
		end
	endgenerate
	genvar _gv_i_36;
	generate
		for (_gv_i_36 = 1; _gv_i_36 < NUM_WORDS; _gv_i_36 = _gv_i_36 + 1) begin : g_rf_latches
			localparam i = _gv_i_36;
			always @(*) begin
				if (_sv2v_0)
					;
				if (mem_clocks[i])
					mem[((NUM_WORDS - 1) - i) * DataWidth+:DataWidth] = wdata_a_q;
			end
		end
		if (DummyInstructions) begin : g_dummy_r0
			wire we_r0_dummy;
			wire r0_clock;
			reg [DataWidth - 1:0] mem_r0;
			assign we_r0_dummy = we_a_i & dummy_instr_wb_i;
			prim_clock_gating cg_i(
				.clk_i(clk_int),
				.en_i(we_r0_dummy),
				.test_en_i(test_en_i),
				.clk_o(r0_clock)
			);
			always @(*) begin : latch_wdata
				if (_sv2v_0)
					;
				if (r0_clock)
					mem_r0 = wdata_a_q;
			end
			wire [DataWidth * 1:1] sv2v_tmp_F614B;
			assign sv2v_tmp_F614B = (dummy_instr_id_i ? mem_r0 : WordZeroVal);
			always @(*) mem[(NUM_WORDS - 1) * DataWidth+:DataWidth] = sv2v_tmp_F614B;
		end
		else begin : g_normal_r0
			wire unused_dummy_instr;
			assign unused_dummy_instr = dummy_instr_id_i ^ dummy_instr_wb_i;
			wire [DataWidth * 1:1] sv2v_tmp_1F6E0;
			assign sv2v_tmp_1F6E0 = WordZeroVal;
			always @(*) mem[(NUM_WORDS - 1) * DataWidth+:DataWidth] = sv2v_tmp_1F6E0;
		end
	endgenerate
	initial _sv2v_0 = 0;
endmodule
