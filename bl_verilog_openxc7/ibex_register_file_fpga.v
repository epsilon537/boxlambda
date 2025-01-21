module ibex_register_file_fpga (
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
	localparam signed [31:0] ADDR_WIDTH = (RV32E ? 4 : 5);
	localparam signed [31:0] NUM_WORDS = 2 ** ADDR_WIDTH;
	reg [(NUM_WORDS * DataWidth) - 1:0] mem;
	wire we;
	wire [DataWidth - 1:0] mem_o_a;
	wire [DataWidth - 1:0] mem_o_b;
	wire oh_raddr_a_err;
	wire oh_raddr_b_err;
	wire oh_we_err;
	assign err_o = (oh_raddr_a_err || oh_raddr_b_err) || oh_we_err;
	generate
		if (RdataMuxCheck) begin : gen_rdata_mux_check
			wire [NUM_WORDS - 1:0] raddr_onehot_a;
			wire [NUM_WORDS - 1:0] raddr_onehot_b;
			wire [NUM_WORDS - 1:0] raddr_onehot_a_buf;
			wire [NUM_WORDS - 1:0] raddr_onehot_b_buf;
			prim_onehot_enc #(.OneHotWidth(NUM_WORDS)) u_prim_onehot_enc_raddr_a(
				.in_i(raddr_a_i),
				.en_i(1'b1),
				.out_o(raddr_onehot_a)
			);
			prim_onehot_enc #(.OneHotWidth(NUM_WORDS)) u_prim_onehot_enc_raddr_b(
				.in_i(raddr_b_i),
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
				.addr_i(raddr_a_i),
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
				.addr_i(raddr_b_i),
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
				.out_o(mem_o_a)
			);
			prim_onehot_mux #(
				.Width(DataWidth),
				.Inputs(NUM_WORDS)
			) u_rdata_b_mux(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.in_i(mem),
				.sel_i(raddr_onehot_b),
				.out_o(mem_o_b)
			);
			assign rdata_a_o = (raddr_a_i == {5 {1'sb0}} ? WordZeroVal : mem_o_a);
			assign rdata_b_o = (raddr_b_i == {5 {1'sb0}} ? WordZeroVal : mem_o_b);
		end
		else begin : gen_no_rdata_mux_check
			assign rdata_a_o = (raddr_a_i == {5 {1'sb0}} ? WordZeroVal : mem[((NUM_WORDS - 1) - raddr_a_i) * DataWidth+:DataWidth]);
			assign rdata_b_o = (raddr_b_i == {5 {1'sb0}} ? WordZeroVal : mem[((NUM_WORDS - 1) - raddr_b_i) * DataWidth+:DataWidth]);
			assign oh_raddr_a_err = 1'b0;
			assign oh_raddr_b_err = 1'b0;
		end
	endgenerate
	assign we = (waddr_a_i == {5 {1'sb0}} ? 1'b0 : we_a_i);
	generate
		if (WrenCheck) begin : gen_wren_check
			assign oh_we_err = we && !we_a_i;
		end
		else begin : gen_no_wren_check
			assign oh_we_err = 1'b0;
		end
	endgenerate
	always @(posedge clk_i) begin : sync_write
		if (we == 1'b1)
			mem[((NUM_WORDS - 1) - waddr_a_i) * DataWidth+:DataWidth] <= wdata_a_i;
	end
	initial begin : sv2v_autoblock_1
		reg signed [31:0] k;
		for (k = 0; k < NUM_WORDS; k = k + 1)
			mem[((NUM_WORDS - 1) - k) * DataWidth+:DataWidth] = WordZeroVal;
	end
	wire unused_rst_ni;
	assign unused_rst_ni = rst_ni;
	wire unused_dummy_instr;
	assign unused_dummy_instr = dummy_instr_id_i ^ dummy_instr_wb_i;
	wire unused_test_en;
	assign unused_test_en = test_en_i;
endmodule
