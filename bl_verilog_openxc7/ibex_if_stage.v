module ibex_if_stage (
	clk_i,
	rst_ni,
	boot_addr_i,
	req_i,
	instr_req_o,
	instr_addr_o,
	instr_gnt_i,
	instr_rvalid_i,
	instr_rdata_i,
	instr_bus_err_i,
	instr_intg_err_o,
	ic_tag_req_o,
	ic_tag_write_o,
	ic_tag_addr_o,
	ic_tag_wdata_o,
	ic_tag_rdata_i,
	ic_data_req_o,
	ic_data_write_o,
	ic_data_addr_o,
	ic_data_wdata_o,
	ic_data_rdata_i,
	ic_scr_key_valid_i,
	ic_scr_key_req_o,
	instr_valid_id_o,
	instr_new_id_o,
	instr_rdata_id_o,
	instr_rdata_alu_id_o,
	instr_rdata_c_id_o,
	instr_is_compressed_id_o,
	instr_bp_taken_o,
	instr_fetch_err_o,
	instr_fetch_err_plus2_o,
	illegal_c_insn_id_o,
	dummy_instr_id_o,
	pc_if_o,
	pc_id_o,
	pmp_err_if_i,
	pmp_err_if_plus2_i,
	instr_valid_clear_i,
	pc_set_i,
	pc_mux_i,
	nt_branch_mispredict_i,
	nt_branch_addr_i,
	exc_pc_mux_i,
	exc_cause,
	dummy_instr_en_i,
	dummy_instr_mask_i,
	dummy_instr_seed_en_i,
	dummy_instr_seed_i,
	icache_enable_i,
	icache_inval_i,
	icache_ecc_error_o,
	branch_target_ex_i,
	csr_mepc_i,
	csr_depc_i,
	csr_mtvec_i,
	csr_mtvec_init_o,
	id_in_ready_i,
	pc_mismatch_alert_o,
	if_busy_o
);
	reg _sv2v_0;
	parameter [31:0] DmHaltAddr = 32'h1a110800;
	parameter [31:0] DmExceptionAddr = 32'h1a110808;
	parameter [0:0] DummyInstructions = 1'b0;
	parameter [0:0] ICache = 1'b0;
	parameter [0:0] ICacheECC = 1'b0;
	localparam [31:0] ibex_pkg_BUS_SIZE = 32;
	parameter [31:0] BusSizeECC = ibex_pkg_BUS_SIZE;
	localparam [31:0] ibex_pkg_ADDR_W = 32;
	localparam [31:0] ibex_pkg_IC_LINE_SIZE = 64;
	localparam [31:0] ibex_pkg_IC_LINE_BYTES = 8;
	localparam [31:0] ibex_pkg_IC_NUM_WAYS = 2;
	localparam [31:0] ibex_pkg_IC_SIZE_BYTES = 4096;
	localparam [31:0] ibex_pkg_IC_NUM_LINES = (ibex_pkg_IC_SIZE_BYTES / ibex_pkg_IC_NUM_WAYS) / ibex_pkg_IC_LINE_BYTES;
	localparam [31:0] ibex_pkg_IC_INDEX_W = $clog2(ibex_pkg_IC_NUM_LINES);
	localparam [31:0] ibex_pkg_IC_LINE_W = 3;
	localparam [31:0] ibex_pkg_IC_TAG_SIZE = ((ibex_pkg_ADDR_W - ibex_pkg_IC_INDEX_W) - ibex_pkg_IC_LINE_W) + 1;
	parameter [31:0] TagSizeECC = ibex_pkg_IC_TAG_SIZE;
	parameter [31:0] LineSizeECC = ibex_pkg_IC_LINE_SIZE;
	parameter [0:0] PCIncrCheck = 1'b0;
	parameter [0:0] ResetAll = 1'b0;
	localparam signed [31:0] ibex_pkg_LfsrWidth = 32;
	localparam [31:0] ibex_pkg_RndCnstLfsrSeedDefault = 32'hac533bf4;
	parameter [31:0] RndCnstLfsrSeed = ibex_pkg_RndCnstLfsrSeedDefault;
	localparam [159:0] ibex_pkg_RndCnstLfsrPermDefault = 160'h1e35ecba467fd1b12e958152c04fa43878a8daed;
	parameter [159:0] RndCnstLfsrPerm = ibex_pkg_RndCnstLfsrPermDefault;
	parameter [0:0] BranchPredictor = 1'b0;
	parameter [0:0] MemECC = 1'b0;
	parameter [31:0] MemDataWidth = (MemECC ? 39 : 32);
	input wire clk_i;
	input wire rst_ni;
	input wire [31:0] boot_addr_i;
	input wire req_i;
	output wire instr_req_o;
	output wire [31:0] instr_addr_o;
	input wire instr_gnt_i;
	input wire instr_rvalid_i;
	input wire [MemDataWidth - 1:0] instr_rdata_i;
	input wire instr_bus_err_i;
	output wire instr_intg_err_o;
	output wire [1:0] ic_tag_req_o;
	output wire ic_tag_write_o;
	output wire [ibex_pkg_IC_INDEX_W - 1:0] ic_tag_addr_o;
	output wire [TagSizeECC - 1:0] ic_tag_wdata_o;
	input wire [(ibex_pkg_IC_NUM_WAYS * TagSizeECC) - 1:0] ic_tag_rdata_i;
	output wire [1:0] ic_data_req_o;
	output wire ic_data_write_o;
	output wire [ibex_pkg_IC_INDEX_W - 1:0] ic_data_addr_o;
	output wire [LineSizeECC - 1:0] ic_data_wdata_o;
	input wire [(ibex_pkg_IC_NUM_WAYS * LineSizeECC) - 1:0] ic_data_rdata_i;
	input wire ic_scr_key_valid_i;
	output wire ic_scr_key_req_o;
	output wire instr_valid_id_o;
	output wire instr_new_id_o;
	output reg [31:0] instr_rdata_id_o;
	output reg [31:0] instr_rdata_alu_id_o;
	output reg [15:0] instr_rdata_c_id_o;
	output reg instr_is_compressed_id_o;
	output wire instr_bp_taken_o;
	output reg instr_fetch_err_o;
	output reg instr_fetch_err_plus2_o;
	output reg illegal_c_insn_id_o;
	output reg dummy_instr_id_o;
	output wire [31:0] pc_if_o;
	output reg [31:0] pc_id_o;
	input wire pmp_err_if_i;
	input wire pmp_err_if_plus2_i;
	input wire instr_valid_clear_i;
	input wire pc_set_i;
	input wire [2:0] pc_mux_i;
	input wire nt_branch_mispredict_i;
	input wire [31:0] nt_branch_addr_i;
	input wire [1:0] exc_pc_mux_i;
	input wire [6:0] exc_cause;
	input wire dummy_instr_en_i;
	input wire [2:0] dummy_instr_mask_i;
	input wire dummy_instr_seed_en_i;
	input wire [31:0] dummy_instr_seed_i;
	input wire icache_enable_i;
	input wire icache_inval_i;
	output wire icache_ecc_error_o;
	input wire [31:0] branch_target_ex_i;
	input wire [31:0] csr_mepc_i;
	input wire [31:0] csr_depc_i;
	input wire [31:0] csr_mtvec_i;
	output wire csr_mtvec_init_o;
	input wire id_in_ready_i;
	output wire pc_mismatch_alert_o;
	output wire if_busy_o;
	wire instr_valid_id_d;
	reg instr_valid_id_q;
	wire instr_new_id_d;
	reg instr_new_id_q;
	wire instr_err;
	wire instr_intg_err;
	wire prefetch_busy;
	wire branch_req;
	reg [31:0] fetch_addr_n;
	wire unused_fetch_addr_n0;
	wire prefetch_branch;
	wire [31:0] prefetch_addr;
	wire fetch_valid_raw;
	wire fetch_valid;
	wire fetch_ready;
	wire [31:0] fetch_rdata;
	wire [31:0] fetch_addr;
	wire fetch_err;
	wire fetch_err_plus2;
	wire [31:0] instr_decompressed;
	wire illegal_c_insn;
	wire instr_is_compressed;
	wire if_instr_valid;
	wire [31:0] if_instr_rdata;
	wire [31:0] if_instr_addr;
	wire if_instr_bus_err;
	wire if_instr_pmp_err;
	wire if_instr_err;
	wire if_instr_err_plus2;
	reg [31:0] exc_pc;
	wire if_id_pipe_reg_we;
	wire stall_dummy_instr;
	wire [31:0] instr_out;
	wire instr_is_compressed_out;
	wire illegal_c_instr_out;
	wire instr_err_out;
	wire predict_branch_taken;
	wire [31:0] predict_branch_pc;
	reg [4:0] irq_vec;
	wire [2:0] pc_mux_internal;
	wire [7:0] unused_boot_addr;
	wire [7:0] unused_csr_mtvec;
	wire unused_exc_cause;
	assign unused_boot_addr = boot_addr_i[7:0];
	assign unused_csr_mtvec = csr_mtvec_i[7:0];
	assign unused_exc_cause = |{exc_cause[5], exc_cause[6]};
	localparam [6:0] ibex_pkg_ExcCauseIrqNm = 7'h3f;
	always @(*) begin : exc_pc_mux
		if (_sv2v_0)
			;
		irq_vec = exc_cause[4-:5];
		if (exc_cause[6])
			irq_vec = ibex_pkg_ExcCauseIrqNm[4-:5];
		(* full_case, parallel_case *)
		case (exc_pc_mux_i)
			2'd0: exc_pc = {csr_mtvec_i[31:8], 8'h00};
			2'd1: exc_pc = {csr_mtvec_i[31:8], 1'b0, irq_vec, 2'b00};
			2'd2: exc_pc = DmHaltAddr;
			2'd3: exc_pc = DmExceptionAddr;
			default: exc_pc = {csr_mtvec_i[31:8], 8'h00};
		endcase
	end
	assign pc_mux_internal = ((BranchPredictor && predict_branch_taken) && !pc_set_i ? 3'd5 : pc_mux_i);
	always @(*) begin : fetch_addr_mux
		if (_sv2v_0)
			;
		(* full_case, parallel_case *)
		case (pc_mux_internal)
			3'd0: fetch_addr_n = {boot_addr_i[31:8], 8'h80};
			3'd1: fetch_addr_n = branch_target_ex_i;
			3'd2: fetch_addr_n = exc_pc;
			3'd3: fetch_addr_n = csr_mepc_i;
			3'd4: fetch_addr_n = csr_depc_i;
			3'd5: fetch_addr_n = (BranchPredictor ? predict_branch_pc : {boot_addr_i[31:8], 8'h80});
			default: fetch_addr_n = {boot_addr_i[31:8], 8'h80};
		endcase
	end
	assign csr_mtvec_init_o = (pc_mux_i == 3'd0) & pc_set_i;
	generate
		if (MemECC) begin : g_mem_ecc
			wire [1:0] ecc_err;
			wire [MemDataWidth - 1:0] instr_rdata_buf;
			prim_buf #(.Width(MemDataWidth)) u_prim_buf_instr_rdata(
				.in_i(instr_rdata_i),
				.out_o(instr_rdata_buf)
			);
			prim_secded_inv_39_32_dec u_instr_intg_dec(
				.data_i(instr_rdata_buf),
				.data_o(),
				.syndrome_o(),
				.err_o(ecc_err)
			);
			assign instr_intg_err = |ecc_err;
		end
		else begin : g_no_mem_ecc
			assign instr_intg_err = 1'b0;
		end
	endgenerate
	assign instr_err = instr_intg_err | instr_bus_err_i;
	assign instr_intg_err_o = instr_intg_err & instr_rvalid_i;
	assign prefetch_branch = branch_req | nt_branch_mispredict_i;
	assign prefetch_addr = (branch_req ? {fetch_addr_n[31:1], 1'b0} : nt_branch_addr_i);
	assign fetch_valid = fetch_valid_raw & ~nt_branch_mispredict_i;
	generate
		if (ICache) begin : gen_icache
			ibex_icache #(
				.ICacheECC(ICacheECC),
				.ResetAll(ResetAll),
				.BusSizeECC(BusSizeECC),
				.TagSizeECC(TagSizeECC),
				.LineSizeECC(LineSizeECC)
			) icache_i(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.req_i(req_i),
				.branch_i(prefetch_branch),
				.addr_i(prefetch_addr),
				.ready_i(fetch_ready),
				.valid_o(fetch_valid_raw),
				.rdata_o(fetch_rdata),
				.addr_o(fetch_addr),
				.err_o(fetch_err),
				.err_plus2_o(fetch_err_plus2),
				.instr_req_o(instr_req_o),
				.instr_addr_o(instr_addr_o),
				.instr_gnt_i(instr_gnt_i),
				.instr_rvalid_i(instr_rvalid_i),
				.instr_rdata_i(instr_rdata_i[31:0]),
				.instr_err_i(instr_err),
				.ic_tag_req_o(ic_tag_req_o),
				.ic_tag_write_o(ic_tag_write_o),
				.ic_tag_addr_o(ic_tag_addr_o),
				.ic_tag_wdata_o(ic_tag_wdata_o),
				.ic_tag_rdata_i(ic_tag_rdata_i),
				.ic_data_req_o(ic_data_req_o),
				.ic_data_write_o(ic_data_write_o),
				.ic_data_addr_o(ic_data_addr_o),
				.ic_data_wdata_o(ic_data_wdata_o),
				.ic_data_rdata_i(ic_data_rdata_i),
				.ic_scr_key_valid_i(ic_scr_key_valid_i),
				.ic_scr_key_req_o(ic_scr_key_req_o),
				.icache_enable_i(icache_enable_i),
				.icache_inval_i(icache_inval_i),
				.busy_o(prefetch_busy),
				.ecc_error_o(icache_ecc_error_o)
			);
		end
		else begin : gen_prefetch_buffer
			ibex_prefetch_buffer #(.ResetAll(ResetAll)) prefetch_buffer_i(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.req_i(req_i),
				.branch_i(prefetch_branch),
				.addr_i(prefetch_addr),
				.ready_i(fetch_ready),
				.valid_o(fetch_valid_raw),
				.rdata_o(fetch_rdata),
				.addr_o(fetch_addr),
				.err_o(fetch_err),
				.err_plus2_o(fetch_err_plus2),
				.instr_req_o(instr_req_o),
				.instr_addr_o(instr_addr_o),
				.instr_gnt_i(instr_gnt_i),
				.instr_rvalid_i(instr_rvalid_i),
				.instr_rdata_i(instr_rdata_i[31:0]),
				.instr_err_i(instr_err),
				.busy_o(prefetch_busy)
			);
			wire unused_icen;
			wire unused_icinv;
			wire unused_scr_key_valid;
			wire [(ibex_pkg_IC_NUM_WAYS * TagSizeECC) - 1:0] unused_tag_ram_input;
			wire [(ibex_pkg_IC_NUM_WAYS * LineSizeECC) - 1:0] unused_data_ram_input;
			assign unused_icen = icache_enable_i;
			assign unused_icinv = icache_inval_i;
			assign unused_tag_ram_input = ic_tag_rdata_i;
			assign unused_data_ram_input = ic_data_rdata_i;
			assign unused_scr_key_valid = ic_scr_key_valid_i;
			assign ic_tag_req_o = 'b0;
			assign ic_tag_write_o = 'b0;
			assign ic_tag_addr_o = 'b0;
			assign ic_tag_wdata_o = 'b0;
			assign ic_data_req_o = 'b0;
			assign ic_data_write_o = 'b0;
			assign ic_data_addr_o = 'b0;
			assign ic_data_wdata_o = 'b0;
			assign ic_scr_key_req_o = 'b0;
			assign icache_ecc_error_o = 'b0;
		end
	endgenerate
	assign unused_fetch_addr_n0 = fetch_addr_n[0];
	assign branch_req = pc_set_i | predict_branch_taken;
	assign pc_if_o = if_instr_addr;
	assign if_busy_o = prefetch_busy;
	assign if_instr_pmp_err = pmp_err_if_i | ((if_instr_addr[1] & ~instr_is_compressed) & pmp_err_if_plus2_i);
	assign if_instr_err = if_instr_bus_err | if_instr_pmp_err;
	assign if_instr_err_plus2 = (((if_instr_addr[1] & ~instr_is_compressed) & pmp_err_if_plus2_i) | fetch_err_plus2) & ~pmp_err_if_i;
	ibex_compressed_decoder compressed_decoder_i(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.valid_i(fetch_valid & ~fetch_err),
		.instr_i(if_instr_rdata),
		.instr_o(instr_decompressed),
		.is_compressed_o(instr_is_compressed),
		.illegal_instr_o(illegal_c_insn)
	);
	generate
		if (DummyInstructions) begin : gen_dummy_instr
			wire insert_dummy_instr;
			wire [31:0] dummy_instr_data;
			ibex_dummy_instr #(
				.RndCnstLfsrSeed(RndCnstLfsrSeed),
				.RndCnstLfsrPerm(RndCnstLfsrPerm)
			) dummy_instr_i(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.dummy_instr_en_i(dummy_instr_en_i),
				.dummy_instr_mask_i(dummy_instr_mask_i),
				.dummy_instr_seed_en_i(dummy_instr_seed_en_i),
				.dummy_instr_seed_i(dummy_instr_seed_i),
				.fetch_valid_i(fetch_valid),
				.id_in_ready_i(id_in_ready_i),
				.insert_dummy_instr_o(insert_dummy_instr),
				.dummy_instr_data_o(dummy_instr_data)
			);
			assign instr_out = (insert_dummy_instr ? dummy_instr_data : instr_decompressed);
			assign instr_is_compressed_out = (insert_dummy_instr ? 1'b0 : instr_is_compressed);
			assign illegal_c_instr_out = (insert_dummy_instr ? 1'b0 : illegal_c_insn);
			assign instr_err_out = (insert_dummy_instr ? 1'b0 : if_instr_err);
			assign stall_dummy_instr = insert_dummy_instr;
			always @(posedge clk_i or negedge rst_ni)
				if (!rst_ni)
					dummy_instr_id_o <= 1'b0;
				else if (if_id_pipe_reg_we)
					dummy_instr_id_o <= insert_dummy_instr;
		end
		else begin : gen_no_dummy_instr
			wire unused_dummy_en;
			wire [2:0] unused_dummy_mask;
			wire unused_dummy_seed_en;
			wire [31:0] unused_dummy_seed;
			assign unused_dummy_en = dummy_instr_en_i;
			assign unused_dummy_mask = dummy_instr_mask_i;
			assign unused_dummy_seed_en = dummy_instr_seed_en_i;
			assign unused_dummy_seed = dummy_instr_seed_i;
			assign instr_out = instr_decompressed;
			assign instr_is_compressed_out = instr_is_compressed;
			assign illegal_c_instr_out = illegal_c_insn;
			assign instr_err_out = if_instr_err;
			assign stall_dummy_instr = 1'b0;
			wire [1:1] sv2v_tmp_C8A0C;
			assign sv2v_tmp_C8A0C = 1'b0;
			always @(*) dummy_instr_id_o = sv2v_tmp_C8A0C;
		end
	endgenerate
	assign instr_valid_id_d = ((if_instr_valid & id_in_ready_i) & ~pc_set_i) | (instr_valid_id_q & ~instr_valid_clear_i);
	assign instr_new_id_d = if_instr_valid & id_in_ready_i;
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni) begin
			instr_valid_id_q <= 1'b0;
			instr_new_id_q <= 1'b0;
		end
		else begin
			instr_valid_id_q <= instr_valid_id_d;
			instr_new_id_q <= instr_new_id_d;
		end
	assign instr_valid_id_o = instr_valid_id_q;
	assign instr_new_id_o = instr_new_id_q;
	assign if_id_pipe_reg_we = instr_new_id_d;
	generate
		if (ResetAll) begin : g_instr_rdata_ra
			always @(posedge clk_i or negedge rst_ni)
				if (!rst_ni) begin
					instr_rdata_id_o <= 1'sb0;
					instr_rdata_alu_id_o <= 1'sb0;
					instr_fetch_err_o <= 1'sb0;
					instr_fetch_err_plus2_o <= 1'sb0;
					instr_rdata_c_id_o <= 1'sb0;
					instr_is_compressed_id_o <= 1'sb0;
					illegal_c_insn_id_o <= 1'sb0;
					pc_id_o <= 1'sb0;
				end
				else if (if_id_pipe_reg_we) begin
					instr_rdata_id_o <= instr_out;
					instr_rdata_alu_id_o <= instr_out;
					instr_fetch_err_o <= instr_err_out;
					instr_fetch_err_plus2_o <= if_instr_err_plus2;
					instr_rdata_c_id_o <= if_instr_rdata[15:0];
					instr_is_compressed_id_o <= instr_is_compressed_out;
					illegal_c_insn_id_o <= illegal_c_instr_out;
					pc_id_o <= pc_if_o;
				end
		end
		else begin : g_instr_rdata_nr
			always @(posedge clk_i)
				if (if_id_pipe_reg_we) begin
					instr_rdata_id_o <= instr_out;
					instr_rdata_alu_id_o <= instr_out;
					instr_fetch_err_o <= instr_err_out;
					instr_fetch_err_plus2_o <= if_instr_err_plus2;
					instr_rdata_c_id_o <= if_instr_rdata[15:0];
					instr_is_compressed_id_o <= instr_is_compressed_out;
					illegal_c_insn_id_o <= illegal_c_instr_out;
					pc_id_o <= pc_if_o;
				end
		end
		if (PCIncrCheck) begin : g_secure_pc
			wire [31:0] prev_instr_addr_incr;
			wire [31:0] prev_instr_addr_incr_buf;
			reg prev_instr_seq_q;
			wire prev_instr_seq_d;
			assign prev_instr_seq_d = (((prev_instr_seq_q | instr_new_id_d) & ~branch_req) & ~if_instr_err) & ~stall_dummy_instr;
			always @(posedge clk_i or negedge rst_ni)
				if (!rst_ni)
					prev_instr_seq_q <= 1'b0;
				else
					prev_instr_seq_q <= prev_instr_seq_d;
			assign prev_instr_addr_incr = pc_id_o + (instr_is_compressed_id_o ? 32'd2 : 32'd4);
			prim_buf #(.Width(32)) u_prev_instr_addr_incr_buf(
				.in_i(prev_instr_addr_incr),
				.out_o(prev_instr_addr_incr_buf)
			);
			assign pc_mismatch_alert_o = prev_instr_seq_q & (pc_if_o != prev_instr_addr_incr_buf);
		end
		else begin : g_no_secure_pc
			assign pc_mismatch_alert_o = 1'b0;
		end
		if (BranchPredictor) begin : g_branch_predictor
			reg [31:0] instr_skid_data_q;
			reg [31:0] instr_skid_addr_q;
			reg instr_skid_bp_taken_q;
			reg instr_skid_valid_q;
			wire instr_skid_valid_d;
			wire instr_skid_en;
			reg instr_bp_taken_q;
			wire instr_bp_taken_d;
			wire predict_branch_taken_raw;
			if (ResetAll) begin : g_bp_taken_ra
				always @(posedge clk_i or negedge rst_ni)
					if (!rst_ni)
						instr_bp_taken_q <= 1'sb0;
					else if (if_id_pipe_reg_we)
						instr_bp_taken_q <= instr_bp_taken_d;
			end
			else begin : g_bp_taken_nr
				always @(posedge clk_i)
					if (if_id_pipe_reg_we)
						instr_bp_taken_q <= instr_bp_taken_d;
			end
			assign instr_skid_en = ((predict_branch_taken & ~pc_set_i) & ~id_in_ready_i) & ~instr_skid_valid_q;
			assign instr_skid_valid_d = ((instr_skid_valid_q & ~id_in_ready_i) & ~stall_dummy_instr) | instr_skid_en;
			always @(posedge clk_i or negedge rst_ni)
				if (!rst_ni)
					instr_skid_valid_q <= 1'b0;
				else
					instr_skid_valid_q <= instr_skid_valid_d;
			if (ResetAll) begin : g_instr_skid_ra
				always @(posedge clk_i or negedge rst_ni)
					if (!rst_ni) begin
						instr_skid_bp_taken_q <= 1'sb0;
						instr_skid_data_q <= 1'sb0;
						instr_skid_addr_q <= 1'sb0;
					end
					else if (instr_skid_en) begin
						instr_skid_bp_taken_q <= predict_branch_taken;
						instr_skid_data_q <= fetch_rdata;
						instr_skid_addr_q <= fetch_addr;
					end
			end
			else begin : g_instr_skid_nr
				always @(posedge clk_i)
					if (instr_skid_en) begin
						instr_skid_bp_taken_q <= predict_branch_taken;
						instr_skid_data_q <= fetch_rdata;
						instr_skid_addr_q <= fetch_addr;
					end
			end
			ibex_branch_predict branch_predict_i(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.fetch_rdata_i(fetch_rdata),
				.fetch_pc_i(fetch_addr),
				.fetch_valid_i(fetch_valid),
				.predict_branch_taken_o(predict_branch_taken_raw),
				.predict_branch_pc_o(predict_branch_pc)
			);
			assign predict_branch_taken = (predict_branch_taken_raw & ~instr_skid_valid_q) & ~fetch_err;
			assign if_instr_valid = fetch_valid | (instr_skid_valid_q & ~nt_branch_mispredict_i);
			assign if_instr_rdata = (instr_skid_valid_q ? instr_skid_data_q : fetch_rdata);
			assign if_instr_addr = (instr_skid_valid_q ? instr_skid_addr_q : fetch_addr);
			assign if_instr_bus_err = ~instr_skid_valid_q & fetch_err;
			assign instr_bp_taken_d = (instr_skid_valid_q ? instr_skid_bp_taken_q : predict_branch_taken);
			assign fetch_ready = (id_in_ready_i & ~stall_dummy_instr) & ~instr_skid_valid_q;
			assign instr_bp_taken_o = instr_bp_taken_q;
		end
		else begin : g_no_branch_predictor
			assign instr_bp_taken_o = 1'b0;
			assign predict_branch_taken = 1'b0;
			assign predict_branch_pc = 32'b00000000000000000000000000000000;
			assign if_instr_valid = fetch_valid;
			assign if_instr_rdata = fetch_rdata;
			assign if_instr_addr = fetch_addr;
			assign if_instr_bus_err = fetch_err;
			assign fetch_ready = id_in_ready_i & ~stall_dummy_instr;
		end
	endgenerate
	initial _sv2v_0 = 0;
endmodule
