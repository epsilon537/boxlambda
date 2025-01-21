module ibex_lockstep (
	clk_i,
	rst_ni,
	hart_id_i,
	boot_addr_i,
	instr_req_i,
	instr_gnt_i,
	instr_rvalid_i,
	instr_addr_i,
	instr_rdata_i,
	instr_err_i,
	data_req_i,
	data_gnt_i,
	data_rvalid_i,
	data_we_i,
	data_be_i,
	data_addr_i,
	data_wdata_i,
	data_rdata_i,
	data_err_i,
	dummy_instr_id_i,
	dummy_instr_wb_i,
	rf_raddr_a_i,
	rf_raddr_b_i,
	rf_waddr_wb_i,
	rf_we_wb_i,
	rf_wdata_wb_ecc_i,
	rf_rdata_a_ecc_i,
	rf_rdata_b_ecc_i,
	ic_tag_req_i,
	ic_tag_write_i,
	ic_tag_addr_i,
	ic_tag_wdata_i,
	ic_tag_rdata_i,
	ic_data_req_i,
	ic_data_write_i,
	ic_data_addr_i,
	ic_data_wdata_i,
	ic_data_rdata_i,
	ic_scr_key_valid_i,
	ic_scr_key_req_i,
	irq_software_i,
	irq_timer_i,
	irq_external_i,
	irq_fast_i,
	irq_nm_i,
	irq_pending_i,
	debug_req_i,
	crash_dump_i,
	double_fault_seen_i,
	fetch_enable_i,
	alert_minor_o,
	alert_major_internal_o,
	alert_major_bus_o,
	core_busy_i,
	test_en_i,
	scan_rst_ni
);
	parameter [31:0] LockstepOffset = 2;
	parameter [0:0] PMPEnable = 1'b0;
	parameter [31:0] PMPGranularity = 0;
	parameter [31:0] PMPNumRegions = 4;
	localparam [95:0] ibex_pkg_PmpCfgRst = 96'b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
	parameter [95:0] PMPRstCfg = ibex_pkg_PmpCfgRst;
	localparam [543:0] ibex_pkg_PmpAddrRst = 544'h0;
	parameter [543:0] PMPRstAddr = ibex_pkg_PmpAddrRst;
	localparam [2:0] ibex_pkg_PmpMseccfgRst = 3'b000;
	parameter [2:0] PMPRstMsecCfg = ibex_pkg_PmpMseccfgRst;
	parameter [31:0] MHPMCounterNum = 0;
	parameter [31:0] MHPMCounterWidth = 40;
	parameter [0:0] RV32E = 1'b0;
	parameter integer RV32M = 32'sd2;
	parameter integer RV32B = 32'sd0;
	parameter [0:0] BranchTargetALU = 1'b0;
	parameter [0:0] WritebackStage = 1'b0;
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
	parameter [0:0] BranchPredictor = 1'b0;
	parameter [0:0] DbgTriggerEn = 1'b0;
	parameter [31:0] DbgHwBreakNum = 1;
	parameter [0:0] ResetAll = 1'b0;
	localparam signed [31:0] ibex_pkg_LfsrWidth = 32;
	localparam [31:0] ibex_pkg_RndCnstLfsrSeedDefault = 32'hac533bf4;
	parameter [31:0] RndCnstLfsrSeed = ibex_pkg_RndCnstLfsrSeedDefault;
	localparam [159:0] ibex_pkg_RndCnstLfsrPermDefault = 160'h1e35ecba467fd1b12e958152c04fa43878a8daed;
	parameter [159:0] RndCnstLfsrPerm = ibex_pkg_RndCnstLfsrPermDefault;
	parameter [0:0] SecureIbex = 1'b0;
	parameter [0:0] DummyInstructions = 1'b0;
	parameter [0:0] RegFileECC = 1'b0;
	parameter [31:0] RegFileDataWidth = 32;
	parameter [0:0] MemECC = 1'b0;
	parameter [31:0] MemDataWidth = (MemECC ? 39 : 32);
	parameter [31:0] DmBaseAddr = 32'h1a110000;
	parameter [31:0] DmAddrMask = 32'h00000fff;
	parameter [31:0] DmHaltAddr = 32'h1a110800;
	parameter [31:0] DmExceptionAddr = 32'h1a110808;
	input wire clk_i;
	input wire rst_ni;
	input wire [31:0] hart_id_i;
	input wire [31:0] boot_addr_i;
	input wire instr_req_i;
	input wire instr_gnt_i;
	input wire instr_rvalid_i;
	input wire [31:0] instr_addr_i;
	input wire [MemDataWidth - 1:0] instr_rdata_i;
	input wire instr_err_i;
	input wire data_req_i;
	input wire data_gnt_i;
	input wire data_rvalid_i;
	input wire data_we_i;
	input wire [3:0] data_be_i;
	input wire [31:0] data_addr_i;
	input wire [MemDataWidth - 1:0] data_wdata_i;
	input wire [MemDataWidth - 1:0] data_rdata_i;
	input wire data_err_i;
	input wire dummy_instr_id_i;
	input wire dummy_instr_wb_i;
	input wire [4:0] rf_raddr_a_i;
	input wire [4:0] rf_raddr_b_i;
	input wire [4:0] rf_waddr_wb_i;
	input wire rf_we_wb_i;
	input wire [RegFileDataWidth - 1:0] rf_wdata_wb_ecc_i;
	input wire [RegFileDataWidth - 1:0] rf_rdata_a_ecc_i;
	input wire [RegFileDataWidth - 1:0] rf_rdata_b_ecc_i;
	input wire [1:0] ic_tag_req_i;
	input wire ic_tag_write_i;
	input wire [ibex_pkg_IC_INDEX_W - 1:0] ic_tag_addr_i;
	input wire [TagSizeECC - 1:0] ic_tag_wdata_i;
	input wire [(ibex_pkg_IC_NUM_WAYS * TagSizeECC) - 1:0] ic_tag_rdata_i;
	input wire [1:0] ic_data_req_i;
	input wire ic_data_write_i;
	input wire [ibex_pkg_IC_INDEX_W - 1:0] ic_data_addr_i;
	input wire [LineSizeECC - 1:0] ic_data_wdata_i;
	input wire [(ibex_pkg_IC_NUM_WAYS * LineSizeECC) - 1:0] ic_data_rdata_i;
	input wire ic_scr_key_valid_i;
	input wire ic_scr_key_req_i;
	input wire irq_software_i;
	input wire irq_timer_i;
	input wire irq_external_i;
	input wire [14:0] irq_fast_i;
	input wire irq_nm_i;
	input wire irq_pending_i;
	input wire debug_req_i;
	input wire [159:0] crash_dump_i;
	input wire double_fault_seen_i;
	localparam signed [31:0] ibex_pkg_IbexMuBiWidth = 4;
	input wire [3:0] fetch_enable_i;
	output wire alert_minor_o;
	output wire alert_major_internal_o;
	output wire alert_major_bus_o;
	input wire [3:0] core_busy_i;
	input wire test_en_i;
	input wire scan_rst_ni;
	localparam [31:0] LockstepOffsetW = $clog2(LockstepOffset);
	localparam [31:0] OutputsOffset = LockstepOffset + 1;
	wire [LockstepOffsetW - 1:0] rst_shadow_cnt;
	wire rst_shadow_cnt_err;
	wire [3:0] rst_shadow_set_d;
	wire [3:0] rst_shadow_set_q;
	wire rst_shadow_n;
	wire rst_shadow_set_single_bit;
	wire [3:0] enable_cmp_d;
	wire [3:0] enable_cmp_q;
	function automatic [LockstepOffsetW - 1:0] sv2v_cast_5A7C9;
		input reg [LockstepOffsetW - 1:0] inp;
		sv2v_cast_5A7C9 = inp;
	endfunction
	localparam signed [31:0] sv2v_uu_u_rst_shadow_cnt_Width = LockstepOffsetW;
	localparam [sv2v_uu_u_rst_shadow_cnt_Width - 1:0] sv2v_uu_u_rst_shadow_cnt_ext_set_cnt_i_0 = 1'sb0;
	prim_count #(
		.Width(LockstepOffsetW),
		.ResetValue(sv2v_cast_5A7C9(1'b0))
	) u_rst_shadow_cnt(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.clr_i(1'b0),
		.set_i(1'b0),
		.set_cnt_i(sv2v_uu_u_rst_shadow_cnt_ext_set_cnt_i_0),
		.incr_en_i(1'b1),
		.decr_en_i(1'b0),
		.step_i(sv2v_cast_5A7C9(1'b1)),
		.commit_i(1'b1),
		.cnt_o(rst_shadow_cnt),
		.cnt_after_commit_o(),
		.err_o(rst_shadow_cnt_err)
	);
	localparam [3:0] ibex_pkg_IbexMuBiOff = 4'b1010;
	localparam [3:0] ibex_pkg_IbexMuBiOn = 4'b0101;
	assign rst_shadow_set_d = (rst_shadow_cnt >= sv2v_cast_5A7C9(LockstepOffset - 1) ? ibex_pkg_IbexMuBiOn : ibex_pkg_IbexMuBiOff);
	assign enable_cmp_d = rst_shadow_set_q;
	assign rst_shadow_set_single_bit = rst_shadow_set_q[0];
	prim_flop #(
		.Width(ibex_pkg_IbexMuBiWidth),
		.ResetValue(ibex_pkg_IbexMuBiOff)
	) u_prim_rst_shadow_set_flop(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.d_i(rst_shadow_set_d),
		.q_o(rst_shadow_set_q)
	);
	prim_flop #(
		.Width(ibex_pkg_IbexMuBiWidth),
		.ResetValue(ibex_pkg_IbexMuBiOff)
	) u_prim_enable_cmp_flop(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.d_i(enable_cmp_d),
		.q_o(enable_cmp_q)
	);
	prim_clock_mux2 #(.NoFpgaBufG(1'b1)) u_prim_rst_shadow_n_mux2(
		.clk0_i(rst_shadow_set_single_bit),
		.clk1_i(scan_rst_ni),
		.sel_i(test_en_i),
		.clk_o(rst_shadow_n)
	);
	reg [((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? (LockstepOffset * (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1)) - 1 : (LockstepOffset * (1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0))) + (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 1)):((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)] shadow_inputs_q;
	wire [((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0:0] shadow_inputs_in;
	reg [(LockstepOffset * TagSizeECC) - 1:0] shadow_tag_rdata_q [0:1];
	reg [(LockstepOffset * LineSizeECC) - 1:0] shadow_data_rdata_q [0:1];
	assign shadow_inputs_in[2 + (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))] = instr_gnt_i;
	assign shadow_inputs_in[1 + (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))] = instr_rvalid_i;
	assign shadow_inputs_in[MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))-:((MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) >= (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25))))) ? ((MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) - (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25)))))) + 1 : ((3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25))))) - (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))) + 1)] = instr_rdata_i;
	assign shadow_inputs_in[3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))] = instr_err_i;
	assign shadow_inputs_in[2 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))] = data_gnt_i;
	assign shadow_inputs_in[1 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))] = data_rvalid_i;
	assign shadow_inputs_in[MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))-:((MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) >= (1 + (RegFileDataWidth + (RegFileDataWidth + 25))) ? ((MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) - (1 + (RegFileDataWidth + (RegFileDataWidth + 25)))) + 1 : ((1 + (RegFileDataWidth + (RegFileDataWidth + 25))) - (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))) + 1)] = data_rdata_i;
	assign shadow_inputs_in[1 + (RegFileDataWidth + (RegFileDataWidth + 24))] = data_err_i;
	assign shadow_inputs_in[RegFileDataWidth + (RegFileDataWidth + 24)-:((RegFileDataWidth + (RegFileDataWidth + 24)) >= (RegFileDataWidth + 25) ? ((RegFileDataWidth + (RegFileDataWidth + 24)) - (RegFileDataWidth + 25)) + 1 : ((RegFileDataWidth + 25) - (RegFileDataWidth + (RegFileDataWidth + 24))) + 1)] = rf_rdata_a_ecc_i;
	assign shadow_inputs_in[RegFileDataWidth + 24-:((RegFileDataWidth + 24) >= 25 ? RegFileDataWidth : 26 - (RegFileDataWidth + 24))] = rf_rdata_b_ecc_i;
	assign shadow_inputs_in[24] = irq_software_i;
	assign shadow_inputs_in[23] = irq_timer_i;
	assign shadow_inputs_in[22] = irq_external_i;
	assign shadow_inputs_in[21-:15] = irq_fast_i;
	assign shadow_inputs_in[6] = irq_nm_i;
	assign shadow_inputs_in[5] = debug_req_i;
	assign shadow_inputs_in[4-:4] = fetch_enable_i;
	assign shadow_inputs_in[0] = ic_scr_key_valid_i;
	function automatic [((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)) - 1:0] sv2v_cast_F4D25;
		input reg [((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)) - 1:0] inp;
		sv2v_cast_F4D25 = inp;
	endfunction
	function automatic [TagSizeECC - 1:0] sv2v_cast_0CBAD;
		input reg [TagSizeECC - 1:0] inp;
		sv2v_cast_0CBAD = inp;
	endfunction
	function automatic [LineSizeECC - 1:0] sv2v_cast_033B0;
		input reg [LineSizeECC - 1:0] inp;
		sv2v_cast_033B0 = inp;
	endfunction
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni) begin : sv2v_autoblock_1
			reg [31:0] i;
			for (i = 0; i < LockstepOffset; i = i + 1)
				begin
					shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) + (i * ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)))+:((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0))] <= sv2v_cast_F4D25(1'sb0);
					shadow_tag_rdata_q[i] <= {LockstepOffset {sv2v_cast_0CBAD(0)}};
					shadow_data_rdata_q[i] <= {LockstepOffset {sv2v_cast_033B0(0)}};
				end
		end
		else begin
			begin : sv2v_autoblock_2
				reg [31:0] i;
				for (i = 0; i < (LockstepOffset - 1); i = i + 1)
					begin
						shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) + (i * ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)))+:((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0))] <= shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) + ((i + 1) * ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)))+:((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0))];
						shadow_tag_rdata_q[i] <= shadow_tag_rdata_q[i + 1];
						shadow_data_rdata_q[i] <= shadow_data_rdata_q[i + 1];
					end
			end
			shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) + ((LockstepOffset - 1) * ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)))+:((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 1 : 1 - (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0))] <= shadow_inputs_in;
			shadow_tag_rdata_q[LockstepOffset - 1] <= ic_tag_rdata_i;
			shadow_data_rdata_q[LockstepOffset - 1] <= ic_data_rdata_i;
		end
	reg [(OutputsOffset * (((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth)) - 1:0] core_outputs_q;
	wire [(((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth) - 1:0] core_outputs_in;
	wire [(((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth) - 1:0] shadow_outputs_d;
	reg [(((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth) - 1:0] shadow_outputs_q;
	assign core_outputs_in[71 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))] = instr_req_i;
	assign core_outputs_in[70 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))-:((70 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) >= (38 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) ? ((70 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) - (38 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))))) + 1 : ((38 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))))) - (70 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))))) + 1)] = instr_addr_i;
	assign core_outputs_in[38 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))] = data_req_i;
	assign core_outputs_in[37 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))] = data_we_i;
	assign core_outputs_in[36 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))-:((36 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) >= (32 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) ? ((36 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) - (32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))))) + 1 : ((32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))))) - (36 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))))) + 1)] = data_be_i;
	assign core_outputs_in[32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))-:((32 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) >= (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))) ? ((32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) - (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))))) + 1 : ((MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) - (32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))))) + 1)] = data_addr_i;
	assign core_outputs_in[MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))-:((MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))) >= (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) - (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) + 1)] = data_wdata_i;
	assign core_outputs_in[18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))] = dummy_instr_id_i;
	assign core_outputs_in[17 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))] = dummy_instr_wb_i;
	assign core_outputs_in[16 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))-:((16 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) >= (11 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((16 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) - (11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (16 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) + 1)] = rf_raddr_a_i;
	assign core_outputs_in[11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))-:((11 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) >= (6 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) - (6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) + 1)] = rf_raddr_b_i;
	assign core_outputs_in[6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))-:((6 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) >= (1 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) - (1 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((1 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) + 1)] = rf_waddr_wb_i;
	assign core_outputs_in[1 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))] = rf_we_wb_i;
	assign core_outputs_in[RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))-:((RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))) >= (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))) ? ((RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))) + 1 : ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) - (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) + 1)] = rf_wdata_wb_ecc_i;
	assign core_outputs_in[ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))-:((3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))) >= (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))) ? ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) - (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) + 1 : ((1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))) + 1)] = ic_tag_req_i;
	assign core_outputs_in[1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))] = ic_tag_write_i;
	assign core_outputs_in[ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))-:((ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))) >= (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))) ? ((ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))) - (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))) + 1 : ((TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))) - (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))) + 1)] = ic_tag_addr_i;
	assign core_outputs_in[TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))-:((TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))) >= (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))) ? ((TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))) + 1 : ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))) - (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))) + 1)] = ic_tag_wdata_i;
	assign core_outputs_in[ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))-:((3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))) >= (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))) ? ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))) - (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))) + 1 : ((1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))) + 1)] = ic_data_req_i;
	assign core_outputs_in[1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))] = ic_data_write_i;
	assign core_outputs_in[ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)-:((ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)) >= (LineSizeECC + 167) ? ((ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)) - (LineSizeECC + 167)) + 1 : ((LineSizeECC + 167) - (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))) + 1)] = ic_data_addr_i;
	assign core_outputs_in[LineSizeECC + 166-:((LineSizeECC + 166) >= 167 ? LineSizeECC : 168 - (LineSizeECC + 166))] = ic_data_wdata_i;
	assign core_outputs_in[166] = ic_scr_key_req_i;
	assign core_outputs_in[165] = irq_pending_i;
	assign core_outputs_in[164-:160] = crash_dump_i;
	assign core_outputs_in[4] = double_fault_seen_i;
	assign core_outputs_in[3-:ibex_pkg_IbexMuBiWidth] = core_busy_i;
	always @(posedge clk_i) begin
		begin : sv2v_autoblock_3
			reg [31:0] i;
			for (i = 0; i < (OutputsOffset - 1); i = i + 1)
				core_outputs_q[i * (((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth)+:((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth] <= core_outputs_q[(i + 1) * (((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth)+:((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth];
		end
		core_outputs_q[(OutputsOffset - 1) * (((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth)+:((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth] <= core_outputs_in;
	end
	wire shadow_alert_minor;
	wire shadow_alert_major_internal;
	wire shadow_alert_major_bus;
	ibex_core #(
		.PMPEnable(PMPEnable),
		.PMPGranularity(PMPGranularity),
		.PMPNumRegions(PMPNumRegions),
		.PMPRstCfg(PMPRstCfg),
		.PMPRstAddr(PMPRstAddr),
		.PMPRstMsecCfg(PMPRstMsecCfg),
		.MHPMCounterNum(MHPMCounterNum),
		.MHPMCounterWidth(MHPMCounterWidth),
		.RV32E(RV32E),
		.RV32M(RV32M),
		.RV32B(RV32B),
		.BranchTargetALU(BranchTargetALU),
		.ICache(ICache),
		.ICacheECC(ICacheECC),
		.BusSizeECC(BusSizeECC),
		.TagSizeECC(TagSizeECC),
		.LineSizeECC(LineSizeECC),
		.BranchPredictor(BranchPredictor),
		.DbgTriggerEn(DbgTriggerEn),
		.DbgHwBreakNum(DbgHwBreakNum),
		.WritebackStage(WritebackStage),
		.ResetAll(ResetAll),
		.RndCnstLfsrSeed(RndCnstLfsrSeed),
		.RndCnstLfsrPerm(RndCnstLfsrPerm),
		.SecureIbex(SecureIbex),
		.DummyInstructions(DummyInstructions),
		.RegFileECC(RegFileECC),
		.RegFileDataWidth(RegFileDataWidth),
		.MemECC(MemECC),
		.MemDataWidth(MemDataWidth),
		.DmBaseAddr(DmBaseAddr),
		.DmAddrMask(DmAddrMask),
		.DmHaltAddr(DmHaltAddr),
		.DmExceptionAddr(DmExceptionAddr)
	) u_shadow_core(
		.clk_i(clk_i),
		.rst_ni(rst_shadow_n),
		.hart_id_i(hart_id_i),
		.boot_addr_i(boot_addr_i),
		.instr_req_o(shadow_outputs_d[71 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))]),
		.instr_gnt_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 2 + (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (2 + (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))))]),
		.instr_rvalid_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 1 + (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (1 + (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))))]),
		.instr_addr_o(shadow_outputs_d[70 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))-:((70 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) >= (38 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) ? ((70 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) - (38 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))))) + 1 : ((38 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))))) - (70 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))))) + 1)]),
		.instr_rdata_i(shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))) : ((0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))))) + ((MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) >= (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25))))) ? ((MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) - (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25)))))) + 1 : ((3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25))))) - (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))) + 1)) - 1)-:((MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) >= (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25))))) ? ((MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) - (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25)))))) + 1 : ((3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 25))))) - (MemDataWidth + (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))) + 1)]),
		.instr_err_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (3 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))]),
		.data_req_o(shadow_outputs_d[38 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))]),
		.data_gnt_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 2 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (2 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))]),
		.data_rvalid_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 1 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (1 + (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))))]),
		.data_we_o(shadow_outputs_d[37 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))]),
		.data_be_o(shadow_outputs_d[36 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))-:((36 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) >= (32 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) ? ((36 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) - (32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))))) + 1 : ((32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))))) - (36 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))))) + 1)]),
		.data_addr_o(shadow_outputs_d[32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))-:((32 + (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) >= (MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))) ? ((32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) - (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))))) + 1 : ((MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) - (32 + (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))))) + 1)]),
		.data_wdata_o(shadow_outputs_d[MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))-:((MemDataWidth + (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))) >= (18 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) - (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (MemDataWidth + (18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))))) + 1)]),
		.data_rdata_i(shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))) : ((0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))))) + ((MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) >= (1 + (RegFileDataWidth + (RegFileDataWidth + 25))) ? ((MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) - (1 + (RegFileDataWidth + (RegFileDataWidth + 25)))) + 1 : ((1 + (RegFileDataWidth + (RegFileDataWidth + 25))) - (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))) + 1)) - 1)-:((MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) >= (1 + (RegFileDataWidth + (RegFileDataWidth + 25))) ? ((MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24)))) - (1 + (RegFileDataWidth + (RegFileDataWidth + 25)))) + 1 : ((1 + (RegFileDataWidth + (RegFileDataWidth + 25))) - (MemDataWidth + (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))) + 1)]),
		.data_err_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 1 + (RegFileDataWidth + (RegFileDataWidth + 24)) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (1 + (RegFileDataWidth + (RegFileDataWidth + 24))))]),
		.dummy_instr_id_o(shadow_outputs_d[18 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))]),
		.dummy_instr_wb_o(shadow_outputs_d[17 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))]),
		.rf_raddr_a_o(shadow_outputs_d[16 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))-:((16 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) >= (11 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((16 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) - (11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (16 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) + 1)]),
		.rf_raddr_b_o(shadow_outputs_d[11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))-:((11 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) >= (6 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) - (6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (11 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) + 1)]),
		.rf_waddr_wb_o(shadow_outputs_d[6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))-:((6 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) >= (1 + (RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) ? ((6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) - (1 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))))) + 1 : ((1 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))))) - (6 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))))) + 1)]),
		.rf_we_wb_o(shadow_outputs_d[1 + (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))]),
		.rf_wdata_wb_ecc_o(shadow_outputs_d[RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))-:((RegFileDataWidth + (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))) >= (3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))) ? ((RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))))) + 1 : ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) - (RegFileDataWidth + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))))) + 1)]),
		.rf_rdata_a_ecc_i(shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? RegFileDataWidth + (RegFileDataWidth + 24) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (RegFileDataWidth + (RegFileDataWidth + 24))) : ((0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? RegFileDataWidth + (RegFileDataWidth + 24) : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (RegFileDataWidth + (RegFileDataWidth + 24)))) + ((RegFileDataWidth + (RegFileDataWidth + 24)) >= (RegFileDataWidth + 25) ? ((RegFileDataWidth + (RegFileDataWidth + 24)) - (RegFileDataWidth + 25)) + 1 : ((RegFileDataWidth + 25) - (RegFileDataWidth + (RegFileDataWidth + 24))) + 1)) - 1)-:((RegFileDataWidth + (RegFileDataWidth + 24)) >= (RegFileDataWidth + 25) ? ((RegFileDataWidth + (RegFileDataWidth + 24)) - (RegFileDataWidth + 25)) + 1 : ((RegFileDataWidth + 25) - (RegFileDataWidth + (RegFileDataWidth + 24))) + 1)]),
		.rf_rdata_b_ecc_i(shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? RegFileDataWidth + 24 : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (RegFileDataWidth + 24)) : ((0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? RegFileDataWidth + 24 : (((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0) - (RegFileDataWidth + 24))) + ((RegFileDataWidth + 24) >= 25 ? RegFileDataWidth : 26 - (RegFileDataWidth + 24))) - 1)-:((RegFileDataWidth + 24) >= 25 ? RegFileDataWidth : 26 - (RegFileDataWidth + 24))]),
		.ic_tag_req_o(shadow_outputs_d[ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))-:((3 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))) >= (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))) ? ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))))) - (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))))) + 1 : ((1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))))) + 1)]),
		.ic_tag_write_o(shadow_outputs_d[1 + (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))]),
		.ic_tag_addr_o(shadow_outputs_d[ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))-:((ibex_pkg_IC_INDEX_W + (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))) >= (TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))) ? ((ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))) - (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))))) + 1 : ((TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))) - (ibex_pkg_IC_INDEX_W + (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))))) + 1)]),
		.ic_tag_wdata_o(shadow_outputs_d[TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))-:((TagSizeECC + (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))) >= (3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))) ? ((TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))))) + 1 : ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))) - (TagSizeECC + (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))))) + 1)]),
		.ic_tag_rdata_i(shadow_tag_rdata_q[0]),
		.ic_data_req_o(shadow_outputs_d[ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))-:((3 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))) >= (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))) ? ((ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)))) - (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167)))) + 1 : ((1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 167))) - (ibex_pkg_IC_NUM_WAYS + (1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))))) + 1)]),
		.ic_data_write_o(shadow_outputs_d[1 + (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))]),
		.ic_data_addr_o(shadow_outputs_d[ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)-:((ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)) >= (LineSizeECC + 167) ? ((ibex_pkg_IC_INDEX_W + (LineSizeECC + 166)) - (LineSizeECC + 167)) + 1 : ((LineSizeECC + 167) - (ibex_pkg_IC_INDEX_W + (LineSizeECC + 166))) + 1)]),
		.ic_data_wdata_o(shadow_outputs_d[LineSizeECC + 166-:((LineSizeECC + 166) >= 167 ? LineSizeECC : 168 - (LineSizeECC + 166))]),
		.ic_data_rdata_i(shadow_data_rdata_q[0]),
		.ic_scr_key_valid_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) + 0)]),
		.ic_scr_key_req_o(shadow_outputs_d[166]),
		.irq_software_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 24 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 24)]),
		.irq_timer_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 23 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 23)]),
		.irq_external_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 22 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 22)]),
		.irq_fast_i(shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 21 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 21) : (0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 21 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 21)) + 14)-:15]),
		.irq_nm_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 6 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 6)]),
		.irq_pending_o(shadow_outputs_d[165]),
		.debug_req_i(shadow_inputs_q[0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 5 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 5)]),
		.crash_dump_o(shadow_outputs_d[164-:160]),
		.double_fault_seen_o(shadow_outputs_d[4]),
		.fetch_enable_i(shadow_inputs_q[((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 4 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 4) : (0 + ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 24) >= 0 ? 4 : ((((((((2 + MemDataWidth) + 3) + MemDataWidth) + 1) + RegFileDataWidth) + RegFileDataWidth) + 20) + ibex_pkg_IbexMuBiWidth) - 4)) + 3)-:4]),
		.alert_minor_o(shadow_alert_minor),
		.alert_major_internal_o(shadow_alert_major_internal),
		.alert_major_bus_o(shadow_alert_major_bus),
		.core_busy_o(shadow_outputs_d[3-:ibex_pkg_IbexMuBiWidth])
	);
	always @(posedge clk_i) shadow_outputs_q <= shadow_outputs_d;
	wire outputs_mismatch;
	assign outputs_mismatch = (enable_cmp_q != ibex_pkg_IbexMuBiOff) & (shadow_outputs_q != core_outputs_q[0+:((((((((((((71 + MemDataWidth) + 18) + RegFileDataWidth) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + TagSizeECC) + ibex_pkg_IC_NUM_WAYS) + 1) + ibex_pkg_IC_INDEX_W) + LineSizeECC) + 163) + ibex_pkg_IbexMuBiWidth]);
	assign alert_major_internal_o = (outputs_mismatch | shadow_alert_major_internal) | rst_shadow_cnt_err;
	assign alert_major_bus_o = shadow_alert_major_bus;
	assign alert_minor_o = shadow_alert_minor;
endmodule
