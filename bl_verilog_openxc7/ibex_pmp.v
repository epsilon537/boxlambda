module ibex_pmp (
	csr_pmp_cfg_i,
	csr_pmp_addr_i,
	csr_pmp_mseccfg_i,
	debug_mode_i,
	priv_mode_i,
	pmp_req_addr_i,
	pmp_req_type_i,
	pmp_req_err_o
);
	reg _sv2v_0;
	parameter [31:0] DmBaseAddr = 32'h1a110000;
	parameter [31:0] DmAddrMask = 32'h00000fff;
	parameter [31:0] PMPGranularity = 0;
	parameter [31:0] PMPNumChan = 2;
	parameter [31:0] PMPNumRegions = 4;
	input wire [(PMPNumRegions * 6) - 1:0] csr_pmp_cfg_i;
	input wire [(PMPNumRegions * 34) - 1:0] csr_pmp_addr_i;
	input wire [2:0] csr_pmp_mseccfg_i;
	input wire debug_mode_i;
	input wire [(PMPNumChan * 2) - 1:0] priv_mode_i;
	input wire [(PMPNumChan * 34) - 1:0] pmp_req_addr_i;
	input wire [(PMPNumChan * 2) - 1:0] pmp_req_type_i;
	output wire [0:PMPNumChan - 1] pmp_req_err_o;
	wire [33:0] region_start_addr [0:PMPNumRegions - 1];
	wire [33:PMPGranularity + 2] region_addr_mask [0:PMPNumRegions - 1];
	wire [(PMPNumChan * PMPNumRegions) - 1:0] region_match_gt;
	wire [(PMPNumChan * PMPNumRegions) - 1:0] region_match_lt;
	wire [(PMPNumChan * PMPNumRegions) - 1:0] region_match_eq;
	reg [(PMPNumChan * PMPNumRegions) - 1:0] region_match_all;
	wire [(PMPNumChan * PMPNumRegions) - 1:0] region_basic_perm_check;
	wire [(PMPNumChan * PMPNumRegions) - 1:0] region_perm_check;
	wire [PMPNumChan - 1:0] debug_mode_allowed_access;
	function automatic mml_perm_check;
		input reg [5:0] region_csr_pmp_cfg;
		input reg [1:0] pmp_req_type;
		input reg [1:0] priv_mode;
		input reg permission_check;
		reg result;
		reg unused_cfg;
		begin
			result = 1'b0;
			unused_cfg = |region_csr_pmp_cfg[4-:2];
			if (!region_csr_pmp_cfg[0] && region_csr_pmp_cfg[1])
				(* full_case, parallel_case *)
				case ({region_csr_pmp_cfg[5], region_csr_pmp_cfg[2]})
					2'b00: result = (pmp_req_type == 2'b10) | ((pmp_req_type == 2'b01) & (priv_mode == 2'b11));
					2'b01: result = (pmp_req_type == 2'b10) | (pmp_req_type == 2'b01);
					2'b10: result = pmp_req_type == 2'b00;
					2'b11: result = (pmp_req_type == 2'b00) | ((pmp_req_type == 2'b10) & (priv_mode == 2'b11));
					default:
						;
				endcase
			else if (((region_csr_pmp_cfg[0] & region_csr_pmp_cfg[1]) & region_csr_pmp_cfg[2]) & region_csr_pmp_cfg[5])
				result = pmp_req_type == 2'b10;
			else
				result = permission_check & (priv_mode == 2'b11 ? region_csr_pmp_cfg[5] : ~region_csr_pmp_cfg[5]);
			mml_perm_check = result;
		end
	endfunction
	function automatic orig_perm_check;
		input reg pmp_cfg_lock;
		input reg [1:0] priv_mode;
		input reg permission_check;
		orig_perm_check = (priv_mode == 2'b11 ? ~pmp_cfg_lock | permission_check : permission_check);
	endfunction
	function automatic perm_check_wrapper;
		input reg csr_pmp_mseccfg_mml;
		input reg [5:0] region_csr_pmp_cfg;
		input reg [1:0] pmp_req_type;
		input reg [1:0] priv_mode;
		input reg permission_check;
		perm_check_wrapper = (csr_pmp_mseccfg_mml ? mml_perm_check(region_csr_pmp_cfg, pmp_req_type, priv_mode, permission_check) : orig_perm_check(region_csr_pmp_cfg[5], priv_mode, permission_check));
	endfunction
	function automatic access_fault_check;
		input reg csr_pmp_mseccfg_mmwp;
		input reg csr_pmp_mseccfg_mml;
		input reg [1:0] pmp_req_type;
		input reg [PMPNumRegions - 1:0] match_all;
		input reg [1:0] priv_mode;
		input reg [PMPNumRegions - 1:0] final_perm_check;
		reg access_fail;
		reg matched;
		begin
			access_fail = (csr_pmp_mseccfg_mmwp | (priv_mode != 2'b11)) | (csr_pmp_mseccfg_mml && (pmp_req_type == 2'b00));
			matched = 1'b0;
			begin : sv2v_autoblock_1
				reg signed [31:0] r;
				for (r = 0; r < PMPNumRegions; r = r + 1)
					if (!matched && match_all[r]) begin
						access_fail = ~final_perm_check[r];
						matched = 1'b1;
					end
			end
			access_fault_check = access_fail;
		end
	endfunction
	genvar _gv_r_2;
	generate
		for (_gv_r_2 = 0; _gv_r_2 < PMPNumRegions; _gv_r_2 = _gv_r_2 + 1) begin : g_addr_exp
			localparam r = _gv_r_2;
			if (r == 0) begin : g_entry0
				assign region_start_addr[r] = (csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 4-:2] == 2'b01 ? 34'h000000000 : csr_pmp_addr_i[((PMPNumRegions - 1) - r) * 34+:34]);
			end
			else begin : g_oth
				assign region_start_addr[r] = (csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 4-:2] == 2'b01 ? csr_pmp_addr_i[((PMPNumRegions - 1) - (r - 1)) * 34+:34] : csr_pmp_addr_i[((PMPNumRegions - 1) - r) * 34+:34]);
			end
			genvar _gv_b_3;
			for (_gv_b_3 = PMPGranularity + 2; _gv_b_3 < 34; _gv_b_3 = _gv_b_3 + 1) begin : g_bitmask
				localparam b = _gv_b_3;
				if (b == 2) begin : g_bit0
					assign region_addr_mask[r][b] = csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 4-:2] != 2'b11;
				end
				else begin : g_others
					if (PMPGranularity == 0) begin : g_region_addr_mask_zero_granularity
						assign region_addr_mask[r][b] = (csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 4-:2] != 2'b11) | ~&csr_pmp_addr_i[(((PMPNumRegions - 1) - r) * 34) + ((b - 1) >= 2 ? b - 1 : ((b - 1) + ((b - 1) >= 2 ? b - 2 : 4 - b)) - 1)-:((b - 1) >= 2 ? b - 2 : 4 - b)];
					end
					else begin : g_region_addr_mask_other_granularity
						assign region_addr_mask[r][b] = (csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 4-:2] != 2'b11) | ~&csr_pmp_addr_i[(((PMPNumRegions - 1) - r) * 34) + ((b - 1) >= (PMPGranularity + 1) ? b - 1 : ((b - 1) + ((b - 1) >= (PMPGranularity + 1) ? ((b - 1) - (PMPGranularity + 1)) + 1 : ((PMPGranularity + 1) - (b - 1)) + 1)) - 1)-:((b - 1) >= (PMPGranularity + 1) ? ((b - 1) - (PMPGranularity + 1)) + 1 : ((PMPGranularity + 1) - (b - 1)) + 1)];
					end
				end
			end
		end
	endgenerate
	genvar _gv_c_1;
	generate
		for (_gv_c_1 = 0; _gv_c_1 < PMPNumChan; _gv_c_1 = _gv_c_1 + 1) begin : g_access_check
			localparam c = _gv_c_1;
			genvar _gv_r_3;
			for (_gv_r_3 = 0; _gv_r_3 < PMPNumRegions; _gv_r_3 = _gv_r_3 + 1) begin : g_regions
				localparam r = _gv_r_3;
				assign region_match_eq[(c * PMPNumRegions) + r] = (pmp_req_addr_i[(((PMPNumChan - 1) - c) * 34) + (33 >= (PMPGranularity + 2) ? 33 : (33 + (33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)) - 1)-:(33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)] & region_addr_mask[r]) == (region_start_addr[r][33:PMPGranularity + 2] & region_addr_mask[r]);
				assign region_match_gt[(c * PMPNumRegions) + r] = pmp_req_addr_i[(((PMPNumChan - 1) - c) * 34) + (33 >= (PMPGranularity + 2) ? 33 : (33 + (33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)) - 1)-:(33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)] > region_start_addr[r][33:PMPGranularity + 2];
				assign region_match_lt[(c * PMPNumRegions) + r] = pmp_req_addr_i[(((PMPNumChan - 1) - c) * 34) + (33 >= (PMPGranularity + 2) ? 33 : (33 + (33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)) - 1)-:(33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)] < csr_pmp_addr_i[(((PMPNumRegions - 1) - r) * 34) + (33 >= (PMPGranularity + 2) ? 33 : (33 + (33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)) - 1)-:(33 >= (PMPGranularity + 2) ? 34 - (PMPGranularity + 2) : PMPGranularity - 30)];
				always @(*) begin
					if (_sv2v_0)
						;
					region_match_all[(c * PMPNumRegions) + r] = 1'b0;
					(* full_case, parallel_case *)
					case (csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 4-:2])
						2'b00: region_match_all[(c * PMPNumRegions) + r] = 1'b0;
						2'b10: region_match_all[(c * PMPNumRegions) + r] = region_match_eq[(c * PMPNumRegions) + r];
						2'b11: region_match_all[(c * PMPNumRegions) + r] = region_match_eq[(c * PMPNumRegions) + r];
						2'b01: region_match_all[(c * PMPNumRegions) + r] = (region_match_eq[(c * PMPNumRegions) + r] | region_match_gt[(c * PMPNumRegions) + r]) & region_match_lt[(c * PMPNumRegions) + r];
						default: region_match_all[(c * PMPNumRegions) + r] = 1'b0;
					endcase
				end
				assign region_basic_perm_check[(c * PMPNumRegions) + r] = (((pmp_req_type_i[((PMPNumChan - 1) - c) * 2+:2] == 2'b00) & csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 2]) | ((pmp_req_type_i[((PMPNumChan - 1) - c) * 2+:2] == 2'b01) & csr_pmp_cfg_i[(((PMPNumRegions - 1) - r) * 6) + 1])) | ((pmp_req_type_i[((PMPNumChan - 1) - c) * 2+:2] == 2'b10) & csr_pmp_cfg_i[((PMPNumRegions - 1) - r) * 6]);
				assign region_perm_check[(c * PMPNumRegions) + r] = perm_check_wrapper(csr_pmp_mseccfg_i[0], csr_pmp_cfg_i[((PMPNumRegions - 1) - r) * 6+:6], pmp_req_type_i[((PMPNumChan - 1) - c) * 2+:2], priv_mode_i[((PMPNumChan - 1) - c) * 2+:2], region_basic_perm_check[(c * PMPNumRegions) + r]);
				wire unused_sigs;
				assign unused_sigs = ^{region_start_addr[r][PMPGranularity + 1:0], pmp_req_addr_i[(((PMPNumChan - 1) - c) * 34) + ((PMPGranularity + 1) >= 0 ? PMPGranularity + 1 : ((PMPGranularity + 1) + ((PMPGranularity + 1) >= 0 ? PMPGranularity + 2 : 1 - (PMPGranularity + 1))) - 1)-:((PMPGranularity + 1) >= 0 ? PMPGranularity + 2 : 1 - (PMPGranularity + 1))]};
			end
			assign debug_mode_allowed_access[c] = debug_mode_i & ((pmp_req_addr_i[(((PMPNumChan - 1) - c) * 34) + 31-:32] & ~DmAddrMask) == DmBaseAddr);
			assign pmp_req_err_o[c] = ~debug_mode_allowed_access[c] & access_fault_check(csr_pmp_mseccfg_i[1], csr_pmp_mseccfg_i[0], pmp_req_type_i[((PMPNumChan - 1) - c) * 2+:2], region_match_all[c * PMPNumRegions+:PMPNumRegions], priv_mode_i[((PMPNumChan - 1) - c) * 2+:2], region_perm_check[c * PMPNumRegions+:PMPNumRegions]);
		end
	endgenerate
	wire unused_csr_pmp_mseccfg_rlb;
	assign unused_csr_pmp_mseccfg_rlb = csr_pmp_mseccfg_i[2];
	initial _sv2v_0 = 0;
endmodule
