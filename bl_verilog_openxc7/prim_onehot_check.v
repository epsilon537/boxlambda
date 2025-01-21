module prim_onehot_check (
	clk_i,
	rst_ni,
	oh_i,
	addr_i,
	en_i,
	err_o
);
	parameter [31:0] AddrWidth = 5;
	parameter [31:0] OneHotWidth = 2 ** AddrWidth;
	parameter [0:0] AddrCheck = 1;
	parameter [0:0] EnableCheck = 1;
	parameter [0:0] StrictCheck = 1;
	parameter [0:0] EnableAlertTriggerSVA = 1;
	input clk_i;
	input rst_ni;
	input wire [OneHotWidth - 1:0] oh_i;
	input wire [AddrWidth - 1:0] addr_i;
	input wire en_i;
	output wire err_o;
	localparam signed [31:0] NumLevels = AddrWidth;
	wire [(2 ** (NumLevels + 1)) - 2:0] or_tree;
	wire [(2 ** (NumLevels + 1)) - 2:0] and_tree;
	wire [(2 ** (NumLevels + 1)) - 2:0] err_tree;
	genvar _gv_level_1;
	generate
		for (_gv_level_1 = 0; _gv_level_1 < (NumLevels + 1); _gv_level_1 = _gv_level_1 + 1) begin : gen_tree
			localparam level = _gv_level_1;
			localparam signed [31:0] Base0 = (2 ** level) - 1;
			localparam signed [31:0] Base1 = (2 ** (level + 1)) - 1;
			genvar _gv_offset_1;
			for (_gv_offset_1 = 0; _gv_offset_1 < (2 ** level); _gv_offset_1 = _gv_offset_1 + 1) begin : gen_level
				localparam offset = _gv_offset_1;
				localparam signed [31:0] Pa = Base0 + offset;
				localparam signed [31:0] C0 = Base1 + (2 * offset);
				localparam signed [31:0] C1 = (Base1 + (2 * offset)) + 1;
				if (level == NumLevels) begin : gen_leafs
					if (offset < OneHotWidth) begin : gen_assign
						assign or_tree[Pa] = oh_i[offset];
						assign and_tree[Pa] = oh_i[offset];
					end
					else begin : gen_tie_off
						assign or_tree[Pa] = 1'b0;
						assign and_tree[Pa] = 1'b0;
					end
					assign err_tree[Pa] = 1'b0;
				end
				else begin : gen_nodes
					assign or_tree[Pa] = or_tree[C0] || or_tree[C1];
					assign and_tree[Pa] = (!addr_i[(AddrWidth - 1) - level] && and_tree[C0]) || (addr_i[(AddrWidth - 1) - level] && and_tree[C1]);
					assign err_tree[Pa] = ((or_tree[C0] && or_tree[C1]) || err_tree[C0]) || err_tree[C1];
				end
			end
		end
	endgenerate
	wire enable_err;
	wire addr_err;
	wire oh0_err;
	assign err_o = (oh0_err || enable_err) || addr_err;
	assign oh0_err = err_tree[0];
	generate
		if (EnableCheck) begin : gen_enable_check
			if (StrictCheck) begin : gen_strict
				assign enable_err = or_tree[0] ^ en_i;
			end
			else begin : gen_not_strict
				assign enable_err = !en_i && or_tree[0];
			end
		end
		else begin : gen_no_enable_check
			wire unused_or_tree;
			assign unused_or_tree = ^or_tree;
			assign enable_err = 1'b0;
		end
		if (AddrCheck) begin : gen_addr_check_strict
			assign addr_err = or_tree[0] ^ and_tree[0];
		end
		else begin : gen_no_addr_check_strict
			wire unused_and_tree;
			assign unused_and_tree = ^and_tree;
			assign addr_err = 1'b0;
		end
	endgenerate
endmodule
