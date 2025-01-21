`default_nettype none
module dm_mem (
	clk_i,
	rst_ni,
	debug_req_o,
	ndmreset_i,
	hartsel_i,
	haltreq_i,
	resumereq_i,
	clear_resumeack_i,
	halted_o,
	resuming_o,
	progbuf_i,
	data_i,
	data_o,
	data_valid_o,
	cmd_valid_i,
	cmd_i,
	cmderror_valid_o,
	cmderror_o,
	cmdbusy_o,
	req_i,
	we_i,
	addr_i,
	wdata_i,
	be_i,
	rdata_o
);
	reg _sv2v_0;
	parameter [31:0] NrHarts = 1;
	parameter [31:0] BusWidth = 32;
	parameter [NrHarts - 1:0] SelectableHarts = {NrHarts {1'b1}};
	parameter [31:0] DmBaseAddress = 1'sb0;
	input wire clk_i;
	input wire rst_ni;
	output wire [NrHarts - 1:0] debug_req_o;
	input wire ndmreset_i;
	input wire [19:0] hartsel_i;
	input wire [NrHarts - 1:0] haltreq_i;
	input wire [NrHarts - 1:0] resumereq_i;
	input wire clear_resumeack_i;
	output wire [NrHarts - 1:0] halted_o;
	output wire [NrHarts - 1:0] resuming_o;
	localparam [4:0] dm_ProgBufSize = 5'h08;
	input wire [255:0] progbuf_i;
	localparam [3:0] dm_DataCount = 4'h2;
	input wire [63:0] data_i;
	output reg [63:0] data_o;
	output reg data_valid_o;
	input wire cmd_valid_i;
	input wire [31:0] cmd_i;
	output reg cmderror_valid_o;
	output reg [2:0] cmderror_o;
	output reg cmdbusy_o;
	input wire req_i;
	input wire we_i;
	input wire [BusWidth - 1:0] addr_i;
	input wire [BusWidth - 1:0] wdata_i;
	input wire [(BusWidth / 8) - 1:0] be_i;
	output wire [BusWidth - 1:0] rdata_o;
	localparam [31:0] DbgAddressBits = 12;
	localparam [31:0] HartSelLen = (NrHarts == 1 ? 1 : $clog2(NrHarts));
	localparam [31:0] NrHartsAligned = 2 ** HartSelLen;
	localparam [31:0] MaxAar = (BusWidth == 64 ? 4 : 3);
	localparam [0:0] HasSndScratch = DmBaseAddress != 0;
	localparam [4:0] LoadBaseAddr = (DmBaseAddress == 0 ? 5'd0 : 5'd10);
	localparam [11:0] dm_DataAddr = 12'h380;
	localparam [11:0] DataBaseAddr = dm_DataAddr;
	localparam [11:0] DataEndAddr = 903;
	localparam [11:0] ProgBufBaseAddr = 864;
	localparam [11:0] ProgBufEndAddr = 895;
	localparam [11:0] AbstractCmdBaseAddr = ProgBufBaseAddr - 40;
	localparam [11:0] AbstractCmdEndAddr = ProgBufBaseAddr - 1;
	localparam [11:0] WhereToAddr = 'h300;
	localparam [11:0] FlagsBaseAddr = 'h400;
	localparam [11:0] FlagsEndAddr = 'h7ff;
	localparam [11:0] HaltedAddr = 'h100;
	localparam [11:0] GoingAddr = 'h108;
	localparam [11:0] ResumingAddr = 'h110;
	localparam [11:0] ExceptionAddr = 'h118;
	wire [255:0] progbuf;
	reg [511:0] abstract_cmd;
	wire [NrHarts - 1:0] halted_d;
	reg [NrHarts - 1:0] halted_q;
	wire [NrHarts - 1:0] resuming_d;
	reg [NrHarts - 1:0] resuming_q;
	reg resume;
	reg go;
	reg going;
	reg exception;
	reg unsupported_command;
	wire [63:0] rom_rdata;
	reg [63:0] rdata_d;
	reg [63:0] rdata_q;
	reg word_enable32_q;
	wire [HartSelLen - 1:0] hartsel;
	wire [HartSelLen - 1:0] wdata_hartsel;
	assign hartsel = hartsel_i[HartSelLen - 1:0];
	assign wdata_hartsel = wdata_i[HartSelLen - 1:0];
	wire [NrHartsAligned - 1:0] resumereq_aligned;
	wire [NrHartsAligned - 1:0] haltreq_aligned;
	reg [NrHartsAligned - 1:0] halted_d_aligned;
	wire [NrHartsAligned - 1:0] halted_q_aligned;
	reg [NrHartsAligned - 1:0] halted_aligned;
	wire [NrHartsAligned - 1:0] resumereq_wdata_aligned;
	reg [NrHartsAligned - 1:0] resuming_d_aligned;
	wire [NrHartsAligned - 1:0] resuming_q_aligned;
	function automatic [NrHartsAligned - 1:0] sv2v_cast_DFF07;
		input reg [NrHartsAligned - 1:0] inp;
		sv2v_cast_DFF07 = inp;
	endfunction
	assign resumereq_aligned = sv2v_cast_DFF07(resumereq_i);
	assign haltreq_aligned = sv2v_cast_DFF07(haltreq_i);
	assign resumereq_wdata_aligned = sv2v_cast_DFF07(resumereq_i);
	assign halted_q_aligned = sv2v_cast_DFF07(halted_q);
	function automatic [NrHarts - 1:0] sv2v_cast_178F2;
		input reg [NrHarts - 1:0] inp;
		sv2v_cast_178F2 = inp;
	endfunction
	assign halted_d = sv2v_cast_178F2(halted_d_aligned);
	assign resuming_q_aligned = sv2v_cast_DFF07(resuming_q);
	assign resuming_d = sv2v_cast_178F2(resuming_d_aligned);
	wire fwd_rom_d;
	reg fwd_rom_q;
	wire [23:0] ac_ar;
	function automatic [23:0] sv2v_cast_24;
		input reg [23:0] inp;
		sv2v_cast_24 = inp;
	endfunction
	assign ac_ar = sv2v_cast_24(cmd_i[23-:24]);
	assign debug_req_o = haltreq_i;
	assign halted_o = halted_q;
	assign resuming_o = resuming_q;
	assign progbuf = progbuf_i;
	reg [1:0] state_d;
	reg [1:0] state_q;
	always @(*) begin : p_hart_ctrl_queue
		if (_sv2v_0)
			;
		cmderror_valid_o = 1'b0;
		cmderror_o = 3'd0;
		state_d = state_q;
		go = 1'b0;
		resume = 1'b0;
		cmdbusy_o = 1'b1;
		(* full_case, parallel_case *)
		case (state_q)
			2'd0: begin
				cmdbusy_o = 1'b0;
				if ((cmd_valid_i && halted_q_aligned[hartsel]) && !unsupported_command)
					state_d = 2'd1;
				else if (cmd_valid_i) begin
					cmderror_valid_o = 1'b1;
					cmderror_o = 3'd4;
				end
				if (((resumereq_aligned[hartsel] && !resuming_q_aligned[hartsel]) && !haltreq_aligned[hartsel]) && halted_q_aligned[hartsel])
					state_d = 2'd2;
			end
			2'd1: begin
				cmdbusy_o = 1'b1;
				go = 1'b1;
				if (going)
					state_d = 2'd3;
			end
			2'd2: begin
				cmdbusy_o = 1'b1;
				resume = 1'b1;
				if (resuming_q_aligned[hartsel])
					state_d = 2'd0;
			end
			2'd3: begin
				cmdbusy_o = 1'b1;
				go = 1'b0;
				if (halted_aligned[hartsel])
					state_d = 2'd0;
			end
			default:
				;
		endcase
		if (unsupported_command && cmd_valid_i) begin
			cmderror_valid_o = 1'b1;
			cmderror_o = 3'd2;
		end
		if (exception) begin
			cmderror_valid_o = 1'b1;
			cmderror_o = 3'd3;
		end
		if (ndmreset_i) begin
			state_d = 2'd0;
			go = 1'b0;
			resume = 1'b0;
		end
	end
	wire [63:0] word_mux;
	assign word_mux = (fwd_rom_q ? rom_rdata : rdata_q);
	generate
		if (BusWidth == 64) begin : gen_word_mux64
			assign rdata_o = word_mux;
		end
		else begin : gen_word_mux32
			assign rdata_o = (word_enable32_q ? word_mux[32+:32] : word_mux[0+:32]);
		end
	endgenerate
	reg [63:0] data_bits;
	reg [63:0] rdata;
	localparam [63:0] dm_HaltAddress = 64'h0000000000000800;
	localparam [63:0] dm_ResumeAddress = 2056;
	function automatic [31:0] dm_jal;
		input reg [4:0] rd;
		input reg [20:0] imm;
		dm_jal = {imm[20], imm[10:1], imm[11], imm[19:12], rd, 7'h6f};
	endfunction
	function automatic [20:0] sv2v_cast_21;
		input reg [20:0] inp;
		sv2v_cast_21 = inp;
	endfunction
	function automatic [$clog2(4'h2) - 1:0] sv2v_cast_68FD0;
		input reg [$clog2(4'h2) - 1:0] inp;
		sv2v_cast_68FD0 = inp;
	endfunction
	function automatic [$clog2(5'h08) - 1:0] sv2v_cast_63A1A;
		input reg [$clog2(5'h08) - 1:0] inp;
		sv2v_cast_63A1A = inp;
	endfunction
	function automatic [2:0] sv2v_cast_3;
		input reg [2:0] inp;
		sv2v_cast_3 = inp;
	endfunction
	function automatic [11:0] sv2v_cast_C1AAB;
		input reg [11:0] inp;
		sv2v_cast_C1AAB = inp;
	endfunction
	always @(*) begin
		if (_sv2v_0)
			;
		(* xprop_off *)
		begin : p_rw_logic
			halted_d_aligned = sv2v_cast_DFF07(halted_q);
			resuming_d_aligned = sv2v_cast_DFF07(resuming_q);
			rdata_d = rdata_q;
			data_bits = data_i;
			rdata = 1'sb0;
			data_valid_o = 1'b0;
			exception = 1'b0;
			halted_aligned = 1'sb0;
			going = 1'b0;
			if (clear_resumeack_i)
				resuming_d_aligned[hartsel] = 1'b0;
			if (req_i) begin
				if (we_i) begin
					(* full_case, parallel_case *)
					if (addr_i[11:0] == HaltedAddr) begin
						halted_aligned[wdata_hartsel] = 1'b1;
						halted_d_aligned[wdata_hartsel] = 1'b1;
					end
					else if (addr_i[11:0] == GoingAddr)
						going = 1'b1;
					else if (addr_i[11:0] == ResumingAddr) begin
						halted_d_aligned[wdata_hartsel] = 1'b0;
						resuming_d_aligned[wdata_hartsel] = 1'b1;
					end
					else if (addr_i[11:0] == ExceptionAddr)
						exception = 1'b1;
					else if ((DataBaseAddr <= addr_i[11:0]) && (DataEndAddr >= addr_i[11:0])) begin
						data_valid_o = 1'b1;
						begin : sv2v_autoblock_1
							reg signed [31:0] dc;
							for (dc = 0; dc < dm_DataCount; dc = dc + 1)
								if ((addr_i[11:2] - DataBaseAddr[11:2]) == dc) begin : sv2v_autoblock_2
									reg signed [31:0] i;
									for (i = 0; i < (BusWidth / 8); i = i + 1)
										if (be_i[i]) begin
											if (i > 3) begin
												if ((dc + 1) < dm_DataCount)
													data_bits[((dc + 1) * 32) + ((i - 4) * 8)+:8] = wdata_i[i * 8+:8];
											end
											else
												data_bits[(dc * 32) + (i * 8)+:8] = wdata_i[i * 8+:8];
										end
								end
						end
					end
				end
				else
					(* full_case, parallel_case *)
					if (addr_i[11:0] == WhereToAddr) begin
						if (resumereq_wdata_aligned[wdata_hartsel])
							rdata_d = {32'b00000000000000000000000000000000, dm_jal(1'sb0, sv2v_cast_21(dm_ResumeAddress[11:0]) - sv2v_cast_21(WhereToAddr))};
						if (cmdbusy_o) begin
							if (((cmd_i[31-:8] == 8'h00) && !ac_ar[17]) && ac_ar[18])
								rdata_d = {32'b00000000000000000000000000000000, dm_jal(1'sb0, sv2v_cast_21(ProgBufBaseAddr) - sv2v_cast_21(WhereToAddr))};
							else
								rdata_d = {32'b00000000000000000000000000000000, dm_jal(1'sb0, sv2v_cast_21(AbstractCmdBaseAddr) - sv2v_cast_21(WhereToAddr))};
						end
					end
					else if ((DataBaseAddr <= addr_i[11:0]) && (DataEndAddr >= addr_i[11:0]))
						rdata_d = {data_i[sv2v_cast_68FD0(((addr_i[11:3] - DataBaseAddr[11:3]) << 1) + 1'b1) * 32+:32], data_i[sv2v_cast_68FD0((addr_i[11:3] - DataBaseAddr[11:3]) << 1) * 32+:32]};
					else if ((ProgBufBaseAddr <= addr_i[11:0]) && (ProgBufEndAddr >= addr_i[11:0]))
						rdata_d = progbuf[sv2v_cast_63A1A(addr_i[11:3] - ProgBufBaseAddr[11:3]) * 64+:64];
					else if ((AbstractCmdBaseAddr <= addr_i[11:0]) && (AbstractCmdEndAddr >= addr_i[11:0]))
						rdata_d = abstract_cmd[sv2v_cast_3(addr_i[11:3] - AbstractCmdBaseAddr[11:3]) * 64+:64];
					else if ((FlagsBaseAddr <= addr_i[11:0]) && (FlagsEndAddr >= addr_i[11:0])) begin
						if (({addr_i[11:3], 3'b000} - FlagsBaseAddr[11:0]) == (sv2v_cast_C1AAB(hartsel) & {{9 {1'b1}}, 3'b000}))
							rdata[(sv2v_cast_C1AAB(hartsel) & sv2v_cast_C1AAB(3'b111)) * 8+:8] = {6'b000000, resume, go};
						rdata_d = rdata;
					end
			end
			if (ndmreset_i) begin
				halted_d_aligned = 1'sb0;
				resuming_d_aligned = 1'sb0;
			end
			data_o = data_bits;
		end
	end
	function automatic [31:0] dm_auipc;
		input reg [4:0] rd;
		input reg [20:0] imm;
		dm_auipc = {imm[20], imm[10:1], imm[11], imm[19:12], rd, 7'h17};
	endfunction
	function automatic [31:0] dm_csrr;
		input reg [11:0] csr;
		input reg [4:0] dest;
		dm_csrr = {csr, 8'h02, dest, 7'h73};
	endfunction
	function automatic [31:0] dm_csrw;
		input reg [11:0] csr;
		input reg [4:0] rs1;
		dm_csrw = {csr, rs1, 15'h1073};
	endfunction
	function automatic [31:0] dm_ebreak;
		input reg _sv2v_unused;
		dm_ebreak = 32'h00100073;
	endfunction
	function automatic [31:0] dm_float_load;
		input reg [2:0] size;
		input reg [4:0] dest;
		input reg [4:0] base;
		input reg [11:0] offset;
		dm_float_load = {offset[11:0], base, size, dest, 7'b0000111};
	endfunction
	function automatic [31:0] dm_float_store;
		input reg [2:0] size;
		input reg [4:0] src;
		input reg [4:0] base;
		input reg [11:0] offset;
		dm_float_store = {offset[11:5], src, base, size, offset[4:0], 7'b0100111};
	endfunction
	function automatic [31:0] dm_illegal;
		input reg _sv2v_unused;
		dm_illegal = 32'h00000000;
	endfunction
	function automatic [31:0] dm_load;
		input reg [2:0] size;
		input reg [4:0] dest;
		input reg [4:0] base;
		input reg [11:0] offset;
		dm_load = {offset[11:0], base, size, dest, 7'h03};
	endfunction
	function automatic [31:0] dm_nop;
		input reg _sv2v_unused;
		dm_nop = 32'h00000013;
	endfunction
	function automatic [31:0] dm_slli;
		input reg [4:0] rd;
		input reg [4:0] rs1;
		input reg [5:0] shamt;
		dm_slli = {6'b000000, shamt[5:0], rs1, 3'h1, rd, 7'h13};
	endfunction
	function automatic [31:0] dm_srli;
		input reg [4:0] rd;
		input reg [4:0] rs1;
		input reg [5:0] shamt;
		dm_srli = {6'b000000, shamt[5:0], rs1, 3'h5, rd, 7'h13};
	endfunction
	function automatic [31:0] dm_store;
		input reg [2:0] size;
		input reg [4:0] src;
		input reg [4:0] base;
		input reg [11:0] offset;
		dm_store = {offset[11:5], src, base, size, offset[4:0], 7'h23};
	endfunction
	function automatic [31:0] sv2v_cast_32;
		input reg [31:0] inp;
		sv2v_cast_32 = inp;
	endfunction
	always @(*) begin : p_abstract_cmd_rom
		if (_sv2v_0)
			;
		unsupported_command = 1'b0;
		abstract_cmd[31-:32] = dm_illegal(0);
		abstract_cmd[63-:32] = (HasSndScratch ? dm_auipc(5'd10, 1'sb0) : dm_nop(0));
		abstract_cmd[95-:32] = (HasSndScratch ? dm_srli(5'd10, 5'd10, 6'd12) : dm_nop(0));
		abstract_cmd[127-:32] = (HasSndScratch ? dm_slli(5'd10, 5'd10, 6'd12) : dm_nop(0));
		abstract_cmd[159-:32] = dm_nop(0);
		abstract_cmd[191-:32] = dm_nop(0);
		abstract_cmd[223-:32] = dm_nop(0);
		abstract_cmd[255-:32] = dm_nop(0);
		abstract_cmd[287-:32] = (HasSndScratch ? dm_csrr(12'h7b3, 5'd10) : dm_nop(0));
		abstract_cmd[319-:32] = dm_ebreak(0);
		abstract_cmd[320+:192] = 1'sb0;
		(* full_case, parallel_case *)
		case (cmd_i[31-:8])
			8'h00: begin
				if (((sv2v_cast_32(ac_ar[22-:3]) < MaxAar) && ac_ar[17]) && ac_ar[16]) begin
					abstract_cmd[31-:32] = (HasSndScratch ? dm_csrw(12'h7b3, 5'd10) : dm_nop(0));
					if (ac_ar[15:14] != {2 {1'sb0}}) begin
						abstract_cmd[31-:32] = dm_ebreak(0);
						unsupported_command = 1'b1;
					end
					else if (((HasSndScratch && ac_ar[12]) && !ac_ar[5]) && (ac_ar[4:0] == 5'd10)) begin
						abstract_cmd[159-:32] = dm_csrw(12'h7b2, 5'd8);
						abstract_cmd[191-:32] = dm_load(ac_ar[22-:3], 5'd8, LoadBaseAddr, dm_DataAddr);
						abstract_cmd[223-:32] = dm_csrw(12'h7b3, 5'd8);
						abstract_cmd[255-:32] = dm_csrr(12'h7b2, 5'd8);
					end
					else if (ac_ar[12]) begin
						if (ac_ar[5])
							abstract_cmd[159-:32] = dm_float_load(ac_ar[22-:3], ac_ar[4:0], LoadBaseAddr, dm_DataAddr);
						else
							abstract_cmd[159-:32] = dm_load(ac_ar[22-:3], ac_ar[4:0], LoadBaseAddr, dm_DataAddr);
					end
					else begin
						abstract_cmd[159-:32] = dm_csrw(12'h7b2, 5'd8);
						abstract_cmd[191-:32] = dm_load(ac_ar[22-:3], 5'd8, LoadBaseAddr, dm_DataAddr);
						abstract_cmd[223-:32] = dm_csrw(ac_ar[11:0], 5'd8);
						abstract_cmd[255-:32] = dm_csrr(12'h7b2, 5'd8);
					end
				end
				else if (((sv2v_cast_32(ac_ar[22-:3]) < MaxAar) && ac_ar[17]) && !ac_ar[16]) begin
					abstract_cmd[31-:32] = (HasSndScratch ? dm_csrw(12'h7b3, LoadBaseAddr) : dm_nop(0));
					if (ac_ar[15:14] != {2 {1'sb0}}) begin
						abstract_cmd[31-:32] = dm_ebreak(0);
						unsupported_command = 1'b1;
					end
					else if (((HasSndScratch && ac_ar[12]) && !ac_ar[5]) && (ac_ar[4:0] == 5'd10)) begin
						abstract_cmd[159-:32] = dm_csrw(12'h7b2, 5'd8);
						abstract_cmd[191-:32] = dm_csrr(12'h7b3, 5'd8);
						abstract_cmd[223-:32] = dm_store(ac_ar[22-:3], 5'd8, LoadBaseAddr, dm_DataAddr);
						abstract_cmd[255-:32] = dm_csrr(12'h7b2, 5'd8);
					end
					else if (ac_ar[12]) begin
						if (ac_ar[5])
							abstract_cmd[159-:32] = dm_float_store(ac_ar[22-:3], ac_ar[4:0], LoadBaseAddr, dm_DataAddr);
						else
							abstract_cmd[159-:32] = dm_store(ac_ar[22-:3], ac_ar[4:0], LoadBaseAddr, dm_DataAddr);
					end
					else begin
						abstract_cmd[159-:32] = dm_csrw(12'h7b2, 5'd8);
						abstract_cmd[191-:32] = dm_csrr(ac_ar[11:0], 5'd8);
						abstract_cmd[223-:32] = dm_store(ac_ar[22-:3], 5'd8, LoadBaseAddr, dm_DataAddr);
						abstract_cmd[255-:32] = dm_csrr(12'h7b2, 5'd8);
					end
				end
				else if ((sv2v_cast_32(ac_ar[22-:3]) >= MaxAar) || (ac_ar[19] == 1'b1)) begin
					abstract_cmd[31-:32] = dm_ebreak(0);
					unsupported_command = 1'b1;
				end
				if (ac_ar[18] && !unsupported_command)
					abstract_cmd[319-:32] = dm_nop(0);
			end
			default: begin
				abstract_cmd[31-:32] = dm_ebreak(0);
				unsupported_command = 1'b1;
			end
		endcase
	end
	wire [63:0] rom_addr;
	function automatic [63:0] sv2v_cast_64;
		input reg [63:0] inp;
		sv2v_cast_64 = inp;
	endfunction
	assign rom_addr = sv2v_cast_64(addr_i);
	generate
		if (HasSndScratch) begin : gen_rom_snd_scratch
			debug_rom i_debug_rom(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.req_i(req_i),
				.addr_i(rom_addr),
				.rdata_o(rom_rdata)
			);
		end
		else begin : gen_rom_one_scratch
			debug_rom_one_scratch i_debug_rom(
				.clk_i(clk_i),
				.rst_ni(rst_ni),
				.req_i(req_i),
				.addr_i(rom_addr),
				.rdata_o(rom_rdata)
			);
		end
	endgenerate
	assign fwd_rom_d = addr_i[11:0] >= dm_HaltAddress[11:0];
	always @(posedge clk_i or negedge rst_ni) begin : p_regs
		if (!rst_ni) begin
			fwd_rom_q <= 1'b0;
			rdata_q <= 1'sb0;
			state_q <= 2'd0;
			word_enable32_q <= 1'b0;
		end
		else begin
			fwd_rom_q <= fwd_rom_d;
			rdata_q <= rdata_d;
			state_q <= state_d;
			word_enable32_q <= addr_i[2];
		end
	end
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni) begin
			halted_q <= 1'b0;
			resuming_q <= 1'b0;
		end
		else begin
			halted_q <= SelectableHarts & halted_d;
			resuming_q <= SelectableHarts & resuming_d;
		end
	initial _sv2v_0 = 0;
endmodule
