`default_nettype none
module dm_csrs (
	clk_i,
	rst_ni,
	testmode_i,
	dmi_rst_ni,
	dmi_req_valid_i,
	dmi_req_ready_o,
	dmi_req_i,
	dmi_resp_valid_o,
	dmi_resp_ready_i,
	dmi_resp_o,
	ndmreset_o,
	dmactive_o,
	hartinfo_i,
	halted_i,
	unavailable_i,
	resumeack_i,
	hartsel_o,
	haltreq_o,
	resumereq_o,
	clear_resumeack_o,
	cmd_valid_o,
	cmd_o,
	cmderror_valid_i,
	cmderror_i,
	cmdbusy_i,
	progbuf_o,
	data_o,
	data_i,
	data_valid_i,
	sbaddress_o,
	sbaddress_i,
	sbaddress_write_valid_o,
	sbreadonaddr_o,
	sbautoincrement_o,
	sbaccess_o,
	sbreadondata_o,
	sbdata_o,
	sbdata_read_valid_o,
	sbdata_write_valid_o,
	sbdata_i,
	sbdata_valid_i,
	sbbusy_i,
	sberror_valid_i,
	sberror_i
);
	reg _sv2v_0;
	parameter [31:0] NrHarts = 1;
	parameter [31:0] BusWidth = 32;
	parameter [NrHarts - 1:0] SelectableHarts = {NrHarts {1'b1}};
	input wire clk_i;
	input wire rst_ni;
	input wire testmode_i;
	input wire dmi_rst_ni;
	input wire dmi_req_valid_i;
	output wire dmi_req_ready_o;
	input wire [40:0] dmi_req_i;
	output wire dmi_resp_valid_o;
	input wire dmi_resp_ready_i;
	output wire [33:0] dmi_resp_o;
	output wire ndmreset_o;
	output wire dmactive_o;
	input wire [(NrHarts * 32) - 1:0] hartinfo_i;
	input wire [NrHarts - 1:0] halted_i;
	input wire [NrHarts - 1:0] unavailable_i;
	input wire [NrHarts - 1:0] resumeack_i;
	output wire [19:0] hartsel_o;
	output reg [NrHarts - 1:0] haltreq_o;
	output reg [NrHarts - 1:0] resumereq_o;
	output reg clear_resumeack_o;
	output wire cmd_valid_o;
	output wire [31:0] cmd_o;
	input wire cmderror_valid_i;
	input wire [2:0] cmderror_i;
	input wire cmdbusy_i;
	localparam [4:0] dm_ProgBufSize = 5'h08;
	output wire [255:0] progbuf_o;
	localparam [3:0] dm_DataCount = 4'h2;
	output wire [63:0] data_o;
	input wire [63:0] data_i;
	input wire data_valid_i;
	output wire [BusWidth - 1:0] sbaddress_o;
	input wire [BusWidth - 1:0] sbaddress_i;
	output reg sbaddress_write_valid_o;
	output wire sbreadonaddr_o;
	output wire sbautoincrement_o;
	output wire [2:0] sbaccess_o;
	output wire sbreadondata_o;
	output wire [BusWidth - 1:0] sbdata_o;
	output reg sbdata_read_valid_o;
	output reg sbdata_write_valid_o;
	input wire [BusWidth - 1:0] sbdata_i;
	input wire sbdata_valid_i;
	input wire sbbusy_i;
	input wire sberror_valid_i;
	input wire [2:0] sberror_i;
	localparam [31:0] HartSelLen = (NrHarts == 1 ? 1 : $clog2(NrHarts));
	localparam [31:0] NrHartsAligned = 2 ** HartSelLen;
	wire [1:0] dtm_op;
	function automatic [1:0] sv2v_cast_2;
		input reg [1:0] inp;
		sv2v_cast_2 = inp;
	endfunction
	assign dtm_op = sv2v_cast_2(dmi_req_i[33-:2]);
	wire resp_queue_full;
	wire resp_queue_empty;
	wire resp_queue_push;
	wire resp_queue_pop;
	function automatic [7:0] sv2v_cast_8;
		input reg [7:0] inp;
		sv2v_cast_8 = inp;
	endfunction
	localparam [7:0] DataEnd = sv2v_cast_8((8'h04 + {4'h0, dm_DataCount}) - 8'h01);
	localparam [7:0] ProgBufEnd = sv2v_cast_8((8'h20 + {4'h0, dm_ProgBufSize}) - 8'h01);
	reg [31:0] haltsum0;
	reg [31:0] haltsum1;
	reg [31:0] haltsum2;
	reg [31:0] haltsum3;
	reg [((((NrHarts - 1) / 32) + 1) * 32) - 1:0] halted;
	reg [(((NrHarts - 1) / 32) >= 0 ? ((((NrHarts - 1) / 32) + 1) * 32) - 1 : ((1 - ((NrHarts - 1) / 32)) * 32) + ((((NrHarts - 1) / 32) * 32) - 1)):(((NrHarts - 1) / 32) >= 0 ? 0 : ((NrHarts - 1) / 32) * 32)] halted_reshaped0;
	reg [(((NrHarts - 1) / 1024) >= 0 ? ((((NrHarts - 1) / 1024) + 1) * 32) - 1 : ((1 - ((NrHarts - 1) / 1024)) * 32) + ((((NrHarts - 1) / 1024) * 32) - 1)):(((NrHarts - 1) / 1024) >= 0 ? 0 : ((NrHarts - 1) / 1024) * 32)] halted_reshaped1;
	reg [(((NrHarts - 1) / 32768) >= 0 ? ((((NrHarts - 1) / 32768) + 1) * 32) - 1 : ((1 - ((NrHarts - 1) / 32768)) * 32) + ((((NrHarts - 1) / 32768) * 32) - 1)):(((NrHarts - 1) / 32768) >= 0 ? 0 : ((NrHarts - 1) / 32768) * 32)] halted_reshaped2;
	reg [((((NrHarts - 1) / 1024) + 1) * 32) - 1:0] halted_flat1;
	reg [((((NrHarts - 1) / 32768) + 1) * 32) - 1:0] halted_flat2;
	reg [31:0] halted_flat3;
	reg [14:0] hartsel_idx0;
	function automatic [14:0] sv2v_cast_15;
		input reg [14:0] inp;
		sv2v_cast_15 = inp;
	endfunction
	always @(*) begin : p_haltsum0
		if (_sv2v_0)
			;
		halted = 1'sb0;
		haltsum0 = 1'sb0;
		hartsel_idx0 = hartsel_o[19:5];
		halted[NrHarts - 1:0] = halted_i;
		halted_reshaped0 = halted;
		if (hartsel_idx0 < sv2v_cast_15(((NrHarts - 1) / 32) + 1))
			haltsum0 = halted_reshaped0[(((NrHarts - 1) / 32) >= 0 ? hartsel_idx0 : ((NrHarts - 1) / 32) - hartsel_idx0) * 32+:32];
	end
	reg [9:0] hartsel_idx1;
	function automatic [9:0] sv2v_cast_10;
		input reg [9:0] inp;
		sv2v_cast_10 = inp;
	endfunction
	always @(*) begin : p_reduction1
		if (_sv2v_0)
			;
		halted_flat1 = 1'sb0;
		haltsum1 = 1'sb0;
		hartsel_idx1 = hartsel_o[19:10];
		begin : sv2v_autoblock_1
			reg [31:0] k;
			for (k = 0; k < (((NrHarts - 1) / 32) + 1); k = k + 1)
				halted_flat1[k] = |halted_reshaped0[(((NrHarts - 1) / 32) >= 0 ? k : ((NrHarts - 1) / 32) - k) * 32+:32];
		end
		halted_reshaped1 = halted_flat1;
		if (hartsel_idx1 < sv2v_cast_10(((NrHarts - 1) / 1024) + 1))
			haltsum1 = halted_reshaped1[(((NrHarts - 1) / 1024) >= 0 ? hartsel_idx1 : ((NrHarts - 1) / 1024) - hartsel_idx1) * 32+:32];
	end
	reg [4:0] hartsel_idx2;
	function automatic [4:0] sv2v_cast_5;
		input reg [4:0] inp;
		sv2v_cast_5 = inp;
	endfunction
	always @(*) begin : p_reduction2
		if (_sv2v_0)
			;
		halted_flat2 = 1'sb0;
		haltsum2 = 1'sb0;
		hartsel_idx2 = hartsel_o[19:15];
		begin : sv2v_autoblock_2
			reg [31:0] k;
			for (k = 0; k < (((NrHarts - 1) / 1024) + 1); k = k + 1)
				halted_flat2[k] = |halted_reshaped1[(((NrHarts - 1) / 1024) >= 0 ? k : ((NrHarts - 1) / 1024) - k) * 32+:32];
		end
		halted_reshaped2 = halted_flat2;
		if (hartsel_idx2 < sv2v_cast_5(((NrHarts - 1) / 32768) + 1))
			haltsum2 = halted_reshaped2[(((NrHarts - 1) / 32768) >= 0 ? hartsel_idx2 : ((NrHarts - 1) / 32768) - hartsel_idx2) * 32+:32];
	end
	always @(*) begin : p_reduction3
		if (_sv2v_0)
			;
		halted_flat3 = 1'sb0;
		begin : sv2v_autoblock_3
			reg [31:0] k;
			for (k = 0; k < ((NrHarts / 32768) + 1); k = k + 1)
				halted_flat3[k] = |halted_reshaped2[(((NrHarts - 1) / 32768) >= 0 ? k : ((NrHarts - 1) / 32768) - k) * 32+:32];
		end
		haltsum3 = halted_flat3;
	end
	reg [31:0] dmstatus;
	reg [31:0] dmcontrol_d;
	reg [31:0] dmcontrol_q;
	reg [31:0] abstractcs;
	reg [2:0] cmderr_d;
	reg [2:0] cmderr_q;
	reg [31:0] command_d;
	reg [31:0] command_q;
	reg cmd_valid_d;
	reg cmd_valid_q;
	reg [31:0] abstractauto_d;
	reg [31:0] abstractauto_q;
	reg [31:0] sbcs_d;
	reg [31:0] sbcs_q;
	reg [63:0] sbaddr_d;
	reg [63:0] sbaddr_q;
	reg [63:0] sbdata_d;
	reg [63:0] sbdata_q;
	wire [NrHarts - 1:0] havereset_d;
	reg [NrHarts - 1:0] havereset_q;
	reg [255:0] progbuf_d;
	reg [255:0] progbuf_q;
	reg [63:0] data_d;
	reg [63:0] data_q;
	reg [HartSelLen - 1:0] selected_hart;
	reg [33:0] resp_queue_inp;
	assign dmi_resp_valid_o = ~resp_queue_empty;
	assign dmi_req_ready_o = ~resp_queue_full;
	assign resp_queue_push = dmi_req_valid_i & dmi_req_ready_o;
	assign sbautoincrement_o = sbcs_q[16];
	assign sbreadonaddr_o = sbcs_q[20];
	assign sbreadondata_o = sbcs_q[15];
	assign sbaccess_o = sbcs_q[19-:3];
	assign sbdata_o = sbdata_q[BusWidth - 1:0];
	assign sbaddress_o = sbaddr_q[BusWidth - 1:0];
	assign hartsel_o = {dmcontrol_q[15-:10], dmcontrol_q[25-:10]};
	reg [NrHartsAligned - 1:0] havereset_d_aligned;
	wire [NrHartsAligned - 1:0] havereset_q_aligned;
	wire [NrHartsAligned - 1:0] resumeack_aligned;
	wire [NrHartsAligned - 1:0] unavailable_aligned;
	wire [NrHartsAligned - 1:0] halted_aligned;
	function automatic [NrHartsAligned - 1:0] sv2v_cast_DFF07;
		input reg [NrHartsAligned - 1:0] inp;
		sv2v_cast_DFF07 = inp;
	endfunction
	assign resumeack_aligned = sv2v_cast_DFF07(resumeack_i);
	assign unavailable_aligned = sv2v_cast_DFF07(unavailable_i);
	assign halted_aligned = sv2v_cast_DFF07(halted_i);
	function automatic [NrHarts - 1:0] sv2v_cast_178F2;
		input reg [NrHarts - 1:0] inp;
		sv2v_cast_178F2 = inp;
	endfunction
	assign havereset_d = sv2v_cast_178F2(havereset_d_aligned);
	assign havereset_q_aligned = sv2v_cast_DFF07(havereset_q);
	reg [(NrHartsAligned * 32) - 1:0] hartinfo_aligned;
	always @(*) begin : p_hartinfo_align
		if (_sv2v_0)
			;
		hartinfo_aligned = 1'sb0;
		hartinfo_aligned[32 * ((NrHarts - 1) - (NrHarts - 1))+:32 * NrHarts] = hartinfo_i;
	end
	wire [7:0] dm_csr_addr;
	reg [31:0] sbcs;
	reg [31:0] a_abstractcs;
	wire [3:0] autoexecdata_idx;
	assign dm_csr_addr = sv2v_cast_8({1'b0, dmi_req_i[40-:7]});
	function automatic [3:0] sv2v_cast_4;
		input reg [3:0] inp;
		sv2v_cast_4 = inp;
	endfunction
	assign autoexecdata_idx = sv2v_cast_4({dm_csr_addr} - 8'h04);
	localparam [3:0] dm_DbgVersion013 = 4'h2;
	function automatic [31:0] sv2v_cast_32;
		input reg [31:0] inp;
		sv2v_cast_32 = inp;
	endfunction
	function automatic [63:0] sv2v_cast_64;
		input reg [63:0] inp;
		sv2v_cast_64 = inp;
	endfunction
	function automatic [$clog2(4'h2) - 1:0] sv2v_cast_68FD0;
		input reg [$clog2(4'h2) - 1:0] inp;
		sv2v_cast_68FD0 = inp;
	endfunction
	function automatic [2:0] sv2v_cast_3;
		input reg [2:0] inp;
		sv2v_cast_3 = inp;
	endfunction
	function automatic [11:0] sv2v_cast_12;
		input reg [11:0] inp;
		sv2v_cast_12 = inp;
	endfunction
	function automatic [15:0] sv2v_cast_16;
		input reg [15:0] inp;
		sv2v_cast_16 = inp;
	endfunction
	function automatic [6:0] sv2v_cast_1B50F;
		input reg [6:0] inp;
		sv2v_cast_1B50F = inp;
	endfunction
	always @(*) begin
		if (_sv2v_0)
			;
		(* xprop_off *)
		begin : csr_read_write
			dmstatus = 1'sb0;
			dmstatus[3-:4] = dm_DbgVersion013;
			dmstatus[7] = 1'b1;
			dmstatus[5] = 1'b0;
			dmstatus[19] = havereset_q_aligned[selected_hart];
			dmstatus[18] = havereset_q_aligned[selected_hart];
			dmstatus[17] = resumeack_aligned[selected_hart];
			dmstatus[16] = resumeack_aligned[selected_hart];
			dmstatus[13] = unavailable_aligned[selected_hart];
			dmstatus[12] = unavailable_aligned[selected_hart];
			dmstatus[15] = sv2v_cast_32(hartsel_o) > (NrHarts - 32'sd1);
			dmstatus[14] = sv2v_cast_32(hartsel_o) > (NrHarts - 32'sd1);
			dmstatus[9] = halted_aligned[selected_hart] & ~unavailable_aligned[selected_hart];
			dmstatus[8] = halted_aligned[selected_hart] & ~unavailable_aligned[selected_hart];
			dmstatus[11] = ~halted_aligned[selected_hart] & ~unavailable_aligned[selected_hart];
			dmstatus[10] = ~halted_aligned[selected_hart] & ~unavailable_aligned[selected_hart];
			abstractcs = 1'sb0;
			abstractcs[3-:4] = dm_DataCount;
			abstractcs[28-:5] = dm_ProgBufSize;
			abstractcs[12] = cmdbusy_i;
			abstractcs[10-:3] = cmderr_q;
			abstractauto_d = abstractauto_q;
			abstractauto_d[15-:4] = 1'sb0;
			havereset_d_aligned = sv2v_cast_DFF07(havereset_q);
			dmcontrol_d = dmcontrol_q;
			cmderr_d = cmderr_q;
			command_d = command_q;
			progbuf_d = progbuf_q;
			data_d = data_q;
			sbcs_d = sbcs_q;
			sbaddr_d = sv2v_cast_64(sbaddress_i);
			sbdata_d = sbdata_q;
			resp_queue_inp[33-:32] = 32'h00000000;
			resp_queue_inp[1-:2] = 2'h0;
			cmd_valid_d = 1'b0;
			sbaddress_write_valid_o = 1'b0;
			sbdata_read_valid_o = 1'b0;
			sbdata_write_valid_o = 1'b0;
			clear_resumeack_o = 1'b0;
			sbcs = 1'sb0;
			a_abstractcs = 1'sb0;
			if ((dmi_req_ready_o && dmi_req_valid_i) && (dtm_op == 2'h1)) begin
				(* full_case, parallel_case *)
				if ((8'h04 <= dm_csr_addr) && (DataEnd >= dm_csr_addr)) begin
					resp_queue_inp[33-:32] = data_q[sv2v_cast_68FD0(autoexecdata_idx) * 32+:32];
					if (!cmdbusy_i)
						cmd_valid_d = abstractauto_q[0 + autoexecdata_idx];
					else begin
						resp_queue_inp[1-:2] = 2'h3;
						if (cmderr_q == 3'd0)
							cmderr_d = 3'd1;
					end
				end
				else if (dm_csr_addr == 8'h10)
					resp_queue_inp[33-:32] = dmcontrol_q;
				else if (dm_csr_addr == 8'h11)
					resp_queue_inp[33-:32] = dmstatus;
				else if (dm_csr_addr == 8'h12)
					resp_queue_inp[33-:32] = hartinfo_aligned[selected_hart * 32+:32];
				else if (dm_csr_addr == 8'h16)
					resp_queue_inp[33-:32] = abstractcs;
				else if (dm_csr_addr == 8'h18)
					resp_queue_inp[33-:32] = abstractauto_q;
				else if (dm_csr_addr == 8'h17)
					resp_queue_inp[33-:32] = 1'sb0;
				else if ((8'h20 <= dm_csr_addr) && (ProgBufEnd >= dm_csr_addr)) begin
					resp_queue_inp[33-:32] = progbuf_q[dmi_req_i[$clog2(5'h08) + 33:34] * 32+:32];
					if (!cmdbusy_i)
						cmd_valid_d = abstractauto_q[0 + {1'b1, dmi_req_i[37:34]}];
					else begin
						resp_queue_inp[1-:2] = 2'h3;
						if (cmderr_q == 3'd0)
							cmderr_d = 3'd1;
					end
				end
				else if (dm_csr_addr == 8'h40)
					resp_queue_inp[33-:32] = haltsum0;
				else if (dm_csr_addr == 8'h13)
					resp_queue_inp[33-:32] = haltsum1;
				else if (dm_csr_addr == 8'h34)
					resp_queue_inp[33-:32] = haltsum2;
				else if (dm_csr_addr == 8'h35)
					resp_queue_inp[33-:32] = haltsum3;
				else if (dm_csr_addr == 8'h38)
					resp_queue_inp[33-:32] = sbcs_q;
				else if (dm_csr_addr == 8'h39)
					resp_queue_inp[33-:32] = sbaddr_q[31:0];
				else if (dm_csr_addr == 8'h3a)
					resp_queue_inp[33-:32] = sbaddr_q[63:32];
				else if (dm_csr_addr == 8'h3c) begin
					if (sbbusy_i || sbcs_q[22]) begin
						sbcs_d[22] = 1'b1;
						resp_queue_inp[1-:2] = 2'h3;
					end
					else begin
						sbdata_read_valid_o = sbcs_q[14-:3] == {3 {1'sb0}};
						resp_queue_inp[33-:32] = sbdata_q[31:0];
					end
				end
				else if (dm_csr_addr == 8'h3d) begin
					if (sbbusy_i || sbcs_q[22]) begin
						sbcs_d[22] = 1'b1;
						resp_queue_inp[1-:2] = 2'h3;
					end
					else
						resp_queue_inp[33-:32] = sbdata_q[63:32];
				end
			end
			if ((dmi_req_ready_o && dmi_req_valid_i) && (dtm_op == 2'h2)) begin
				(* full_case, parallel_case *)
				if ((8'h04 <= dm_csr_addr) && (DataEnd >= dm_csr_addr)) begin
					if (!cmdbusy_i) begin
						data_d[dmi_req_i[$clog2(4'h2) + 33:34] * 32+:32] = dmi_req_i[31-:32];
						cmd_valid_d = abstractauto_q[0 + autoexecdata_idx];
					end
					else begin
						resp_queue_inp[1-:2] = 2'h3;
						if (cmderr_q == 3'd0)
							cmderr_d = 3'd1;
					end
				end
				else if (dm_csr_addr == 8'h10) begin
					dmcontrol_d = dmi_req_i[31-:32];
					if (dmcontrol_d[28])
						havereset_d_aligned[selected_hart] = 1'b0;
				end
				else if (dm_csr_addr == 8'h11)
					;
				else if (dm_csr_addr == 8'h12)
					;
				else if (dm_csr_addr == 8'h16) begin
					a_abstractcs = sv2v_cast_32(dmi_req_i[31-:32]);
					if (!cmdbusy_i)
						cmderr_d = sv2v_cast_3(~a_abstractcs[10-:3] & cmderr_q);
					else begin
						resp_queue_inp[1-:2] = 2'h3;
						if (cmderr_q == 3'd0)
							cmderr_d = 3'd1;
					end
				end
				else if (dm_csr_addr == 8'h17) begin
					if (!cmdbusy_i) begin
						cmd_valid_d = 1'b1;
						command_d = sv2v_cast_32(dmi_req_i[31-:32]);
					end
					else begin
						resp_queue_inp[1-:2] = 2'h3;
						if (cmderr_q == 3'd0)
							cmderr_d = 3'd1;
					end
				end
				else if (dm_csr_addr == 8'h18) begin
					if (!cmdbusy_i) begin
						abstractauto_d = 32'h00000000;
						abstractauto_d[11-:12] = sv2v_cast_12(dmi_req_i[1:0]);
						abstractauto_d[31-:16] = sv2v_cast_16(dmi_req_i[23:16]);
					end
					else begin
						resp_queue_inp[1-:2] = 2'h3;
						if (cmderr_q == 3'd0)
							cmderr_d = 3'd1;
					end
				end
				else if ((8'h20 <= dm_csr_addr) && (ProgBufEnd >= dm_csr_addr)) begin
					if (!cmdbusy_i) begin
						progbuf_d[dmi_req_i[$clog2(5'h08) + 33:34] * 32+:32] = dmi_req_i[31-:32];
						cmd_valid_d = abstractauto_q[0 + {1'b1, dmi_req_i[37:34]}];
					end
					else begin
						resp_queue_inp[1-:2] = 2'h3;
						if (cmderr_q == 3'd0)
							cmderr_d = 3'd1;
					end
				end
				else if (dm_csr_addr == 8'h38) begin
					if (sbbusy_i) begin
						sbcs_d[22] = 1'b1;
						resp_queue_inp[1-:2] = 2'h3;
					end
					else begin
						sbcs = sv2v_cast_32(dmi_req_i[31-:32]);
						sbcs_d = sbcs;
						sbcs_d[22] = sbcs_q[22] & ~sbcs[22];
						sbcs_d[14-:3] = (|sbcs[14-:3] ? 3'b000 : sbcs_q[14-:3]);
					end
				end
				else if (dm_csr_addr == 8'h39) begin
					if (sbbusy_i || sbcs_q[22]) begin
						sbcs_d[22] = 1'b1;
						resp_queue_inp[1-:2] = 2'h3;
					end
					else begin
						sbaddr_d[31:0] = dmi_req_i[31-:32];
						sbaddress_write_valid_o = sbcs_q[14-:3] == {3 {1'sb0}};
					end
				end
				else if (dm_csr_addr == 8'h3a) begin
					if (sbbusy_i || sbcs_q[22]) begin
						sbcs_d[22] = 1'b1;
						resp_queue_inp[1-:2] = 2'h3;
					end
					else
						sbaddr_d[63:32] = dmi_req_i[31-:32];
				end
				else if (dm_csr_addr == 8'h3c) begin
					if (sbbusy_i || sbcs_q[22]) begin
						sbcs_d[22] = 1'b1;
						resp_queue_inp[1-:2] = 2'h3;
					end
					else begin
						sbdata_d[31:0] = dmi_req_i[31-:32];
						sbdata_write_valid_o = sbcs_q[14-:3] == {3 {1'sb0}};
					end
				end
				else if (dm_csr_addr == 8'h3d) begin
					if (sbbusy_i || sbcs_q[22]) begin
						sbcs_d[22] = 1'b1;
						resp_queue_inp[1-:2] = 2'h3;
					end
					else
						sbdata_d[63:32] = dmi_req_i[31-:32];
				end
			end
			if (cmderror_valid_i)
				cmderr_d = cmderror_i;
			if (data_valid_i)
				data_d = data_i;
			if (ndmreset_o)
				havereset_d_aligned[NrHarts - 1:0] = 1'sb1;
			if (sberror_valid_i)
				sbcs_d[14-:3] = sberror_i;
			if (sbdata_valid_i)
				sbdata_d = sv2v_cast_64(sbdata_i);
			dmcontrol_d[26] = 1'b0;
			dmcontrol_d[29] = 1'b0;
			dmcontrol_d[3] = 1'b0;
			dmcontrol_d[2] = 1'b0;
			dmcontrol_d[27] = 1'sb0;
			dmcontrol_d[5-:2] = 1'sb0;
			dmcontrol_d[28] = 1'b0;
			if (!dmcontrol_q[30] && dmcontrol_d[30])
				clear_resumeack_o = 1'b1;
			if (dmcontrol_q[30] && resumeack_i)
				dmcontrol_d[30] = 1'b0;
			sbcs_d[31-:3] = 3'd1;
			sbcs_d[21] = sbbusy_i;
			sbcs_d[11-:7] = sv2v_cast_1B50F(BusWidth);
			sbcs_d[4] = BusWidth >= 32'd128;
			sbcs_d[3] = BusWidth >= 32'd64;
			sbcs_d[2] = BusWidth >= 32'd32;
			sbcs_d[1] = BusWidth >= 32'd16;
			sbcs_d[0] = BusWidth >= 32'd8;
		end
	end
	function automatic [HartSelLen - 1:0] sv2v_cast_FFD0D;
		input reg [HartSelLen - 1:0] inp;
		sv2v_cast_FFD0D = inp;
	endfunction
	always @(*) begin : p_outmux
		if (_sv2v_0)
			;
		selected_hart = hartsel_o[HartSelLen - 1:0];
		haltreq_o = 1'sb0;
		resumereq_o = 1'sb0;
		if (selected_hart <= sv2v_cast_FFD0D(NrHarts - 1)) begin
			haltreq_o[selected_hart] = dmcontrol_q[31];
			resumereq_o[selected_hart] = dmcontrol_q[30];
		end
	end
	assign dmactive_o = dmcontrol_q[0];
	assign cmd_o = command_q;
	assign cmd_valid_o = cmd_valid_q;
	assign progbuf_o = progbuf_q;
	assign data_o = data_q;
	assign resp_queue_pop = dmi_resp_ready_i & ~resp_queue_empty;
	assign ndmreset_o = dmcontrol_q[1];
	fifo_v2_EBA1F #(.DEPTH(2)) i_fifo(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.flush_i(~dmi_rst_ni),
		.testmode_i(testmode_i),
		.full_o(resp_queue_full),
		.empty_o(resp_queue_empty),
		.alm_full_o(),
		.alm_empty_o(),
		.data_i(resp_queue_inp),
		.push_i(resp_queue_push),
		.data_o(dmi_resp_o),
		.pop_i(resp_queue_pop)
	);
	always @(posedge clk_i or negedge rst_ni) begin : p_regs
		if (!rst_ni) begin
			dmcontrol_q <= 1'sb0;
			cmderr_q <= 3'd0;
			command_q <= 1'sb0;
			cmd_valid_q <= 1'sb0;
			abstractauto_q <= 1'sb0;
			progbuf_q <= 1'sb0;
			data_q <= 1'sb0;
			sbcs_q <= 32'h00040000;
			sbaddr_q <= 1'sb0;
			sbdata_q <= 1'sb0;
			havereset_q <= 1'sb1;
		end
		else begin
			havereset_q <= SelectableHarts & havereset_d;
			if (!dmcontrol_q[0]) begin
				dmcontrol_q[31] <= 1'sb0;
				dmcontrol_q[30] <= 1'sb0;
				dmcontrol_q[29] <= 1'sb0;
				dmcontrol_q[28] <= 1'sb0;
				dmcontrol_q[27] <= 1'sb0;
				dmcontrol_q[26] <= 1'sb0;
				dmcontrol_q[25-:10] <= 1'sb0;
				dmcontrol_q[15-:10] <= 1'sb0;
				dmcontrol_q[5-:2] <= 1'sb0;
				dmcontrol_q[3] <= 1'sb0;
				dmcontrol_q[2] <= 1'sb0;
				dmcontrol_q[1] <= 1'sb0;
				dmcontrol_q[0] <= dmcontrol_d[0];
				cmderr_q <= 3'd0;
				command_q <= 1'sb0;
				cmd_valid_q <= 1'sb0;
				abstractauto_q <= 1'sb0;
				progbuf_q <= 1'sb0;
				data_q <= 1'sb0;
				sbcs_q <= 32'h00040000;
				sbaddr_q <= 1'sb0;
				sbdata_q <= 1'sb0;
			end
			else begin
				dmcontrol_q <= dmcontrol_d;
				cmderr_q <= cmderr_d;
				command_q <= command_d;
				cmd_valid_q <= cmd_valid_d;
				abstractauto_q <= abstractauto_d;
				progbuf_q <= progbuf_d;
				data_q <= data_d;
				sbcs_q <= sbcs_d;
				sbaddr_q <= sbaddr_d;
				sbdata_q <= sbdata_d;
			end
		end
	end
	initial _sv2v_0 = 0;
endmodule
