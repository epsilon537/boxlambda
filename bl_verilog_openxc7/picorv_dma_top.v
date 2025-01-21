`default_nettype none
module picorv_dma_top (
	sys_clk,
	sys_clkx2,
	rst,
	wbm_adr_o,
	wbm_dat_o,
	wbm_dat_i,
	wbm_we_o,
	wbm_sel_o,
	wbm_stb_o,
	wbm_ack_i,
	wbm_stall_i,
	wbm_cyc_o,
	wbm_err_i,
	wbs_adr,
	wbs_dat_w,
	wbs_dat_r,
	wbs_sel,
	wbs_stall,
	wbs_cyc,
	wbs_stb,
	wbs_ack,
	wbs_we,
	wbs_err,
	irq_in,
	irq_out
);
	reg _sv2v_0;
	parameter BASE_ADDR = 32'h10002000;
	input wire sys_clk;
	input wire sys_clkx2;
	input wire rst;
	output wire [27:0] wbm_adr_o;
	output wire [31:0] wbm_dat_o;
	input wire [31:0] wbm_dat_i;
	output wire wbm_we_o;
	output wire [3:0] wbm_sel_o;
	output wire wbm_stb_o;
	input wire wbm_ack_i;
	input wire wbm_stall_i;
	output wire wbm_cyc_o;
	input wire wbm_err_i;
	input wire [10:0] wbs_adr;
	input wire [31:0] wbs_dat_w;
	output wire [31:0] wbs_dat_r;
	input wire [3:0] wbs_sel;
	output wire wbs_stall;
	input wire wbs_cyc;
	input wire wbs_stb;
	output wire wbs_ack;
	input wire wbs_we;
	output wire wbs_err;
	input wire [31:0] irq_in;
	output wire irq_out;
	localparam integer MEM_SZ_WORDS = 1024;
	localparam integer REG_SZ_WORDS = 32;
	localparam integer WBS_REG_BASE_ADDR = MEM_SZ_WORDS;
	localparam integer PICO_REG_BASE_ADDR = BASE_ADDR + (MEM_SZ_WORDS * 4);
	localparam integer PICO_MEM_BASE_ADDR = BASE_ADDR;
	localparam integer BURST_REG_BASE_ADDR = PICO_REG_BASE_ADDR + (REG_SZ_WORDS * 4);
	wire trap;
	wire burst_fsm_valid;
	reg burst_fsm_ready;
	wire [3:0] burst_fsm_wstrb;
	wire [31:2] burst_fsm_addr;
	wire [31:0] burst_fsm_wdata;
	reg [31:0] burst_fsm_rdata;
	wire reg_we;
	wire reg_valid;
	reg reg_ready;
	wire [3:0] reg_wstrb;
	wire [31:0] reg_addr;
	wire [31:0] reg_wdata;
	reg [31:0] reg_rdata;
	wire mem_valid;
	wire mem_ready;
	wire [31:0] mem_addr;
	wire [31:0] mem_wdata;
	wire [3:0] mem_wstrb;
	wire [31:0] mem_rdata;
	wire ram_valid;
	reg ram_ready;
	wire [3:0] ram_wstrb;
	wire [11:2] ram_addr;
	wire [31:0] ram_wdata;
	wire [31:0] ram_rdata;
	reg [31:0] wbs_ram_rdata;
	reg [31:0] gp_reg [0:15];
	reg [31:0] ctrl_reg;
	reg [31:0] irq_in_reg;
	reg [31:0] irq_out_reg;
	reg [31:0] irq_out_next;
	wire picorv_rst_n;
	reg do_ack_wbs;
	wire do_wbs_wr_mem;
	wire do_wbs_wr_reg;
	reg [31:0] wbs_dat_read_from_reg;
	reg unused = &{wbs_sel, burst_fsm_addr, wbm_err_i};
	function automatic signed [10:0] sv2v_cast_11_signed;
		input reg signed [10:0] inp;
		sv2v_cast_11_signed = inp;
	endfunction
	assign do_wbs_wr_reg = ((wbs_cyc && wbs_stb) && wbs_we) && (wbs_adr >= sv2v_cast_11_signed(WBS_REG_BASE_ADDR));
	assign do_wbs_wr_mem = ((wbs_cyc && wbs_stb) && wbs_we) && (wbs_adr < sv2v_cast_11_signed(WBS_REG_BASE_ADDR));
	always @(posedge sys_clk) begin
		do_ack_wbs <= 1'b0;
		if (wbs_stb) begin
			wbs_ram_rdata <= ram_rdata;
			do_ack_wbs <= 1'b1;
		end
	end
	assign wbs_dat_r = (wbs_adr < sv2v_cast_11_signed(WBS_REG_BASE_ADDR) ? wbs_ram_rdata : wbs_dat_read_from_reg);
	assign wbs_ack = do_ack_wbs & wbs_cyc;
	assign wbs_stall = 1'b0;
	assign wbs_err = 1'b0;
	assign reg_we = |reg_wstrb;
	assign reg_valid = mem_valid && ((mem_addr >= PICO_REG_BASE_ADDR) && (mem_addr < (PICO_REG_BASE_ADDR + (4 * REG_SZ_WORDS))));
	assign reg_wstrb = mem_wstrb;
	assign reg_addr = mem_addr;
	assign reg_wdata = mem_wdata;
	always @(posedge sys_clk) reg_ready <= ((reg_valid && !reg_ready) && (reg_addr >= PICO_REG_BASE_ADDR)) && (reg_addr < (PICO_REG_BASE_ADDR + (4 * REG_SZ_WORDS)));
	assign ram_valid = mem_valid && ((mem_addr >= PICO_MEM_BASE_ADDR) && (mem_addr < PICO_REG_BASE_ADDR));
	assign ram_wstrb = mem_wstrb;
	assign ram_addr = mem_addr[11:2];
	assign ram_wdata = mem_wdata;
	assign burst_fsm_valid = (mem_valid && ~reg_valid) && ~ram_valid;
	assign burst_fsm_wstrb = mem_wstrb;
	assign burst_fsm_addr = mem_addr[31:2];
	assign burst_fsm_wdata = mem_wdata;
	always @(posedge sys_clkx2)
		if (picorv_rst_n)
			ram_ready <= ram_valid && !mem_ready;
		else
			ram_ready <= 1'b0;
	assign mem_ready = ((reg_valid && reg_ready) || (burst_fsm_valid && burst_fsm_ready)) || (ram_valid && ram_ready);
	assign mem_rdata = (reg_valid && reg_ready ? reg_rdata : (burst_fsm_valid && burst_fsm_ready ? burst_fsm_rdata : (ram_valid && ram_ready ? ram_rdata : 32'h00000000)));
	assign picorv_rst_n = ctrl_reg[0];
	assign irq_out = |irq_out_reg;
	always @(*) begin
		if (_sv2v_0)
			;
		begin : sv2v_autoblock_1
			reg signed [31:0] i;
			for (i = 0; i < 32; i = i + 1)
				begin
					irq_out_next[i] = irq_out_reg[i];
					if ((do_wbs_wr_reg && (wbs_adr == sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 0))) && wbs_dat_w[i])
						irq_out_next[i] = 1'b0;
				end
		end
		begin : sv2v_autoblock_2
			reg signed [31:0] i;
			for (i = 0; i < 31; i = i + 1)
				if (((reg_we && reg_valid) && (reg_addr == (PICO_REG_BASE_ADDR + 0))) && reg_wdata[i])
					irq_out_next[i] = 1'b1;
		end
		if (trap)
			irq_out_next[31] = 1'b1;
	end
	always @(posedge sys_clk)
		if (rst) begin
			gp_reg[0] <= 32'b00000000000000000000000000000000;
			gp_reg[1] <= 32'b00000000000000000000000000000000;
			gp_reg[2] <= 32'b00000000000000000000000000000000;
			gp_reg[3] <= 32'b00000000000000000000000000000000;
			gp_reg[4] <= 32'b00000000000000000000000000000000;
			gp_reg[5] <= 32'b00000000000000000000000000000000;
			gp_reg[6] <= 32'b00000000000000000000000000000000;
			gp_reg[7] <= 32'b00000000000000000000000000000000;
			gp_reg[8] <= 32'b00000000000000000000000000000000;
			gp_reg[9] <= 32'b00000000000000000000000000000000;
			gp_reg[10] <= 32'b00000000000000000000000000000000;
			gp_reg[11] <= 32'b00000000000000000000000000000000;
			gp_reg[12] <= 32'b00000000000000000000000000000000;
			gp_reg[13] <= 32'b00000000000000000000000000000000;
			gp_reg[14] <= 32'b00000000000000000000000000000000;
			gp_reg[15] <= 32'b00000000000000000000000000000000;
			ctrl_reg <= 32'b00000000000000000000000000000000;
			irq_in_reg <= 32'b00000000000000000000000000000000;
			irq_out_reg <= 32'b00000000000000000000000000000000;
		end
		else begin
			irq_in_reg <= irq_in;
			irq_out_reg <= irq_out_next;
			if (do_wbs_wr_reg)
				case (wbs_adr)
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 2): ctrl_reg <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 16): gp_reg[0] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 17): gp_reg[1] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 18): gp_reg[2] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 19): gp_reg[3] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 20): gp_reg[4] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 21): gp_reg[5] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 22): gp_reg[6] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 23): gp_reg[7] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 24): gp_reg[8] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 25): gp_reg[9] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 26): gp_reg[10] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 27): gp_reg[11] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 28): gp_reg[12] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 29): gp_reg[13] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 30): gp_reg[14] <= wbs_dat_w;
					sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 31): gp_reg[15] <= wbs_dat_w;
					default:
						;
				endcase
			if (reg_we && reg_valid)
				case (reg_addr)
					PICO_REG_BASE_ADDR + 64: gp_reg[0] <= reg_wdata;
					PICO_REG_BASE_ADDR + 68: gp_reg[1] <= reg_wdata;
					PICO_REG_BASE_ADDR + 72: gp_reg[2] <= reg_wdata;
					PICO_REG_BASE_ADDR + 76: gp_reg[3] <= reg_wdata;
					PICO_REG_BASE_ADDR + 80: gp_reg[4] <= reg_wdata;
					PICO_REG_BASE_ADDR + 84: gp_reg[5] <= reg_wdata;
					PICO_REG_BASE_ADDR + 88: gp_reg[6] <= reg_wdata;
					PICO_REG_BASE_ADDR + 92: gp_reg[7] <= reg_wdata;
					PICO_REG_BASE_ADDR + 96: gp_reg[8] <= reg_wdata;
					PICO_REG_BASE_ADDR + 100: gp_reg[9] <= reg_wdata;
					PICO_REG_BASE_ADDR + 104: gp_reg[10] <= reg_wdata;
					PICO_REG_BASE_ADDR + 108: gp_reg[11] <= reg_wdata;
					PICO_REG_BASE_ADDR + 112: gp_reg[12] <= reg_wdata;
					PICO_REG_BASE_ADDR + 116: gp_reg[13] <= reg_wdata;
					PICO_REG_BASE_ADDR + 120: gp_reg[14] <= reg_wdata;
					PICO_REG_BASE_ADDR + 124: gp_reg[15] <= reg_wdata;
					default:
						;
				endcase
		end
	always @(*) begin
		if (_sv2v_0)
			;
		case (wbs_adr)
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR): wbs_dat_read_from_reg = irq_out_reg;
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 1): wbs_dat_read_from_reg = irq_in_reg;
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 2): wbs_dat_read_from_reg = ctrl_reg;
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 16): wbs_dat_read_from_reg = gp_reg[0];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 17): wbs_dat_read_from_reg = gp_reg[1];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 18): wbs_dat_read_from_reg = gp_reg[2];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 19): wbs_dat_read_from_reg = gp_reg[3];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 20): wbs_dat_read_from_reg = gp_reg[4];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 21): wbs_dat_read_from_reg = gp_reg[5];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 22): wbs_dat_read_from_reg = gp_reg[6];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 23): wbs_dat_read_from_reg = gp_reg[7];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 24): wbs_dat_read_from_reg = gp_reg[8];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 25): wbs_dat_read_from_reg = gp_reg[9];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 26): wbs_dat_read_from_reg = gp_reg[10];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 27): wbs_dat_read_from_reg = gp_reg[11];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 28): wbs_dat_read_from_reg = gp_reg[12];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 29): wbs_dat_read_from_reg = gp_reg[13];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 30): wbs_dat_read_from_reg = gp_reg[14];
			sv2v_cast_11_signed(WBS_REG_BASE_ADDR + 31): wbs_dat_read_from_reg = gp_reg[15];
			default: wbs_dat_read_from_reg = 32'd0;
		endcase
		case (reg_addr)
			PICO_REG_BASE_ADDR + 4: reg_rdata = irq_in_reg;
			PICO_REG_BASE_ADDR + 64: reg_rdata = gp_reg[0];
			PICO_REG_BASE_ADDR + 68: reg_rdata = gp_reg[1];
			PICO_REG_BASE_ADDR + 72: reg_rdata = gp_reg[2];
			PICO_REG_BASE_ADDR + 76: reg_rdata = gp_reg[3];
			PICO_REG_BASE_ADDR + 80: reg_rdata = gp_reg[4];
			PICO_REG_BASE_ADDR + 84: reg_rdata = gp_reg[5];
			PICO_REG_BASE_ADDR + 88: reg_rdata = gp_reg[6];
			PICO_REG_BASE_ADDR + 92: reg_rdata = gp_reg[7];
			PICO_REG_BASE_ADDR + 96: reg_rdata = gp_reg[8];
			PICO_REG_BASE_ADDR + 100: reg_rdata = gp_reg[9];
			PICO_REG_BASE_ADDR + 104: reg_rdata = gp_reg[10];
			PICO_REG_BASE_ADDR + 108: reg_rdata = gp_reg[11];
			PICO_REG_BASE_ADDR + 112: reg_rdata = gp_reg[12];
			PICO_REG_BASE_ADDR + 116: reg_rdata = gp_reg[13];
			PICO_REG_BASE_ADDR + 120: reg_rdata = gp_reg[14];
			PICO_REG_BASE_ADDR + 124: reg_rdata = gp_reg[15];
			default: reg_rdata = 32'd0;
		endcase
	end
	picorv32 #(
		.ENABLE_COUNTERS(1),
		.ENABLE_COUNTERS64(0),
		.ENABLE_REGS_16_31(1),
		.ENABLE_REGS_DUALPORT(1),
		.LATCHED_MEM_RDATA(0),
		.TWO_STAGE_SHIFT(0),
		.BARREL_SHIFTER(1),
		.TWO_CYCLE_COMPARE(0),
		.TWO_CYCLE_ALU(0),
		.COMPRESSED_ISA(0),
		.CATCH_MISALIGN(0),
		.CATCH_ILLINSN(1),
		.ENABLE_PCPI(0),
		.ENABLE_MUL(0),
		.ENABLE_FAST_MUL(0),
		.ENABLE_DIV(0),
		.ENABLE_IRQ(0),
		.ENABLE_IRQ_QREGS(0),
		.ENABLE_IRQ_TIMER(0),
		.ENABLE_TRACE(0),
		.REGS_INIT_ZERO(0),
		.MASKED_IRQ(32'h00000000),
		.LATCHED_IRQ(32'hffffffff),
		.PROGADDR_RESET(PICO_MEM_BASE_ADDR),
		.PROGADDR_IRQ(32'h00000000),
		.STACKADDR((PICO_MEM_BASE_ADDR + (MEM_SZ_WORDS * 4)) - 32'h00000004)
	) picorv32_inst(
		.clk(sys_clkx2),
		.resetn(picorv_rst_n),
		.trap(trap),
		.mem_valid(mem_valid),
		.mem_instr(),
		.mem_ready(mem_ready),
		.mem_addr(mem_addr),
		.mem_wdata(mem_wdata),
		.mem_wstrb(mem_wstrb),
		.mem_rdata(mem_rdata),
		.mem_la_read(),
		.mem_la_write(),
		.mem_la_addr(),
		.mem_la_wdata(),
		.mem_la_wstrb(),
		.pcpi_valid(),
		.pcpi_insn(),
		.pcpi_rs1(),
		.pcpi_rs2(),
		.pcpi_wr(1'b0),
		.pcpi_rd(32'b00000000000000000000000000000000),
		.pcpi_wait(1'b0),
		.pcpi_ready(1'b0),
		.irq(32'b00000000000000000000000000000000),
		.eoi(),
		.trace_valid(),
		.trace_data()
	);
	wire [3:0] wbs_wen;
	wire [3:0] ram_wen;
	assign wbs_wen = (do_wbs_wr_mem ? wbs_sel : 4'b0000);
	assign ram_wen = (ram_valid ? ram_wstrb : 4'b0000);
	wire [21:0] picosoc_mem_addr_mux;
	function automatic [9:0] sv2v_cast_10;
		input reg [9:0] inp;
		sv2v_cast_10 = inp;
	endfunction
	assign picosoc_mem_addr_mux = {12'b000000000000, (picorv_rst_n ? ram_addr[11:2] : sv2v_cast_10(wbs_adr))};
	picosoc_mem #(.WORDS(MEM_SZ_WORDS)) pico_mem_inst(
		.clk(sys_clkx2),
		.wen((picorv_rst_n ? ram_wen : wbs_wen)),
		.addr(picosoc_mem_addr_mux),
		.wdata((picorv_rst_n ? ram_wdata : wbs_dat_w)),
		.rdata(ram_rdata)
	);
	wire [31:0] burst_fsm_rdata_xfer;
	wire burst_fsm_ready_xfer;
	always @(posedge sys_clkx2) begin
		burst_fsm_rdata <= burst_fsm_rdata_xfer;
		burst_fsm_ready <= burst_fsm_ready_xfer;
	end
	picorv_burst_fsm #(.BURST_REG_BASE_ADDR(BURST_REG_BASE_ADDR)) picorv_burst_fsm_instance(
		.clk(sys_clk),
		.rst(rst),
		.picorv_valid_i(burst_fsm_valid),
		.picorv_rdy_o(burst_fsm_ready_xfer),
		.picorv_addr_i(burst_fsm_addr),
		.picorv_wdata_i(burst_fsm_wdata),
		.picorv_wstrb_i(burst_fsm_wstrb),
		.picorv_rdata_o(burst_fsm_rdata_xfer),
		.wbm_adr_o(wbm_adr_o),
		.wbm_dat_o(wbm_dat_o),
		.wbm_dat_i(wbm_dat_i),
		.wbm_we_o(wbm_we_o),
		.wbm_sel_o(wbm_sel_o),
		.wbm_stb_o(wbm_stb_o),
		.wbm_ack_i(wbm_ack_i),
		.wbm_stall_i(wbm_stall_i),
		.wbm_cyc_o(wbm_cyc_o),
		.wbm_err_i(1'b0)
	);
	initial _sv2v_0 = 0;
endmodule
