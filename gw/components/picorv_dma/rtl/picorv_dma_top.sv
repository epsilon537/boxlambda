`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

module picorv_dma_top #(
    parameter BASE_ADDR = 32'h10002000
) (
    input logic clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface.
    output logic [29:0] wbm_adr_o,
	output logic [31:0] wbm_dat_o,
	input logic [31:0] wbm_dat_i,
	output logic wbm_we_o,
	output logic [3:0] wbm_sel_o,
	output logic wbm_stb_o,
	input logic wbm_ack_i,
    input logic wbm_stall_i,
	output logic wbm_cyc_o,
    input logic wbm_err_i,

    //32-bit pipelined Wishbone slave interface.
    input logic [10:0] wbs_adr,
	input logic [31:0] wbs_dat_w,
	output logic [31:0] wbs_dat_r,
	input logic [3:0] wbs_sel,
    output logic wbs_stall,
	input logic wbs_cyc,
	input logic wbs_stb,
	output logic wbs_ack,
	input logic wbs_we,
	output logic wbs_err,

    //Input IRQs from the rest of the system.
    input wire [31:0] irq_in,
    //Output IRQ signal
    output wire irq_out
);
    localparam integer MEM_SZ_WORDS = 1024; //PicoRV Program and data memory size
    localparam integer REG_SZ_WORDS = 32; //Register Space.
    localparam integer WBS_REG_BASE_ADDR = MEM_SZ_WORDS; //Register base address as see by WB slave.
    localparam integer PICO_REG_BASE_ADDR = BASE_ADDR+MEM_SZ_WORDS*4; //Register base address as seen by PicoRV.
    localparam integer PICO_MEM_BASE_ADDR = BASE_ADDR; //Memory base address as seen by PicoRV
    logic trap;

    //Non-local memory access signals, i.e. memory accesses that will be turned Wishbone bus master transactions.
    logic        iomem_valid;
	logic        iomem_ready;
	logic [ 3:0] iomem_wstrb;
	logic [31:2] iomem_addr;
	logic [31:0] iomem_wdata;
	logic  [31:0] iomem_rdata;

    //Local system- and general purpose register access signals.
    logic        reg_we;
    logic        reg_valid;
	logic        reg_ready;
	logic [ 3:0] reg_wstrb;
	logic [31:0] reg_addr;
	logic [31:0] reg_wdata;
	logic [31:0] reg_rdata;

    //'memory' access signals from PicoRV. Will be categorized into iomem_, reg_, or ram_
    logic mem_valid;
	logic mem_ready;
    logic [31:0] mem_addr;
	logic [31:0] mem_wdata;
	logic [ 3:0] mem_wstrb;
	logic [31:0] mem_rdata;

    //PicoRV Program Memory access signals.
    logic        ram_valid;
	logic        ram_ready;
	logic [ 3:0] ram_wstrb;
	logic [$clog2(MEM_SZ_WORDS)+1:2] ram_addr;
	logic [31:0] ram_wdata;
	logic  [31:0] ram_rdata;

    logic [31:0] gp_reg[0:15]; //16 general purpose registers

    //Three system registers: ctrl, irq-in and irq-out.
    logic [31:0] ctrl_reg;
    logic [31:0] irq_in_reg;
    logic [31:0] irq_out_reg, irq_out_next;

    //The PicoRV reset signal is controlled through the register interface. It's not tied
    //to the system reset register.
    logic picorv_rst_n;

    logic do_ack_wbs;
    logic do_wbs_wr_mem, do_wbs_wr_reg;
    logic [31:0] wbs_dat_read_from_reg;

    logic unused = &{wbm_stall_i, wbs_sel, iomem_addr, wbm_err_i};

    //WB slave handshake
    assign do_wbs_wr_reg = wbs_cyc && wbs_stb && wbs_we && (wbs_adr >= 11'(WBS_REG_BASE_ADDR));
    assign do_wbs_wr_mem = wbs_cyc && wbs_stb && wbs_we && (wbs_adr < 11'(WBS_REG_BASE_ADDR));
    
    always @(posedge clk) begin
        do_ack_wbs <= 1'b0;
        if (wbs_stb) begin
            do_ack_wbs <= 1'b1;
        end
    end

    assign wbs_dat_r = (wbs_adr < 11'(WBS_REG_BASE_ADDR)) ? ram_rdata : wbs_dat_read_from_reg;
    assign wbs_ack = do_ack_wbs & wbs_cyc;
    assign wbs_stall = 1'b0;
    assign wbs_err = 1'b0;

    //PicoRV access to core's system and general purpose registers.
	assign reg_we = |reg_wstrb;
    assign reg_valid = mem_valid && ((mem_addr >= PICO_REG_BASE_ADDR) && (mem_addr < (PICO_REG_BASE_ADDR + 4*REG_SZ_WORDS)));
	assign reg_wstrb = mem_wstrb;
	assign reg_addr = mem_addr;
	assign reg_wdata = mem_wdata;

    always @(posedge clk)
	    reg_ready <= reg_valid && !reg_ready && (reg_addr >= PICO_REG_BASE_ADDR) && (reg_addr < (PICO_REG_BASE_ADDR + 4*REG_SZ_WORDS));

    //PicoRV access to system memory space outside of this core.
	assign iomem_valid = mem_valid && ((mem_addr < PICO_MEM_BASE_ADDR) || (mem_addr > (PICO_REG_BASE_ADDR + 4*REG_SZ_WORDS)));
	assign iomem_wstrb = mem_wstrb;
	assign iomem_addr = mem_addr[31:2];
	assign iomem_wdata = mem_wdata;

    //PicoRV access to local program and data memory
	assign ram_valid = mem_valid && ((mem_addr >= PICO_MEM_BASE_ADDR) && (mem_addr < PICO_REG_BASE_ADDR));
	assign ram_wstrb = mem_wstrb;
	assign ram_addr = mem_addr[$clog2(MEM_SZ_WORDS)+1:2];
	assign ram_wdata = mem_wdata;

    always @(posedge clk)
        if (picorv_rst_n) //When not in reset, PicoRV gets RAM access
		    ram_ready <= ram_valid && !mem_ready;
        else begin //When in reset, PicoRV does not have RAM access.
            ram_ready <= 1'b0;
        end

    //Return signal mux.
    assign mem_ready = (reg_valid && reg_ready) || (iomem_valid && iomem_ready) || (ram_valid && ram_ready);
	assign mem_rdata = (reg_valid && reg_ready) ? reg_rdata : (iomem_valid && iomem_ready) ? iomem_rdata : (ram_valid && ram_ready) ? ram_rdata : 32'h 0000_0000;

    //Reset Control via ctrl register bit 0.
    assign picorv_rst_n = ctrl_reg[0];

    //IRQ handling
    assign irq_out = |irq_out_reg;
    always_comb begin
        for (int i=0; i<32; i++) begin
            irq_out_next[i] = irq_out_reg[i];
            //Ack IRQ by writing to register 0
            if (do_wbs_wr_reg && (wbs_adr==11'(WBS_REG_BASE_ADDR+0)) && wbs_dat_w[i])
                irq_out_next[i] = 1'b0;
        end
        for (int i=0; i<31; i++) begin
            if (reg_we && reg_valid && (reg_addr==PICO_REG_BASE_ADDR+0) && reg_wdata[i])
                irq_out_next[i] = 1'b1; //PicoRV can set IRQs by writing to irq_out register.
        end
        if (trap)
            irq_out_next[31] = 1'b1; //Bit 31 is reserved for the PicoRV trap signal.   
    end

    //Register writes incoming from WBS and PicoRV.
    always_ff @(posedge clk) begin
        if (rst) begin
            gp_reg[0] <= 32'b0;
            gp_reg[1] <= 32'b0;
            gp_reg[2] <= 32'b0;
            gp_reg[3] <= 32'b0;
            gp_reg[4] <= 32'b0;
            gp_reg[5] <= 32'b0;
            gp_reg[6] <= 32'b0;
            gp_reg[7] <= 32'b0;
            gp_reg[8] <= 32'b0;
            gp_reg[9] <= 32'b0;
            gp_reg[10] <= 32'b0;
            gp_reg[11] <= 32'b0;
            gp_reg[12] <= 32'b0;
            gp_reg[13] <= 32'b0;
            gp_reg[14] <= 32'b0;
            gp_reg[15] <= 32'b0;

            ctrl_reg <= 32'b0;

            irq_in_reg <= 32'b0;
            irq_out_reg <= 32'b0;
        end
        else begin
            irq_in_reg <= irq_in;
            irq_out_reg <= irq_out_next;

            if (do_wbs_wr_reg) begin
                case(wbs_adr) //wbs address is a word address.
                    11'(WBS_REG_BASE_ADDR+2): ctrl_reg <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+16): gp_reg[0] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+17): gp_reg[1] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+18): gp_reg[2] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+19): gp_reg[3] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+20): gp_reg[4] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+21): gp_reg[5] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+22): gp_reg[6] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+23): gp_reg[7] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+24): gp_reg[8] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+25): gp_reg[9] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+26): gp_reg[10] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+27): gp_reg[11] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+28): gp_reg[12] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+29): gp_reg[13] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+30): gp_reg[14] <= wbs_dat_w;
                    11'(WBS_REG_BASE_ADDR+31): gp_reg[15] <= wbs_dat_w;
                    default:;
                endcase
            end
            
            if (reg_we && reg_valid) begin
                case(reg_addr) //PicoRV address is a byte address.
                    PICO_REG_BASE_ADDR+16*4: gp_reg[0] <= reg_wdata;
                    PICO_REG_BASE_ADDR+17*4: gp_reg[1] <= reg_wdata;
                    PICO_REG_BASE_ADDR+18*4: gp_reg[2] <= reg_wdata;
                    PICO_REG_BASE_ADDR+19*4: gp_reg[3] <= reg_wdata;
                    PICO_REG_BASE_ADDR+20*4: gp_reg[4] <= reg_wdata;
                    PICO_REG_BASE_ADDR+21*4: gp_reg[5] <= reg_wdata;
                    PICO_REG_BASE_ADDR+22*4: gp_reg[6] <= reg_wdata;
                    PICO_REG_BASE_ADDR+23*4: gp_reg[7] <= reg_wdata;
                    PICO_REG_BASE_ADDR+24*4: gp_reg[8] <= reg_wdata;
                    PICO_REG_BASE_ADDR+25*4: gp_reg[9] <= reg_wdata;
                    PICO_REG_BASE_ADDR+26*4: gp_reg[10] <= reg_wdata;
                    PICO_REG_BASE_ADDR+27*4: gp_reg[11] <= reg_wdata;
                    PICO_REG_BASE_ADDR+28*4: gp_reg[12] <= reg_wdata;
                    PICO_REG_BASE_ADDR+29*4: gp_reg[13] <= reg_wdata;
                    PICO_REG_BASE_ADDR+30*4: gp_reg[14] <= reg_wdata;
                    PICO_REG_BASE_ADDR+31*4: gp_reg[15] <= reg_wdata;
                    default:;
                endcase
            end
        end
    end 
    
    //WBS and PicoRV register reads
    always_comb begin
        case(wbs_adr) //wbs address is a word address.
            11'(WBS_REG_BASE_ADDR): wbs_dat_read_from_reg = irq_out_reg;
            11'(WBS_REG_BASE_ADDR+1): wbs_dat_read_from_reg = irq_in_reg;
            11'(WBS_REG_BASE_ADDR+2): wbs_dat_read_from_reg = ctrl_reg;
            11'(WBS_REG_BASE_ADDR+16): wbs_dat_read_from_reg = gp_reg[0];
            11'(WBS_REG_BASE_ADDR+17): wbs_dat_read_from_reg = gp_reg[1];
            11'(WBS_REG_BASE_ADDR+18): wbs_dat_read_from_reg = gp_reg[2];
            11'(WBS_REG_BASE_ADDR+19): wbs_dat_read_from_reg = gp_reg[3];
            11'(WBS_REG_BASE_ADDR+20): wbs_dat_read_from_reg = gp_reg[4];
            11'(WBS_REG_BASE_ADDR+21): wbs_dat_read_from_reg = gp_reg[5];
            11'(WBS_REG_BASE_ADDR+22): wbs_dat_read_from_reg = gp_reg[6];
            11'(WBS_REG_BASE_ADDR+23): wbs_dat_read_from_reg = gp_reg[7];
            11'(WBS_REG_BASE_ADDR+24): wbs_dat_read_from_reg = gp_reg[8];
            11'(WBS_REG_BASE_ADDR+25): wbs_dat_read_from_reg = gp_reg[9];
            11'(WBS_REG_BASE_ADDR+26): wbs_dat_read_from_reg = gp_reg[10];
            11'(WBS_REG_BASE_ADDR+27): wbs_dat_read_from_reg = gp_reg[11];
            11'(WBS_REG_BASE_ADDR+28): wbs_dat_read_from_reg = gp_reg[12];
            11'(WBS_REG_BASE_ADDR+29): wbs_dat_read_from_reg = gp_reg[13];
            11'(WBS_REG_BASE_ADDR+30): wbs_dat_read_from_reg = gp_reg[14];
            11'(WBS_REG_BASE_ADDR+31): wbs_dat_read_from_reg = gp_reg[15];
            default: wbs_dat_read_from_reg = 32'd0;
        endcase
        
        case(reg_addr) //PicoRV address is a byte address.
            PICO_REG_BASE_ADDR+1*4: reg_rdata = irq_in_reg;
            PICO_REG_BASE_ADDR+16*4: reg_rdata = gp_reg[0]; 
            PICO_REG_BASE_ADDR+17*4: reg_rdata = gp_reg[1]; 
            PICO_REG_BASE_ADDR+18*4: reg_rdata = gp_reg[2]; 
            PICO_REG_BASE_ADDR+19*4: reg_rdata = gp_reg[3]; 
            PICO_REG_BASE_ADDR+20*4: reg_rdata = gp_reg[4]; 
            PICO_REG_BASE_ADDR+21*4: reg_rdata = gp_reg[5]; 
            PICO_REG_BASE_ADDR+22*4: reg_rdata = gp_reg[6]; 
            PICO_REG_BASE_ADDR+23*4: reg_rdata = gp_reg[7]; 
            PICO_REG_BASE_ADDR+24*4: reg_rdata = gp_reg[8]; 
            PICO_REG_BASE_ADDR+25*4: reg_rdata = gp_reg[9]; 
            PICO_REG_BASE_ADDR+26*4: reg_rdata = gp_reg[10]; 
            PICO_REG_BASE_ADDR+27*4: reg_rdata = gp_reg[11]; 
            PICO_REG_BASE_ADDR+28*4: reg_rdata = gp_reg[12]; 
            PICO_REG_BASE_ADDR+29*4: reg_rdata = gp_reg[13]; 
            PICO_REG_BASE_ADDR+30*4: reg_rdata = gp_reg[14]; 
            PICO_REG_BASE_ADDR+31*4: reg_rdata = gp_reg[15];
            default: reg_rdata = 32'd0; 
        endcase
    end

    //The PicoRV32 processor
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
	    .MASKED_IRQ(32'h 0000_0000),
	    .LATCHED_IRQ(32'h ffff_ffff),
	    .PROGADDR_RESET(PICO_MEM_BASE_ADDR),
	    .PROGADDR_IRQ(32'h 0000_0000),
	    .STACKADDR(PICO_MEM_BASE_ADDR + MEM_SZ_WORDS*4 - 32'h4)
    ) picorv32_inst (
        .clk(clk), 
        .resetn(picorv_rst_n), //PicoRV reset is controlled through ctrl registers, not system reset.
        .trap(trap),

        //memory accesses generated by the processor
        .mem_valid(mem_valid),
        .mem_instr(),
        .mem_ready(mem_ready),

        .mem_addr(mem_addr),
        .mem_wdata(mem_wdata),
        .mem_wstrb(mem_wstrb),
        .mem_rdata(mem_rdata),

        // Look-Ahead Interface, not used
        .mem_la_read(),
        .mem_la_write(),
        .mem_la_addr(),
        .mem_la_wdata(),
        .mem_la_wstrb(),

        // Pico Co-Processor Interface (PCPI), not used
        .pcpi_valid(),
        .pcpi_insn(),
        .pcpi_rs1(),
        .pcpi_rs2(),
        .pcpi_wr(1'b0),
        .pcpi_rd(32'b0),
        .pcpi_wait(1'b0),
        .pcpi_ready(1'b0),

        // IRQ Interface, not used
        .irq(32'b0),
        .eoi(),

        // Trace Interface, not used
        .trace_valid(),
        .trace_data()
    );

    //Program memory for the PicoRV processor.
    //When PicoRV is in reset, WBS has RAM access. When PicoRV is not in reset, it has RAM access.
    logic [3:0] wbs_wen, ram_wen;
    assign wbs_wen = do_wbs_wr_mem ? wbs_sel : 4'b0;
    assign ram_wen = ram_valid ? ram_wstrb : 4'b0;

    logic [21:0] picosoc_mem_addr_mux;
    //WBS address are word addresses, PicoRV addresses are byte addresses.
    assign picosoc_mem_addr_mux = 
        {   (22-$clog2(MEM_SZ_WORDS))'(1'b0), 
            picorv_rst_n ?  ram_addr[$clog2(MEM_SZ_WORDS)+1:2] : 
                            $clog2(MEM_SZ_WORDS)'(wbs_adr)};

    picosoc_mem #(
        .WORDS(MEM_SZ_WORDS)
    ) pico_mem_inst (
        .clk(clk),
		.wen(picorv_rst_n ? ram_wen : wbs_wen),
		.addr(picosoc_mem_addr_mux), 
		.wdata(picorv_rst_n ? ram_wdata : wbs_dat_w),
		.rdata(ram_rdata)
    );

    //WB master interworking logic below is based on picorv32_wb
    localparam IDLE = 2'b00;
	localparam WBSTART = 2'b01;
	localparam WBEND = 2'b10;

	logic [1:0] state;

	logic we;
	assign we = (iomem_wstrb[0] | iomem_wstrb[1] | iomem_wstrb[2] | iomem_wstrb[3]);

	always @(posedge clk) begin
		if (rst) begin
			wbm_adr_o <= 0;
			wbm_dat_o <= 0;
			wbm_we_o <= 0;
			wbm_sel_o <= 0;
			wbm_stb_o <= 0;
			wbm_cyc_o <= 0;
			state <= IDLE;
		end else begin
			case (state)
				IDLE: begin
					if (iomem_valid) begin
						wbm_adr_o <= iomem_addr[31:2];
						wbm_dat_o <= iomem_wdata;
						wbm_we_o <= we;
						wbm_sel_o <= we ? iomem_wstrb : 4'b1111;

						wbm_stb_o <= 1'b1;
						wbm_cyc_o <= 1'b1;
						state <= WBSTART;
					end else begin
						iomem_ready <= 1'b0;

						wbm_stb_o <= 1'b0;
						wbm_cyc_o <= 1'b0;
						wbm_we_o <= 1'b0;
					end
				end
				WBSTART:begin
					if (wbm_ack_i) begin
						iomem_rdata <= wbm_dat_i;
						iomem_ready <= 1'b1;

						state <= WBEND;

						wbm_stb_o <= 1'b0;
						wbm_cyc_o <= 1'b0;
						wbm_we_o <= 1'b0;
					end
				end
				WBEND: begin
					iomem_ready <= 1'b0;

					state <= IDLE;
				end
				default:
					state <= IDLE;
			endcase
		end
	end
endmodule
