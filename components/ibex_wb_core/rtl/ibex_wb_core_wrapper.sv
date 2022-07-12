module ibex_wb_core_wrapper
   (input logic 	clk, rst,
    // Wishbone inputs
    input logic 	i_instr_wb_err, 
    input logic 	i_instr_wb_stall,
    input logic 	i_instr_wb_ack,
    input logic [31:0] 	i_instr_wb_data,

    // Wishbone outputs
    output logic [31:0] o_instr_addr,
    output logic 	o_instr_cyc,
    output logic 	o_instr_stb,
    output logic 	o_instr_we,
    output logic [3:0] 	o_instr_sel,
    output logic [31:0] o_instr_data,
    
    // Wishbone inputs
    input logic 	i_data_wb_err, 
    input logic 	i_data_wb_stall,
    input logic 	i_data_wb_ack,
    input logic [31:0] 	i_data_wb_data,
    output logic [31:0] o_data_data,

    // Wishbone outputs
    output logic [31:0] o_data_addr,
    output logic 	o_data_cyc,
    output logic 	o_data_stb,
    output logic 	o_data_we,
    output logic [3:0] 	o_data_sel,

    input wire 		test_en, // Test input, enables clock

    input wire [31:0] 	hart_id, // Hart ID, usually static, can be read from Hardware Thread ID (mhartid) CSR
    input wire [31:0] 	boot_addr, // First program counter after reset = boot_addr + 0x80

    input wire 		irq_software, // Connected to memory-mapped (inter-processor) interrupt register
    input wire 		irq_timer, // Connected to timer module
    input wire 		irq_external, // Connected to platform-level interrupt controller
    input wire [14:0] 	irq_fast, // 15 fast, local interrupts
    input wire 		irq_nm, // Non-maskable interrupt (NMI)

    input wire 		debug_req, // Request to enter debug mode

    input 		ibex_pkg::fetch_enable_t fetch_enable, // Enable the core, won't fetch when 0
    output logic 	core_sleep);             // Core in WFI with no outstanding data or instruction accesses.

   logic 		rst_n;

   wb_if instr_wb(.rst(rst),
		  .clk(clk));
   wb_if data_wb(.rst(rst),
		  .clk(clk));

   assign rst_n = ~rst;

   assign instr_wb.ack = i_instr_wb_ack;
   assign instr_wb.stall = i_instr_wb_stall;
   assign instr_wb.dat_s = i_instr_wb_data;
   assign instr_wb.err = i_instr_wb_err;

   assign o_instr_addr = instr_wb.adr;
   assign o_instr_cyc = instr_wb.cyc;
   assign o_instr_stb = instr_wb.stb;
   assign o_instr_we = instr_wb.we;
   assign o_instr_sel = instr_wb.sel;
   assign o_instr_data = instr_wb.dat_m;

   assign data_wb.ack = i_data_wb_ack;
   assign data_wb.stall = i_data_wb_stall;
   assign data_wb.dat_s = i_data_wb_data;
   assign data_wb.err = i_data_wb_err;

   assign o_data_addr = data_wb.adr;
   assign o_data_cyc = data_wb.cyc;
   assign o_data_stb = data_wb.stb;
   assign o_data_we = data_wb.we;
   assign o_data_sel = data_wb.sel;
   assign o_data_data = data_wb.dat_m;

   wb_ibex_core #(.RV32M(ibex_pkg::RV32MFast),
		  .RV32B(ibex_pkg::RV32BBalanced))
   wb_ibex_core_inst(.instr_wb(instr_wb),
		     .data_wb(data_wb),
		     .*);
 
endmodule

`resetall
