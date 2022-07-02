//A wrapper module only used during ibex OOC synthesis. It instantiates ibex_top the way we want it.
  
module ibex_top_wrap import ibex_pkg::*;
  (
   // Clock and Reset
   input logic 			      clk_i,
   input logic 			      rst_ni,

   input logic 			      test_en_i, // enable all clock gates for testing
   input 			      prim_ram_1p_pkg::ram_1p_cfg_t ram_cfg_i,

   input logic [31:0] 		      hart_id_i,
   input logic [31:0] 		      boot_addr_i,

   // Instruction memory interface
   output logic 		      instr_req_o,
   input logic 			      instr_gnt_i,
   input logic 			      instr_rvalid_i,
   output logic [31:0] 		      instr_addr_o,
   input logic [31:0] 		      instr_rdata_i,
   input logic [6:0] 		      instr_rdata_intg_i,
   input logic 			      instr_err_i,

   // Data memory interface
   output logic 		      data_req_o,
   input logic 			      data_gnt_i,
   input logic 			      data_rvalid_i,
   output logic 		      data_we_o,
   output logic [3:0] 		      data_be_o,
   output logic [31:0] 		      data_addr_o,
   output logic [31:0] 		      data_wdata_o,
   output logic [6:0] 		      data_wdata_intg_o,
   input logic [31:0] 		      data_rdata_i,
   input logic [6:0] 		      data_rdata_intg_i,
   input logic 			      data_err_i,

   // Interrupt inputs
   input logic 			      irq_software_i,
   input logic 			      irq_timer_i,
   input logic 			      irq_external_i,
   input logic [14:0] 		      irq_fast_i,
   input logic 			      irq_nm_i, // non-maskeable interrupt

   // Scrambling Interface
   input logic 			      scramble_key_valid_i,
   input logic [SCRAMBLE_KEY_W-1:0]   scramble_key_i,
   input logic [SCRAMBLE_NONCE_W-1:0] scramble_nonce_i,
   output logic 		      scramble_req_o,

   // Debug Interface
   input logic 			      debug_req_i,
   output 			      crash_dump_t crash_dump_o,
   output logic 		      double_fault_seen_o,

   // CPU Control Signals
   input 			      fetch_enable_t fetch_enable_i,
   output logic 		      alert_minor_o,
   output logic 		      alert_major_internal_o,
   output logic 		      alert_major_bus_o,
   output logic 		      core_sleep_o,

   // DFT bypass controls
   input logic 			      scan_rst_ni
   );

   ibex_top #(.RV32M(RV32MFast),.RV32B(RV32BBalanced),.RegFile(RegFileFPGA)) ibex_top_inst(.*);
   
endmodule
