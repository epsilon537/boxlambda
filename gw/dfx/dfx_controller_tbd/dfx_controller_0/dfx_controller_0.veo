// (c) Copyright 1986-2022 Xilinx, Inc. All Rights Reserved.
// (c) Copyright 2022-2024 Advanced Micro Devices, Inc. All rights reserved.
// 
// This file contains confidential and proprietary information
// of AMD and is protected under U.S. and international copyright
// and other intellectual property laws.
// 
// DISCLAIMER
// This disclaimer is not a license and does not grant any
// rights to the materials distributed herewith. Except as
// otherwise provided in a valid license issued to you by
// AMD, and to the maximum extent permitted by applicable
// law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
// WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
// AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
// BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
// INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
// (2) AMD shall not be liable (whether in contract or tort,
// including negligence, or under any other theory of
// liability) for any loss or damage of any kind or nature
// related to, arising under or in connection with these
// materials, including for any direct, or any indirect,
// special, incidental, or consequential loss or damage
// (including loss of data, profits, goodwill, or any type of
// loss or damage suffered as a result of any action brought
// by a third party) even if such damage or loss was
// reasonably foreseeable or AMD had been advised of the
// possibility of the same.
// 
// CRITICAL APPLICATIONS
// AMD products are not designed or intended to be fail-
// safe, or for use in any application requiring fail-safe
// performance, such as life-support or safety devices or
// systems, Class III medical devices, nuclear facilities,
// applications related to the deployment of airbags, or any
// other applications that could lead to death, personal
// injury, or severe property or environmental damage
// (individually and collectively, "Critical
// Applications"). Customer assumes the sole risk and
// liability of any use of AMD products in Critical
// Applications, subject only to applicable laws and
// regulations governing limitations on product liability.
// 
// THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
// PART OF THIS FILE AT ALL TIMES.
// 
// DO NOT MODIFY THIS FILE.

// IP VLNV: xilinx.com:ip:dfx_controller:1.0
// IP Revision: 4

// The following must be inserted into your Verilog file for this
// core to be instantiated. Change the instance name and port connections
// (in parentheses) to your own signal names.

//----------- Begin Cut here for INSTANTIATION Template ---// INST_TAG
dfx_controller_0 your_instance_name (
  .m_axi_mem_araddr(m_axi_mem_araddr),                  // output wire [31 : 0] m_axi_mem_araddr
  .m_axi_mem_arlen(m_axi_mem_arlen),                    // output wire [7 : 0] m_axi_mem_arlen
  .m_axi_mem_arsize(m_axi_mem_arsize),                  // output wire [2 : 0] m_axi_mem_arsize
  .m_axi_mem_arburst(m_axi_mem_arburst),                // output wire [1 : 0] m_axi_mem_arburst
  .m_axi_mem_arprot(m_axi_mem_arprot),                  // output wire [2 : 0] m_axi_mem_arprot
  .m_axi_mem_arcache(m_axi_mem_arcache),                // output wire [3 : 0] m_axi_mem_arcache
  .m_axi_mem_aruser(m_axi_mem_aruser),                  // output wire [3 : 0] m_axi_mem_aruser
  .m_axi_mem_arvalid(m_axi_mem_arvalid),                // output wire m_axi_mem_arvalid
  .m_axi_mem_arready(m_axi_mem_arready),                // input wire m_axi_mem_arready
  .m_axi_mem_rdata(m_axi_mem_rdata),                    // input wire [31 : 0] m_axi_mem_rdata
  .m_axi_mem_rresp(m_axi_mem_rresp),                    // input wire [1 : 0] m_axi_mem_rresp
  .m_axi_mem_rlast(m_axi_mem_rlast),                    // input wire m_axi_mem_rlast
  .m_axi_mem_rvalid(m_axi_mem_rvalid),                  // input wire m_axi_mem_rvalid
  .m_axi_mem_rready(m_axi_mem_rready),                  // output wire m_axi_mem_rready
  .clk(clk),                                            // input wire clk
  .reset(reset),                                        // input wire reset
  .icap_clk(icap_clk),                                  // input wire icap_clk
  .icap_reset(icap_reset),                              // input wire icap_reset
  .icap_csib(icap_csib),                                // output wire icap_csib
  .icap_rdwrb(icap_rdwrb),                              // output wire icap_rdwrb
  .icap_i(icap_i),                                      // input wire [31 : 0] icap_i
  .icap_o(icap_o),                                      // output wire [31 : 0] icap_o
  .vsm_VS_0_rm_shutdown_req(vsm_VS_0_rm_shutdown_req),  // output wire vsm_VS_0_rm_shutdown_req
  .vsm_VS_0_rm_shutdown_ack(vsm_VS_0_rm_shutdown_ack),  // input wire vsm_VS_0_rm_shutdown_ack
  .vsm_VS_0_rm_decouple(vsm_VS_0_rm_decouple),          // output wire vsm_VS_0_rm_decouple
  .vsm_VS_0_rm_reset(vsm_VS_0_rm_reset),                // output wire vsm_VS_0_rm_reset
  .vsm_VS_0_event_error(vsm_VS_0_event_error),          // output wire vsm_VS_0_event_error
  .vsm_VS_0_sw_shutdown_req(vsm_VS_0_sw_shutdown_req),  // output wire vsm_VS_0_sw_shutdown_req
  .vsm_VS_0_sw_startup_req(vsm_VS_0_sw_startup_req),    // output wire vsm_VS_0_sw_startup_req
  .s_axi_reg_awaddr(s_axi_reg_awaddr),                  // input wire [31 : 0] s_axi_reg_awaddr
  .s_axi_reg_awvalid(s_axi_reg_awvalid),                // input wire s_axi_reg_awvalid
  .s_axi_reg_awready(s_axi_reg_awready),                // output wire s_axi_reg_awready
  .s_axi_reg_wdata(s_axi_reg_wdata),                    // input wire [31 : 0] s_axi_reg_wdata
  .s_axi_reg_wvalid(s_axi_reg_wvalid),                  // input wire s_axi_reg_wvalid
  .s_axi_reg_wready(s_axi_reg_wready),                  // output wire s_axi_reg_wready
  .s_axi_reg_bresp(s_axi_reg_bresp),                    // output wire [1 : 0] s_axi_reg_bresp
  .s_axi_reg_bvalid(s_axi_reg_bvalid),                  // output wire s_axi_reg_bvalid
  .s_axi_reg_bready(s_axi_reg_bready),                  // input wire s_axi_reg_bready
  .s_axi_reg_araddr(s_axi_reg_araddr),                  // input wire [31 : 0] s_axi_reg_araddr
  .s_axi_reg_arvalid(s_axi_reg_arvalid),                // input wire s_axi_reg_arvalid
  .s_axi_reg_arready(s_axi_reg_arready),                // output wire s_axi_reg_arready
  .s_axi_reg_rdata(s_axi_reg_rdata),                    // output wire [31 : 0] s_axi_reg_rdata
  .s_axi_reg_rresp(s_axi_reg_rresp),                    // output wire [1 : 0] s_axi_reg_rresp
  .s_axi_reg_rvalid(s_axi_reg_rvalid),                  // output wire s_axi_reg_rvalid
  .s_axi_reg_rready(s_axi_reg_rready)                  // input wire s_axi_reg_rready
);
// INST_TAG_END ------ End INSTANTIATION Template ---------

// You must compile the wrapper file dfx_controller_0.v when simulating
// the core, dfx_controller_0. When compiling the wrapper file, be sure to
// reference the Verilog simulation library.

