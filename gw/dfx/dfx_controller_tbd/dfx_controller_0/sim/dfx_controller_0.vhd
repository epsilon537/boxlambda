-- (c) Copyright 1986-2022 Xilinx, Inc. All Rights Reserved.
-- (c) Copyright 2022-2024 Advanced Micro Devices, Inc. All rights reserved.
-- 
-- This file contains confidential and proprietary information
-- of AMD and is protected under U.S. and international copyright
-- and other intellectual property laws.
-- 
-- DISCLAIMER
-- This disclaimer is not a license and does not grant any
-- rights to the materials distributed herewith. Except as
-- otherwise provided in a valid license issued to you by
-- AMD, and to the maximum extent permitted by applicable
-- law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
-- WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
-- AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
-- BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
-- INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
-- (2) AMD shall not be liable (whether in contract or tort,
-- including negligence, or under any other theory of
-- liability) for any loss or damage of any kind or nature
-- related to, arising under or in connection with these
-- materials, including for any direct, or any indirect,
-- special, incidental, or consequential loss or damage
-- (including loss of data, profits, goodwill, or any type of
-- loss or damage suffered as a result of any action brought
-- by a third party) even if such damage or loss was
-- reasonably foreseeable or AMD had been advised of the
-- possibility of the same.
-- 
-- CRITICAL APPLICATIONS
-- AMD products are not designed or intended to be fail-
-- safe, or for use in any application requiring fail-safe
-- performance, such as life-support or safety devices or
-- systems, Class III medical devices, nuclear facilities,
-- applications related to the deployment of airbags, or any
-- other applications that could lead to death, personal
-- injury, or severe property or environmental damage
-- (individually and collectively, "Critical
-- Applications"). Customer assumes the sole risk and
-- liability of any use of AMD products in Critical
-- Applications, subject only to applicable laws and
-- regulations governing limitations on product liability.
-- 
-- THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
-- PART OF THIS FILE AT ALL TIMES.
-- 
-- DO NOT MODIFY THIS FILE.

-- IP VLNV: xilinx.com:ip:dfx_controller:1.0
-- IP Revision: 4

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY dfx_controller_0 IS
  PORT (
    m_axi_mem_araddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axi_mem_arlen : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    m_axi_mem_arsize : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    m_axi_mem_arburst : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    m_axi_mem_arprot : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    m_axi_mem_arcache : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    m_axi_mem_aruser : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    m_axi_mem_arvalid : OUT STD_LOGIC;
    m_axi_mem_arready : IN STD_LOGIC;
    m_axi_mem_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axi_mem_rresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    m_axi_mem_rlast : IN STD_LOGIC;
    m_axi_mem_rvalid : IN STD_LOGIC;
    m_axi_mem_rready : OUT STD_LOGIC;
    clk : IN STD_LOGIC;
    reset : IN STD_LOGIC;
    icap_clk : IN STD_LOGIC;
    icap_reset : IN STD_LOGIC;
    icap_csib : OUT STD_LOGIC;
    icap_rdwrb : OUT STD_LOGIC;
    icap_i : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    icap_o : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    vsm_VS_0_rm_shutdown_req : OUT STD_LOGIC;
    vsm_VS_0_rm_shutdown_ack : IN STD_LOGIC;
    vsm_VS_0_rm_decouple : OUT STD_LOGIC;
    vsm_VS_0_rm_reset : OUT STD_LOGIC;
    vsm_VS_0_event_error : OUT STD_LOGIC;
    vsm_VS_0_sw_shutdown_req : OUT STD_LOGIC;
    vsm_VS_0_sw_startup_req : OUT STD_LOGIC;
    s_axi_reg_awaddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axi_reg_awvalid : IN STD_LOGIC;
    s_axi_reg_awready : OUT STD_LOGIC;
    s_axi_reg_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axi_reg_wvalid : IN STD_LOGIC;
    s_axi_reg_wready : OUT STD_LOGIC;
    s_axi_reg_bresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    s_axi_reg_bvalid : OUT STD_LOGIC;
    s_axi_reg_bready : IN STD_LOGIC;
    s_axi_reg_araddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axi_reg_arvalid : IN STD_LOGIC;
    s_axi_reg_arready : OUT STD_LOGIC;
    s_axi_reg_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axi_reg_rresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    s_axi_reg_rvalid : OUT STD_LOGIC;
    s_axi_reg_rready : IN STD_LOGIC
  );
END dfx_controller_0;

ARCHITECTURE dfx_controller_0_arch OF dfx_controller_0 IS
  ATTRIBUTE DowngradeIPIdentifiedWarnings : STRING;
  ATTRIBUTE DowngradeIPIdentifiedWarnings OF dfx_controller_0_arch: ARCHITECTURE IS "yes";
  COMPONENT dfx_controller_dfx_controller_0 IS
    GENERIC (
      C_XDEVICEFAMILY : STRING
    );
    PORT (
      m_axi_mem_araddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      m_axi_mem_arlen : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      m_axi_mem_arsize : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
      m_axi_mem_arburst : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      m_axi_mem_arprot : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
      m_axi_mem_arcache : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      m_axi_mem_aruser : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      m_axi_mem_arvalid : OUT STD_LOGIC;
      m_axi_mem_arready : IN STD_LOGIC;
      m_axi_mem_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      m_axi_mem_rresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      m_axi_mem_rlast : IN STD_LOGIC;
      m_axi_mem_rvalid : IN STD_LOGIC;
      m_axi_mem_rready : OUT STD_LOGIC;
      clk : IN STD_LOGIC;
      reset : IN STD_LOGIC;
      icap_clk : IN STD_LOGIC;
      icap_reset : IN STD_LOGIC;
      icap_csib : OUT STD_LOGIC;
      icap_rdwrb : OUT STD_LOGIC;
      icap_i : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      icap_o : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      vsm_VS_0_rm_shutdown_req : OUT STD_LOGIC;
      vsm_VS_0_rm_shutdown_ack : IN STD_LOGIC;
      vsm_VS_0_rm_decouple : OUT STD_LOGIC;
      vsm_VS_0_rm_reset : OUT STD_LOGIC;
      vsm_VS_0_event_error : OUT STD_LOGIC;
      vsm_VS_0_sw_shutdown_req : OUT STD_LOGIC;
      vsm_VS_0_sw_startup_req : OUT STD_LOGIC;
      s_axi_reg_awaddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      s_axi_reg_awvalid : IN STD_LOGIC;
      s_axi_reg_awready : OUT STD_LOGIC;
      s_axi_reg_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      s_axi_reg_wvalid : IN STD_LOGIC;
      s_axi_reg_wready : OUT STD_LOGIC;
      s_axi_reg_bresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      s_axi_reg_bvalid : OUT STD_LOGIC;
      s_axi_reg_bready : IN STD_LOGIC;
      s_axi_reg_araddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      s_axi_reg_arvalid : IN STD_LOGIC;
      s_axi_reg_arready : OUT STD_LOGIC;
      s_axi_reg_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      s_axi_reg_rresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      s_axi_reg_rvalid : OUT STD_LOGIC;
      s_axi_reg_rready : IN STD_LOGIC
    );
  END COMPONENT dfx_controller_dfx_controller_0;
  ATTRIBUTE X_INTERFACE_INFO : STRING;
  ATTRIBUTE X_INTERFACE_PARAMETER : STRING;
  ATTRIBUTE X_INTERFACE_PARAMETER OF clk: SIGNAL IS "XIL_INTERFACENAME CLK, ASSOCIATED_BUSIF M_AXI_MEM:s_axi_reg, ASSOCIATED_RESET reset, FREQ_HZ 100000000, FREQ_TOLERANCE_HZ 0, PHASE 0.0, INSERT_VIP 0";
  ATTRIBUTE X_INTERFACE_INFO OF clk: SIGNAL IS "xilinx.com:signal:clock:1.0 CLK CLK";
  ATTRIBUTE X_INTERFACE_PARAMETER OF icap_clk: SIGNAL IS "XIL_INTERFACENAME ICAP_CLK, ASSOCIATED_BUSIF ICAP, ASSOCIATED_RESET icap_reset, FREQ_HZ 100000000, FREQ_TOLERANCE_HZ 0, PHASE 0.0, INSERT_VIP 0";
  ATTRIBUTE X_INTERFACE_INFO OF icap_clk: SIGNAL IS "xilinx.com:signal:clock:1.0 ICAP_CLK CLK";
  ATTRIBUTE X_INTERFACE_INFO OF icap_csib: SIGNAL IS "xilinx.com:interface:icap:1.0 ICAP csib";
  ATTRIBUTE X_INTERFACE_INFO OF icap_i: SIGNAL IS "xilinx.com:interface:icap:1.0 ICAP o";
  ATTRIBUTE X_INTERFACE_INFO OF icap_o: SIGNAL IS "xilinx.com:interface:icap:1.0 ICAP i";
  ATTRIBUTE X_INTERFACE_INFO OF icap_rdwrb: SIGNAL IS "xilinx.com:interface:icap:1.0 ICAP rdwrb";
  ATTRIBUTE X_INTERFACE_PARAMETER OF icap_reset: SIGNAL IS "XIL_INTERFACENAME icap_reset, POLARITY ACTIVE_HIGH, INSERT_VIP 0";
  ATTRIBUTE X_INTERFACE_INFO OF icap_reset: SIGNAL IS "xilinx.com:signal:reset:1.0 icap_reset RST";
  ATTRIBUTE X_INTERFACE_PARAMETER OF m_axi_mem_araddr: SIGNAL IS "XIL_INTERFACENAME M_AXI_MEM, DATA_WIDTH 32, PROTOCOL AXI4, FREQ_HZ 100000000, ID_WIDTH 0, ADDR_WIDTH 32, AWUSER_WIDTH 0, ARUSER_WIDTH 4, WUSER_WIDTH 0, RUSER_WIDTH 0, BUSER_WIDTH 0, READ_WRITE_MODE READ_ONLY, HAS_BURST 1, HAS_LOCK 0, HAS_PROT 1, HAS_CACHE 1, HAS_QOS 0, HAS_REGION 0, HAS_WSTRB 0, HAS_BRESP 0, HAS_RRESP 1, SUPPORTS_NARROW_BURST 1, NUM_READ_OUTSTANDING 2, NUM_WRITE_OUTSTANDING 2, MAX_BURST_LENGTH 256, PHASE 0.0, NUM_READ_THREADS 1, NUM_WRITE_THREADS 1, RUSER_BITS_PER_BYTE 0, WUSER_" & 
"BITS_PER_BYTE 0, INSERT_VIP 0";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_araddr: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARADDR";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_arburst: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARBURST";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_arcache: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARCACHE";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_arlen: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARLEN";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_arprot: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARPROT";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_arready: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARREADY";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_arsize: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARSIZE";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_aruser: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARUSER";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_arvalid: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM ARVALID";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_rdata: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM RDATA";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_rlast: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM RLAST";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_rready: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM RREADY";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_rresp: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM RRESP";
  ATTRIBUTE X_INTERFACE_INFO OF m_axi_mem_rvalid: SIGNAL IS "xilinx.com:interface:aximm:1.0 M_AXI_MEM RVALID";
  ATTRIBUTE X_INTERFACE_PARAMETER OF reset: SIGNAL IS "XIL_INTERFACENAME reset, POLARITY ACTIVE_HIGH, INSERT_VIP 0";
  ATTRIBUTE X_INTERFACE_INFO OF reset: SIGNAL IS "xilinx.com:signal:reset:1.0 reset RST";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_araddr: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg ARADDR";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_arready: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg ARREADY";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_arvalid: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg ARVALID";
  ATTRIBUTE X_INTERFACE_PARAMETER OF s_axi_reg_awaddr: SIGNAL IS "XIL_INTERFACENAME s_axi_reg, DATA_WIDTH 32, PROTOCOL AXI4LITE, FREQ_HZ 100000000, ID_WIDTH 0, ADDR_WIDTH 32, AWUSER_WIDTH 0, ARUSER_WIDTH 0, WUSER_WIDTH 0, RUSER_WIDTH 0, BUSER_WIDTH 0, READ_WRITE_MODE READ_WRITE, HAS_BURST 0, HAS_LOCK 0, HAS_PROT 0, HAS_CACHE 0, HAS_QOS 0, HAS_REGION 0, HAS_WSTRB 0, HAS_BRESP 1, HAS_RRESP 1, SUPPORTS_NARROW_BURST 0, NUM_READ_OUTSTANDING 1, NUM_WRITE_OUTSTANDING 1, MAX_BURST_LENGTH 1, PHASE 0.0, NUM_READ_THREADS 1, NUM_WRITE_THREADS 1, RUSER_BITS_PER_BYTE 0, WUS" & 
"ER_BITS_PER_BYTE 0, INSERT_VIP 0";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_awaddr: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg AWADDR";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_awready: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg AWREADY";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_awvalid: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg AWVALID";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_bready: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg BREADY";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_bresp: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg BRESP";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_bvalid: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg BVALID";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_rdata: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg RDATA";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_rready: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg RREADY";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_rresp: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg RRESP";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_rvalid: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg RVALID";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_wdata: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg WDATA";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_wready: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg WREADY";
  ATTRIBUTE X_INTERFACE_INFO OF s_axi_reg_wvalid: SIGNAL IS "xilinx.com:interface:aximm:1.0 s_axi_reg WVALID";
BEGIN
  U0 : dfx_controller_dfx_controller_0
    GENERIC MAP (
      C_XDEVICEFAMILY => "artix7"
    )
    PORT MAP (
      m_axi_mem_araddr => m_axi_mem_araddr,
      m_axi_mem_arlen => m_axi_mem_arlen,
      m_axi_mem_arsize => m_axi_mem_arsize,
      m_axi_mem_arburst => m_axi_mem_arburst,
      m_axi_mem_arprot => m_axi_mem_arprot,
      m_axi_mem_arcache => m_axi_mem_arcache,
      m_axi_mem_aruser => m_axi_mem_aruser,
      m_axi_mem_arvalid => m_axi_mem_arvalid,
      m_axi_mem_arready => m_axi_mem_arready,
      m_axi_mem_rdata => m_axi_mem_rdata,
      m_axi_mem_rresp => m_axi_mem_rresp,
      m_axi_mem_rlast => m_axi_mem_rlast,
      m_axi_mem_rvalid => m_axi_mem_rvalid,
      m_axi_mem_rready => m_axi_mem_rready,
      clk => clk,
      reset => reset,
      icap_clk => icap_clk,
      icap_reset => icap_reset,
      icap_csib => icap_csib,
      icap_rdwrb => icap_rdwrb,
      icap_i => icap_i,
      icap_o => icap_o,
      vsm_VS_0_rm_shutdown_req => vsm_VS_0_rm_shutdown_req,
      vsm_VS_0_rm_shutdown_ack => vsm_VS_0_rm_shutdown_ack,
      vsm_VS_0_rm_decouple => vsm_VS_0_rm_decouple,
      vsm_VS_0_rm_reset => vsm_VS_0_rm_reset,
      vsm_VS_0_event_error => vsm_VS_0_event_error,
      vsm_VS_0_sw_shutdown_req => vsm_VS_0_sw_shutdown_req,
      vsm_VS_0_sw_startup_req => vsm_VS_0_sw_startup_req,
      s_axi_reg_awaddr => s_axi_reg_awaddr,
      s_axi_reg_awvalid => s_axi_reg_awvalid,
      s_axi_reg_awready => s_axi_reg_awready,
      s_axi_reg_wdata => s_axi_reg_wdata,
      s_axi_reg_wvalid => s_axi_reg_wvalid,
      s_axi_reg_wready => s_axi_reg_wready,
      s_axi_reg_bresp => s_axi_reg_bresp,
      s_axi_reg_bvalid => s_axi_reg_bvalid,
      s_axi_reg_bready => s_axi_reg_bready,
      s_axi_reg_araddr => s_axi_reg_araddr,
      s_axi_reg_arvalid => s_axi_reg_arvalid,
      s_axi_reg_arready => s_axi_reg_arready,
      s_axi_reg_rdata => s_axi_reg_rdata,
      s_axi_reg_rresp => s_axi_reg_rresp,
      s_axi_reg_rvalid => s_axi_reg_rvalid,
      s_axi_reg_rready => s_axi_reg_rready
    );
END dfx_controller_0_arch;
