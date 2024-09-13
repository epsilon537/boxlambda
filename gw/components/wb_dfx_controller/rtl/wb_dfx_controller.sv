//DFX Controller with Wishbone interface
module wb_dfx_controller (
    input logic clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface.
    output logic [27:0] wbm_adr_o,
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
    input logic [4:0] wbs_adr,
    input logic [31:0] wbs_dat_w,
    output logic [31:0] wbs_dat_r,
    input logic [3:0] wbs_sel,
    output logic wbs_stall,
    input logic wbs_cyc,
    input logic wbs_stb,
    output logic wbs_ack,
    input logic wbs_we,
    output logic wbs_err,

    output logic vsm_VS_0_rm_reset,
    output logic vsm_VS_0_event_error
);

  logic [   1:0] unused;

  //ICAP:
  logic          icap_csib;
  logic          icap_rdwrb;
  logic [31 : 0] icap_i;
  logic [31 : 0] icap_o;

  //Master AXI:
  logic [31 : 0] m_axi_araddr;
  logic [ 7 : 0] m_axi_arlen;
  logic [ 2 : 0] m_axi_arsize;
  logic [ 1 : 0] m_axi_arburst;
  logic [ 2 : 0] m_axi_arprot;
  logic [ 3 : 0] m_axi_arcache;
  logic          m_axi_arvalid;
  logic          m_axi_arready;
  logic [31 : 0] m_axi_rdata;
  logic [ 1 : 0] m_axi_rresp;
  logic          m_axi_rlast;
  logic          m_axi_rvalid;
  logic          m_axi_rready;

  // Slave AXI:
  // AXI write address channel signals
  logic          s_axi_awvalid;
  logic          s_axi_awready;
  logic [  31:0] s_axi_awaddr;
  // AXI write data channel signals
  logic          s_axi_wvalid;
  logic          s_axi_wready;
  logic [  31:0] s_axi_wdata;
  // AXI write response channel signals
  logic          s_axi_bvalid;
  logic          s_axi_bready;
  logic [   1:0] s_axi_bresp;
  // AXI read address channel signals
  logic          s_axi_arvalid;
  logic          s_axi_arready;
  logic [  31:0] s_axi_araddr;
  // AXI read data channel signals
  logic          s_axi_rvalid;
  logic          s_axi_rready;
  logic [  31:0] s_axi_rdata;
  logic [   1:0] s_axi_rresp;

  //Wishbone bridge to DFX Controller's AXI-lite slave register interface.
  wbm2axilite #(
      .C_AXI_ADDR_WIDTH(32)  // AXI Address width
  ) wbm2axilite_inst (
      .i_clk(clk),
      .i_reset(rst),
      // Wishbone
      .i_wb_cyc(wbs_cyc),
      .i_wb_stb(wbs_stb),
      .i_wb_we(wbs_we),
      .i_wb_addr({25'b0, wbs_adr}),
      .i_wb_data(wbs_dat_w),
      .i_wb_sel(wbs_sel),
      .o_wb_stall(wbs_stall),
      .o_wb_ack(wbs_ack),
      .o_wb_data(wbs_dat_r),
      .o_wb_err(wbs_err),
      // AXI-Lite
      // AXI write address channel signals
      .o_axi_awvalid(s_axi_awvalid),
      .i_axi_awready(s_axi_awready),
      .o_axi_awaddr(s_axi_awaddr),
      .o_axi_awprot(),
      // AXI write data channel signals
      .o_axi_wvalid(s_axi_wvalid),
      .i_axi_wready(s_axi_wready),
      .o_axi_wdata(s_axi_wdata),
      .o_axi_wstrb(),
      // AXI write response channel signals
      .i_axi_bvalid(s_axi_bvalid),
      .o_axi_bready(s_axi_bready),
      .i_axi_bresp(s_axi_bresp),
      // AXI read address channel signals
      .o_axi_arvalid(s_axi_arvalid),
      .i_axi_arready(s_axi_arready),
      .o_axi_araddr(s_axi_araddr),
      .o_axi_arprot(),
      // AXI read data channel signals
      .i_axi_rvalid(s_axi_rvalid),
      .o_axi_rready(s_axi_rready),
      .i_axi_rdata(s_axi_rdata),
      .i_axi_rresp(s_axi_rresp)
  );

  logic rm_shutdown_req;
  logic rm_decouple;
  logic sw_shutdown_req;
  logic sw_startup_req;

  ila_0 ila_inst (
      .clk(clk),  // input wire clk
      .probe0({wbm_cyc_o, wbm_stall_i, wbm_stb_o, wbm_ack_i, wbm_adr_o, wbm_dat_i}),
      .probe1({
        m_axi_arlen,
        m_axi_arsize,
        m_axi_arburst,
        m_axi_araddr,
        m_axi_arvalid,
        m_axi_arready,
        icap_csib,
        rm_shutdown_req,
        rm_decouple,
        vsm_VS_0_rm_reset,
        sw_shutdown_req,
        sw_startup_req
      })  // input wire [63:0]  probe1
  );

  // ICAPE2: Internal Configuration Access Port
  //         7 Series
  ICAPE2 ICAPE2_inst (
      .O    (icap_o),     // 32-bit output: Configuration data output bus
      .CLK  (clk),        // 1-bit input: Clock Input
      .CSIB (icap_csib),  // 1-bit input: Active-Low ICAP Enable
      .I    (icap_i),     // 32-bit input: Configuration data input bus
      .RDWRB(icap_rdwrb)  // 1-bit input: Read/Write Select input
  );

  dfx_controller_0 dfx_controller_inst (
      .m_axi_mem_araddr        (m_axi_araddr),          // output wire [31 : 0] m_axi_mem_araddr
      .m_axi_mem_arlen         (m_axi_arlen),           // output wire [7 : 0] m_axi_mem_arlen
      .m_axi_mem_arsize        (m_axi_arsize),          // output wire [2 : 0] m_axi_mem_arsize
      .m_axi_mem_arburst       (m_axi_arburst),         // output wire [1 : 0] m_axi_mem_arburst
      .m_axi_mem_arprot        (m_axi_arprot),          // output wire [2 : 0] m_axi_mem_arprot
      .m_axi_mem_arcache       (m_axi_arcache),         // output wire [3 : 0] m_axi_mem_arcache
      .m_axi_mem_aruser        (),                      // output wire [3 : 0] m_axi_mem_aruser
      .m_axi_mem_arvalid       (m_axi_arvalid),         // output wire m_axi_mem_arvalid
      .m_axi_mem_arready       (m_axi_arready),         // input wire m_axi_mem_arready
      .m_axi_mem_rdata         (m_axi_rdata),           // input wire [31 : 0] m_axi_mem_rdata
      .m_axi_mem_rresp         (m_axi_rresp),           // input wire [1 : 0] m_axi_mem_rresp
      .m_axi_mem_rlast         (m_axi_rlast),           // input wire m_axi_mem_rlast
      .m_axi_mem_rvalid        (m_axi_rvalid),          // input wire m_axi_mem_rvalid
      .m_axi_mem_rready        (m_axi_rready),          // output wire m_axi_mem_rready
      .clk                     (clk),                   // input wire clk
      .reset                   (rst),                   // input wire reset
      .icap_clk                (clk),                   // input wire icap_clk
      .icap_reset              (rst),                   // input wire icap_reset
      .icap_csib               (icap_csib),             // output wire icap_csib
      .icap_rdwrb              (icap_rdwrb),            // output wire icap_rdwrb
      .icap_i                  (icap_o),                // input wire [31 : 0] icap_i
      .icap_o                  (icap_i),                // output wire [31 : 0] icap_o
      .vsm_VS_0_rm_shutdown_req(rm_shutdown_req),       // output wire vsm_VS_0_rm_shutdown_req
      .vsm_VS_0_rm_shutdown_ack(1'b1),                  // input wire vsm_VS_0_rm_shutdown_ack
      .vsm_VS_0_rm_decouple    (rm_decouple),           // output wire vsm_VS_0_rm_decouple
      .vsm_VS_0_rm_reset       (vsm_VS_0_rm_reset),     // output wire vsm_VS_0_rm_reset
      .vsm_VS_0_event_error    (vsm_VS_0_event_error),  // output wire vsm_VS_0_event_error
      .vsm_VS_0_sw_shutdown_req(sw_shutdown_req),       // output wire vsm_VS_0_sw_shutdown_req
      .vsm_VS_0_sw_startup_req (sw_startup_req),        // output wire vsm_VS_0_sw_startup_req
      .s_axi_reg_awaddr        (s_axi_awaddr),          // input wire [31 : 0] s_axi_reg_awaddr
      .s_axi_reg_awvalid       (s_axi_awvalid),         // input wire s_axi_reg_awvalid
      .s_axi_reg_awready       (s_axi_awready),         // output wire s_axi_reg_awready
      .s_axi_reg_wdata         (s_axi_wdata),           // input wire [31 : 0] s_axi_reg_wdata
      .s_axi_reg_wvalid        (s_axi_wvalid),          // input wire s_axi_reg_wvalid
      .s_axi_reg_wready        (s_axi_wready),          // output wire s_axi_reg_wready
      .s_axi_reg_bresp         (s_axi_bresp),           // output wire [1 : 0] s_axi_reg_bresp
      .s_axi_reg_bvalid        (s_axi_bvalid),          // output wire s_axi_reg_bvalid
      .s_axi_reg_bready        (s_axi_bready),          // input wire s_axi_reg_bready
      .s_axi_reg_araddr        (s_axi_araddr),          // input wire [31 : 0] s_axi_reg_araddr
      .s_axi_reg_arvalid       (s_axi_arvalid),         // input wire s_axi_reg_arvalid
      .s_axi_reg_arready       (s_axi_arready),         // output wire s_axi_reg_arready
      .s_axi_reg_rdata         (s_axi_rdata),           // output wire [31 : 0] s_axi_reg_rdata
      .s_axi_reg_rresp         (s_axi_rresp),           // output wire [1 : 0] s_axi_reg_rresp
      .s_axi_reg_rvalid        (s_axi_rvalid),          // output wire s_axi_reg_rvalid
      .s_axi_reg_rready        (s_axi_rready)           // input wire s_axi_reg_rready
  );

  aximrd2wbsp #(
      .C_AXI_ID_WIDTH  (2),   // The AXI id width used for R&W
      .C_AXI_DATA_WIDTH(32),  // Width of the AXI R&W data
      .C_AXI_ADDR_WIDTH(32)   // AXI Address width
  ) aximrd2wsp_inst (
      .S_AXI_ACLK(clk),     // Bus clock
      .S_AXI_ARESETN(~rst),  // Bus reset
      // AXI
      // AXI read address channel signals
      .S_AXI_ARVALID(m_axi_arvalid),
      .S_AXI_ARREADY(m_axi_arready),
      .S_AXI_ARID(2'b00),
      .S_AXI_ARADDR(m_axi_araddr),
      .S_AXI_ARLEN(m_axi_arlen),
      .S_AXI_ARSIZE(m_axi_arsize),
      .S_AXI_ARBURST(m_axi_arburst),
      .S_AXI_ARLOCK(1'b0),
      .S_AXI_ARCACHE(m_axi_arcache),
      .S_AXI_ARPROT(m_axi_arprot),
      .S_AXI_ARQOS(4'b0000),
      // AXI read data channel signals
      .S_AXI_RVALID(m_axi_rvalid),
      .S_AXI_RREADY(m_axi_rready),
      .S_AXI_RID(),
      .S_AXI_RDATA(m_axi_rdata),
      .S_AXI_RLAST(m_axi_rlast),
      .S_AXI_RRESP(m_axi_rresp),
      // Wishbone channel
      .o_wb_cyc(wbm_cyc_o),
      .o_wb_stb(wbm_stb_o),
      .o_wb_addr({unused, wbm_adr_o}),
      .o_wb_sel(wbm_sel_o),
      .i_wb_stall(wbm_stall_i),
      .i_wb_ack(wbm_ack_i),
      .i_wb_data(wbm_dat_i),
      .i_wb_err(wbm_err_i)
  );

  assign wbm_dat_o = 0;
  assign wbm_we_o  = 0;

endmodule

