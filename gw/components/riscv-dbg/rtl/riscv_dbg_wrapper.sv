//This is a wrapper for riscv-dbg component-level linting, bringing all submodules under one denominator, making lint happy.
module riscv_dbg_wrapper (
    input  wire clk,
    input  wire rst_n,
    input  wire tck,
    input  wire trst_n,
    input  wire tms,
    input  wire tdi,
    output wire tdo,

    output logic ndmreset,
    output logic dmactive,
    output logic debug_req

);

  logic unavailable = 1'b0;
  dm::hartinfo_t hartinfo = '{
      zero1: 0,
      nscratch: 2,
      zero0: 0,
      dataaccess: 1,
      datasize: dm::DataCount,
      dataaddr: dm::DataAddr
  };
  logic dmi_rst_n;
  logic dmi_req_valid;
  logic dmi_req_ready;
  dm::dmi_req_t dmi_req;
  logic dmi_resp_valid;
  logic dmi_resp_ready;
  dm::dmi_resp_t dmi_resp;
  logic tdo_o;
  logic tdo_oe;

  assign tdo = tdo_oe ? tdo_o : 1'bz;

  wb_if wbm (
      .rst(rst_n),
      .clk(clk)
  );
  wb_if wbs (
      .rst(rst_n),
      .clk(clk)
  );

  wb_dm_top wb_dm (
      .testmode (1'b0),
      .wbm      (wbm),
      .wbs      (wbs),
      .dmi_rst_n(dmi_rst_n),
      .*
  );

  dmi_jtag dmi_jtag_inst (
      .clk_i           (clk),
      .rst_ni          (rst_n),
      .testmode_i      (1'b0),
      .dmi_rst_no      (dmi_rst_n),
      .dmi_req_o       (dmi_req),
      .dmi_req_valid_o (dmi_req_valid),
      .dmi_req_ready_i (dmi_req_ready),
      .dmi_resp_i      (dmi_resp),
      .dmi_resp_ready_o(dmi_resp_ready),
      .dmi_resp_valid_i(dmi_resp_valid),
      .tck_i           (tck),
      .tms_i           (tms),
      .trst_ni         (trst_n),
      .td_i            (tdi),
      .td_o            (tdo_o),
      .tdo_oe_o        (tdo_oe)
  );

endmodule
