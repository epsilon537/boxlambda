`default_nettype none
module dm_top (
	clk_i,
	rst_ni,
	testmode_i,
	ndmreset_o,
	dmactive_o,
	debug_req_o,
	unavailable_i,
	hartinfo_i,
	slave_req_i,
	slave_we_i,
	slave_addr_i,
	slave_be_i,
	slave_wdata_i,
	slave_rdata_o,
	master_req_o,
	master_add_o,
	master_we_o,
	master_wdata_o,
	master_be_o,
	master_gnt_i,
	master_r_valid_i,
	master_r_err_i,
	master_r_other_err_i,
	master_r_rdata_i,
	dmi_rst_ni,
	dmi_req_valid_i,
	dmi_req_ready_o,
	dmi_req_i,
	dmi_resp_valid_o,
	dmi_resp_ready_i,
	dmi_resp_o
);
	parameter [31:0] NrHarts = 1;
	parameter [31:0] BusWidth = 32;
	parameter [31:0] DmBaseAddress = 'h1000;
	parameter [NrHarts - 1:0] SelectableHarts = {NrHarts {1'b1}};
	parameter [0:0] ReadByteEnable = 1;
	input wire clk_i;
	input wire rst_ni;
	input wire testmode_i;
	output wire ndmreset_o;
	output wire dmactive_o;
	output wire [NrHarts - 1:0] debug_req_o;
	input wire [NrHarts - 1:0] unavailable_i;
	input wire [(NrHarts * 32) - 1:0] hartinfo_i;
	input wire slave_req_i;
	input wire slave_we_i;
	input wire [BusWidth - 1:0] slave_addr_i;
	input wire [(BusWidth / 8) - 1:0] slave_be_i;
	input wire [BusWidth - 1:0] slave_wdata_i;
	output wire [BusWidth - 1:0] slave_rdata_o;
	output wire master_req_o;
	output wire [BusWidth - 1:0] master_add_o;
	output wire master_we_o;
	output wire [BusWidth - 1:0] master_wdata_o;
	output wire [(BusWidth / 8) - 1:0] master_be_o;
	input wire master_gnt_i;
	input wire master_r_valid_i;
	input wire master_r_err_i;
	input wire master_r_other_err_i;
	input wire [BusWidth - 1:0] master_r_rdata_i;
	input wire dmi_rst_ni;
	input wire dmi_req_valid_i;
	output wire dmi_req_ready_o;
	input wire [40:0] dmi_req_i;
	output wire dmi_resp_valid_o;
	input wire dmi_resp_ready_i;
	output wire [33:0] dmi_resp_o;
	wire [NrHarts - 1:0] halted;
	wire [NrHarts - 1:0] resumeack;
	wire [NrHarts - 1:0] haltreq;
	wire [NrHarts - 1:0] resumereq;
	wire clear_resumeack;
	wire cmd_valid;
	wire [31:0] cmd;
	wire cmderror_valid;
	wire [2:0] cmderror;
	wire cmdbusy;
	localparam [4:0] dm_ProgBufSize = 5'h08;
	wire [255:0] progbuf;
	localparam [3:0] dm_DataCount = 4'h2;
	wire [63:0] data_csrs_mem;
	wire [63:0] data_mem_csrs;
	wire data_valid;
	wire ndmreset;
	wire [19:0] hartsel;
	wire [BusWidth - 1:0] sbaddress_csrs_sba;
	wire [BusWidth - 1:0] sbaddress_sba_csrs;
	wire sbaddress_write_valid;
	wire sbreadonaddr;
	wire sbautoincrement;
	wire [2:0] sbaccess;
	wire sbreadondata;
	wire [BusWidth - 1:0] sbdata_write;
	wire sbdata_read_valid;
	wire sbdata_write_valid;
	wire [BusWidth - 1:0] sbdata_read;
	wire sbdata_valid;
	wire sbbusy;
	wire sberror_valid;
	wire [2:0] sberror;
	assign ndmreset_o = ndmreset;
	dm_csrs #(
		.NrHarts(NrHarts),
		.BusWidth(BusWidth),
		.SelectableHarts(SelectableHarts)
	) i_dm_csrs(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.testmode_i(testmode_i),
		.dmi_rst_ni(dmi_rst_ni),
		.dmi_req_valid_i(dmi_req_valid_i),
		.dmi_req_ready_o(dmi_req_ready_o),
		.dmi_req_i(dmi_req_i),
		.dmi_resp_valid_o(dmi_resp_valid_o),
		.dmi_resp_ready_i(dmi_resp_ready_i),
		.dmi_resp_o(dmi_resp_o),
		.ndmreset_o(ndmreset),
		.dmactive_o(dmactive_o),
		.hartsel_o(hartsel),
		.hartinfo_i(hartinfo_i),
		.halted_i(halted),
		.unavailable_i(unavailable_i),
		.resumeack_i(resumeack),
		.haltreq_o(haltreq),
		.resumereq_o(resumereq),
		.clear_resumeack_o(clear_resumeack),
		.cmd_valid_o(cmd_valid),
		.cmd_o(cmd),
		.cmderror_valid_i(cmderror_valid),
		.cmderror_i(cmderror),
		.cmdbusy_i(cmdbusy),
		.progbuf_o(progbuf),
		.data_i(data_mem_csrs),
		.data_valid_i(data_valid),
		.data_o(data_csrs_mem),
		.sbaddress_o(sbaddress_csrs_sba),
		.sbaddress_i(sbaddress_sba_csrs),
		.sbaddress_write_valid_o(sbaddress_write_valid),
		.sbreadonaddr_o(sbreadonaddr),
		.sbautoincrement_o(sbautoincrement),
		.sbaccess_o(sbaccess),
		.sbreadondata_o(sbreadondata),
		.sbdata_o(sbdata_write),
		.sbdata_read_valid_o(sbdata_read_valid),
		.sbdata_write_valid_o(sbdata_write_valid),
		.sbdata_i(sbdata_read),
		.sbdata_valid_i(sbdata_valid),
		.sbbusy_i(sbbusy),
		.sberror_valid_i(sberror_valid),
		.sberror_i(sberror)
	);
	dm_sba #(
		.BusWidth(BusWidth),
		.ReadByteEnable(ReadByteEnable)
	) i_dm_sba(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.dmactive_i(dmactive_o),
		.master_req_o(master_req_o),
		.master_add_o(master_add_o),
		.master_we_o(master_we_o),
		.master_wdata_o(master_wdata_o),
		.master_be_o(master_be_o),
		.master_gnt_i(master_gnt_i),
		.master_r_valid_i(master_r_valid_i),
		.master_r_err_i(master_r_err_i),
		.master_r_other_err_i(master_r_other_err_i),
		.master_r_rdata_i(master_r_rdata_i),
		.sbaddress_i(sbaddress_csrs_sba),
		.sbaddress_o(sbaddress_sba_csrs),
		.sbaddress_write_valid_i(sbaddress_write_valid),
		.sbreadonaddr_i(sbreadonaddr),
		.sbautoincrement_i(sbautoincrement),
		.sbaccess_i(sbaccess),
		.sbreadondata_i(sbreadondata),
		.sbdata_i(sbdata_write),
		.sbdata_read_valid_i(sbdata_read_valid),
		.sbdata_write_valid_i(sbdata_write_valid),
		.sbdata_o(sbdata_read),
		.sbdata_valid_o(sbdata_valid),
		.sbbusy_o(sbbusy),
		.sberror_valid_o(sberror_valid),
		.sberror_o(sberror)
	);
	dm_mem #(
		.NrHarts(NrHarts),
		.BusWidth(BusWidth),
		.SelectableHarts(SelectableHarts),
		.DmBaseAddress(DmBaseAddress)
	) i_dm_mem(
		.clk_i(clk_i),
		.rst_ni(rst_ni),
		.debug_req_o(debug_req_o),
		.ndmreset_i(ndmreset),
		.hartsel_i(hartsel),
		.haltreq_i(haltreq),
		.resumereq_i(resumereq),
		.clear_resumeack_i(clear_resumeack),
		.halted_o(halted),
		.resuming_o(resumeack),
		.cmd_valid_i(cmd_valid),
		.cmd_i(cmd),
		.cmderror_valid_o(cmderror_valid),
		.cmderror_o(cmderror),
		.cmdbusy_o(cmdbusy),
		.progbuf_i(progbuf),
		.data_i(data_csrs_mem),
		.data_o(data_mem_csrs),
		.data_valid_o(data_valid),
		.req_i(slave_req_i),
		.we_i(slave_we_i),
		.addr_i(slave_addr_i),
		.wdata_i(slave_wdata_i),
		.be_i(slave_be_i),
		.rdata_o(slave_rdata_o)
	);
endmodule
