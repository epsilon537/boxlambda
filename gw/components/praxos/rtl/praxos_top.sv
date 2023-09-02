`timescale 1 ns/1 ps

module praxos_top
(
    input                                     clk,
    input                                     rst_n,

    //32-bit pipelined Wishbone slave interface.
    input wire [4:0]                          wb_s_adr,
	input wire [31:0]                         wb_s_dat_w,
	output wire [31:0]                        wb_s_dat_r,
	input wire [3:0]                          wb_s_sel,
    output wire                               wb_s_stall,
	input wire                                wb_s_cyc,
	input wire                                wb_s_stb,
	output wire                               wb_s_ack,
	input wire                                wb_s_we,
	output wire                               wb_s_err,

    //IRQs
    input wire [31:0]                         irq_in,
    output wire                               irq_out,

    //32-bit pipelined Wishbone master interface.
    output wire [31:0] wb_m_adr,
	output wire [31:0] wb_m_dat_w,
	input wire [31:0] wb_m_dat_r,
	output wire [3:0] wb_m_sel,
    input wire wb_m_stall,
	output wire wb_m_cyc,
	output wire wb_m_stb,
	input wire wb_m_ack,
	output wire wb_m_we,
	input wire wb_m_err
);

logic        praxos_rst_n;
logic [7:0]  praxos_pm_wr_addr;
logic        praxos_pm_wr;
logic [35:0] praxos_pm_wr_data;

logic [15:0] praxos_port_addr;
logic        praxos_port_rd;
logic        praxos_port_wr;
logic [31:0] praxos_port_wr_data;
logic [31:0] praxos_port_rd_data;

logic [31:0] av_address;
logic [31:0] av_readdata;
logic [31:0] av_writedata;
logic [3:0] av_byteenable;
logic av_write;
logic av_read;
logic av_waitrequest;

logic unused = &{praxos_port_addr[15:5]};

praxos_ctrl praxos_ctrl_inst(
    .clk(clk),
    .rst_n(rst_n),

    //32-bit pipelined Wishbone interface.
    .wb_adr(wb_s_adr),
	.wb_dat_w(wb_s_dat_w),
	.wb_dat_r(wb_s_dat_r),
	.wb_sel(wb_s_sel),
    .wb_stall(wb_s_stall),
	.wb_cyc(wb_s_cyc),
	.wb_stb(wb_s_stb),
	.wb_ack(wb_s_ack),
	.wb_we(wb_s_we),
	.wb_err(wb_s_err),

    //IRQs
    .irq_in(irq_in),
    .irq_out(irq_out),

    //Praxos PM access
    .praxos_rst_n(praxos_rst_n),
    .praxos_pm_wr_addr(praxos_pm_wr_addr),
	.praxos_pm_wr(praxos_pm_wr),
	.praxos_pm_wr_data(praxos_pm_wr_data),

    //Praxos Port I/O
    .praxos_port_addr(praxos_port_addr[4:0]),
	.praxos_port_rd(praxos_port_rd),
    .praxos_port_wr(praxos_port_wr),
    .praxos_port_wr_data(praxos_port_wr_data),
    .praxos_port_rd_data(praxos_port_rd_data)
);

praxos_cpu praxos_cpu_inst(
	.clk(clk),
	.resetn(praxos_rst_n),
	//IO
	.port_addr(praxos_port_addr),
	.port_rd(praxos_port_rd),
	.port_wr(praxos_port_wr),
	.port_in(praxos_port_rd_data),
	.port_out(praxos_port_wr_data),
	// Avalon
	.av_address(av_address),
	.av_readdata(av_readdata),
	.av_writedata(av_writedata),
	.av_byteenable(av_byteenable),
	.av_write(av_write),
	.av_read(av_read),
	.av_waitrequest(av_waitrequest),
	// PM Memory Access
	.pm_wr_addr(praxos_pm_wr_addr),
	.pm_wr(praxos_pm_wr),
	.pm_wr_data(praxos_pm_wr_data)
);

av2wb av2wb_inst(
	.clk(clk),
	.rst_n(rst_n),

	//Avalon Slave
	.av_address(av_address),
	.av_readdata(av_readdata),
	.av_writedata(av_writedata),
	.av_byteenable(av_byteenable),
	.av_write(av_write),
	.av_read(av_read),
	.av_waitrequest(av_waitrequest),

	//WB Master
	.wb_adr(wb_m_adr),
	.wb_dat_w(wb_m_dat_w),
	.wb_dat_r(wb_m_dat_r),
	.wb_sel(wb_m_sel),
    .wb_stall(wb_m_stall),
	.wb_cyc(wb_m_cyc),
	.wb_stb(wb_m_stb),
	.wb_ack(wb_m_ack),
	.wb_we(wb_m_we),
	.wb_err(wb_m_err)
);
endmodule
