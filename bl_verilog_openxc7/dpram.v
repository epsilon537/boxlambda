`default_nettype none
module dpram (
	wr_clk,
	wr_addr,
	wr_en,
	wr_data,
	rd_clk,
	rd_addr,
	rd_data
);
	parameter ADDR_WIDTH = 8;
	parameter DATA_WIDTH = 8;
	input wire wr_clk;
	input wire [ADDR_WIDTH - 1:0] wr_addr;
	input wire wr_en;
	input wire [DATA_WIDTH - 1:0] wr_data;
	input wire rd_clk;
	input wire [ADDR_WIDTH - 1:0] rd_addr;
	output reg [DATA_WIDTH - 1:0] rd_data;
	reg [DATA_WIDTH - 1:0] mem [(1 << ADDR_WIDTH) - 1:0];
	always @(posedge wr_clk)
		if (wr_en)
			mem[wr_addr] <= wr_data;
	always @(posedge rd_clk) rd_data <= mem[rd_addr];
endmodule
