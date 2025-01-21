`default_nettype none
module picosoc_mem (
	clk,
	wen,
	addr,
	wdata,
	rdata
);
	parameter integer WORDS = 256;
	input wire clk;
	input wire [3:0] wen;
	input wire [21:0] addr;
	input wire [31:0] wdata;
	output reg [31:0] rdata;
	reg [31:0] mem [0:WORDS - 1];
	always @(posedge clk) begin
		rdata <= mem[addr];
		if (wen[0])
			mem[addr][7:0] <= wdata[7:0];
		if (wen[1])
			mem[addr][15:8] <= wdata[15:8];
		if (wen[2])
			mem[addr][23:16] <= wdata[23:16];
		if (wen[3])
			mem[addr][31:24] <= wdata[31:24];
	end
endmodule
