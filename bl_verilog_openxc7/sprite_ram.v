`default_nettype none
module sprite_ram (
	rst_i,
	wr_clk_i,
	rd_clk_i,
	wr_clk_en_i,
	rd_en_i,
	rd_clk_en_i,
	wr_en_i,
	ben_i,
	wr_data_i,
	wr_addr_i,
	rd_addr_i,
	rd_data_o
);
	input wire rst_i;
	input wire wr_clk_i;
	input wire rd_clk_i;
	input wire wr_clk_en_i;
	input wire rd_en_i;
	input wire rd_clk_en_i;
	input wire wr_en_i;
	input wire [3:0] ben_i;
	input wire [31:0] wr_data_i;
	input wire [7:0] wr_addr_i;
	input wire [7:0] rd_addr_i;
	output reg [31:0] rd_data_o;
	reg [31:0] mem [0:255];
	always @(posedge wr_clk_i)
		if (wr_en_i) begin
			if (ben_i[3])
				mem[wr_addr_i][31:24] <= wr_data_i[31:24];
			if (ben_i[2])
				mem[wr_addr_i][23:16] <= wr_data_i[23:16];
			if (ben_i[1])
				mem[wr_addr_i][15:8] <= wr_data_i[15:8];
			if (ben_i[0])
				mem[wr_addr_i][7:0] <= wr_data_i[7:0];
		end
	always @(posedge rd_clk_i) rd_data_o <= mem[rd_addr_i];
	initial begin : INIT
		integer i;
		for (i = 0; i < 256; i = i + 1)
			mem[i] = 0;
	end
endmodule
