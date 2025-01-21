`default_nettype none
module palette_ram (
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
	input wire [1:0] ben_i;
	input wire [15:0] wr_data_i;
	input wire [7:0] wr_addr_i;
	input wire [7:0] rd_addr_i;
	output reg [15:0] rd_data_o;
	reg [15:0] mem [0:255];
	always @(posedge wr_clk_i)
		if (wr_en_i && wr_clk_en_i) begin
			if (ben_i[1])
				mem[wr_addr_i][15:8] <= wr_data_i[15:8];
			if (ben_i[0])
				mem[wr_addr_i][7:0] <= wr_data_i[7:0];
		end
	always @(posedge rd_clk_i)
		if (rd_en_i && rd_clk_en_i)
			rd_data_o <= mem[rd_addr_i];
	initial $readmemh("palette_ram.mem", mem);
endmodule
