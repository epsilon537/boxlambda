module layer_line_buffer (
	rst,
	clk,
	active_render_buffer,
	renderer_wr_idx,
	renderer_wr_data,
	renderer_wr_en,
	composer_rd_idx,
	composer_rd_data
);
	input wire rst;
	input wire clk;
	input wire active_render_buffer;
	input wire [9:0] renderer_wr_idx;
	input wire [7:0] renderer_wr_data;
	input wire renderer_wr_en;
	input wire [9:0] composer_rd_idx;
	output reg [7:0] composer_rd_data;
	wire active_composer_buffer = !active_render_buffer;
	wire [8:0] wr_addr_a = renderer_wr_idx[8:0];
	wire [8:0] wr_addr_b = renderer_wr_idx[8:0];
	wire [8:0] wr_addr_c = {active_render_buffer, renderer_wr_idx[7:0]};
	wire wr_en_a = (renderer_wr_en && !renderer_wr_idx[9]) && !active_render_buffer;
	wire wr_en_b = (renderer_wr_en && !renderer_wr_idx[9]) && active_render_buffer;
	wire wr_en_c = renderer_wr_en && renderer_wr_idx[9];
	wire [8:0] rd_addr_a = composer_rd_idx[8:0];
	wire [8:0] rd_addr_b = composer_rd_idx[8:0];
	wire [8:0] rd_addr_c = {active_composer_buffer, composer_rd_idx[7:0]};
	wire [7:0] rd_data_a;
	wire [7:0] rd_data_b;
	wire [7:0] rd_data_c;
	dpram #(
		.ADDR_WIDTH(9),
		.DATA_WIDTH(8)
	) linebuf_a(
		.wr_clk(clk),
		.wr_addr(wr_addr_a),
		.wr_data(renderer_wr_data),
		.wr_en(wr_en_a),
		.rd_clk(clk),
		.rd_addr(rd_addr_a),
		.rd_data(rd_data_a)
	);
	dpram #(
		.ADDR_WIDTH(9),
		.DATA_WIDTH(8)
	) linebuf_b(
		.wr_clk(clk),
		.wr_addr(wr_addr_b),
		.wr_data(renderer_wr_data),
		.wr_en(wr_en_b),
		.rd_clk(clk),
		.rd_addr(rd_addr_b),
		.rd_data(rd_data_b)
	);
	dpram #(
		.ADDR_WIDTH(9),
		.DATA_WIDTH(8)
	) linebuf_c(
		.wr_clk(clk),
		.wr_addr(wr_addr_c),
		.wr_data(renderer_wr_data),
		.wr_en(wr_en_c),
		.rd_clk(clk),
		.rd_addr(rd_addr_c),
		.rd_data(rd_data_c)
	);
	reg rd_idx9_r;
	always @(posedge clk) rd_idx9_r <= composer_rd_idx[9];
	always @(*)
		if (!rd_idx9_r)
			composer_rd_data = (!active_composer_buffer ? rd_data_a : rd_data_b);
		else
			composer_rd_data = rd_data_c;
endmodule
