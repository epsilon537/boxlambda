module sprite_line_buffer (
	rst,
	clk,
	active_render_buffer,
	renderer_rd_idx,
	renderer_rd_data,
	renderer_wr_idx,
	renderer_wr_data,
	renderer_wr_en,
	composer_rd_idx,
	composer_rd_data,
	composer_erase_start
);
	input wire rst;
	input wire clk;
	input wire active_render_buffer;
	input wire [9:0] renderer_rd_idx;
	output wire [15:0] renderer_rd_data;
	input wire [9:0] renderer_wr_idx;
	input wire [15:0] renderer_wr_data;
	input wire renderer_wr_en;
	input wire [9:0] composer_rd_idx;
	output wire [15:0] composer_rd_data;
	input wire composer_erase_start;
	wire [9:0] wr_addr_1;
	wire [15:0] wr_data_1;
	wire wr_en_1a;
	wire wr_en_1b;
	wire wr_en_1c;
	wire wr_en_1d;
	wire [9:0] rd_addr_1;
	wire [15:0] rd_data_1a;
	wire [15:0] rd_data_1b;
	wire [15:0] rd_data_1c;
	wire [15:0] rd_data_1d;
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_1a(
		.wr_clk(clk),
		.wr_addr(wr_addr_1[9:2]),
		.wr_data(wr_data_1),
		.wr_en(wr_en_1a),
		.rd_clk(clk),
		.rd_addr(rd_addr_1[9:2]),
		.rd_data(rd_data_1a)
	);
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_1b(
		.wr_clk(clk),
		.wr_addr(wr_addr_1[9:2]),
		.wr_data(wr_data_1),
		.wr_en(wr_en_1b),
		.rd_clk(clk),
		.rd_addr(rd_addr_1[9:2]),
		.rd_data(rd_data_1b)
	);
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_1c(
		.wr_clk(clk),
		.wr_addr(wr_addr_1[9:2]),
		.wr_data(wr_data_1),
		.wr_en(wr_en_1c),
		.rd_clk(clk),
		.rd_addr(rd_addr_1[9:2]),
		.rd_data(rd_data_1c)
	);
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_1d(
		.wr_clk(clk),
		.wr_addr(wr_addr_1[9:2]),
		.wr_data(wr_data_1),
		.wr_en(wr_en_1d),
		.rd_clk(clk),
		.rd_addr(rd_addr_1[9:2]),
		.rd_data(rd_data_1d)
	);
	reg [1:0] rd_addr_1_sel_r;
	always @(posedge clk) rd_addr_1_sel_r <= rd_addr_1[1:0];
	reg [15:0] rd_data_1;
	always @(*)
		case (rd_addr_1_sel_r)
			2'b00: rd_data_1 = rd_data_1a;
			2'b01: rd_data_1 = rd_data_1b;
			2'b10: rd_data_1 = rd_data_1c;
			2'b11: rd_data_1 = rd_data_1d;
		endcase
	wire [9:0] wr_addr_2;
	wire [15:0] wr_data_2;
	wire wr_en_2a;
	wire wr_en_2b;
	wire wr_en_2c;
	wire wr_en_2d;
	wire [9:0] rd_addr_2;
	wire [15:0] rd_data_2a;
	wire [15:0] rd_data_2b;
	wire [15:0] rd_data_2c;
	wire [15:0] rd_data_2d;
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_2a(
		.wr_clk(clk),
		.wr_addr(wr_addr_2[9:2]),
		.wr_data(wr_data_2),
		.wr_en(wr_en_2a),
		.rd_clk(clk),
		.rd_addr(rd_addr_2[9:2]),
		.rd_data(rd_data_2a)
	);
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_2b(
		.wr_clk(clk),
		.wr_addr(wr_addr_2[9:2]),
		.wr_data(wr_data_2),
		.wr_en(wr_en_2b),
		.rd_clk(clk),
		.rd_addr(rd_addr_2[9:2]),
		.rd_data(rd_data_2b)
	);
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_2c(
		.wr_clk(clk),
		.wr_addr(wr_addr_2[9:2]),
		.wr_data(wr_data_2),
		.wr_en(wr_en_2c),
		.rd_clk(clk),
		.rd_addr(rd_addr_2[9:2]),
		.rd_data(rd_data_2c)
	);
	dpram #(
		.ADDR_WIDTH(8),
		.DATA_WIDTH(16)
	) linebuf_2d(
		.wr_clk(clk),
		.wr_addr(wr_addr_2[9:2]),
		.wr_data(wr_data_2),
		.wr_en(wr_en_2d),
		.rd_clk(clk),
		.rd_addr(rd_addr_2[9:2]),
		.rd_data(rd_data_2d)
	);
	reg [1:0] rd_addr_2_sel_r;
	always @(posedge clk) rd_addr_2_sel_r <= rd_addr_2[1:0];
	reg [15:0] rd_data_2;
	always @(*)
		case (rd_addr_2_sel_r)
			2'b00: rd_data_2 = rd_data_2a;
			2'b01: rd_data_2 = rd_data_2b;
			2'b10: rd_data_2 = rd_data_2c;
			2'b11: rd_data_2 = rd_data_2d;
		endcase
	reg [7:0] composer_wr_idx;
	reg composer_wr_en;
	wire composer_erase_busy = composer_wr_idx != 'd159;
	always @(posedge clk)
		if (rst) begin
			composer_wr_idx <= 'd159;
			composer_wr_en <= 0;
		end
		else begin
			composer_wr_en <= 0;
			if (composer_erase_start) begin
				composer_wr_idx <= 0;
				composer_wr_en <= 1;
			end
			else if (composer_erase_busy) begin
				composer_wr_idx <= composer_wr_idx + 8'd1;
				composer_wr_en <= 1;
			end
		end
	wire renderer_wr_en_a = renderer_wr_en && (renderer_wr_idx[1:0] == 2'b00);
	wire renderer_wr_en_b = renderer_wr_en && (renderer_wr_idx[1:0] == 2'b01);
	wire renderer_wr_en_c = renderer_wr_en && (renderer_wr_idx[1:0] == 2'b10);
	wire renderer_wr_en_d = renderer_wr_en && (renderer_wr_idx[1:0] == 2'b11);
	assign rd_addr_1 = (!active_render_buffer ? renderer_rd_idx : composer_rd_idx);
	assign rd_addr_2 = (active_render_buffer ? renderer_rd_idx : composer_rd_idx);
	assign wr_addr_1 = (!active_render_buffer ? renderer_wr_idx : {composer_wr_idx, 2'b00});
	assign wr_addr_2 = (active_render_buffer ? renderer_wr_idx : {composer_wr_idx, 2'b00});
	assign wr_data_1 = (!active_render_buffer ? renderer_wr_data : 16'b0000000000000000);
	assign wr_data_2 = (active_render_buffer ? renderer_wr_data : 16'b0000000000000000);
	assign wr_data_1 = (!active_render_buffer ? renderer_wr_data : 16'b0000000000000000);
	assign wr_data_2 = (active_render_buffer ? renderer_wr_data : 16'b0000000000000000);
	assign wr_en_1a = (!active_render_buffer ? renderer_wr_en_a : composer_wr_en);
	assign wr_en_2a = (active_render_buffer ? renderer_wr_en_a : composer_wr_en);
	assign wr_en_1b = (!active_render_buffer ? renderer_wr_en_b : composer_wr_en);
	assign wr_en_2b = (active_render_buffer ? renderer_wr_en_b : composer_wr_en);
	assign wr_en_1c = (!active_render_buffer ? renderer_wr_en_c : composer_wr_en);
	assign wr_en_2c = (active_render_buffer ? renderer_wr_en_c : composer_wr_en);
	assign wr_en_1d = (!active_render_buffer ? renderer_wr_en_d : composer_wr_en);
	assign wr_en_2d = (active_render_buffer ? renderer_wr_en_d : composer_wr_en);
	assign renderer_rd_data = (!active_render_buffer ? rd_data_1 : rd_data_2);
	assign composer_rd_data = (active_render_buffer ? rd_data_1 : rd_data_2);
endmodule
