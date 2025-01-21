module layer_renderer (
	rst,
	clk,
	line_idx,
	line_render_start,
	color_depth,
	bitmap_mode,
	attr_mode,
	tile_height,
	tile_width,
	map_height,
	map_width,
	map_baseaddr,
	tile_baseaddr,
	hscroll,
	vscroll,
	bus_addr,
	bus_rddata,
	bus_strobe,
	bus_ack,
	linebuf_wridx,
	linebuf_wrdata,
	linebuf_wren
);
	input wire rst;
	input wire clk;
	input wire [8:0] line_idx;
	input wire line_render_start;
	input wire [1:0] color_depth;
	input wire bitmap_mode;
	input wire attr_mode;
	input wire tile_height;
	input wire tile_width;
	input wire [1:0] map_height;
	input wire [1:0] map_width;
	input wire [7:0] map_baseaddr;
	input wire [7:0] tile_baseaddr;
	input wire [11:0] hscroll;
	input wire [11:0] vscroll;
	output reg [14:0] bus_addr;
	input wire [31:0] bus_rddata;
	output wire bus_strobe;
	input wire bus_ack;
	output reg [9:0] linebuf_wridx;
	output reg [7:0] linebuf_wrdata;
	output reg linebuf_wren;
	wire [2:0] mode = {bitmap_mode, color_depth};
	reg [4:0] pixels_per_word_minus1;
	always @(*)
		case (mode)
			3'd0: pixels_per_word_minus1 = (tile_width ? 5'd15 : 5'd7);
			3'd1: pixels_per_word_minus1 = (tile_width ? 5'd15 : 5'd7);
			3'd2: pixels_per_word_minus1 = 5'd7;
			3'd3: pixels_per_word_minus1 = 5'd3;
			3'd4: pixels_per_word_minus1 = 5'd31;
			3'd5: pixels_per_word_minus1 = 5'd15;
			3'd6: pixels_per_word_minus1 = 5'd7;
			3'd7: pixels_per_word_minus1 = 5'd3;
		endcase
	reg [1:0] lines_per_word_minus1;
	always @(*)
		case (mode)
			3'd0: lines_per_word_minus1 = (tile_width ? 2'd1 : 2'd3);
			3'd1: lines_per_word_minus1 = (tile_width ? 2'd0 : 2'd1);
			3'd2: lines_per_word_minus1 = 2'd0;
			3'd3: lines_per_word_minus1 = 2'd0;
			3'd4: lines_per_word_minus1 = 2'd0;
			3'd5: lines_per_word_minus1 = 2'd0;
			3'd6: lines_per_word_minus1 = 2'd0;
			3'd7: lines_per_word_minus1 = 2'd0;
		endcase
	reg [1:0] words_per_line_minus1;
	always @(*)
		case (mode)
			3'd0: words_per_line_minus1 = 2'd0;
			3'd1: words_per_line_minus1 = 2'd0;
			3'd2: words_per_line_minus1 = (tile_width ? 2'd1 : 2'd0);
			3'd3: words_per_line_minus1 = (tile_width ? 2'd3 : 2'd1);
			3'd4: words_per_line_minus1 = 2'd0;
			3'd5: words_per_line_minus1 = 2'd0;
			3'd6: words_per_line_minus1 = 2'd0;
			3'd7: words_per_line_minus1 = 2'd0;
		endcase
	wire [11:0] scrolled_line_idx = {3'b000, line_idx} + vscroll;
	wire [7:0] vmap_idx = (tile_height ? {scrolled_line_idx[11:4]} : scrolled_line_idx[10:3]);
	reg [7:0] wrapped_vmap_idx;
	always @(*)
		case (map_height)
			2'd0: wrapped_vmap_idx = {3'b000, vmap_idx[4:0]};
			2'd1: wrapped_vmap_idx = {2'b00, vmap_idx[5:0]};
			2'd2: wrapped_vmap_idx = {1'b0, vmap_idx[6:0]};
			2'd3: wrapped_vmap_idx = {vmap_idx[7:0]};
		endcase
	reg [7:0] htile_cnt_r;
	reg [1:0] word_cnt_r;
	wire [7:0] scrolled_htile_cnt = htile_cnt_r + (tile_width ? hscroll[11:4] : {hscroll[10:3]});
	reg [15:0] map_idx;
	always @(*)
		case (map_width)
			2'd0: map_idx = {3'b000, wrapped_vmap_idx, scrolled_htile_cnt[4:0]};
			2'd1: map_idx = {2'b00, wrapped_vmap_idx, scrolled_htile_cnt[5:0]};
			2'd2: map_idx = {1'b0, wrapped_vmap_idx, scrolled_htile_cnt[6:0]};
			2'd3: map_idx = {wrapped_vmap_idx, scrolled_htile_cnt[7:0]};
		endcase
	wire [14:0] map_addr = {map_baseaddr, 7'b0000000} + map_idx[15:1];
	reg [31:0] map_data_r;
	wire [15:0] cur_map_data = (map_idx[0] ? map_data_r[31:16] : map_data_r[15:0]);
	wire [9:0] cur_tile_idx = (color_depth == 'd0 ? {2'b00, cur_map_data[7:0]} : cur_map_data[9:0]);
	wire cur_tile_vflip = (color_depth == 'd0 ? 1'b0 : cur_map_data[11]);
	wire cur_tile_hflip = (color_depth == 'd0 ? 1'b0 : cur_map_data[10]);
	wire [3:0] vflipped_line_idx = (cur_tile_vflip ? ~scrolled_line_idx[3:0] : scrolled_line_idx[3:0]);
	wire [1:0] hflipped_word_cnt = (cur_tile_hflip ? ~word_cnt_r : word_cnt_r);
	wire [14:0] tile_addr_1bpp_8x8 = {4'b0000, cur_tile_idx, vflipped_line_idx[2]};
	wire [14:0] tile_addr_1bpp_8x16 = {3'b000, cur_tile_idx, vflipped_line_idx[3:2]};
	wire [14:0] tile_addr_1bpp_16x8 = {3'b000, cur_tile_idx, vflipped_line_idx[2:1]};
	wire [14:0] tile_addr_1bpp_16x16 = {2'b00, cur_tile_idx, vflipped_line_idx[3:1]};
	reg [14:0] tile_addr_1bpp;
	always @(*)
		case ({tile_width, tile_height})
			2'b00: tile_addr_1bpp = tile_addr_1bpp_8x8;
			2'b01: tile_addr_1bpp = tile_addr_1bpp_8x16;
			2'b10: tile_addr_1bpp = tile_addr_1bpp_16x8;
			2'b11: tile_addr_1bpp = tile_addr_1bpp_16x16;
		endcase
	wire [14:0] tile_addr_2bpp_8x8 = {3'b000, cur_tile_idx, vflipped_line_idx[2:1]};
	wire [14:0] tile_addr_2bpp_8x16 = {2'b00, cur_tile_idx, vflipped_line_idx[3:1]};
	wire [14:0] tile_addr_2bpp_16x8 = {2'b00, cur_tile_idx, vflipped_line_idx[2:0]};
	wire [14:0] tile_addr_2bpp_16x16 = {1'b0, cur_tile_idx, vflipped_line_idx[3:0]};
	reg [14:0] tile_addr_2bpp;
	always @(*)
		case ({tile_width, tile_height})
			2'b00: tile_addr_2bpp = tile_addr_2bpp_8x8;
			2'b01: tile_addr_2bpp = tile_addr_2bpp_8x16;
			2'b10: tile_addr_2bpp = tile_addr_2bpp_16x8;
			2'b11: tile_addr_2bpp = tile_addr_2bpp_16x16;
		endcase
	wire [14:0] tile_addr_4bpp_8x8 = {2'b00, cur_tile_idx, vflipped_line_idx[2:0]};
	wire [14:0] tile_addr_4bpp_8x16 = {1'b0, cur_tile_idx, vflipped_line_idx[3:0]};
	wire [14:0] tile_addr_4bpp_16x8 = {1'b0, cur_tile_idx, vflipped_line_idx[2:0], hflipped_word_cnt[0]};
	wire [14:0] tile_addr_4bpp_16x16 = {cur_tile_idx, vflipped_line_idx[3:0], hflipped_word_cnt[0]};
	reg [14:0] tile_addr_4bpp;
	always @(*)
		case ({tile_width, tile_height})
			2'b00: tile_addr_4bpp = tile_addr_4bpp_8x8;
			2'b01: tile_addr_4bpp = tile_addr_4bpp_8x16;
			2'b10: tile_addr_4bpp = tile_addr_4bpp_16x8;
			2'b11: tile_addr_4bpp = tile_addr_4bpp_16x16;
		endcase
	wire [14:0] tile_addr_8bpp_8x8 = {1'b0, cur_tile_idx, vflipped_line_idx[2:0], hflipped_word_cnt[0]};
	wire [14:0] tile_addr_8bpp_8x16 = {cur_tile_idx, vflipped_line_idx[3:0], hflipped_word_cnt[0]};
	wire [14:0] tile_addr_8bpp_16x8 = {cur_tile_idx, vflipped_line_idx[2:0], hflipped_word_cnt[1:0]};
	wire [14:0] tile_addr_8bpp_16x16 = {cur_tile_idx[8:0], vflipped_line_idx[3:0], hflipped_word_cnt[1:0]};
	reg [14:0] tile_addr_8bpp;
	always @(*)
		case ({tile_width, tile_height})
			2'b00: tile_addr_8bpp = tile_addr_8bpp_8x8;
			2'b01: tile_addr_8bpp = tile_addr_8bpp_8x16;
			2'b10: tile_addr_8bpp = tile_addr_8bpp_16x8;
			2'b11: tile_addr_8bpp = tile_addr_8bpp_16x16;
		endcase
	reg [14:0] tile_addr_xbpp;
	always @(*)
		case (color_depth)
			2'd0: tile_addr_xbpp = tile_addr_1bpp;
			2'd1: tile_addr_xbpp = tile_addr_2bpp;
			2'd2: tile_addr_xbpp = tile_addr_4bpp;
			2'd3: tile_addr_xbpp = tile_addr_8bpp;
		endcase
	wire [14:0] tile_addr = {tile_baseaddr, 7'b0000000} + tile_addr_xbpp;
	wire [11:0] line_idx_mul5 = {3'b000, line_idx} + {1'b0, line_idx, 2'b00};
	reg [14:0] bm_line_addr_tmp;
	always @(*)
		case (color_depth)
			2'd0: bm_line_addr_tmp = (tile_width ? {1'b0, line_idx_mul5, 2'b00} : {2'b00, line_idx_mul5, 1'b0});
			2'd1: bm_line_addr_tmp = (tile_width ? {line_idx_mul5, 3'b000} : {1'b0, line_idx_mul5, 2'b00});
			2'd2: bm_line_addr_tmp = (tile_width ? {line_idx_mul5[10:0], 4'b0000} : {line_idx_mul5, 3'b000});
			2'd3: bm_line_addr_tmp = (tile_width ? {line_idx_mul5[9:0], 5'b00000} : {line_idx_mul5[10:0], 4'b0000});
		endcase
	wire [14:0] bitmap_line_addr = {tile_baseaddr, 7'b0000000} + bm_line_addr_tmp;
	reg bus_strobe_r;
	assign bus_strobe = bus_strobe_r && !bus_ack;
	reg [2:0] state_r;
	parameter WAIT_START = 3'b000;
	parameter FETCH_MAP = 3'b001;
	parameter WAIT_FETCH_MAP = 3'b010;
	parameter FETCH_TILE = 3'b011;
	parameter WAIT_FETCH_TILE = 3'b100;
	parameter RENDER = 3'b101;
	reg [31:0] tile_data_r;
	reg [31:0] render_data_r;
	reg [14:0] bitmap_addr_r;
	reg [7:0] next_render_mapdata_r;
	reg [7:0] render_mapdata_r;
	reg render_start;
	wire render_busy;
	wire line_done;
	always @(posedge clk)
		if (rst) begin
			state_r <= WAIT_START;
			bus_addr <= 0;
			bus_strobe_r <= 0;
			htile_cnt_r <= 0;
			word_cnt_r <= 0;
			bitmap_addr_r <= 0;
			tile_data_r <= 0;
			render_start <= 0;
			render_mapdata_r <= 0;
			render_data_r <= 0;
			next_render_mapdata_r <= 0;
			map_data_r <= 0;
		end
		else begin
			render_start <= 0;
			case (state_r)
				WAIT_START:
					;
				FETCH_MAP: begin
					bus_addr <= map_addr;
					bus_strobe_r <= 1;
					state_r <= WAIT_FETCH_MAP;
				end
				WAIT_FETCH_MAP:
					if (bus_ack) begin
						map_data_r <= bus_rddata;
						bus_strobe_r <= 0;
						state_r <= FETCH_TILE;
					end
				FETCH_TILE: begin
					bus_addr <= (bitmap_mode ? bitmap_addr_r : tile_addr);
					bitmap_addr_r <= bitmap_addr_r + 15'd1;
					bus_strobe_r <= 1;
					state_r <= WAIT_FETCH_TILE;
				end
				WAIT_FETCH_TILE:
					if (bus_ack) begin
						tile_data_r <= bus_rddata;
						bus_strobe_r <= 0;
						next_render_mapdata_r <= cur_map_data[15:8];
						state_r <= RENDER;
					end
				RENDER: begin
					if (!render_busy) begin
						case (lines_per_word_minus1)
							2'd1: render_data_r <= {16'b0000000000000000, (vflipped_line_idx[0] ? tile_data_r[31:16] : tile_data_r[15:0])};
							2'd3:
								case (vflipped_line_idx[1:0])
									2'd0: render_data_r <= {24'b000000000000000000000000, tile_data_r[7:0]};
									2'd1: render_data_r <= {24'b000000000000000000000000, tile_data_r[15:8]};
									2'd2: render_data_r <= {24'b000000000000000000000000, tile_data_r[23:16]};
									2'd3: render_data_r <= {24'b000000000000000000000000, tile_data_r[31:24]};
								endcase
							default: render_data_r <= tile_data_r;
						endcase
						render_mapdata_r <= (bitmap_mode ? {hscroll[11:8], 4'b0000} : next_render_mapdata_r);
						render_start <= 1;
						if (bitmap_mode)
							state_r <= FETCH_TILE;
						else if (word_cnt_r == words_per_line_minus1) begin
							word_cnt_r <= 0;
							htile_cnt_r <= htile_cnt_r + 8'd1;
							state_r <= (scrolled_htile_cnt[0] ? FETCH_MAP : FETCH_TILE);
						end
						else begin
							word_cnt_r <= word_cnt_r + 2'd1;
							state_r <= FETCH_TILE;
						end
					end
					if (line_done)
						state_r <= WAIT_START;
				end
				default:
					;
			endcase
			if (line_render_start) begin
				state_r <= (bitmap_mode ? FETCH_TILE : FETCH_MAP);
				htile_cnt_r <= 0;
				word_cnt_r <= 0;
				bitmap_addr_r <= bitmap_line_addr;
			end
		end
	reg [4:0] xcnt_r;
	reg [4:0] xcnt_next;
	wire [3:0] hflipped_xcnt = (render_mapdata_r[2] ? ~xcnt_r[3:0] : xcnt_r[3:0]);
	reg cur_pixel_data_1bpp;
	always @(*)
		case (xcnt_r)
			5'd0: cur_pixel_data_1bpp = render_data_r[7];
			5'd1: cur_pixel_data_1bpp = render_data_r[6];
			5'd2: cur_pixel_data_1bpp = render_data_r[5];
			5'd3: cur_pixel_data_1bpp = render_data_r[4];
			5'd4: cur_pixel_data_1bpp = render_data_r[3];
			5'd5: cur_pixel_data_1bpp = render_data_r[2];
			5'd6: cur_pixel_data_1bpp = render_data_r[1];
			5'd7: cur_pixel_data_1bpp = render_data_r[0];
			5'd8: cur_pixel_data_1bpp = render_data_r[15];
			5'd9: cur_pixel_data_1bpp = render_data_r[14];
			5'd10: cur_pixel_data_1bpp = render_data_r[13];
			5'd11: cur_pixel_data_1bpp = render_data_r[12];
			5'd12: cur_pixel_data_1bpp = render_data_r[11];
			5'd13: cur_pixel_data_1bpp = render_data_r[10];
			5'd14: cur_pixel_data_1bpp = render_data_r[9];
			5'd15: cur_pixel_data_1bpp = render_data_r[8];
			5'd16: cur_pixel_data_1bpp = render_data_r[23];
			5'd17: cur_pixel_data_1bpp = render_data_r[22];
			5'd18: cur_pixel_data_1bpp = render_data_r[21];
			5'd19: cur_pixel_data_1bpp = render_data_r[20];
			5'd20: cur_pixel_data_1bpp = render_data_r[19];
			5'd21: cur_pixel_data_1bpp = render_data_r[18];
			5'd22: cur_pixel_data_1bpp = render_data_r[17];
			5'd23: cur_pixel_data_1bpp = render_data_r[16];
			5'd24: cur_pixel_data_1bpp = render_data_r[31];
			5'd25: cur_pixel_data_1bpp = render_data_r[30];
			5'd26: cur_pixel_data_1bpp = render_data_r[29];
			5'd27: cur_pixel_data_1bpp = render_data_r[28];
			5'd28: cur_pixel_data_1bpp = render_data_r[27];
			5'd29: cur_pixel_data_1bpp = render_data_r[26];
			5'd30: cur_pixel_data_1bpp = render_data_r[25];
			5'd31: cur_pixel_data_1bpp = render_data_r[24];
		endcase
	reg [1:0] cur_pixel_data_2bpp;
	always @(*)
		case (hflipped_xcnt[3:0])
			4'd0: cur_pixel_data_2bpp = render_data_r[7:6];
			4'd1: cur_pixel_data_2bpp = render_data_r[5:4];
			4'd2: cur_pixel_data_2bpp = render_data_r[3:2];
			4'd3: cur_pixel_data_2bpp = render_data_r[1:0];
			4'd4: cur_pixel_data_2bpp = render_data_r[15:14];
			4'd5: cur_pixel_data_2bpp = render_data_r[13:12];
			4'd6: cur_pixel_data_2bpp = render_data_r[11:10];
			4'd7: cur_pixel_data_2bpp = render_data_r[9:8];
			4'd8: cur_pixel_data_2bpp = render_data_r[23:22];
			4'd9: cur_pixel_data_2bpp = render_data_r[21:20];
			4'd10: cur_pixel_data_2bpp = render_data_r[19:18];
			4'd11: cur_pixel_data_2bpp = render_data_r[17:16];
			4'd12: cur_pixel_data_2bpp = render_data_r[31:30];
			4'd13: cur_pixel_data_2bpp = render_data_r[29:28];
			4'd14: cur_pixel_data_2bpp = render_data_r[27:26];
			4'd15: cur_pixel_data_2bpp = render_data_r[25:24];
		endcase
	reg [3:0] cur_pixel_data_4bpp;
	always @(*)
		case (hflipped_xcnt[2:0])
			3'd0: cur_pixel_data_4bpp = render_data_r[7:4];
			3'd1: cur_pixel_data_4bpp = render_data_r[3:0];
			3'd2: cur_pixel_data_4bpp = render_data_r[15:12];
			3'd3: cur_pixel_data_4bpp = render_data_r[11:8];
			3'd4: cur_pixel_data_4bpp = render_data_r[23:20];
			3'd5: cur_pixel_data_4bpp = render_data_r[19:16];
			3'd6: cur_pixel_data_4bpp = render_data_r[31:28];
			3'd7: cur_pixel_data_4bpp = render_data_r[27:24];
		endcase
	reg [7:0] cur_pixel_data_8bpp;
	always @(*)
		case (hflipped_xcnt[1:0])
			2'd0: cur_pixel_data_8bpp = render_data_r[7:0];
			2'd1: cur_pixel_data_8bpp = render_data_r[15:8];
			2'd2: cur_pixel_data_8bpp = render_data_r[23:16];
			2'd3: cur_pixel_data_8bpp = render_data_r[31:24];
		endcase
	reg [7:0] tmp_pixel_color;
	always @(*)
		case (color_depth)
			2'd0:
				if (bitmap_mode)
					tmp_pixel_color = {7'b0000000, cur_pixel_data_1bpp};
				else if (!attr_mode)
					tmp_pixel_color = (cur_pixel_data_1bpp ? {4'b0000, render_mapdata_r[3:0]} : {4'b0000, render_mapdata_r[7:4]});
				else
					tmp_pixel_color = (cur_pixel_data_1bpp ? render_mapdata_r[7:0] : 8'd0);
			2'd1: tmp_pixel_color = {6'b000000, cur_pixel_data_2bpp};
			2'd2: tmp_pixel_color = {4'b0000, cur_pixel_data_4bpp};
			2'd3: tmp_pixel_color = cur_pixel_data_8bpp;
		endcase
	reg [7:0] cur_pixel_color;
	always @(*) begin
		cur_pixel_color[3:0] = tmp_pixel_color[3:0];
		if (((color_depth != 0) && (tmp_pixel_color[7:4] == 0)) && (tmp_pixel_color[3:0] != 0))
			cur_pixel_color[7:4] = render_mapdata_r[7:4];
		else
			cur_pixel_color[7:4] = tmp_pixel_color[7:4];
	end
	reg [9:0] lb_wridx_r;
	assign line_done = lb_wridx_r[9:7] == 3'b101;
	reg [9:0] linebuf_wridx_next;
	reg [7:0] linebuf_wrdata_next;
	reg linebuf_wren_next;
	reg [9:0] lb_wridx_next;
	wire [3:0] subtile_hscroll = (tile_width ? hscroll[3:0] : {1'b0, hscroll[2:0]});
	wire [9:0] lb_wridx_start = 10'd0 - {6'd0, subtile_hscroll};
	reg render_busy_r;
	reg render_busy_next;
	assign render_busy = render_busy_next;
	always @(*) begin
		xcnt_next = xcnt_r;
		linebuf_wridx_next = linebuf_wridx;
		linebuf_wrdata_next = linebuf_wrdata;
		linebuf_wren_next = 0;
		lb_wridx_next = lb_wridx_r;
		render_busy_next = render_busy_r;
		if (!line_done && (render_busy_r || render_start)) begin
			if (xcnt_r == pixels_per_word_minus1) begin
				render_busy_next = 0;
				xcnt_next = 0;
			end
			else
				xcnt_next = xcnt_r + 5'd1;
			if (render_start) begin
				xcnt_next = 'd1;
				render_busy_next = 1;
			end
			linebuf_wridx_next = lb_wridx_r;
			linebuf_wrdata_next = cur_pixel_color;
			linebuf_wren_next = 1;
			lb_wridx_next = lb_wridx_r + 10'd1;
		end
		if (line_render_start) begin
			xcnt_next = 0;
			render_busy_next = 0;
			lb_wridx_next = (bitmap_mode ? 10'd0 : lb_wridx_start);
		end
	end
	always @(posedge clk)
		if (rst) begin
			xcnt_r <= 0;
			linebuf_wridx <= 0;
			linebuf_wrdata <= 0;
			linebuf_wren <= 0;
			lb_wridx_r <= 0;
			render_busy_r <= 0;
		end
		else begin
			xcnt_r <= xcnt_next;
			linebuf_wridx <= linebuf_wridx_next;
			linebuf_wrdata <= linebuf_wrdata_next;
			linebuf_wren <= linebuf_wren_next;
			lb_wridx_r <= lb_wridx_next;
			render_busy_r <= render_busy_next;
		end
endmodule
