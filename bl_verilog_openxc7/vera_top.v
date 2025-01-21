`default_nettype none
module vera_top (
	clk,
	reset,
	wb_adr,
	wb_dat_w,
	wb_dat_r,
	wb_sel,
	wb_stall,
	wb_cyc,
	wb_stb,
	wb_ack,
	wb_we,
	wb_err,
	irq_n,
	vga_r,
	vga_g,
	vga_b,
	vga_hsync,
	vga_vsync
);
	parameter VRAM_SIZE_BYTES = 131072;
	input wire clk;
	input wire reset;
	input wire [16:0] wb_adr;
	input wire [31:0] wb_dat_w;
	output wire [31:0] wb_dat_r;
	input wire [3:0] wb_sel;
	output wire wb_stall;
	input wire wb_cyc;
	input wire wb_stb;
	output wire wb_ack;
	input wire wb_we;
	output wire wb_err;
	output wire irq_n;
	output reg [3:0] vga_r;
	output reg [3:0] vga_g;
	output reg [3:0] vga_b;
	output reg vga_hsync;
	output reg vga_vsync;
	wire [31:0] vram_dat_r;
	reg sprite_bank_select_r;
	reg sprite_bank_select_next;
	reg irq_enable_vsync_r;
	reg irq_enable_vsync_next;
	reg irq_enable_line_r;
	reg irq_enable_line_next;
	reg irq_enable_sprite_collision_r;
	reg irq_enable_sprite_collision_next;
	reg irq_status_vsync_r;
	reg irq_status_vsync_next;
	reg irq_status_line_r;
	reg irq_status_line_next;
	reg irq_status_sprite_collision_r;
	reg irq_status_sprite_collision_next;
	reg [9:0] irq_line_r;
	reg [9:0] irq_line_next;
	reg sprites_enabled_r;
	reg sprites_enabled_next;
	reg l0_enabled_r;
	reg l0_enabled_next;
	reg l1_enabled_r;
	reg l1_enabled_next;
	reg [7:0] dc_hscale_r;
	reg [7:0] dc_hscale_next;
	reg [7:0] dc_vscale_r;
	reg [7:0] dc_vscale_next;
	reg [7:0] dc_border_color_r;
	reg [7:0] dc_border_color_next;
	reg [9:0] dc_active_hstart_r;
	reg [9:0] dc_active_hstart_next;
	reg [9:0] dc_active_hstop_r;
	reg [9:0] dc_active_hstop_next;
	reg [8:0] dc_active_vstart_r;
	reg [8:0] dc_active_vstart_next;
	reg [8:0] dc_active_vstop_r;
	reg [8:0] dc_active_vstop_next;
	reg [1:0] l0_color_depth_r;
	reg [1:0] l0_color_depth_next;
	reg l0_bitmap_mode_r;
	reg l0_bitmap_mode_next;
	reg l0_attr_mode_r;
	reg l0_attr_mode_next;
	reg l0_tile_height_r;
	reg l0_tile_height_next;
	reg l0_tile_width_r;
	reg l0_tile_width_next;
	reg [1:0] l0_map_height_r;
	reg [1:0] l0_map_height_next;
	reg [1:0] l0_map_width_r;
	reg [1:0] l0_map_width_next;
	reg [7:0] l0_map_baseaddr_r;
	reg [7:0] l0_map_baseaddr_next;
	reg [7:0] l0_tile_baseaddr_r;
	reg [7:0] l0_tile_baseaddr_next;
	reg [11:0] l0_hscroll_r;
	reg [11:0] l0_hscroll_next;
	reg [11:0] l0_vscroll_r;
	reg [11:0] l0_vscroll_next;
	reg [1:0] l1_color_depth_r;
	reg [1:0] l1_color_depth_next;
	reg l1_bitmap_mode_r;
	reg l1_bitmap_mode_next;
	reg l1_attr_mode_r;
	reg l1_attr_mode_next;
	reg l1_tile_height_r;
	reg l1_tile_height_next;
	reg l1_tile_width_r;
	reg l1_tile_width_next;
	reg [1:0] l1_map_height_r;
	reg [1:0] l1_map_height_next;
	reg [1:0] l1_map_width_r;
	reg [1:0] l1_map_width_next;
	reg [7:0] l1_map_baseaddr_r;
	reg [7:0] l1_map_baseaddr_next;
	reg [7:0] l1_tile_baseaddr_r;
	reg [7:0] l1_tile_baseaddr_next;
	reg [11:0] l1_hscroll_r;
	reg [11:0] l1_hscroll_next;
	reg [11:0] l1_vscroll_r;
	reg [11:0] l1_vscroll_next;
	reg [1:0] video_output_mode_r;
	reg [1:0] video_output_mode_next;
	wire [3:0] sprite_collisions;
	wire sprcol_irq;
	wire vblank_pulse;
	wire line_irq;
	wire [9:0] scanline;
	reg [31:0] reg_rddata;
	always @(*) begin
		reg_rddata = 32'h00000000;
		if (wb_stb && !wb_we)
			case (wb_adr[5:0])
				6'h00: reg_rddata = {31'b0000000000000000000000000000000, sprite_bank_select_r};
				6'h01: reg_rddata = {24'b000000000000000000000000, dc_border_color_r};
				6'h02: reg_rddata = {29'b00000000000000000000000000000, irq_enable_sprite_collision_r, irq_enable_line_r, irq_enable_vsync_r};
				6'h03: reg_rddata = {24'b000000000000000000000000, sprite_collisions, 1'b0, irq_status_sprite_collision_r, irq_status_line_r, irq_status_vsync_r};
				6'h04: reg_rddata = {22'b0000000000000000000000, irq_line_r};
				6'h05: reg_rddata = {22'b0000000000000000000000, scanline};
				6'h06: reg_rddata = {25'b0000000000000000000000000, sprites_enabled_r, l1_enabled_r, l0_enabled_r, 2'b00, video_output_mode_r};
				6'h08: reg_rddata = {24'b000000000000000000000000, dc_hscale_r};
				6'h09: reg_rddata = {24'b000000000000000000000000, dc_vscale_r};
				6'h0a: reg_rddata = {22'b0000000000000000000000, dc_active_hstart_r};
				6'h0b: reg_rddata = {22'b0000000000000000000000, dc_active_hstop_r};
				6'h0c: reg_rddata = {23'b00000000000000000000000, dc_active_vstart_r};
				6'h0d: reg_rddata = {23'b00000000000000000000000, dc_active_vstop_r};
				6'h10: reg_rddata = {24'b000000000000000000000000, l0_map_height_r, l0_map_width_r, l0_attr_mode_r, l0_bitmap_mode_r, l0_color_depth_r};
				6'h11: reg_rddata = {24'b000000000000000000000000, l0_map_baseaddr_r};
				6'h12: reg_rddata = {24'b000000000000000000000000, l0_tile_baseaddr_r[7:2], l0_tile_height_r, l0_tile_width_r};
				6'h14: reg_rddata = {20'b00000000000000000000, l0_hscroll_r};
				6'h15: reg_rddata = {20'b00000000000000000000, l0_vscroll_r};
				6'h20: reg_rddata = {24'b000000000000000000000000, l1_map_height_r, l1_map_width_r, l1_attr_mode_r, l1_bitmap_mode_r, l1_color_depth_r};
				6'h21: reg_rddata = {24'b000000000000000000000000, l1_map_baseaddr_r};
				6'h22: reg_rddata = {24'b000000000000000000000000, l1_tile_baseaddr_r[7:2], l1_tile_height_r, l1_tile_width_r};
				6'h24: reg_rddata = {20'b00000000000000000000, l1_hscroll_r};
				6'h25: reg_rddata = {20'b00000000000000000000, l1_vscroll_r};
				default: reg_rddata = 32'h00000000;
			endcase
	end
	assign wb_dat_r = (wb_adr < (17'h01000 >> 2) ? reg_rddata : vram_dat_r);
	wire [3:0] irq_enable = {1'b0, irq_enable_sprite_collision_r, irq_enable_line_r, irq_enable_vsync_r};
	wire [3:0] irq_status = {1'b0, irq_status_sprite_collision_r, irq_status_line_r, irq_status_vsync_r};
	assign irq_n = (irq_status & irq_enable) == 0;
	reg [5:0] wraddr_r;
	reg [31:0] wrdata_r;
	reg do_reg_read;
	reg do_reg_write;
	reg spr_pal_ram_wb_ack_r;
	wire vram_ack;
	always @(posedge clk) begin
		do_reg_read <= 1'b0;
		do_reg_write <= 1'b0;
		if (((!do_reg_write && wb_stb) && wb_we) && (wb_adr < (17'h01000 >> 2))) begin
			wrdata_r <= wb_dat_w;
			wraddr_r <= wb_adr[5:0];
			do_reg_write <= 1'b1;
		end
		if (((!do_reg_read && wb_stb) && !wb_we) && (wb_adr < (17'h01000 >> 2)))
			do_reg_read <= 1'b1;
	end
	assign wb_ack = (((do_reg_read | do_reg_write) | vram_ack) | spr_pal_ram_wb_ack_r) & wb_cyc;
	assign wb_err = 1'b0;
	assign wb_stall = (!wb_cyc ? 1'b0 : !wb_ack);
	always @(*) begin
		sprite_bank_select_next = sprite_bank_select_r;
		irq_enable_vsync_next = irq_enable_vsync_r;
		irq_enable_line_next = irq_enable_line_r;
		irq_enable_sprite_collision_next = irq_enable_sprite_collision_r;
		irq_status_vsync_next = irq_status_vsync_r;
		irq_status_line_next = irq_status_line_r;
		irq_status_sprite_collision_next = irq_status_sprite_collision_r;
		irq_line_next = irq_line_r;
		sprites_enabled_next = sprites_enabled_r;
		l0_enabled_next = l0_enabled_r;
		l1_enabled_next = l1_enabled_r;
		dc_hscale_next = dc_hscale_r;
		dc_vscale_next = dc_vscale_r;
		dc_border_color_next = dc_border_color_r;
		dc_active_hstart_next = dc_active_hstart_r;
		dc_active_hstop_next = dc_active_hstop_r;
		dc_active_vstart_next = dc_active_vstart_r;
		dc_active_vstop_next = dc_active_vstop_r;
		l0_color_depth_next = l0_color_depth_r;
		l0_bitmap_mode_next = l0_bitmap_mode_r;
		l0_attr_mode_next = l0_attr_mode_r;
		l0_tile_height_next = l0_tile_height_r;
		l0_tile_width_next = l0_tile_width_r;
		l0_map_height_next = l0_map_height_r;
		l0_map_width_next = l0_map_width_r;
		l0_map_baseaddr_next = l0_map_baseaddr_r;
		l0_tile_baseaddr_next = l0_tile_baseaddr_r;
		l0_hscroll_next = l0_hscroll_r;
		l0_vscroll_next = l0_vscroll_r;
		l1_color_depth_next = l1_color_depth_r;
		l1_bitmap_mode_next = l1_bitmap_mode_r;
		l1_attr_mode_next = l1_attr_mode_r;
		l1_tile_height_next = l1_tile_height_r;
		l1_tile_width_next = l1_tile_width_r;
		l1_map_height_next = l1_map_height_r;
		l1_map_width_next = l1_map_width_r;
		l1_map_baseaddr_next = l1_map_baseaddr_r;
		l1_tile_baseaddr_next = l1_tile_baseaddr_r;
		l1_hscroll_next = l1_hscroll_r;
		l1_vscroll_next = l1_vscroll_r;
		video_output_mode_next = video_output_mode_r;
		if (do_reg_write)
			case (wraddr_r[5:0])
				6'h00: sprite_bank_select_next = wrdata_r[0];
				6'h01: dc_border_color_next = wrdata_r[7:0];
				6'h02: begin
					irq_enable_sprite_collision_next = wrdata_r[2];
					irq_enable_line_next = wrdata_r[1];
					irq_enable_vsync_next = wrdata_r[0];
				end
				6'h03: begin
					irq_status_sprite_collision_next = irq_status_sprite_collision_r & !wrdata_r[2];
					irq_status_line_next = irq_status_line_r & !wrdata_r[1];
					irq_status_vsync_next = irq_status_vsync_r & !wrdata_r[0];
				end
				6'h04: irq_line_next = wrdata_r[9:0];
				6'h06: begin
					sprites_enabled_next = wrdata_r[6];
					l1_enabled_next = wrdata_r[5];
					l0_enabled_next = wrdata_r[4];
					video_output_mode_next = wrdata_r[1:0];
				end
				6'h08: dc_hscale_next = wrdata_r[7:0];
				6'h09: dc_vscale_next = wrdata_r[7:0];
				6'h0a: dc_active_hstart_next = wrdata_r[9:0];
				6'h0b: dc_active_hstop_next = wrdata_r[9:0];
				6'h0c: dc_active_vstart_next = wrdata_r[8:0];
				6'h0d: dc_active_vstop_next = wrdata_r[8:0];
				6'h10: begin
					l0_map_height_next = wrdata_r[7:6];
					l0_map_width_next = wrdata_r[5:4];
					l0_attr_mode_next = wrdata_r[3];
					l0_bitmap_mode_next = wrdata_r[2];
					l0_color_depth_next = wrdata_r[1:0];
				end
				6'h11: l0_map_baseaddr_next = wrdata_r[7:0];
				6'h12: begin
					l0_tile_baseaddr_next[7:2] = wrdata_r[7:2];
					l0_tile_baseaddr_next[1:0] = 0;
					l0_tile_height_next = wrdata_r[1];
					l0_tile_width_next = wrdata_r[0];
				end
				6'h14: l0_hscroll_next = wrdata_r[11:0];
				6'h15: l0_vscroll_next = wrdata_r[11:0];
				6'h20: begin
					l1_map_height_next = wrdata_r[7:6];
					l1_map_width_next = wrdata_r[5:4];
					l1_attr_mode_next = wrdata_r[3];
					l1_bitmap_mode_next = wrdata_r[2];
					l1_color_depth_next = wrdata_r[1:0];
				end
				6'h21: l1_map_baseaddr_next = wrdata_r[7:0];
				6'h22: begin
					l1_tile_baseaddr_next[7:2] = wrdata_r[7:2];
					l1_tile_baseaddr_next[1:0] = 0;
					l1_tile_height_next = wrdata_r[1];
					l1_tile_width_next = wrdata_r[0];
				end
				6'h24: l1_hscroll_next = wrdata_r[11:0];
				6'h25: l1_vscroll_next = wrdata_r[11:0];
				default:
					;
			endcase
		if (sprcol_irq)
			irq_status_sprite_collision_next = 1;
		if (line_irq)
			irq_status_line_next = 1;
		if (vblank_pulse)
			irq_status_vsync_next = 1;
	end
	always @(posedge clk)
		if (reset) begin
			sprite_bank_select_r <= 0;
			irq_enable_vsync_r <= 0;
			irq_enable_line_r <= 0;
			irq_enable_sprite_collision_r <= 0;
			irq_status_vsync_r <= 0;
			irq_status_line_r <= 0;
			irq_status_sprite_collision_r <= 0;
			irq_line_r <= 0;
			sprites_enabled_r <= 0;
			l0_enabled_r <= 0;
			l1_enabled_r <= 0;
			dc_hscale_r <= 8'd128;
			dc_vscale_r <= 8'd128;
			dc_border_color_r <= 0;
			dc_active_hstart_r <= 10'd0;
			dc_active_hstop_r <= 10'd640;
			dc_active_vstart_r <= 9'd0;
			dc_active_vstop_r <= 9'd480;
			l0_color_depth_r <= 0;
			l0_bitmap_mode_r <= 0;
			l0_attr_mode_r <= 0;
			l0_tile_height_r <= 0;
			l0_tile_width_r <= 0;
			l0_map_height_r <= 0;
			l0_map_width_r <= 0;
			l0_map_baseaddr_r <= 0;
			l0_tile_baseaddr_r <= 0;
			l0_hscroll_r <= 0;
			l0_vscroll_r <= 0;
			l1_color_depth_r <= 0;
			l1_bitmap_mode_r <= 0;
			l1_attr_mode_r <= 0;
			l1_tile_height_r <= 0;
			l1_tile_width_r <= 0;
			l1_map_height_r <= 0;
			l1_map_width_r <= 0;
			l1_map_baseaddr_r <= 0;
			l1_tile_baseaddr_r <= 0;
			l1_hscroll_r <= 0;
			l1_vscroll_r <= 0;
			video_output_mode_r <= 0;
		end
		else begin
			sprite_bank_select_r <= sprite_bank_select_next;
			irq_enable_vsync_r <= irq_enable_vsync_next;
			irq_enable_line_r <= irq_enable_line_next;
			irq_enable_sprite_collision_r <= irq_enable_sprite_collision_next;
			irq_status_vsync_r <= irq_status_vsync_next;
			irq_status_line_r <= irq_status_line_next;
			irq_status_sprite_collision_r <= irq_status_sprite_collision_next;
			irq_line_r <= irq_line_next;
			sprites_enabled_r <= sprites_enabled_next;
			l0_enabled_r <= l0_enabled_next;
			l1_enabled_r <= l1_enabled_next;
			dc_hscale_r <= dc_hscale_next;
			dc_vscale_r <= dc_vscale_next;
			dc_border_color_r <= dc_border_color_next;
			dc_active_hstart_r <= dc_active_hstart_next;
			dc_active_hstop_r <= dc_active_hstop_next;
			dc_active_vstart_r <= dc_active_vstart_next;
			dc_active_vstop_r <= dc_active_vstop_next;
			l0_color_depth_r <= l0_color_depth_next;
			l0_bitmap_mode_r <= l0_bitmap_mode_next;
			l0_attr_mode_r <= l0_attr_mode_next;
			l0_tile_height_r <= l0_tile_height_next;
			l0_tile_width_r <= l0_tile_width_next;
			l0_map_height_r <= l0_map_height_next;
			l0_map_width_r <= l0_map_width_next;
			l0_map_baseaddr_r <= l0_map_baseaddr_next;
			l0_tile_baseaddr_r <= l0_tile_baseaddr_next;
			l0_hscroll_r <= l0_hscroll_next;
			l0_vscroll_r <= l0_vscroll_next;
			l1_color_depth_r <= l1_color_depth_next;
			l1_bitmap_mode_r <= l1_bitmap_mode_next;
			l1_attr_mode_r <= l1_attr_mode_next;
			l1_tile_height_r <= l1_tile_height_next;
			l1_tile_width_r <= l1_tile_width_next;
			l1_map_height_r <= l1_map_height_next;
			l1_map_width_r <= l1_map_width_next;
			l1_map_baseaddr_r <= l1_map_baseaddr_next;
			l1_tile_baseaddr_r <= l1_tile_baseaddr_next;
			l1_hscroll_r <= l1_hscroll_next;
			l1_vscroll_r <= l1_vscroll_next;
			video_output_mode_r <= video_output_mode_next;
		end
	wire [14:0] l0_addr;
	wire [31:0] l0_rddata;
	wire l0_strobe;
	wire l0_ack;
	wire [14:0] l1_addr;
	wire [31:0] l1_rddata;
	wire l1_strobe;
	wire l1_ack;
	wire [14:0] spr_addr;
	wire [31:0] spr_rddata;
	wire spr_strobe;
	wire spr_ack;
	localparam VRAM_START_WORD_ADDR = 17'h10000;
	vram_if #(.VRAM_SIZE_BYTES(VRAM_SIZE_BYTES)) vram_if(
		.clk(clk),
		.if0_addr(wb_adr[14:0]),
		.if0_wrdata(wb_dat_w),
		.if0_rddata(vram_dat_r),
		.if0_wrbytesel(wb_sel),
		.if0_strobe(wb_stb && (wb_adr >= VRAM_START_WORD_ADDR)),
		.if0_write(wb_we),
		.if0_ack(vram_ack),
		.if1_addr(l0_addr),
		.if1_rddata(l0_rddata),
		.if1_strobe(l0_strobe & l0_enabled_r),
		.if1_ack(l0_ack),
		.if2_addr(l1_addr),
		.if2_rddata(l1_rddata),
		.if2_strobe(l1_strobe & l1_enabled_r),
		.if2_ack(l1_ack),
		.if3_addr(spr_addr),
		.if3_rddata(spr_rddata),
		.if3_strobe(spr_strobe),
		.if3_ack(spr_ack)
	);
	wire next_line;
	wire [9:0] lb_rdidx;
	wire [7:0] l0_lb_rddata;
	wire [7:0] l1_lb_rddata;
	wire [15:0] spr_lb_rddata;
	wire spr_lb_erase_start;
	wire [8:0] line_idx;
	wire line_render_start;
	reg active_line_buf_r;
	reg clk_en = 0;
	always @(posedge clk)
		if (reset) begin
			active_line_buf_r <= 0;
			clk_en <= 0;
		end
		else begin
			clk_en <= ~clk_en;
			if (next_line && clk_en)
				active_line_buf_r <= !active_line_buf_r;
		end
	wire [9:0] l0_linebuf_wridx;
	wire [7:0] l0_linebuf_wrdata;
	wire l0_linebuf_wren;
	layer_renderer l0_renderer(
		.rst(reset),
		.clk(clk),
		.line_idx(line_idx),
		.line_render_start(line_render_start),
		.color_depth(l0_color_depth_r),
		.bitmap_mode(l0_bitmap_mode_r),
		.attr_mode(l0_attr_mode_r),
		.tile_height(l0_tile_height_r),
		.tile_width(l0_tile_width_r),
		.map_height(l0_map_height_r),
		.map_width(l0_map_width_r),
		.map_baseaddr(l0_map_baseaddr_r),
		.tile_baseaddr(l0_tile_baseaddr_r),
		.hscroll(l0_hscroll_r),
		.vscroll(l0_vscroll_r),
		.bus_addr(l0_addr),
		.bus_rddata(l0_rddata),
		.bus_strobe(l0_strobe),
		.bus_ack(l0_ack),
		.linebuf_wridx(l0_linebuf_wridx),
		.linebuf_wrdata(l0_linebuf_wrdata),
		.linebuf_wren(l0_linebuf_wren)
	);
	layer_line_buffer l0_line_buffer(
		.rst(reset),
		.clk(clk),
		.active_render_buffer(active_line_buf_r),
		.renderer_wr_idx(l0_linebuf_wridx),
		.renderer_wr_data(l0_linebuf_wrdata),
		.renderer_wr_en(l0_linebuf_wren),
		.composer_rd_idx(lb_rdidx),
		.composer_rd_data(l0_lb_rddata)
	);
	wire [9:0] l1_linebuf_wridx;
	wire [7:0] l1_linebuf_wrdata;
	wire l1_linebuf_wren;
	layer_renderer l1_renderer(
		.rst(reset),
		.clk(clk),
		.line_idx(line_idx),
		.line_render_start(line_render_start),
		.color_depth(l1_color_depth_r),
		.bitmap_mode(l1_bitmap_mode_r),
		.attr_mode(l1_attr_mode_r),
		.tile_height(l1_tile_height_r),
		.tile_width(l1_tile_width_r),
		.map_height(l1_map_height_r),
		.map_width(l1_map_width_r),
		.map_baseaddr(l1_map_baseaddr_r),
		.tile_baseaddr(l1_tile_baseaddr_r),
		.hscroll(l1_hscroll_r),
		.vscroll(l1_vscroll_r),
		.bus_addr(l1_addr),
		.bus_rddata(l1_rddata),
		.bus_strobe(l1_strobe),
		.bus_ack(l1_ack),
		.linebuf_wridx(l1_linebuf_wridx),
		.linebuf_wrdata(l1_linebuf_wrdata),
		.linebuf_wren(l1_linebuf_wren)
	);
	layer_line_buffer l1_line_buffer(
		.rst(reset),
		.clk(clk),
		.active_render_buffer(active_line_buf_r),
		.renderer_wr_idx(l1_linebuf_wridx),
		.renderer_wr_data(l1_linebuf_wrdata),
		.renderer_wr_en(l1_linebuf_wren),
		.composer_rd_idx(lb_rdidx),
		.composer_rd_data(l1_lb_rddata)
	);
	wire [7:0] sprite_idx;
	wire [31:0] sprite_attr;
	wire [9:0] sprite_lb_renderer_rd_idx;
	wire [15:0] sprite_lb_renderer_rd_data;
	wire [9:0] sprite_lb_renderer_wr_idx;
	wire [15:0] sprite_lb_renderer_wr_data;
	wire sprite_lb_renderer_wr_en;
	sprite_renderer sprite_renderer(
		.rst(reset),
		.clk(clk),
		.sprite_bank(sprite_bank_select_r),
		.collisions(sprite_collisions),
		.sprcol_irq(sprcol_irq),
		.line_idx(line_idx),
		.line_render_start(line_render_start),
		.frame_done(vblank_pulse),
		.bus_addr(spr_addr),
		.bus_rddata(spr_rddata),
		.bus_strobe(spr_strobe),
		.bus_ack(spr_ack),
		.sprite_idx(sprite_idx),
		.sprite_attr(sprite_attr),
		.linebuf_rdidx(sprite_lb_renderer_rd_idx),
		.linebuf_rddata(sprite_lb_renderer_rd_data),
		.linebuf_wridx(sprite_lb_renderer_wr_idx),
		.linebuf_wrdata(sprite_lb_renderer_wr_data),
		.linebuf_wren(sprite_lb_renderer_wr_en)
	);
	sprite_line_buffer sprite_line_buffer(
		.rst(reset),
		.clk(clk),
		.active_render_buffer(active_line_buf_r),
		.renderer_rd_idx(sprite_lb_renderer_rd_idx),
		.renderer_rd_data(sprite_lb_renderer_rd_data),
		.renderer_wr_idx(sprite_lb_renderer_wr_idx),
		.renderer_wr_data(sprite_lb_renderer_wr_data),
		.renderer_wr_en(sprite_lb_renderer_wr_en),
		.composer_rd_idx(lb_rdidx),
		.composer_rd_data(spr_lb_rddata),
		.composer_erase_start(spr_lb_erase_start)
	);
	localparam SPRITE_RAM_START = 17'h01000;
	localparam SPRITE_RAM_END = 17'h01400;
	localparam PALETTE_RAM_START = 17'h02000;
	localparam PALETTE_RAM_END = 17'h02400;
	initial spr_pal_ram_wb_ack_r = 0;
	always @(posedge clk)
		if (((wb_adr >= (SPRITE_RAM_START >> 2)) && (wb_adr < (SPRITE_RAM_END >> 2))) || ((wb_adr >= (PALETTE_RAM_START >> 2)) && (wb_adr < (PALETTE_RAM_END >> 2))))
			spr_pal_ram_wb_ack_r <= wb_stb;
		else
			spr_pal_ram_wb_ack_r <= 0;
	sprite_ram sprite_attr_ram(
		.rst_i(1'b0),
		.wr_clk_i(clk),
		.rd_clk_i(clk),
		.wr_clk_en_i(1'b1),
		.rd_en_i(1'b1),
		.rd_clk_en_i(1'b1),
		.wr_en_i((((wb_adr >= (SPRITE_RAM_START >> 2)) && (wb_adr < (SPRITE_RAM_END >> 2))) && wb_stb) && wb_we),
		.wr_data_i(wb_dat_w),
		.ben_i(wb_sel),
		.wr_addr_i(wb_adr[7:0]),
		.rd_addr_i(sprite_idx),
		.rd_data_o(sprite_attr)
	);
	wire [7:0] composer_display_data;
	wire next_pixel;
	wire next_frame;
	wire composer_display_current_field;
	wire dc_interlaced = video_output_mode_r[1];
	composer composer(
		.rst(reset),
		.clk(clk),
		.interlaced(dc_interlaced),
		.frac_x_incr(dc_hscale_r),
		.frac_y_incr(dc_vscale_r),
		.border_color(dc_border_color_r),
		.active_hstart(dc_active_hstart_r),
		.active_hstop(dc_active_hstop_r),
		.active_vstart(dc_active_vstart_r),
		.active_vstop(dc_active_vstop_r),
		.irqline(irq_line_r),
		.layer0_enabled(l0_enabled_r),
		.layer1_enabled(l1_enabled_r),
		.sprites_enabled(sprites_enabled_r),
		.current_field(),
		.line_irq(line_irq),
		.scanline(scanline),
		.line_idx(line_idx),
		.line_render_start(line_render_start),
		.lb_rdidx(lb_rdidx),
		.layer0_lb_rddata(l0_lb_rddata),
		.layer1_lb_rddata(l1_lb_rddata),
		.sprite_lb_rddata(spr_lb_rddata),
		.sprite_lb_erase_start(spr_lb_erase_start),
		.display_next_frame(next_frame),
		.display_next_line(next_line),
		.display_next_pixel(next_pixel),
		.display_current_field(composer_display_current_field),
		.display_data(composer_display_data)
	);
	wire [15:0] palette_rgb_data;
	palette_ram palette_ram(
		.rst_i(1'b0),
		.wr_clk_i(clk),
		.rd_clk_i(clk),
		.wr_clk_en_i(1'b1),
		.rd_en_i(1'b1),
		.rd_clk_en_i(1'b1),
		.wr_en_i((((wb_adr >= (PALETTE_RAM_START >> 2)) && (wb_adr < (PALETTE_RAM_END >> 2))) && wb_stb) && wb_we),
		.wr_data_i(wb_dat_w[15:0]),
		.ben_i(wb_sel[1:0]),
		.wr_addr_i(wb_adr[7:0]),
		.rd_addr_i(composer_display_data),
		.rd_data_o(palette_rgb_data)
	);
	assign composer_display_current_field = 1'b0;
	wire video_vga_next_frame;
	wire video_vga_next_line;
	wire video_vga_display_next_pixel;
	wire video_vga_vblank_pulse;
	wire [3:0] video_vga_r;
	wire [3:0] video_vga_g;
	wire [3:0] video_vga_b;
	wire video_vga_hsync;
	wire video_vga_vsync;
	video_vga video_vga(
		.rst(reset),
		.clk(clk),
		.palette_rgb_data(palette_rgb_data[11:0]),
		.next_frame(video_vga_next_frame),
		.next_line(video_vga_next_line),
		.next_pixel(video_vga_display_next_pixel),
		.vblank_pulse(video_vga_vblank_pulse),
		.vga_r(video_vga_r),
		.vga_g(video_vga_g),
		.vga_b(video_vga_b),
		.vga_hsync(video_vga_hsync),
		.vga_vsync(video_vga_vsync)
	);
	assign next_frame = video_vga_next_frame;
	assign next_line = video_vga_next_line;
	assign next_pixel = video_vga_display_next_pixel;
	assign vblank_pulse = video_vga_vblank_pulse;
	always @(posedge clk)
		case (video_output_mode_r)
			2'b01: begin
				vga_r <= video_vga_r;
				vga_g <= video_vga_g;
				vga_b <= video_vga_b;
				vga_hsync <= video_vga_hsync;
				vga_vsync <= video_vga_vsync;
			end
			default: begin
				vga_r <= 0;
				vga_g <= 0;
				vga_b <= 0;
				vga_hsync <= 0;
				vga_vsync <= 0;
			end
		endcase
endmodule
