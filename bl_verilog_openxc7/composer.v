module composer (
	rst,
	clk,
	interlaced,
	frac_x_incr,
	frac_y_incr,
	border_color,
	active_hstart,
	active_hstop,
	active_vstart,
	active_vstop,
	irqline,
	layer0_enabled,
	layer1_enabled,
	sprites_enabled,
	current_field,
	line_irq,
	scanline,
	line_idx,
	line_render_start,
	lb_rdidx,
	layer0_lb_rddata,
	layer1_lb_rddata,
	sprite_lb_rddata,
	sprite_lb_erase_start,
	display_next_frame,
	display_next_line,
	display_next_pixel,
	display_current_field,
	display_data
);
	input wire rst;
	input wire clk;
	input wire interlaced;
	input wire [7:0] frac_x_incr;
	input wire [7:0] frac_y_incr;
	input wire [7:0] border_color;
	input wire [9:0] active_hstart;
	input wire [9:0] active_hstop;
	input wire [8:0] active_vstart;
	input wire [8:0] active_vstop;
	input wire [9:0] irqline;
	input wire layer0_enabled;
	input wire layer1_enabled;
	input wire sprites_enabled;
	output reg current_field;
	output reg line_irq;
	output wire [9:0] scanline;
	output wire [8:0] line_idx;
	output wire line_render_start;
	output wire [9:0] lb_rdidx;
	input wire [7:0] layer0_lb_rddata;
	input wire [7:0] layer1_lb_rddata;
	input wire [15:0] sprite_lb_rddata;
	output wire sprite_lb_erase_start;
	input wire display_next_frame;
	input wire display_next_line;
	input wire display_next_pixel;
	input wire display_current_field;
	output reg [7:0] display_data;
	wire [7:0] frac_x_incr_int = (interlaced ? {1'b0, frac_x_incr[7:1]} : frac_x_incr);
	reg clk_en = 0;
	reg [16:0] scaled_x_counter_r;
	wire [9:0] scaled_x_counter = scaled_x_counter_r[16:7];
	reg [15:0] scaled_y_counter_r;
	wire [8:0] scaled_y_counter = scaled_y_counter_r[15:7];
	reg render_start_r;
	assign line_idx = scaled_y_counter;
	assign line_render_start = render_start_r;
	assign lb_rdidx = scaled_x_counter;
	wire layer0_opaque = layer0_lb_rddata[7:0] != 8'h00;
	wire layer1_opaque = layer1_lb_rddata[7:0] != 8'h00;
	wire sprite_opaque = sprite_lb_rddata[7:0] != 8'h00;
	wire sprite_z1 = sprite_lb_rddata[9:8] == 2'd1;
	wire sprite_z2 = sprite_lb_rddata[9:8] == 2'd2;
	wire sprite_z3 = sprite_lb_rddata[9:8] == 2'd3;
	reg [9:0] y_counter_r;
	reg [9:0] y_counter_rr;
	reg next_line_r;
	always @(posedge clk)
		if (rst) begin
			clk_en <= 0;
			y_counter_r <= 0;
			y_counter_rr <= 0;
			next_line_r <= 0;
			current_field <= 0;
		end
		else begin
			clk_en <= ~clk_en;
			if (clk_en) begin
				next_line_r <= display_next_line;
				if (display_next_line) begin
					y_counter_r <= y_counter_r + (interlaced ? 10'd2 : 10'd1);
					y_counter_rr <= y_counter_r;
				end
				if (display_next_frame) begin
					current_field <= !display_current_field;
					y_counter_r <= (interlaced && !display_current_field ? 10'd1 : 10'd0);
				end
			end
		end
	always @(posedge clk)
		if (rst)
			line_irq <= 0;
		else if (clk_en)
			line_irq <= display_next_line && ((!interlaced && (y_counter_r == irqline)) || (interlaced && (y_counter_r[9:1] == irqline[9:1])));
	reg [10:0] x_counter_r;
	always @(posedge clk)
		if (rst)
			x_counter_r <= 0;
		else if (clk_en) begin
			if (display_next_pixel)
				x_counter_r <= x_counter_r + (interlaced ? 11'd1 : 11'd2);
			if (display_next_line)
				x_counter_r <= 0;
		end
	wire [9:0] x_counter = x_counter_r[10:1];
	wire [9:0] y_counter = y_counter_rr;
	assign scanline = y_counter_r;
	assign sprite_lb_erase_start = x_counter_r == {10'd639, interlaced};
	wire hactive = (x_counter >= active_hstart) && (x_counter < active_hstop);
	wire vactive = (y_counter >= {1'b0, active_vstart}) && (y_counter < {1'b0, active_vstop});
	reg display_active;
	always @(posedge clk)
		if (clk_en)
			display_active <= hactive && vactive;
	reg vactive_started_r;
	always @(posedge clk)
		if (rst) begin
			scaled_y_counter_r <= 'd0;
			render_start_r <= 0;
			vactive_started_r <= 0;
		end
		else if (clk_en) begin
			render_start_r <= 0;
			if (next_line_r) begin
				if ((!vactive_started_r && next_line_r) && (y_counter_r >= {1'b0, active_vstart})) begin
					vactive_started_r <= 1;
					render_start_r <= 1;
					scaled_y_counter_r <= (interlaced && (current_field ^ active_vstart[0]) ? {8'b00000000, frac_y_incr} : 16'd0);
				end
				else if ((scaled_y_counter < 'd480) && vactive) begin
					render_start_r <= 1;
					scaled_y_counter_r <= scaled_y_counter_r + (interlaced ? {7'b0000000, frac_y_incr, 1'b0} : {8'b00000000, frac_y_incr});
				end
			end
			if (display_next_frame)
				vactive_started_r <= 0;
		end
	always @(posedge clk)
		if (rst)
			scaled_x_counter_r <= 'd0;
		else if (clk_en) begin
			if (display_next_pixel && hactive) begin
				if (scaled_x_counter < 'd640)
					scaled_x_counter_r <= scaled_x_counter_r + {9'b000000000, frac_x_incr_int};
			end
			if (display_next_line)
				scaled_x_counter_r <= 0;
		end
	always @(*) begin
		display_data = border_color;
		if (display_active) begin
			display_data = 8'h00;
			if ((sprites_enabled && sprite_opaque) && sprite_z1)
				display_data = sprite_lb_rddata[7:0];
			if (layer0_enabled && layer0_opaque)
				display_data = layer0_lb_rddata;
			if ((sprites_enabled && sprite_opaque) && sprite_z2)
				display_data = sprite_lb_rddata[7:0];
			if (layer1_enabled && layer1_opaque)
				display_data = layer1_lb_rddata;
			if ((sprites_enabled && sprite_opaque) && sprite_z3)
				display_data = sprite_lb_rddata[7:0];
		end
	end
endmodule
