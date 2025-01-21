module video_vga (
	rst,
	clk,
	palette_rgb_data,
	next_frame,
	next_line,
	next_pixel,
	vblank_pulse,
	vga_r,
	vga_g,
	vga_b,
	vga_hsync,
	vga_vsync
);
	input wire rst;
	input wire clk;
	input wire [11:0] palette_rgb_data;
	output wire next_frame;
	output wire next_line;
	output wire next_pixel;
	output wire vblank_pulse;
	output reg [3:0] vga_r;
	output reg [3:0] vga_g;
	output reg [3:0] vga_b;
	output reg vga_hsync;
	output reg vga_vsync;
	parameter H_ACTIVE = 640;
	parameter H_FRONT_PORCH = 16;
	parameter H_SYNC = 96;
	parameter H_BACK_PORCH = 48;
	parameter H_TOTAL = ((H_ACTIVE + H_FRONT_PORCH) + H_SYNC) + H_BACK_PORCH;
	parameter V_ACTIVE = 480;
	parameter V_FRONT_PORCH = 10;
	parameter V_SYNC = 2;
	parameter V_BACK_PORCH = 33;
	parameter V_TOTAL = ((V_ACTIVE + V_FRONT_PORCH) + V_SYNC) + V_BACK_PORCH;
	reg [9:0] x_counter = 0;
	reg [9:0] y_counter = 0;
	reg clk_en = 0;
	wire h_last = x_counter == (H_TOTAL - 1);
	wire v_last = y_counter == (V_TOTAL - 1);
	wire v_last2 = y_counter == (V_TOTAL - 2);
	always @(posedge clk)
		if (rst) begin
			x_counter <= 10'd0;
			y_counter <= 10'd0;
			clk_en <= 0;
		end
		else begin
			clk_en <= ~clk_en;
			if (clk_en) begin
				x_counter <= (h_last ? 10'd0 : x_counter + 10'd1);
				if (h_last)
					y_counter <= (v_last ? 10'd0 : y_counter + 10'd1);
			end
		end
	wire hsync = (x_counter >= (H_ACTIVE + H_FRONT_PORCH)) && (x_counter < ((H_ACTIVE + H_FRONT_PORCH) + H_SYNC));
	wire vsync = (y_counter >= (V_ACTIVE + V_FRONT_PORCH)) && (y_counter < ((V_ACTIVE + V_FRONT_PORCH) + V_SYNC));
	wire h_active = x_counter < H_ACTIVE;
	wire v_active = y_counter < V_ACTIVE;
	wire active = h_active && v_active;
	assign vblank_pulse = h_last && (y_counter == (V_ACTIVE - 1));
	assign next_frame = h_last && v_last2;
	assign next_line = h_last;
	assign next_pixel = 1'b1;
	reg [1:0] hsync_r;
	reg [1:0] vsync_r;
	reg [1:0] active_r;
	always @(posedge clk)
		if (clk_en) begin
			hsync_r <= {hsync_r[0], hsync};
			vsync_r <= {vsync_r[0], vsync};
			active_r <= {active_r[0], active};
		end
	always @(posedge clk)
		if (rst) begin
			vga_r <= 4'd0;
			vga_g <= 4'd0;
			vga_b <= 4'd0;
			vga_hsync <= 1;
			vga_vsync <= 1;
		end
		else if (clk_en) begin
			if (active_r[1]) begin
				vga_r <= palette_rgb_data[11:8];
				vga_g <= palette_rgb_data[7:4];
				vga_b <= palette_rgb_data[3:0];
			end
			else begin
				vga_r <= 4'd0;
				vga_g <= 4'd0;
				vga_b <= 4'd0;
			end
			vga_hsync <= ~hsync_r[1];
			vga_vsync <= ~vsync_r[1];
		end
endmodule
