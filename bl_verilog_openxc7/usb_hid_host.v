`default_nettype none
module usb_hid_host (
	usbclk,
	usbrst_n,
	usb_dm_i,
	usb_dp_i,
	usb_dm_o,
	usb_dp_o,
	usb_oe,
	update_leds_stb,
	ack_update_leds_stb,
	leds,
	typ,
	report,
	conerr,
	key_modifiers,
	key1,
	key2,
	key3,
	key4,
	mouse_btn,
	mouse_dx,
	mouse_dy,
	game_l,
	game_r,
	game_u,
	game_d,
	game_a,
	game_b,
	game_x,
	game_y,
	game_sel,
	game_sta,
	dbg_hid_report
);
	input wire usbclk;
	input wire usbrst_n;
	input wire usb_dm_i;
	input wire usb_dp_i;
	output wire usb_dm_o;
	output wire usb_dp_o;
	output wire usb_oe;
	input wire update_leds_stb;
	output wire ack_update_leds_stb;
	input wire [3:0] leds;
	output reg [1:0] typ;
	output reg report;
	output wire conerr;
	output reg [7:0] key_modifiers;
	output reg [7:0] key1;
	output reg [7:0] key2;
	output reg [7:0] key3;
	output reg [7:0] key4;
	output reg [7:0] mouse_btn;
	output reg signed [7:0] mouse_dx;
	output reg signed [7:0] mouse_dy;
	output reg game_l;
	output reg game_r;
	output reg game_u;
	output reg game_d;
	output reg game_a;
	output reg game_b;
	output reg game_x;
	output reg game_y;
	output reg game_sel;
	output reg game_sta;
	output wire [63:0] dbg_hid_report;
	wire data_rdy;
	wire data_strobe;
	wire [7:0] ukpdat;
	reg [7:0] regs [0:7];
	wire save;
	wire [2:0] save_r;
	wire [2:0] save_b;
	wire connected;
	reg [15:0] crc16;
	always @(leds)
		case (leds)
			4'h0: crc16 = 16'hbf40;
			4'h1: crc16 = 16'h7f81;
			4'h2: crc16 = 16'h7ec1;
			4'h3: crc16 = 16'hbe00;
			4'h4: crc16 = 16'h7c41;
			4'h5: crc16 = 16'hbc80;
			4'h6: crc16 = 16'hbdc0;
			4'h7: crc16 = 16'h7d01;
			4'h8: crc16 = 16'h7941;
			4'h9: crc16 = 16'hb980;
			4'ha: crc16 = 16'hb8c0;
			4'hb: crc16 = 16'h7801;
			4'hc: crc16 = 16'hba40;
			4'hd: crc16 = 16'h7a81;
			4'he: crc16 = 16'h7bc1;
			4'hf: crc16 = 16'hbb00;
		endcase
	ukp ukp(
		.usbrst_n(usbrst_n),
		.usbclk(usbclk),
		.usb_dp_i(usb_dp_i),
		.usb_dm_i(usb_dm_i),
		.usb_dp_o(usb_dp_o),
		.usb_dm_o(usb_dm_o),
		.usb_oe(usb_oe),
		.req_branch_stb(update_leds_stb),
		.ack_req_branch_stb(ack_update_leds_stb),
		.outr0({4'b0000, leds}),
		.outr1(crc16[7:0]),
		.outr2(crc16[15:8]),
		.ukprdy(data_rdy),
		.ukpstb(data_strobe),
		.ukpdat(ukpdat),
		.save(save),
		.save_r(save_r),
		.save_b(save_b),
		.connected(connected),
		.conerr(conerr)
	);
	reg [2:0] rcvct;
	reg data_strobe_r;
	reg data_rdy_r;
	reg [7:0] dat [0:7];
	assign dbg_hid_report = {dat[7], dat[6], dat[5], dat[4], dat[3], dat[2], dat[1], dat[0]};
	reg valid = 0;
	always @(posedge usbclk) begin : process_in_data
		data_rdy_r <= data_rdy;
		data_strobe_r <= data_strobe;
		report <= 0;
		if (report == 1) begin
			mouse_dx <= 0;
			mouse_dy <= 0;
		end
		if (~data_rdy)
			rcvct <= 0;
		else if (data_strobe && ~data_strobe_r) begin
			dat[rcvct] <= ukpdat;
			if (typ == 1)
				case (rcvct)
					0: key_modifiers <= ukpdat;
					2: key1 <= ukpdat;
					3: key2 <= ukpdat;
					4: key3 <= ukpdat;
					5: key4 <= ukpdat;
				endcase
			else if (typ == 2)
				case (rcvct)
					0: mouse_btn <= ukpdat;
					1: mouse_dx <= ukpdat;
					2: mouse_dy <= ukpdat;
				endcase
			else if (typ == 3)
				case (rcvct)
					0: begin
						if (ukpdat[1:0] != 2'b10) begin
							valid <= 1;
							game_l <= 0;
							game_r <= 0;
							game_u <= 0;
							game_d <= 0;
						end
						else
							valid <= 0;
						if (ukpdat == 8'h00)
							{game_l, game_r} <= 2'b10;
						if (ukpdat == 8'hff)
							{game_l, game_r} <= 2'b01;
					end
					1: begin
						if (ukpdat == 8'h00)
							{game_u, game_d} <= 2'b10;
						if (ukpdat == 8'hff)
							{game_u, game_d} <= 2'b01;
					end
					3:
						if (valid) begin
							if (ukpdat[7:6] == 2'b00)
								{game_l, game_r} <= 2'b10;
							if (ukpdat[7:6] == 2'b11)
								{game_l, game_r} <= 2'b01;
						end
					4:
						if (valid) begin
							if (ukpdat[7:6] == 2'b00)
								{game_u, game_d} <= 2'b10;
							if (ukpdat[7:6] == 2'b11)
								{game_u, game_d} <= 2'b01;
						end
					5:
						if (valid) begin
							game_x <= ukpdat[4];
							game_a <= ukpdat[5];
							game_b <= ukpdat[6];
							game_y <= ukpdat[7];
						end
					6:
						if (valid) begin
							game_sel <= ukpdat[4];
							game_sta <= ukpdat[5];
						end
				endcase
			rcvct <= rcvct + 1;
		end
		if ((~data_rdy && data_rdy_r) && (typ != 0))
			report <= 1;
	end
	reg save_delayed;
	reg connected_r;
	always @(posedge usbclk) begin : response_recognition
		save_delayed <= save;
		if (save)
			regs[save_r] <= dat[save_b];
		else if ((save_delayed && ~save) && (save_r == 6)) begin
			if (regs[4] == 3) begin
				if (regs[5] == 1)
					typ <= (regs[6] == 1 ? 1 : 2);
				else
					typ <= 3;
			end
			else
				typ <= 0;
		end
		connected_r <= connected;
		if (~connected & connected_r)
			typ <= 0;
	end
endmodule
