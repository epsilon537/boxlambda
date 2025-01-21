`default_nettype none
module spitxdata (
	i_clk,
	i_reset,
	i_start,
	i_lgblksz,
	i_fifo,
	o_busy,
	o_read,
	o_addr,
	i_data,
	i_ll_busy,
	o_ll_stb,
	o_ll_byte,
	i_ll_stb,
	i_ll_byte,
	o_rxvalid,
	o_response
);
	parameter DW = 32;
	parameter AW = 8;
	parameter RDDELAY = 2;
	parameter [0:0] OPT_LITTLE_ENDIAN = 1'b0;
	localparam CRC_POLYNOMIAL = 16'h1021;
	input wire i_clk;
	input wire i_reset;
	input wire i_start;
	input wire [3:0] i_lgblksz;
	input wire i_fifo;
	output reg o_busy;
	output reg o_read;
	output reg [AW - 1:0] o_addr;
	input wire [DW - 1:0] i_data;
	input wire i_ll_busy;
	output reg o_ll_stb;
	output wire [7:0] o_ll_byte;
	input wire i_ll_stb;
	input wire [7:0] i_ll_byte;
	output reg o_rxvalid;
	output reg [7:0] o_response;
	reg [RDDELAY - 1:0] rdvalid;
	reg [(8 + DW) - 1:0] gearbox;
	reg [(1 + (DW / 8)) - 1:0] fill;
	reg crc_flag;
	reg crc_stb;
	reg data_read;
	reg all_mem_read;
	reg lastaddr;
	reg data_sent;
	reg received_token;
	reg all_idle;
	reg crc_active;
	reg [$clog2(1 + (DW / 2)) - 1:0] crc_fill;
	(* keep *) reg [DW - 1:0] crc_gearbox;
	reg [15:0] crc_data;
	reg token;
	reg [2:0] r_lgblksz_m3;
	reg [15:0] next_crc_data;
	always @(*) token = ((data_sent && i_ll_stb) && i_ll_byte[0]) && !i_ll_byte[4];
	initial o_busy = 0;
	always @(posedge i_clk)
		if (i_reset)
			o_busy <= 0;
		else if (!o_busy)
			o_busy <= i_start;
		else if ((all_idle && i_ll_stb) && &i_ll_byte)
			o_busy <= 0;
	initial o_rxvalid = 0;
	initial received_token = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			{received_token, o_rxvalid} <= 0;
		else if (token && !received_token)
			{received_token, o_rxvalid} <= 2'b11;
		else
			o_rxvalid <= 0;
	initial all_idle = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			all_idle <= 0;
		else if ((received_token && i_ll_stb) && &i_ll_byte)
			all_idle <= 1'b1;
	always @(posedge i_clk)
		if (token)
			o_response <= i_ll_byte;
	initial rdvalid = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			rdvalid <= 0;
		else
			rdvalid <= {rdvalid[RDDELAY - 2:0], o_read};
	initial o_ll_stb = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			o_ll_stb <= 0;
		else if (rdvalid[RDDELAY - 1])
			o_ll_stb <= 1;
		else if (data_read && !fill[4])
			o_ll_stb <= 0;
	initial fill = 0;
	always @(posedge i_clk) begin
		if (!o_ll_stb && rdvalid[RDDELAY - 1]) begin
			if (OPT_LITTLE_ENDIAN)
				gearbox <= {i_data, 8'hfe};
			else
				gearbox <= {8'hfe, i_data};
			fill <= 5'h1f;
		end
		else if (rdvalid[RDDELAY - 1]) begin
			if (OPT_LITTLE_ENDIAN)
				gearbox <= {i_data, gearbox[7:0]};
			else
				gearbox <= {gearbox[DW + 7:DW], i_data};
			fill <= 5'h1f;
		end
		else if (crc_stb) begin
			if (OPT_LITTLE_ENDIAN)
				gearbox <= {16'h00ff, crc_data[7:0], crc_data[15:8], gearbox[7:0]};
			else
				gearbox <= {gearbox[DW + 7:DW], crc_data, 16'h00ff};
			fill[3:0] <= 4'hc;
		end
		else if (o_ll_stb && !i_ll_busy) begin
			if (OPT_LITTLE_ENDIAN)
				gearbox <= {8'hff, gearbox[DW + 7:8]};
			else
				gearbox <= {gearbox[DW - 1:0], 8'hff};
			fill <= fill << 1;
		end
		if (!o_busy) begin
			if (OPT_LITTLE_ENDIAN)
				gearbox[7:0] <= 8'hfe;
			else
				gearbox[39:32] <= 8'hfe;
		end
		if (i_reset)
			fill <= 0;
		else if (!o_busy)
			fill <= (i_start ? 5'h10 : 0);
	end
	generate
		if (OPT_LITTLE_ENDIAN) begin : genblk1
			assign o_ll_byte = gearbox[7:0];
		end
		else begin : GEN_BIG_ENDIAN
			assign o_ll_byte = gearbox[39:32];
		end
	endgenerate
	initial {crc_stb, o_read} = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			{crc_stb, o_read} <= 0;
		else if (!fill[3] || (!fill[2] && (!o_ll_stb || !i_ll_busy))) begin
			{crc_stb, o_read} <= 0;
			if ((!o_read && (rdvalid == 0)) && !data_read) begin
				if (!all_mem_read)
					o_read <= 1;
				else
					crc_stb <= !crc_flag && !crc_stb;
			end
		end
		else
			{crc_stb, o_read} <= 0;
	always @(posedge i_clk)
		if (!o_busy)
			o_addr <= {i_fifo, {AW - 1 {1'b0}}};
		else if (o_read && !lastaddr)
			o_addr[AW - 2:0] <= o_addr[AW - 2:0] + 1;
	initial all_mem_read = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			all_mem_read <= 0;
		else if (o_read && lastaddr)
			all_mem_read <= 1;
	initial crc_flag = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			crc_flag <= 0;
		else if (crc_stb)
			crc_flag <= 1;
	always @(*) data_read = crc_flag;
	initial data_sent = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			data_sent <= 1'b0;
		else if (((data_read && !fill[3]) && o_ll_stb) && !i_ll_busy)
			data_sent <= 1'b1;
	initial r_lgblksz_m3 = 0;
	initial lastaddr = 0;
	always @(posedge i_clk)
		if (!o_busy) begin
			lastaddr <= i_lgblksz < 4;
			r_lgblksz_m3 <= i_lgblksz - 3;
		end
		else if (o_read && !lastaddr)
			case (r_lgblksz_m3)
				0:
					;
				1: lastaddr <= &o_addr[1:1];
				2: lastaddr <= &o_addr[2:1];
				3: lastaddr <= &o_addr[3:1];
				4: lastaddr <= &o_addr[4:1];
				5: lastaddr <= &o_addr[5:1];
				default: lastaddr <= &o_addr[6:1];
			endcase
	initial crc_fill = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy) begin
			crc_fill <= 0;
			crc_active <= 0;
		end
		else if (crc_active || rdvalid[RDDELAY - 1]) begin
			crc_fill <= (crc_fill - (crc_active ? 1 : 0)) + (rdvalid[RDDELAY - 1] ? DW / 2 : 0);
			if (rdvalid[RDDELAY - 1])
				crc_active <= 1;
			else
				crc_active <= crc_fill > 1;
		end
	always @(posedge i_clk)
		if (!crc_active) begin
			if (OPT_LITTLE_ENDIAN)
				crc_gearbox <= {i_data[7:0], i_data[15:8], i_data[23:16], i_data[31:24]};
			else
				crc_gearbox <= i_data;
		end
		else
			crc_gearbox <= {crc_gearbox[DW - 3:0], 2'b00};
	always @(*) begin
		next_crc_data = crc_data << 1;
		if (crc_data[15] ^ crc_gearbox[31])
			next_crc_data = next_crc_data ^ CRC_POLYNOMIAL;
		if (next_crc_data[15] ^ crc_gearbox[30])
			next_crc_data = (next_crc_data << 1) ^ CRC_POLYNOMIAL;
		else
			next_crc_data = next_crc_data << 1;
	end
	always @(posedge i_clk)
		if (!o_busy)
			crc_data <= 0;
		else if (crc_active)
			crc_data <= next_crc_data;
endmodule
