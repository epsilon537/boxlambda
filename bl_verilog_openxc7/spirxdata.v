`default_nettype none
module spirxdata (
	i_clk,
	i_reset,
	i_start,
	i_lgblksz,
	i_fifo,
	o_busy,
	i_ll_stb,
	i_ll_byte,
	o_write,
	o_addr,
	o_data,
	o_rxvalid,
	o_response
);
	parameter DW = 32;
	parameter AW = 8;
	parameter [0:0] OPT_LITTLE_ENDIAN = 1'b0;
	localparam CRC_POLYNOMIAL = 16'h1021;
	input wire i_clk;
	input wire i_reset;
	input wire i_start;
	input wire [3:0] i_lgblksz;
	input wire i_fifo;
	output reg o_busy;
	input wire i_ll_stb;
	input wire [7:0] i_ll_byte;
	output reg o_write;
	output reg [AW - 1:0] o_addr;
	output reg [DW - 1:0] o_data;
	output reg o_rxvalid;
	output reg [7:0] o_response;
	reg error_token;
	reg start_token;
	reg token;
	reg received_token;
	reg done;
	reg lastaddr;
	reg all_mem_written;
	reg lastdata;
	reg [1:0] crc_byte;
	reg [2:0] r_lgblksz_m3;
	reg new_data_byte;
	reg [3:0] crc_fill;
	reg [7:0] crc_gearbox;
	reg [15:0] next_crc_data;
	reg [15:0] crc_data;
	reg crc_err;
	reg crc_active;
	reg [2:0] fill;
	reg [23:0] gearbox;
	reg [15:0] first_crc_data;
	always @(*) begin
		error_token = 0;
		if (i_ll_byte[7:4] == 0)
			error_token = 1;
		if (!i_ll_stb || received_token)
			error_token = 0;
	end
	always @(*) begin
		start_token = 0;
		if (!i_ll_byte[0])
			start_token = 1;
		if (!i_ll_stb || received_token)
			start_token = 0;
	end
	always @(*) token = start_token || error_token;
	always @(*) done = i_ll_stb && (crc_byte > 1);
	initial received_token = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			received_token <= 0;
		else if (token)
			received_token <= 1;
	initial o_busy = 0;
	always @(posedge i_clk)
		if (i_reset)
			o_busy <= 0;
		else if (!o_busy)
			o_busy <= i_start;
		else if (error_token || done)
			o_busy <= 0;
	initial o_rxvalid = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			o_rxvalid <= 0;
		else if (error_token || done)
			o_rxvalid <= 1;
	initial o_response = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			o_response <= 0;
		else if (error_token)
			o_response <= i_ll_byte;
		else if (done)
			o_response <= (crc_err || (crc_data[7:0] != i_ll_byte) ? 8'h10 : 0);
	initial o_write = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			o_write <= 0;
		else if (received_token && !all_mem_written)
			o_write <= &fill && i_ll_stb;
		else
			o_write <= 0;
	initial o_data = 0;
	always @(posedge i_clk)
		if (received_token && !all_mem_written) begin
			if (OPT_LITTLE_ENDIAN)
				o_data <= {i_ll_byte, gearbox};
			else
				o_data <= {gearbox, i_ll_byte};
		end
	always @(posedge i_clk)
		if (!o_busy)
			o_addr <= {i_fifo, {AW - 1 {1'b0}}};
		else if (o_write && !lastaddr)
			o_addr <= o_addr + 1;
	initial fill = 0;
	always @(posedge i_clk) begin
		if (i_ll_stb) begin
			if (OPT_LITTLE_ENDIAN)
				gearbox <= {i_ll_byte, gearbox[23:8]};
			else
				gearbox <= {gearbox[15:0], i_ll_byte};
		end
		if (!o_busy || !received_token)
			fill <= 0;
		else if (&fill && i_ll_stb)
			fill <= 0;
		else if (i_ll_stb)
			fill <= {fill[1:0], 1'b1};
	end
	always @(posedge i_clk)
		if (!o_busy)
			lastdata <= 0;
		else if (!lastdata)
			lastdata <= lastaddr && &fill;
	initial all_mem_written = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			all_mem_written <= 0;
		else if (o_write && lastaddr)
			all_mem_written <= 1;
	initial crc_byte = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			crc_byte <= 0;
		else if ((i_ll_stb && lastaddr) && lastdata)
			crc_byte <= crc_byte + 1;
	initial r_lgblksz_m3 = 0;
	initial lastaddr = 0;
	always @(posedge i_clk)
		if (!o_busy) begin
			lastaddr <= i_lgblksz < 4;
			r_lgblksz_m3 <= i_lgblksz - 3;
		end
		else if (o_write && !lastaddr)
			case (r_lgblksz_m3)
				0: lastaddr <= 1;
				1: lastaddr <= &o_addr[1:1];
				2: lastaddr <= &o_addr[2:1];
				3: lastaddr <= &o_addr[3:1];
				4: lastaddr <= &o_addr[4:1];
				5: lastaddr <= &o_addr[5:1];
				default: lastaddr <= &o_addr[6:1];
			endcase
	always @(*) new_data_byte = i_ll_stb && !all_mem_written;
	initial crc_fill = 0;
	initial crc_active = 0;
	always @(posedge i_clk)
		if ((i_reset || !o_busy) || !received_token) begin
			crc_fill <= 0;
			crc_active <= 0;
		end
		else if (crc_active || new_data_byte) begin
			crc_fill <= (crc_fill - (crc_active ? 1 : 0)) + (new_data_byte ? 4 : 0);
			if (new_data_byte)
				crc_active <= 1;
			else
				crc_active <= crc_fill > 1;
		end
	always @(posedge i_clk)
		if (!crc_active)
			crc_gearbox <= i_ll_byte;
		else
			crc_gearbox <= {crc_gearbox[5:0], 2'b00};
	always @(*) begin
		first_crc_data = crc_data << 1;
		if (crc_data[15] ^ crc_gearbox[7])
			first_crc_data = first_crc_data ^ CRC_POLYNOMIAL;
		if (first_crc_data[15] ^ crc_gearbox[6])
			next_crc_data = (first_crc_data << 1) ^ CRC_POLYNOMIAL;
		else
			next_crc_data = first_crc_data << 1;
	end
	initial crc_data = 0;
	always @(posedge i_clk)
		if (!o_busy)
			crc_data <= 0;
		else if (crc_active)
			crc_data <= next_crc_data;
	initial crc_err = 0;
	always @(posedge i_clk)
		if (i_reset || !o_busy)
			crc_err <= 0;
		else if (i_ll_stb && (crc_byte == 1))
			crc_err <= crc_data[15:8] != i_ll_byte;
endmodule
