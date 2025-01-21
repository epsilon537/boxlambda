`default_nettype none
module wbi2cmaster (
	i_clk,
	i_reset,
	i_wb_cyc,
	i_wb_stb,
	i_wb_we,
	i_wb_addr,
	i_wb_data,
	i_wb_sel,
	o_wb_stall,
	o_wb_ack,
	o_wb_data,
	i_i2c_scl,
	i_i2c_sda,
	o_i2c_scl,
	o_i2c_sda,
	o_int,
	o_dbg
);
	parameter [0:0] CONSTANT_SPEED = 1'b0;
	parameter [0:0] READ_ONLY = 1'b0;
	parameter [0:0] LITTLE_ENDIAN = 1'b0;
	parameter [5:0] TICKBITS = 6'd20;
	parameter [TICKBITS - 1:0] CLOCKS_PER_TICK = 20'd1000;
	parameter MEM_ADDR_BITS = 7;
	input wire i_clk;
	input wire i_reset;
	input wire i_wb_cyc;
	input wire i_wb_stb;
	input wire i_wb_we;
	input wire [MEM_ADDR_BITS - 2:0] i_wb_addr;
	input wire [31:0] i_wb_data;
	input wire [3:0] i_wb_sel;
	output wire o_wb_stall;
	output reg o_wb_ack;
	output reg [31:0] o_wb_data;
	input wire i_i2c_scl;
	input wire i_i2c_sda;
	output wire o_i2c_scl;
	output wire o_i2c_sda;
	output wire o_int;
	output wire [31:0] o_dbg;
	localparam [2:0] I2MIDLE = 3'h0;
	localparam [2:0] I2MDEVADDR = 3'h1;
	localparam [2:0] I2MRDSTOP = 3'h2;
	localparam [2:0] I2MRDDEV = 3'h3;
	localparam [2:0] I2MTXDATA = 3'h4;
	localparam [2:0] I2MRXDATA = 3'h5;
	localparam [2:0] I2MCLEANUP = 3'h6;
	reg [31:0] mem [0:(1 << (MEM_ADDR_BITS - 2)) - 1];
	reg zero_speed_err;
	reg [TICKBITS - 1:0] r_speed;
	reg ll_i2c_cyc;
	reg ll_i2c_stb;
	reg ll_i2c_we;
	reg [7:0] ll_i2c_tx_data;
	wire ll_i2c_ack;
	wire ll_i2c_stall;
	wire ll_i2c_err;
	wire [7:0] ll_i2c_rx_data;
	wire [31:0] ll_dbg;
	reg [MEM_ADDR_BITS - 1:0] wr_addr;
	reg [3:0] wr_sel;
	reg [31:0] wr_data;
	reg wr_inc;
	reg r_write_lock;
	reg start_request;
	reg [7:1] newdev;
	reg newrx_txn;
	reg [MEM_ADDR_BITS - 1:0] newadr;
	reg r_busy;
	reg isr;
	reg ien;
	reg last_op;
	reg rd_inc;
	reg last_err;
	reg [6:0] last_dev;
	reg [MEM_ADDR_BITS - 1:0] last_adr;
	reg [MEM_ADDR_BITS - 1:0] count_left;
	reg [31:0] w_wb_status;
	reg [MEM_ADDR_BITS - 1:0] rd_addr;
	reg rd_stb;
	reg [31:0] rd_word;
	reg [7:0] rd_byte;
	reg [1:0] rd_sel;
	wire [7:0] w_byte_addr;
	reg last_ack;
	reg last_addr_flag;
	reg [2:0] mstate;
	reg [1:0] acks_pending;
	reg [1:0] r_write_pause;
	lli2cm lowlvl(
		.i_clk(i_clk),
		.i_rst(i_reset),
		.i_clocks(r_speed),
		.i_cyc(ll_i2c_cyc),
		.i_stb(ll_i2c_stb),
		.i_we(ll_i2c_we),
		.i_data(ll_i2c_tx_data),
		.o_ack(ll_i2c_ack),
		.o_busy(ll_i2c_stall),
		.o_err(ll_i2c_err),
		.o_data(ll_i2c_rx_data),
		.i_scl(i_i2c_scl),
		.i_sda(i_i2c_sda),
		.o_scl(o_i2c_scl),
		.o_sda(o_i2c_sda),
		.o_dbg(ll_dbg)
	);
	initial start_request = 1'b0;
	initial newdev = 7'h00;
	initial newrx_txn = 1'b0;
	initial newadr = 0;
	initial r_speed = CLOCKS_PER_TICK;
	initial zero_speed_err = 1'b0;
	initial isr = 1'b0;
	initial ien = 1'b0;
	always @(posedge i_clk)
		if (i_reset) begin
			start_request <= 1'b0;
			newdev <= 7'h00;
			newrx_txn <= 1'b0;
			newadr <= 0;
			r_speed <= CLOCKS_PER_TICK;
			zero_speed_err <= 1'b0;
			isr <= 1'b0;
			ien <= 1'b0;
		end
		else begin
			start_request <= 1'b0;
			if (((i_wb_stb && i_wb_we) && !r_busy) && !i_wb_addr[MEM_ADDR_BITS - 2]) begin
				if (i_wb_addr[1:0] == 2'b00) begin
					newdev <= i_wb_data[23:17];
					newrx_txn <= i_wb_data[16];
					newadr <= i_wb_data[(8 + MEM_ADDR_BITS) - 1:8];
					start_request <= (i_wb_data[MEM_ADDR_BITS - 1:0] != 0) && (!READ_ONLY || i_wb_data[16]);
				end
				if ((i_wb_addr[1:0] == 2'b01) && !CONSTANT_SPEED)
					r_speed <= i_wb_data[TICKBITS - 1:0];
				if (i_wb_addr[1:0] == 2'b10)
					isr <= 1'b0;
				if (i_wb_addr[1:0] == 2'b11)
					ien <= i_wb_data[0];
			end
			else if (zero_speed_err)
				r_speed <= CLOCKS_PER_TICK;
			zero_speed_err <= r_speed == 0;
			wr_sel <= 4'h0;
			wr_inc <= 1'b0;
			if (r_write_lock) begin
				if (ll_i2c_ack) begin
					wr_data <= {4 {ll_i2c_rx_data}};
					wr_addr <= newadr[MEM_ADDR_BITS - 1:0];
					if (LITTLE_ENDIAN)
						case (newadr[1:0])
							2'b11: wr_sel <= 4'b1000;
							2'b10: wr_sel <= 4'b0100;
							2'b01: wr_sel <= 4'b0010;
							2'b00: wr_sel <= 4'b0001;
						endcase
					else
						case (newadr[1:0])
							2'b00: wr_sel <= 4'b1000;
							2'b01: wr_sel <= 4'b0100;
							2'b10: wr_sel <= 4'b0010;
							2'b11: wr_sel <= 4'b0001;
						endcase
					newadr <= newadr + 1'b1;
					wr_inc <= 1'b1;
				end
			end
			else if (!READ_ONLY) begin
				wr_data <= i_wb_data;
				wr_sel <= ((i_wb_stb && i_wb_we) && i_wb_addr[MEM_ADDR_BITS - 2] ? i_wb_sel : 4'h0);
				wr_addr <= {i_wb_addr[MEM_ADDR_BITS - 3:0], 2'b00};
			end
			if (wr_sel[3])
				mem[wr_addr[MEM_ADDR_BITS - 1:2]][31:24] <= wr_data[31:24];
			if (wr_sel[2])
				mem[wr_addr[MEM_ADDR_BITS - 1:2]][23:16] <= wr_data[23:16];
			if (wr_sel[1])
				mem[wr_addr[MEM_ADDR_BITS - 1:2]][15:8] <= wr_data[15:8];
			if (wr_sel[0])
				mem[wr_addr[MEM_ADDR_BITS - 1:2]][7:0] <= wr_data[7:0];
			if (((mstate == I2MIDLE) && r_busy) && !ll_i2c_stall)
				isr <= 1'b1;
		end
	always @(*) begin
		w_wb_status = 0;
		w_wb_status[MEM_ADDR_BITS - 1:0] = count_left;
		w_wb_status[(8 + MEM_ADDR_BITS) - 1:8] = last_adr;
		w_wb_status[23:16] = {last_dev, 1'b0};
		w_wb_status[31:24] = {r_busy, last_err, 6'h00};
	end
	always @(posedge i_clk) begin
		if (((i_wb_stb && i_wb_we) && !r_busy) && (i_wb_addr[1:0] == 2'b00)) begin
			count_left <= i_wb_data[MEM_ADDR_BITS - 1:0];
			last_op <= 1'b0;
		end
		else
			last_op <= count_left[MEM_ADDR_BITS - 1:0] == 0;
		if (wr_inc) begin
			last_dev <= newdev;
			last_adr <= wr_addr + 1'b1;
			if (|count_left)
				count_left <= count_left - 1'b1;
		end
		else if (rd_inc) begin
			last_dev <= newdev;
			last_adr <= rd_addr - 1'b1;
			if (|count_left)
				count_left <= count_left - 1'b1;
		end
		casez ({i_wb_addr[MEM_ADDR_BITS - 2], i_wb_addr[1:0]})
			3'b000: o_wb_data <= w_wb_status;
			3'b001: o_wb_data <= {{32 - TICKBITS {1'b0}}, r_speed};
			3'b010: o_wb_data <= {31'b0000000000000000000000000000000, isr};
			3'b011: o_wb_data <= {31'b0000000000000000000000000000000, ien};
			3'b1zz: o_wb_data <= mem[i_wb_addr[MEM_ADDR_BITS - 3:0]];
		endcase
	end
	initial o_wb_ack = 1'b0;
	always @(posedge i_clk) o_wb_ack <= !i_reset && i_wb_stb;
	assign o_wb_stall = 1'b0;
	always @(posedge i_clk) begin
		if (rd_stb) begin
			rd_word <= mem[rd_addr[MEM_ADDR_BITS - 1:2]];
			rd_sel <= rd_addr[1:0];
		end
		if (LITTLE_ENDIAN)
			case (rd_sel)
				2'b11: rd_byte <= rd_word[31:24];
				2'b10: rd_byte <= rd_word[23:16];
				2'b01: rd_byte <= rd_word[15:8];
				2'b00: rd_byte <= rd_word[7:0];
			endcase
		else
			case (rd_sel)
				2'b00: rd_byte <= rd_word[31:24];
				2'b01: rd_byte <= rd_word[23:16];
				2'b10: rd_byte <= rd_word[15:8];
				2'b11: rd_byte <= rd_word[7:0];
			endcase
	end
	assign w_byte_addr[MEM_ADDR_BITS - 1:0] = newadr;
	generate
		if (MEM_ADDR_BITS < 8) begin : genblk1
			assign w_byte_addr[7:MEM_ADDR_BITS] = 0;
		end
	endgenerate
	initial rd_inc = 1'b0;
	initial r_write_lock = 1'b0;
	initial mstate = I2MIDLE;
	initial r_busy = 1'b0;
	always @(posedge i_clk)
		if (i_reset) begin
			rd_inc <= 1'b0;
			r_write_lock <= 1'b0;
			mstate <= I2MIDLE;
			r_busy <= 1'b0;
		end
		else begin
			if (!ll_i2c_cyc)
				last_addr_flag <= 1'b0;
			else if (last_op)
				last_addr_flag <= 1'b1;
			rd_stb <= 1'b0;
			if (((!r_busy && i_wb_stb) && (i_wb_addr[1:0] == 2'b00)) && !i_wb_addr[MEM_ADDR_BITS - 2])
				last_err <= 1'b0;
			else if (r_busy && ll_i2c_err)
				last_err <= 1'b1;
			if (mstate == I2MIDLE)
				acks_pending <= 2'h0;
			else
				case ({ll_i2c_stb && !ll_i2c_stall, ll_i2c_ack})
					2'b00: acks_pending <= acks_pending;
					2'b01: acks_pending <= (|acks_pending ? acks_pending - 1'b1 : 0);
					2'b10: acks_pending <= acks_pending + 1'b1;
					2'b11: acks_pending <= acks_pending;
				endcase
			last_ack <= (acks_pending[1] == 1'b0) && !ll_i2c_stb;
			rd_inc <= 1'b0;
			case (mstate)
				I2MIDLE: begin
					ll_i2c_cyc <= 1'b0;
					ll_i2c_stb <= 1'b0;
					r_write_lock <= 1'b0;
					if (start_request && !ll_i2c_stall) begin
						ll_i2c_cyc <= 1'b1;
						ll_i2c_stb <= 1'b1;
						ll_i2c_we <= 1'b1;
						ll_i2c_tx_data <= {newdev, 1'b0};
						rd_addr <= newadr;
						mstate <= I2MDEVADDR;
						rd_stb <= 1'b1;
						r_busy <= 1'b1;
					end
					else
						r_busy <= ll_i2c_stall;
				end
				I2MDEVADDR: begin
					r_write_lock <= 1'b0;
					if (!ll_i2c_stall) begin
						ll_i2c_we <= 1'b1;
						ll_i2c_stb <= 1'b1;
						ll_i2c_tx_data <= w_byte_addr;
						if (newrx_txn)
							mstate <= I2MRDSTOP;
						else
							mstate <= I2MTXDATA;
					end
					if (ll_i2c_err) begin
						mstate <= I2MCLEANUP;
						ll_i2c_stb <= 1'b0;
					end
				end
				I2MRDSTOP: begin
					r_write_lock <= 1'b0;
					if (!ll_i2c_stall)
						ll_i2c_stb <= 1'b0;
					if ((!ll_i2c_stb && last_ack) && ll_i2c_ack) begin
						ll_i2c_cyc <= 1'b0;
						mstate <= I2MRDDEV;
					end
					if (ll_i2c_err) begin
						mstate <= I2MCLEANUP;
						ll_i2c_stb <= 1'b0;
					end
				end
				I2MRDDEV: begin
					ll_i2c_stb <= 1'b0;
					r_write_lock <= 1'b0;
					if (!ll_i2c_stall) begin
						ll_i2c_cyc <= 1'b1;
						ll_i2c_stb <= 1'b1;
						ll_i2c_we <= 1'b1;
						ll_i2c_tx_data <= {newdev, 1'b1};
						mstate <= I2MRXDATA;
						r_write_pause <= 2'b01;
					end
					if (ll_i2c_err) begin
						mstate <= I2MCLEANUP;
						ll_i2c_stb <= 1'b0;
					end
				end
				I2MTXDATA: begin
					ll_i2c_stb <= 1'b1;
					r_write_lock <= 1'b0;
					if (!ll_i2c_stall) begin
						rd_inc <= 1'b1;
						rd_addr <= rd_addr + 1'b1;
						ll_i2c_tx_data <= rd_byte;
						rd_stb <= 1'b1;
						if (last_addr_flag) begin
							ll_i2c_stb <= 1'b0;
							mstate <= I2MCLEANUP;
						end
					end
					if (ll_i2c_err) begin
						mstate <= I2MCLEANUP;
						ll_i2c_stb <= 1'b0;
					end
				end
				I2MRXDATA: begin
					ll_i2c_we <= 1'b0;
					if (!ll_i2c_stall) begin
						if (|r_write_pause)
							r_write_pause <= r_write_pause - 1'b1;
						r_write_lock <= r_write_pause == 2'b00;
						ll_i2c_tx_data <= rd_byte;
					end
					if (last_addr_flag) begin
						ll_i2c_stb <= 1'b0;
						mstate <= I2MCLEANUP;
					end
					if (ll_i2c_err) begin
						mstate <= I2MCLEANUP;
						ll_i2c_stb <= 1'b0;
					end
				end
				I2MCLEANUP: begin
					ll_i2c_cyc <= 1'b1;
					ll_i2c_stb <= 1'b0;
					if (ll_i2c_we && ll_i2c_ack)
						rd_inc <= 1'b1;
					if (last_ack) begin
						mstate <= I2MIDLE;
						ll_i2c_cyc <= 1'b1;
					end
				end
				default: mstate <= I2MIDLE;
			endcase
		end
	assign o_int = isr & ien;
	assign o_dbg = {ll_dbg[31:29], last_adr[6:0], wr_inc, count_left[5:0], ll_dbg[14:0]};
	wire unused;
	assign unused = &{1'b0, i_wb_cyc, ll_dbg[28:15]};
endmodule
