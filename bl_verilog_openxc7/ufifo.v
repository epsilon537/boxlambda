`default_nettype none
module ufifo (
	i_clk,
	i_reset,
	i_wr,
	i_data,
	o_empty_n,
	i_rd,
	o_data,
	o_status,
	o_err
);
	parameter BW = 8;
	parameter [3:0] LGFLEN = 4;
	parameter [0:0] RXFIFO = 1'b1;
	localparam FLEN = 1 << LGFLEN;
	input wire i_clk;
	input wire i_reset;
	input wire i_wr;
	input wire [BW - 1:0] i_data;
	output wire o_empty_n;
	input wire i_rd;
	output wire [BW - 1:0] o_data;
	output wire [15:0] o_status;
	output wire o_err;
	reg [BW - 1:0] fifo [0:FLEN - 1];
	reg [BW - 1:0] r_data;
	reg [BW - 1:0] last_write;
	reg [LGFLEN - 1:0] wr_addr;
	reg [LGFLEN - 1:0] rd_addr;
	reg [LGFLEN - 1:0] r_next;
	reg will_overflow;
	reg will_underflow;
	reg osrc;
	wire [LGFLEN - 1:0] w_waddr_plus_one;
	wire [LGFLEN - 1:0] w_waddr_plus_two;
	wire w_write;
	wire w_read;
	reg [LGFLEN - 1:0] r_fill;
	wire [3:0] lglen;
	wire w_half_full;
	reg [9:0] w_fill;
	assign w_write = i_wr && (!will_overflow || i_rd);
	assign w_read = i_rd && o_empty_n;
	assign w_waddr_plus_two = wr_addr + 2;
	assign w_waddr_plus_one = wr_addr + 1;
	initial will_overflow = 1'b0;
	always @(posedge i_clk)
		if (i_reset)
			will_overflow <= 1'b0;
		else if (i_rd)
			will_overflow <= will_overflow && i_wr;
		else if (w_write)
			will_overflow <= will_overflow || (w_waddr_plus_two == rd_addr);
		else if (w_waddr_plus_one == rd_addr)
			will_overflow <= 1'b1;
	initial wr_addr = 0;
	always @(posedge i_clk)
		if (i_reset)
			wr_addr <= {LGFLEN {1'b0}};
		else if (w_write)
			wr_addr <= w_waddr_plus_one;
	always @(posedge i_clk)
		if (w_write)
			fifo[wr_addr] <= i_data;
	initial will_underflow = 1'b1;
	always @(posedge i_clk)
		if (i_reset)
			will_underflow <= 1'b1;
		else if (i_wr)
			will_underflow <= 1'b0;
		else if (w_read)
			will_underflow <= will_underflow || (r_next == wr_addr);
	initial rd_addr = 0;
	initial r_next = 1;
	always @(posedge i_clk)
		if (i_reset) begin
			rd_addr <= 0;
			r_next <= 1;
		end
		else if (w_read) begin
			rd_addr <= rd_addr + 1;
			r_next <= rd_addr + 2;
		end
	always @(posedge i_clk)
		if (w_read)
			r_data <= fifo[r_next[LGFLEN - 1:0]];
	always @(posedge i_clk)
		if (i_wr && (!o_empty_n || (w_read && (r_next == wr_addr))))
			last_write <= i_data;
	initial osrc = 1'b0;
	always @(posedge i_clk)
		if (i_reset)
			osrc <= 1'b0;
		else if (i_wr && (!o_empty_n || (w_read && (r_next == wr_addr))))
			osrc <= 1'b1;
		else if (i_rd)
			osrc <= 1'b0;
	assign o_data = (osrc ? last_write : r_data);
	generate
		if (RXFIFO) begin : RXFIFO_FILL
			initial r_fill = 0;
			always @(posedge i_clk)
				if (i_reset)
					r_fill <= 0;
				else
					case ({w_write, w_read})
						2'b01: r_fill <= r_fill - 1'b1;
						2'b10: r_fill <= r_fill + 1'b1;
						default:
							;
					endcase
		end
		else begin : TXFIFO_FILL
			initial r_fill = -1;
			always @(posedge i_clk)
				if (i_reset)
					r_fill <= -1;
				else
					case ({w_write, w_read})
						2'b01: r_fill <= r_fill + 1'b1;
						2'b10: r_fill <= r_fill - 1'b1;
						default:
							;
					endcase
		end
	endgenerate
	assign o_err = i_wr && !w_write;
	assign lglen = LGFLEN;
	always @(*) begin
		w_fill = 0;
		w_fill[LGFLEN - 1:0] = r_fill;
	end
	assign w_half_full = r_fill[LGFLEN - 1];
	assign o_status = {lglen, w_fill, w_half_full, (RXFIFO != 0 ? !will_underflow : !will_overflow)};
	assign o_empty_n = !will_underflow;
endmodule
