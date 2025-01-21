`default_nettype none
module sdspi (
	i_clk,
	i_sd_reset,
	i_wb_cyc,
	i_wb_stb,
	i_wb_we,
	i_wb_addr,
	i_wb_data,
	i_wb_sel,
	o_wb_stall,
	o_wb_ack,
	o_wb_data,
	o_cs_n,
	o_sck,
	o_mosi,
	i_miso,
	i_card_detect,
	o_int,
	i_bus_grant,
	o_debug
);
	parameter [0:0] OPT_CARD_DETECT = 1'b1;
	parameter [0:0] OPT_LITTLE_ENDIAN = 1'b0;
	parameter LGFIFOLN = 7;
	parameter POWERUP_IDLE = 1000;
	parameter STARTUP_CLOCKS = 75;
	parameter CKDIV_BITS = 8;
	parameter [CKDIV_BITS - 1:0] INITIAL_CLKDIV = 8'h7c;
	parameter [0:0] OPT_SPI_ARBITRATION = 1'b0;
	localparam AW = 3;
	localparam DW = 32;
	input wire i_clk;
	input wire i_sd_reset;
	input wire i_wb_cyc;
	input wire i_wb_stb;
	input wire i_wb_we;
	input wire [2:0] i_wb_addr;
	input wire [31:0] i_wb_data;
	input wire [3:0] i_wb_sel;
	output wire o_wb_stall;
	output reg o_wb_ack;
	output reg [31:0] o_wb_data;
	output wire o_cs_n;
	output wire o_sck;
	output wire o_mosi;
	input wire i_miso;
	input wire i_card_detect;
	output wire o_int;
	input wire i_bus_grant;
	output reg [31:0] o_debug;
	localparam [2:0] SDSPI_CMD_ADDRESS = 3'b000;
	localparam [2:0] SDSPI_DAT_ADDRESS = 3'b001;
	localparam [2:0] SDSPI_FIFO_A_ADDR = 3'b010;
	localparam [2:0] SDSPI_FIFO_B_ADDR = 3'b011;
	localparam [2:0] SDSPI_ISR_ADDRESS = 3'b100;
	localparam [2:0] SDSPI_IEN_ADDRESS = 3'b101;
	localparam BLKBASE = 16;
	localparam IRQ_BUSY_BIT = 0;
	localparam IRQ_CARD_REMOVED_BIT = 1;
	localparam CARD_REMOVED_BIT = 18;
	localparam ERR_BIT = 15;
	localparam FIFO_ID_BIT = 12;
	localparam USE_FIFO_BIT = 11;
	localparam FIFO_WRITE_BIT = 10;
	reg r_cmd_busy;
	reg dbg_trigger;
	wire wb_stb;
	wire write_stb;
	wire wb_cmd_stb;
	wire wb_isr_stb;
	wire wb_ien_stb;
	wire new_data;
	wire [2:0] wb_addr;
	wire [31:0] wb_data;
	reg [2:0] pipe_addr;
	reg dly_stb;
	reg [31:0] fifo_a [0:(1 << LGFIFOLN) - 1];
	reg [31:0] fifo_b [0:(1 << LGFIFOLN) - 1];
	reg [LGFIFOLN - 1:0] fifo_wb_addr;
	reg [LGFIFOLN - 1:0] write_fifo_a_addr;
	reg [LGFIFOLN - 1:0] write_fifo_b_addr;
	reg [LGFIFOLN - 1:0] read_fifo_a_addr;
	reg [LGFIFOLN - 1:0] read_fifo_b_addr;
	wire [LGFIFOLN:0] spi_read_addr;
	wire [LGFIFOLN:0] spi_write_addr;
	reg [31:0] write_fifo_a_data;
	reg [31:0] write_fifo_b_data;
	reg [31:0] fifo_a_word;
	reg [31:0] fifo_b_word;
	reg [31:0] spi_read_data;
	wire [31:0] spi_write_data;
	reg write_fifo_a;
	reg write_fifo_b;
	reg [31:0] r_data_reg;
	reg r_cmd_err;
	reg [7:0] r_last_r_one;
	reg card_removed;
	reg card_present;
	reg [3:0] r_lgblklen;
	wire [3:0] max_lgblklen;
	reg [25:0] r_watchdog;
	reg r_watchdog_err;
	reg [31:0] card_status;
	wire ll_advance;
	reg [CKDIV_BITS - 1:0] r_sdspi_clk;
	reg ll_cmd_stb;
	reg [7:0] ll_cmd_dat;
	wire ll_out_stb;
	wire ll_idle;
	wire [7:0] ll_out_dat;
	reg r_fifo_id;
	reg r_use_fifo;
	reg write_to_card;
	wire w_reset;
	wire cmd_out_stb;
	wire [7:0] cmd_out_byte;
	wire cmd_sent;
	wire cmd_valid;
	wire cmd_busy;
	wire [39:0] cmd_response;
	reg rx_start;
	wire spi_write_to_fifo;
	wire rx_valid;
	wire rx_busy;
	wire [7:0] rx_response;
	reg tx_start;
	wire spi_read_from_fifo;
	wire tx_stb;
	wire [7:0] tx_byte;
	wire tx_valid;
	wire tx_busy;
	wire [7:0] tx_response;
	reg last_busy;
	reg [1:0] isr;
	reg [1:0] ien;
	assign wb_stb = i_wb_stb && !o_wb_stall;
	assign write_stb = wb_stb && i_wb_we;
	assign wb_cmd_stb = (!r_cmd_busy && write_stb) && (i_wb_addr == SDSPI_CMD_ADDRESS);
	assign wb_isr_stb = write_stb && (i_wb_addr == SDSPI_ISR_ADDRESS);
	assign wb_ien_stb = write_stb && (i_wb_addr == SDSPI_IEN_ADDRESS);
	assign wb_addr = i_wb_addr;
	assign wb_data = i_wb_data;
	assign new_data = ((i_wb_stb && !o_wb_stall) && i_wb_we) && (i_wb_addr == SDSPI_DAT_ADDRESS);
	llsdspi #(
		.SPDBITS(CKDIV_BITS),
		.STARTUP_CLOCKS(STARTUP_CLOCKS),
		.POWERUP_IDLE(POWERUP_IDLE),
		.OPT_SPI_ARBITRATION(OPT_SPI_ARBITRATION)
	) lowlevel(
		.i_clk(i_clk),
		.i_reset(i_sd_reset),
		.i_speed(r_sdspi_clk),
		.i_cs(r_cmd_busy),
		.i_stb(ll_cmd_stb),
		.i_byte(ll_cmd_dat),
		.o_cs_n(o_cs_n),
		.o_sclk(o_sck),
		.o_mosi(o_mosi),
		.i_miso(i_miso),
		.o_stb(ll_out_stb),
		.o_byte(ll_out_dat),
		.o_idle(ll_idle),
		.i_bus_grant(i_bus_grant)
	);
	assign w_reset = i_sd_reset || r_watchdog_err;
	spicmd spicmdi(
		.i_clk(i_clk),
		.i_reset(w_reset),
		.i_cmd_stb(wb_cmd_stb && (wb_data[7:6] == 2'b01)),
		.i_cmd_type(wb_data[9:8]),
		.i_cmd(wb_data[5:0]),
		.i_cmd_data(r_data_reg),
		.o_busy(cmd_busy),
		.o_ll_stb(cmd_out_stb),
		.o_ll_byte(cmd_out_byte),
		.i_ll_busy(!ll_advance),
		.i_ll_stb(ll_out_stb),
		.i_ll_byte(ll_out_dat),
		.o_cmd_sent(cmd_sent),
		.o_rxvalid(cmd_valid),
		.o_response(cmd_response)
	);
	spirxdata #(.OPT_LITTLE_ENDIAN(OPT_LITTLE_ENDIAN)) spirxdatai(
		.i_clk(i_clk),
		.i_reset(w_reset | r_cmd_err),
		.i_start(rx_start),
		.i_lgblksz(r_lgblklen),
		.i_fifo(r_fifo_id),
		.o_busy(rx_busy),
		.i_ll_stb(ll_out_stb && !cmd_busy),
		.i_ll_byte(ll_out_dat),
		.o_write(spi_write_to_fifo),
		.o_addr(spi_write_addr),
		.o_data(spi_write_data),
		.o_rxvalid(rx_valid),
		.o_response(rx_response)
	);
	spitxdata #(
		.RDDELAY(2),
		.OPT_LITTLE_ENDIAN(OPT_LITTLE_ENDIAN)
	) spitxdatai(
		.i_clk(i_clk),
		.i_reset(w_reset | r_cmd_err),
		.i_start(tx_start),
		.i_lgblksz(r_lgblklen),
		.i_fifo(r_fifo_id),
		.o_busy(tx_busy),
		.o_read(spi_read_from_fifo),
		.o_addr(spi_read_addr),
		.i_data(spi_read_data),
		.i_ll_busy(!ll_advance || cmd_busy),
		.o_ll_stb(tx_stb),
		.o_ll_byte(tx_byte),
		.i_ll_stb(ll_out_stb && !cmd_busy),
		.i_ll_byte(ll_out_dat),
		.o_rxvalid(tx_valid),
		.o_response(tx_response)
	);
	always @(posedge i_clk)
		if (write_stb && (wb_addr == SDSPI_CMD_ADDRESS))
			fifo_wb_addr <= {LGFIFOLN {1'b0}};
		else if (wb_stb && wb_addr[1])
			fifo_wb_addr <= fifo_wb_addr + 1;
	initial write_fifo_a = 0;
	always @(posedge i_clk)
		if ((r_use_fifo && rx_busy) && !spi_write_addr[LGFIFOLN]) begin
			write_fifo_a <= spi_write_to_fifo;
			write_fifo_a_data <= spi_write_data;
			write_fifo_a_addr <= spi_write_addr[LGFIFOLN - 1:0];
		end
		else begin
			write_fifo_a <= write_stb && (wb_addr == SDSPI_FIFO_A_ADDR);
			write_fifo_a_data <= wb_data;
			write_fifo_a_addr <= fifo_wb_addr;
		end
	initial write_fifo_b = 0;
	always @(posedge i_clk)
		if ((r_use_fifo && rx_busy) && spi_write_addr[LGFIFOLN]) begin
			write_fifo_b <= spi_write_to_fifo;
			write_fifo_b_data <= spi_write_data;
			write_fifo_b_addr <= spi_write_addr[LGFIFOLN - 1:0];
		end
		else begin
			write_fifo_b <= write_stb && (wb_addr == SDSPI_FIFO_B_ADDR);
			write_fifo_b_data <= wb_data;
			write_fifo_b_addr <= fifo_wb_addr;
		end
	always @(posedge i_clk)
		if (write_fifo_a)
			fifo_a[write_fifo_a_addr] <= write_fifo_a_data;
	always @(posedge i_clk)
		if (write_fifo_b)
			fifo_b[write_fifo_b_addr] <= write_fifo_b_data;
	always @(*)
		if ((r_use_fifo && tx_busy) && !spi_read_addr[LGFIFOLN])
			read_fifo_a_addr = spi_read_addr[LGFIFOLN - 1:0];
		else
			read_fifo_a_addr = fifo_wb_addr;
	always @(*)
		if ((r_use_fifo && tx_busy) && spi_read_addr[LGFIFOLN])
			read_fifo_b_addr = spi_read_addr[LGFIFOLN - 1:0];
		else
			read_fifo_b_addr = fifo_wb_addr;
	always @(posedge i_clk) fifo_a_word <= fifo_a[read_fifo_a_addr];
	always @(posedge i_clk) fifo_b_word <= fifo_b[read_fifo_b_addr];
	always @(posedge i_clk)
		if (!spi_read_addr[LGFIFOLN])
			spi_read_data <= fifo_a_word;
		else
			spi_read_data <= fifo_b_word;
	initial r_fifo_id = 0;
	always @(posedge i_clk)
		if (!r_cmd_busy && wb_cmd_stb)
			r_fifo_id <= wb_data[FIFO_ID_BIT];
	initial r_cmd_busy = 0;
	initial tx_start = 0;
	initial rx_start = 0;
	always @(posedge i_clk)
		if (i_sd_reset) begin
			r_cmd_busy <= 0;
			r_use_fifo <= 0;
			tx_start <= 0;
			rx_start <= 0;
		end
		else if (!r_cmd_busy) begin
			r_cmd_busy <= wb_cmd_stb && (wb_data[7:6] == 2'b01);
			tx_start <= 0;
			rx_start <= 0;
			if (wb_cmd_stb && (wb_data[7:6] == 2'b01)) begin
				write_to_card <= wb_data[FIFO_WRITE_BIT];
				r_use_fifo <= wb_data[USE_FIFO_BIT];
				if (wb_data[USE_FIFO_BIT]) begin
					tx_start <= wb_data[FIFO_WRITE_BIT];
					rx_start <= !wb_data[FIFO_WRITE_BIT];
				end
			end
			if (r_watchdog_err) begin
				r_use_fifo <= 0;
				tx_start <= 0;
				rx_start <= 0;
			end
		end
		else begin
			if ((((ll_idle && !ll_cmd_stb) && !cmd_busy) && !rx_busy) && !tx_busy) begin
				r_cmd_busy <= 0;
				r_use_fifo <= 0;
			end
			if ((r_cmd_err || tx_busy) || rx_busy) begin
				tx_start <= 0;
				rx_start <= 0;
			end
			if (r_watchdog_err) begin
				r_use_fifo <= 0;
				tx_start <= 0;
				rx_start <= 0;
			end
		end
	initial r_cmd_err = 0;
	always @(posedge i_clk)
		if (r_watchdog_err)
			r_cmd_err <= 1;
		else if (r_cmd_busy) begin
			if (cmd_valid)
				r_cmd_err <= r_cmd_err || (cmd_response[38:33] != 0);
			if (rx_valid)
				r_cmd_err <= r_cmd_err || rx_response[3];
		end
		else if (wb_cmd_stb)
			r_cmd_err <= r_cmd_err && !wb_data[ERR_BIT];
	always @(posedge i_clk)
		if (!r_cmd_busy) begin
			if (new_data)
				r_data_reg <= wb_data;
			else if (wb_cmd_stb && wb_data[7])
				r_data_reg <= {4'h0, max_lgblklen, 4'h0, r_lgblklen, {16 - CKDIV_BITS {1'b0}}, r_sdspi_clk};
		end
		else if (cmd_valid) begin
			r_data_reg <= cmd_response[31:0];
			r_last_r_one <= cmd_response[39:32];
		end
		else if (tx_valid)
			r_data_reg <= {24'h000000, tx_response[7:0]};
		else if (rx_valid)
			r_data_reg <= {24'h000000, rx_response[7:0]};
	assign ll_advance = !ll_cmd_stb || ll_idle;
	initial ll_cmd_stb = 0;
	always @(posedge i_clk) begin
		if (ll_advance) begin
			if (cmd_busy) begin
				ll_cmd_stb <= ll_cmd_stb || cmd_out_stb;
				ll_cmd_dat <= (cmd_out_stb ? cmd_out_byte : 8'hff);
			end
			else begin
				ll_cmd_stb <= ll_cmd_stb || tx_stb;
				ll_cmd_dat <= (tx_stb ? tx_byte : 8'hff);
			end
		end
		if (((ll_idle && !cmd_busy) && !rx_busy) && !tx_busy)
			ll_cmd_stb <= 1'b0;
		if (!r_cmd_busy || i_sd_reset)
			ll_cmd_stb <= 1'b0;
	end
	assign max_lgblklen = LGFIFOLN + 2;
	initial r_sdspi_clk = INITIAL_CLKDIV;
	initial r_lgblklen = 9;
	always @(posedge i_clk) begin
		if (wb_cmd_stb && (wb_data[7:6] == 2'b11)) begin
			if (r_data_reg[CKDIV_BITS - 1:0] != 0)
				r_sdspi_clk <= r_data_reg[CKDIV_BITS - 1:0];
			if ((r_data_reg[BLKBASE+:4] >= 3) && (r_data_reg[BLKBASE+:4] <= max_lgblklen))
				r_lgblklen <= r_data_reg[BLKBASE+:4];
		end
		if (!card_present)
			r_sdspi_clk <= INITIAL_CLKDIV;
	end
	always @(posedge i_clk) pipe_addr <= wb_addr;
	always @(*) card_status = {10'h000, r_watchdog_err, i_sd_reset, !card_present, card_removed, 2'b00, r_cmd_err, r_cmd_busy, 1'b0, r_fifo_id, r_use_fifo, write_to_card, 2'b00, r_last_r_one};
	always @(posedge i_clk)
		case (pipe_addr)
			SDSPI_CMD_ADDRESS: o_wb_data <= card_status;
			SDSPI_DAT_ADDRESS: o_wb_data <= r_data_reg;
			SDSPI_FIFO_A_ADDR: o_wb_data <= fifo_a_word;
			SDSPI_FIFO_B_ADDR: o_wb_data <= fifo_b_word;
			SDSPI_ISR_ADDRESS: o_wb_data <= {30'b000000000000000000000000000000, isr};
			SDSPI_IEN_ADDRESS: o_wb_data <= {30'b000000000000000000000000000000, ien};
			default: o_wb_data <= 32'b00000000000000000000000000000000;
		endcase
	initial dly_stb = 0;
	always @(posedge i_clk)
		if (!i_wb_cyc)
			dly_stb <= 0;
		else
			dly_stb <= wb_stb;
	initial o_wb_ack = 0;
	always @(posedge i_clk)
		if (!i_wb_cyc)
			o_wb_ack <= 1'b0;
		else
			o_wb_ack <= dly_stb;
	assign o_wb_stall = 1'b0;
	initial last_busy = 0;
	always @(posedge i_clk) last_busy <= r_cmd_busy;
	initial begin
		isr = 2'b00;
		ien = 2'b00;
	end
	always @(posedge i_clk)
		if (i_sd_reset) begin
			isr <= 2'b00;
			ien <= 2'b00;
		end
		else begin
			if (!r_cmd_busy && last_busy)
				isr[IRQ_BUSY_BIT] <= 1'b1;
			if (!card_removed && !card_present)
				isr[IRQ_CARD_REMOVED_BIT] <= 1'b1;
			if (wb_isr_stb)
				isr <= isr & ~wb_data[1:0];
			if (wb_ien_stb)
				ien <= wb_data[1:0];
		end
	assign o_int = |(isr & ien);
	generate
		if (OPT_CARD_DETECT) begin : GEN_CARD_DETECT
			reg [2:0] raw_card_present;
			reg [9:0] card_detect_counter;
			initial card_removed = 1'b1;
			always @(posedge i_clk)
				if (i_sd_reset)
					card_removed <= 1'b1;
				else if (!card_present)
					card_removed <= 1'b1;
				else if (wb_cmd_stb && wb_data[CARD_REMOVED_BIT])
					card_removed <= 1'b0;
			initial raw_card_present = 0;
			always @(posedge i_clk) raw_card_present <= {raw_card_present[1:0], i_card_detect};
			initial card_detect_counter = 0;
			always @(posedge i_clk)
				if (i_sd_reset || !raw_card_present[2])
					card_detect_counter <= 0;
				else if (!(&card_detect_counter))
					card_detect_counter <= card_detect_counter + 1;
			initial card_present = 1'b0;
			always @(posedge i_clk)
				if (i_sd_reset || !raw_card_present[2])
					card_present <= 1'b0;
				else if (&card_detect_counter)
					card_present <= 1'b1;
		end
		else begin : NO_CARD_DETECT_SIGNAL
			always @(*) card_present = 1'b1;
			always @(*) card_removed = 1'b0;
		end
	endgenerate
	initial r_watchdog_err = 1'b0;
	always @(posedge i_clk)
		if (!r_cmd_busy)
			r_watchdog_err <= 1'b0;
		else if (r_watchdog == 0)
			r_watchdog_err <= 1'b1;
	initial r_watchdog = 26'h3ffffff;
	always @(posedge i_clk)
		if (!r_cmd_busy)
			r_watchdog <= 26'h03fffff;
		else if (|r_watchdog)
			r_watchdog <= r_watchdog - 26'h0000001;
	initial dbg_trigger = 0;
	always @(posedge i_clk) dbg_trigger <= cmd_valid && (cmd_response[38:33] != 0);
	always @(posedge i_clk) o_debug <= {dbg_trigger, ll_cmd_stb, ll_cmd_stb & ll_idle, ll_out_stb, o_cs_n, o_sck, o_mosi, i_miso, 3'b000, i_sd_reset, 3'b000, r_cmd_busy, ll_cmd_dat, ll_out_dat};
	wire unused;
	assign unused = &{1'b0, i_wb_cyc, i_wb_sel, cmd_sent, spi_read_from_fifo};
endmodule
