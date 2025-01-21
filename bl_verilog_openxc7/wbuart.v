`default_nettype none
module wbuart (
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
	o_wb_err,
	o_wb_data,
	i_uart_rx,
	o_uart_tx,
	i_cts_n,
	o_rts_n,
	o_uart_int
);
	parameter [30:0] INITIAL_SETUP = 31'd25;
	parameter [3:0] LGFLEN = 4;
	parameter [0:0] HARDWARE_FLOW_CONTROL_PRESENT = 1'b1;
	localparam [3:0] LCLLGFLEN = (LGFLEN > 4'ha ? 4'ha : (LGFLEN < 4'h2 ? 4'h2 : LGFLEN));
	input wire i_clk;
	input wire i_reset;
	input wire i_wb_cyc;
	input wire i_wb_stb;
	input wire i_wb_we;
	input wire [2:0] i_wb_addr;
	input wire [31:0] i_wb_data;
	input wire [3:0] i_wb_sel;
	output wire o_wb_stall;
	output reg o_wb_ack;
	output wire o_wb_err;
	output reg [31:0] o_wb_data;
	input wire i_uart_rx;
	output wire o_uart_tx;
	input wire i_cts_n;
	output reg o_rts_n;
	output wire o_uart_int;
	localparam [2:0] UART_SETUP = 3'b000;
	localparam [2:0] UART_FIFO = 3'b001;
	localparam [2:0] UART_RXREG = 3'b010;
	localparam [2:0] UART_TXREG = 3'b011;
	localparam [2:0] UART_ISR = 3'b100;
	localparam [2:0] UART_IEN = 3'b101;
	wire tx_busy;
	reg [30:0] uart_setup;
	wire rx_stb;
	wire rx_break;
	wire rx_perr;
	wire rx_ferr;
	wire ck_uart;
	wire [7:0] rx_uart_data;
	reg rx_uart_reset;
	wire rx_empty_n;
	wire rx_fifo_err;
	wire [7:0] rxf_wb_data;
	wire [15:0] rxf_status;
	reg rxf_wb_read;
	wire [LCLLGFLEN - 1:0] check_cutoff;
	reg r_rx_perr;
	reg r_rx_ferr;
	wire [31:0] wb_rx_data;
	wire tx_empty_n;
	wire txf_err;
	wire tx_break;
	wire [7:0] tx_data;
	wire [15:0] txf_status;
	reg txf_wb_write;
	reg tx_uart_reset;
	reg [7:0] txf_wb_data;
	wire [31:0] wb_tx_data;
	wire [31:0] wb_fifo_data;
	reg [2:0] r_wb_addr;
	reg r_wb_ack;
	reg rx_data_avl_status_r;
	reg rx_fifo_half_full_status_r;
	reg tx_fifo_half_empty_status_r;
	reg tx_fifo_empty_status_r;
	reg rx_data_avl_isr;
	reg rx_fifo_half_full_isr;
	reg tx_fifo_half_empty_isr;
	reg tx_fifo_empty_isr;
	reg rx_data_avl_ien;
	reg rx_fifo_half_full_ien;
	reg tx_fifo_half_empty_ien;
	reg tx_fifo_empty_ien;
	wire [31:0] wb_isr_data;
	wire [31:0] wb_ien_data;
	initial uart_setup = INITIAL_SETUP | (HARDWARE_FLOW_CONTROL_PRESENT == 1'b0 ? 31'h40000000 : 0);
	always @(posedge i_clk)
		if ((i_wb_stb && (i_wb_addr == UART_SETUP)) && i_wb_we) begin
			if (i_wb_sel[0])
				uart_setup[7:0] <= i_wb_data[7:0];
			if (i_wb_sel[1])
				uart_setup[15:8] <= i_wb_data[15:8];
			if (i_wb_sel[2])
				uart_setup[23:16] <= i_wb_data[23:16];
			if (i_wb_sel[3])
				uart_setup[30:24] <= {i_wb_data[30] || !HARDWARE_FLOW_CONTROL_PRESENT, i_wb_data[29:24]};
		end
	rxuart #(.INITIAL_SETUP(INITIAL_SETUP)) rx(
		.i_clk(i_clk),
		.i_reset(i_reset || rx_uart_reset),
		.i_setup(uart_setup),
		.i_uart_rx(i_uart_rx),
		.o_wr(rx_stb),
		.o_data(rx_uart_data),
		.o_break(rx_break),
		.o_parity_err(rx_perr),
		.o_frame_err(rx_ferr),
		.o_ck_uart(ck_uart)
	);
	ufifo #(
		.LGFLEN(LCLLGFLEN),
		.RXFIFO(1)
	) rxfifo(
		.i_clk(i_clk),
		.i_reset((i_reset || rx_break) || rx_uart_reset),
		.i_wr(rx_stb),
		.i_data(rx_uart_data),
		.o_empty_n(rx_empty_n),
		.i_rd(rxf_wb_read),
		.o_data(rxf_wb_data),
		.o_status(rxf_status),
		.o_err(rx_fifo_err)
	);
	assign check_cutoff = -3;
	always @(posedge i_clk) o_rts_n <= (HARDWARE_FLOW_CONTROL_PRESENT && !uart_setup[30]) && (rxf_status[LCLLGFLEN + 1:2] > check_cutoff);
	initial rxf_wb_read = 1'b0;
	always @(posedge i_clk) rxf_wb_read <= (i_wb_stb && (i_wb_addr == UART_RXREG)) && !i_wb_we;
	initial r_rx_perr = 1'b0;
	initial r_rx_ferr = 1'b0;
	always @(posedge i_clk)
		if (rx_uart_reset || rx_break) begin
			r_rx_perr <= 1'b0;
			r_rx_ferr <= 1'b0;
		end
		else if ((i_wb_stb && (i_wb_addr == UART_RXREG)) && i_wb_we) begin
			if (i_wb_sel[1]) begin
				r_rx_perr <= r_rx_perr && ~i_wb_data[9];
				r_rx_ferr <= r_rx_ferr && ~i_wb_data[10];
			end
		end
		else if (rx_stb) begin
			r_rx_perr <= r_rx_perr || rx_perr;
			r_rx_ferr <= r_rx_ferr || rx_ferr;
		end
	initial rx_uart_reset = 1'b1;
	always @(posedge i_clk)
		if (i_reset || ((i_wb_stb && (i_wb_addr == UART_SETUP)) && i_wb_we))
			rx_uart_reset <= 1'b1;
		else if (((i_wb_stb && (i_wb_addr == UART_RXREG)) && i_wb_we) && i_wb_sel[1])
			rx_uart_reset <= i_wb_data[12];
		else
			rx_uart_reset <= 1'b0;
	assign wb_rx_data = {19'h00000, rx_fifo_err, rx_break, rx_ferr, r_rx_perr, !rx_empty_n, rxf_wb_data};
	initial txf_wb_write = 1'b0;
	always @(posedge i_clk) begin
		txf_wb_write <= ((i_wb_stb && (i_wb_addr == UART_TXREG)) && i_wb_we) && i_wb_sel[0];
		txf_wb_data <= i_wb_data[7:0];
	end
	ufifo #(
		.LGFLEN(LGFLEN),
		.RXFIFO(0)
	) txfifo(
		.i_clk(i_clk),
		.i_reset(tx_break || tx_uart_reset),
		.i_wr(txf_wb_write),
		.i_data(txf_wb_data),
		.o_empty_n(tx_empty_n),
		.i_rd(!tx_busy && tx_empty_n),
		.o_data(tx_data),
		.o_status(txf_status),
		.o_err(txf_err)
	);
	reg r_tx_break;
	initial r_tx_break = 1'b0;
	always @(posedge i_clk)
		if (i_reset)
			r_tx_break <= 1'b0;
		else if (((i_wb_stb && (i_wb_addr == UART_TXREG)) && i_wb_we) && i_wb_sel[1])
			r_tx_break <= i_wb_data[9];
	assign tx_break = r_tx_break;
	initial tx_uart_reset = 1'b1;
	always @(posedge i_clk)
		if (i_reset || ((i_wb_stb && (i_wb_addr == UART_SETUP)) && i_wb_we))
			tx_uart_reset <= 1'b1;
		else if (((i_wb_stb && (i_wb_addr == UART_TXREG)) && i_wb_we) && i_wb_sel[1])
			tx_uart_reset <= i_wb_data[12];
		else
			tx_uart_reset <= 1'b0;
	wire cts_n;
	assign cts_n = HARDWARE_FLOW_CONTROL_PRESENT && i_cts_n;
	txuart #(.INITIAL_SETUP(INITIAL_SETUP)) tx(
		.i_clk(i_clk),
		.i_reset(1'b0),
		.i_setup(uart_setup),
		.i_break(r_tx_break),
		.i_wr(tx_empty_n),
		.i_data(tx_data),
		.i_cts_n(cts_n),
		.o_uart_tx(o_uart_tx),
		.o_busy(tx_busy)
	);
	assign wb_tx_data = {16'h0000, i_cts_n, txf_status[1:0], txf_err, ck_uart, o_uart_tx, tx_break, tx_busy | txf_status[0], (tx_busy | txf_status[0] ? txf_wb_data : 8'b00000000)};
	initial begin
		rx_data_avl_status_r = 1'b0;
		rx_fifo_half_full_status_r = 1'b0;
		tx_fifo_half_empty_status_r = 1'b0;
		tx_fifo_empty_status_r = 1'b0;
		rx_data_avl_isr = 1'b0;
		rx_fifo_half_full_isr = 1'b0;
		tx_fifo_half_empty_isr = 1'b0;
		tx_fifo_empty_isr = 1'b0;
		rx_data_avl_ien = 1'b0;
		rx_fifo_half_full_ien = 1'b0;
		tx_fifo_half_empty_ien = 1'b0;
		tx_fifo_empty_ien = 1'b0;
	end
	always @(posedge i_clk)
		if (i_reset) begin
			rx_data_avl_status_r <= 1'b0;
			rx_fifo_half_full_status_r <= 1'b0;
			tx_fifo_half_empty_status_r <= 1'b0;
			tx_fifo_empty_status_r <= 1'b0;
			rx_data_avl_isr <= 1'b0;
			rx_fifo_half_full_isr <= 1'b0;
			tx_fifo_half_empty_isr <= 1'b0;
			tx_fifo_empty_isr <= 1'b0;
			rx_data_avl_ien <= 1'b0;
			rx_fifo_half_full_ien <= 1'b0;
			tx_fifo_half_empty_ien <= 1'b0;
			tx_fifo_empty_ien <= 1'b0;
		end
		else begin
			if (rxf_status[0] && !rx_data_avl_status_r)
				rx_data_avl_isr <= 1'b1;
			rx_data_avl_status_r <= rxf_status[0];
			if (rxf_status[1] && !rx_fifo_half_full_status_r)
				rx_fifo_half_full_isr <= 1'b1;
			rx_fifo_half_full_status_r <= rxf_status[1];
			if (txf_status[1] && !tx_fifo_half_empty_status_r)
				tx_fifo_half_empty_isr <= 1'b1;
			tx_fifo_half_empty_status_r <= txf_status[1];
			if (!tx_empty_n && !tx_fifo_empty_status_r)
				tx_fifo_empty_isr <= 1'b1;
			tx_fifo_empty_status_r <= ~tx_empty_n;
			if (((i_wb_stb && (i_wb_addr == UART_ISR)) && i_wb_we) && i_wb_sel[1])
				{tx_fifo_half_empty_isr, tx_fifo_empty_isr, rx_fifo_half_full_isr, rx_data_avl_isr} <= {tx_fifo_half_empty_isr, tx_fifo_empty_isr, rx_fifo_half_full_isr, rx_data_avl_isr} & ~i_wb_data[3:0];
			if (((i_wb_stb && (i_wb_addr == UART_IEN)) && i_wb_we) && i_wb_sel[1])
				{tx_fifo_half_empty_ien, tx_fifo_empty_ien, rx_fifo_half_full_ien, rx_data_avl_ien} <= i_wb_data[3:0];
		end
	assign o_uart_int = |({rx_data_avl_isr, rx_fifo_half_full_isr, tx_fifo_empty_isr, tx_fifo_half_empty_isr} & {rx_data_avl_ien, rx_fifo_half_full_ien, tx_fifo_empty_ien, tx_fifo_half_empty_ien});
	assign wb_isr_data = {28'd0, tx_fifo_half_empty_isr, tx_fifo_empty_isr, rx_fifo_half_full_isr, rx_data_avl_isr};
	assign wb_ien_data = {28'd0, tx_fifo_half_empty_ien, tx_fifo_empty_ien, rx_fifo_half_full_ien, rx_data_avl_ien};
	assign wb_fifo_data = {txf_status, rxf_status};
	always @(posedge i_clk) r_wb_addr <= i_wb_addr;
	initial r_wb_ack = 1'b0;
	always @(posedge i_clk) r_wb_ack <= i_wb_stb;
	initial o_wb_ack = 1'b0;
	always @(posedge i_clk) o_wb_ack <= i_wb_cyc && r_wb_ack;
	always @(posedge i_clk)
		casez (r_wb_addr)
			UART_SETUP: o_wb_data <= {1'b0, uart_setup};
			UART_FIFO: o_wb_data <= wb_fifo_data;
			UART_RXREG: o_wb_data <= wb_rx_data;
			UART_TXREG: o_wb_data <= wb_tx_data;
			UART_ISR: o_wb_data <= wb_isr_data;
			UART_IEN: o_wb_data <= wb_ien_data;
			default: o_wb_data <= 32'b00000000000000000000000000000000;
		endcase
	assign o_wb_stall = 1'b0;
	assign o_wb_err = 1'b0;
	wire unused;
	assign unused = &{1'b0, i_wb_data[31]};
endmodule
