`default_nettype none
module rxuart (
	i_clk,
	i_reset,
	i_setup,
	i_uart_rx,
	o_wr,
	o_data,
	o_break,
	o_parity_err,
	o_frame_err,
	o_ck_uart
);
	parameter [30:0] INITIAL_SETUP = 31'd868;
	localparam [3:0] RXU_BIT_ZERO = 4'h0;
	localparam [3:0] RXU_BIT_ONE = 4'h1;
	localparam [3:0] RXU_BIT_TWO = 4'h2;
	localparam [3:0] RXU_BIT_THREE = 4'h3;
	localparam [3:0] RXU_BIT_SEVEN = 4'h7;
	localparam [3:0] RXU_PARITY = 4'h8;
	localparam [3:0] RXU_STOP = 4'h9;
	localparam [3:0] RXU_SECOND_STOP = 4'ha;
	localparam [3:0] RXU_BREAK = 4'hd;
	localparam [3:0] RXU_RESET_IDLE = 4'he;
	localparam [3:0] RXU_IDLE = 4'hf;
	input wire i_clk;
	input wire i_reset;
	input wire [30:0] i_setup;
	input wire i_uart_rx;
	output reg o_wr;
	output reg [7:0] o_data;
	output reg o_break;
	output reg o_parity_err;
	output reg o_frame_err;
	output wire o_ck_uart;
	wire [27:0] clocks_per_baud;
	wire [27:0] break_condition;
	wire [27:0] half_baud;
	wire [1:0] data_bits;
	wire use_parity;
	wire parity_even;
	wire dblstop;
	wire fixd_parity;
	reg [29:0] r_setup;
	reg [3:0] state;
	reg [27:0] baud_counter;
	reg zero_baud_counter;
	reg q_uart;
	reg qq_uart;
	reg ck_uart;
	reg [27:0] chg_counter;
	reg line_synch;
	reg half_baud_time;
	reg [7:0] data_reg;
	reg calc_parity;
	reg pre_wr;
	assign clocks_per_baud = {4'h0, r_setup[23:0]};
	assign data_bits = r_setup[29:28];
	assign dblstop = r_setup[27];
	assign use_parity = r_setup[26];
	assign fixd_parity = r_setup[25];
	assign parity_even = r_setup[24];
	assign break_condition = {r_setup[23:0], 4'h0};
	assign half_baud = {5'h00, r_setup[23:1]} - 28'h0000001;
	initial q_uart = 1'b0;
	initial qq_uart = 1'b0;
	initial ck_uart = 1'b0;
	always @(posedge i_clk) begin
		q_uart <= i_uart_rx;
		qq_uart <= q_uart;
		ck_uart <= qq_uart;
	end
	assign o_ck_uart = ck_uart;
	initial chg_counter = 28'h0000000;
	always @(posedge i_clk)
		if (i_reset)
			chg_counter <= 28'h0000000;
		else if (qq_uart != ck_uart)
			chg_counter <= 28'h0000000;
		else if (chg_counter < break_condition)
			chg_counter <= chg_counter + 1;
	initial o_break = 1'b0;
	always @(posedge i_clk) o_break <= ((chg_counter >= break_condition) && ~ck_uart ? 1'b1 : 1'b0);
	initial line_synch = 1'b0;
	always @(posedge i_clk) line_synch <= (chg_counter >= break_condition) && ck_uart;
	initial half_baud_time = 0;
	always @(posedge i_clk) half_baud_time <= ~ck_uart && (chg_counter >= half_baud);
	initial r_setup = INITIAL_SETUP[29:0];
	always @(posedge i_clk)
		if (state >= RXU_RESET_IDLE)
			r_setup <= i_setup[29:0];
	initial state = RXU_RESET_IDLE;
	always @(posedge i_clk)
		if (i_reset)
			state <= RXU_RESET_IDLE;
		else if (state == RXU_RESET_IDLE) begin
			if (line_synch)
				state <= RXU_IDLE;
			else
				state <= RXU_RESET_IDLE;
		end
		else if (o_break)
			state <= RXU_BREAK;
		else if (state == RXU_BREAK) begin
			if (ck_uart)
				state <= RXU_IDLE;
			else
				state <= RXU_BREAK;
		end
		else if (state == RXU_IDLE) begin
			if (~ck_uart && half_baud_time)
				case (data_bits)
					2'b00: state <= RXU_BIT_ZERO;
					2'b01: state <= RXU_BIT_ONE;
					2'b10: state <= RXU_BIT_TWO;
					2'b11: state <= RXU_BIT_THREE;
				endcase
			else
				state <= RXU_IDLE;
		end
		else if (zero_baud_counter) begin
			if (state < RXU_BIT_SEVEN)
				state <= state + 1;
			else if (state == RXU_BIT_SEVEN)
				state <= (use_parity ? RXU_PARITY : RXU_STOP);
			else if (state == RXU_PARITY)
				state <= RXU_STOP;
			else if (state == RXU_STOP) begin
				if (~ck_uart)
					state <= RXU_RESET_IDLE;
				else if (dblstop)
					state <= RXU_SECOND_STOP;
				else
					state <= RXU_IDLE;
			end
			else if (~ck_uart)
				state <= RXU_RESET_IDLE;
			else
				state <= RXU_IDLE;
		end
	always @(posedge i_clk)
		if (zero_baud_counter && (state != RXU_PARITY))
			data_reg <= {ck_uart, data_reg[7:1]};
	always @(posedge i_clk)
		if (state == RXU_IDLE)
			calc_parity <= 0;
		else if (zero_baud_counter)
			calc_parity <= calc_parity ^ ck_uart;
	initial o_parity_err = 1'b0;
	always @(posedge i_clk)
		if (zero_baud_counter && (state == RXU_PARITY)) begin
			if (fixd_parity)
				o_parity_err <= ck_uart ^ parity_even;
			else if (parity_even)
				o_parity_err <= calc_parity != ck_uart;
			else
				o_parity_err <= calc_parity == ck_uart;
		end
		else if (state >= RXU_BREAK)
			o_parity_err <= 1'b0;
	initial o_frame_err = 1'b0;
	always @(posedge i_clk)
		if (zero_baud_counter && ((state == RXU_STOP) || (state == RXU_SECOND_STOP)))
			o_frame_err <= o_frame_err || ~ck_uart;
		else if (zero_baud_counter || (state >= RXU_BREAK))
			o_frame_err <= 1'b0;
	initial o_data = 8'h00;
	initial pre_wr = 1'b0;
	always @(posedge i_clk)
		if (i_reset) begin
			pre_wr <= 1'b0;
			o_data <= 8'h00;
		end
		else if (zero_baud_counter && (state == RXU_STOP)) begin
			pre_wr <= 1'b1;
			case (data_bits)
				2'b00: o_data <= data_reg;
				2'b01: o_data <= {1'b0, data_reg[7:1]};
				2'b10: o_data <= {2'b00, data_reg[7:2]};
				2'b11: o_data <= {3'b000, data_reg[7:3]};
			endcase
		end
		else if (zero_baud_counter || (state == RXU_IDLE))
			pre_wr <= 1'b0;
	initial o_wr = 1'b0;
	always @(posedge i_clk)
		if (zero_baud_counter || (state == RXU_IDLE))
			o_wr <= pre_wr && !i_reset;
		else
			o_wr <= 1'b0;
	always @(posedge i_clk)
		if (i_reset)
			baud_counter <= clocks_per_baud - 28'h0000001;
		else if (zero_baud_counter)
			baud_counter <= clocks_per_baud - 28'h0000001;
		else
			case (state)
				RXU_RESET_IDLE: baud_counter <= clocks_per_baud - 28'h0000001;
				RXU_BREAK: baud_counter <= clocks_per_baud - 28'h0000001;
				RXU_IDLE: baud_counter <= clocks_per_baud - 28'h0000001;
				default: baud_counter <= baud_counter - 28'h0000001;
			endcase
	initial zero_baud_counter = 1'b0;
	always @(posedge i_clk)
		if (state == RXU_IDLE)
			zero_baud_counter <= 1'b0;
		else
			zero_baud_counter <= baud_counter == 28'h0000001;
endmodule
