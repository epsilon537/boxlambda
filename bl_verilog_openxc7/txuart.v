`default_nettype none
module txuart (
	i_clk,
	i_reset,
	i_setup,
	i_break,
	i_wr,
	i_data,
	i_cts_n,
	o_uart_tx,
	o_busy
);
	parameter [30:0] INITIAL_SETUP = 31'd868;
	localparam [3:0] TXU_BIT_ZERO = 4'h0;
	localparam [3:0] TXU_BIT_ONE = 4'h1;
	localparam [3:0] TXU_BIT_TWO = 4'h2;
	localparam [3:0] TXU_BIT_THREE = 4'h3;
	localparam [3:0] TXU_BIT_SEVEN = 4'h7;
	localparam [3:0] TXU_PARITY = 4'h8;
	localparam [3:0] TXU_STOP = 4'h9;
	localparam [3:0] TXU_SECOND_STOP = 4'ha;
	localparam [3:0] TXU_BREAK = 4'he;
	localparam [3:0] TXU_IDLE = 4'hf;
	input wire i_clk;
	input wire i_reset;
	input wire [30:0] i_setup;
	input wire i_break;
	input wire i_wr;
	input wire [7:0] i_data;
	input wire i_cts_n;
	output reg o_uart_tx;
	output wire o_busy;
	wire [27:0] clocks_per_baud;
	wire [27:0] break_condition;
	wire [1:0] i_data_bits;
	wire [1:0] data_bits;
	wire use_parity;
	wire parity_odd;
	wire dblstop;
	wire fixd_parity;
	wire fixdp_value;
	wire hw_flow_control;
	wire i_parity_odd;
	reg [30:0] r_setup;
	assign clocks_per_baud = {4'h0, r_setup[23:0]};
	assign break_condition = {r_setup[23:0], 4'h0};
	assign hw_flow_control = !r_setup[30];
	assign i_data_bits = i_setup[29:28];
	assign data_bits = r_setup[29:28];
	assign dblstop = r_setup[27];
	assign use_parity = r_setup[26];
	assign fixd_parity = r_setup[25];
	assign i_parity_odd = i_setup[24];
	assign parity_odd = r_setup[24];
	assign fixdp_value = r_setup[24];
	reg [27:0] baud_counter;
	reg [3:0] state;
	reg [7:0] lcl_data;
	reg calc_parity;
	reg r_busy;
	reg zero_baud_counter;
	reg last_state;
	reg q_cts_n;
	reg qq_cts_n;
	reg ck_cts;
	always @(posedge i_clk) {qq_cts_n, q_cts_n} <= {q_cts_n, i_cts_n};
	always @(posedge i_clk) ck_cts <= !qq_cts_n || !hw_flow_control;
	initial r_busy = 1'b1;
	initial state = TXU_IDLE;
	always @(posedge i_clk)
		if (i_reset) begin
			r_busy <= 1'b1;
			state <= TXU_IDLE;
		end
		else if (i_break) begin
			state <= TXU_BREAK;
			r_busy <= 1'b1;
		end
		else if (!zero_baud_counter)
			r_busy <= 1'b1;
		else if (state == TXU_BREAK) begin
			state <= TXU_IDLE;
			r_busy <= !ck_cts;
		end
		else if (state == TXU_IDLE) begin
			if (i_wr && !r_busy) begin
				r_busy <= 1'b1;
				case (i_data_bits)
					2'b00: state <= TXU_BIT_ZERO;
					2'b01: state <= TXU_BIT_ONE;
					2'b10: state <= TXU_BIT_TWO;
					2'b11: state <= TXU_BIT_THREE;
				endcase
			end
			else
				r_busy <= !ck_cts;
		end
		else begin
			r_busy <= 1'b1;
			if (state[3] == 0) begin
				if (state == TXU_BIT_SEVEN)
					state <= (use_parity ? TXU_PARITY : TXU_STOP);
				else
					state <= state + 1;
			end
			else if (state == TXU_PARITY)
				state <= TXU_STOP;
			else if (state == TXU_STOP) begin
				if (dblstop)
					state <= TXU_SECOND_STOP;
				else
					state <= TXU_IDLE;
			end
			else
				state <= TXU_IDLE;
		end
	assign o_busy = r_busy;
	initial r_setup = INITIAL_SETUP;
	always @(posedge i_clk)
		if (!o_busy)
			r_setup <= i_setup;
	initial lcl_data = 8'hff;
	always @(posedge i_clk)
		if (!r_busy)
			lcl_data <= i_data;
		else if (zero_baud_counter)
			lcl_data <= {1'b0, lcl_data[7:1]};
	initial o_uart_tx = 1'b1;
	always @(posedge i_clk)
		if (i_reset)
			o_uart_tx <= 1'b1;
		else if (i_break || (i_wr && !r_busy))
			o_uart_tx <= 1'b0;
		else if (zero_baud_counter)
			casez (state)
				4'b0zzz: o_uart_tx <= lcl_data[0];
				TXU_PARITY: o_uart_tx <= calc_parity;
				default: o_uart_tx <= 1'b1;
			endcase
	initial calc_parity = 1'b0;
	always @(posedge i_clk)
		if (!o_busy)
			calc_parity <= i_setup[24];
		else if (fixd_parity)
			calc_parity <= fixdp_value;
		else if (zero_baud_counter) begin
			if (state[3] == 0)
				calc_parity <= calc_parity ^ lcl_data[0];
			else if (state == TXU_IDLE)
				calc_parity <= parity_odd;
		end
		else if (!r_busy)
			calc_parity <= parity_odd;
	initial zero_baud_counter = 1'b0;
	initial baud_counter = 28'h0000005;
	always @(posedge i_clk) begin
		zero_baud_counter <= baud_counter == 28'h0000001;
		if (i_reset || i_break) begin
			baud_counter <= break_condition;
			zero_baud_counter <= 1'b0;
		end
		else if (!zero_baud_counter)
			baud_counter <= baud_counter - 28'h0000001;
		else if (state == TXU_BREAK) begin
			baud_counter <= 0;
			zero_baud_counter <= 1'b1;
		end
		else if (state == TXU_IDLE) begin
			baud_counter <= 28'h0000000;
			zero_baud_counter <= 1'b1;
			if (i_wr && !r_busy) begin
				baud_counter <= {4'h0, i_setup[23:0]} - 28'h0000001;
				zero_baud_counter <= 1'b0;
			end
		end
		else if (last_state)
			baud_counter <= clocks_per_baud - 28'h0000002;
		else
			baud_counter <= clocks_per_baud - 28'h0000001;
	end
	initial last_state = 1'b0;
	always @(posedge i_clk)
		if (dblstop)
			last_state <= state == TXU_SECOND_STOP;
		else
			last_state <= state == TXU_STOP;
	wire unused;
	assign unused = &{1'b0, i_parity_odd, data_bits};
endmodule
