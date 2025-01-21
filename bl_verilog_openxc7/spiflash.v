`default_nettype none
module spiflash (
	i_clk,
	i_reset,
	i_wb_cyc,
	i_wb_stb,
	i_cfg_stb,
	i_wb_we,
	i_wb_addr,
	i_wb_data,
	o_wb_stall,
	o_wb_ack,
	o_wb_data,
	o_spi_cs_n,
	o_spi_sck,
	o_spi_mosi,
	i_spi_miso
);
	parameter SCK_CLKDIV = 2;
	input wire i_clk;
	input wire i_reset;
	input wire i_wb_cyc;
	input wire i_wb_stb;
	input wire i_cfg_stb;
	input wire i_wb_we;
	input wire [21:0] i_wb_addr;
	input wire [31:0] i_wb_data;
	output reg o_wb_stall;
	output reg o_wb_ack;
	output reg [31:0] o_wb_data;
	output reg o_spi_cs_n;
	output reg o_spi_sck;
	output reg o_spi_mosi;
	input wire i_spi_miso;
	localparam SCK_DIV_NUM_BITS = $clog2(SCK_CLKDIV);
	localparam SCK_COUNTER_MIDPOINT = {1'b0, {SCK_DIV_NUM_BITS - 1 {1'b1}}};
	localparam SCK_COUNTER_ENDPOINT = {SCK_DIV_NUM_BITS {1'b1}};
	reg cfg_user_mode;
	reg [31:0] wdata_pipe;
	reg [SCK_DIV_NUM_BITS + 6:0] ack_delay;
	reg [SCK_DIV_NUM_BITS - 1:0] sck_counter;
	wire shift_out_mosi;
	wire shift_in_miso;
	reg o_spi_sck_en;
	wire bus_request;
	wire user_request;
	assign bus_request = ((i_wb_stb && !o_wb_stall) && !i_wb_we) && !cfg_user_mode;
	assign user_request = ((i_cfg_stb && !o_wb_stall) && i_wb_we) && !i_wb_data[8];
	initial o_spi_sck_en = 1'b0;
	always @(posedge i_clk)
		if (i_reset)
			o_spi_sck_en <= 1'b0;
		else if (bus_request || user_request)
			o_spi_sck_en <= 1'b1;
		else if (i_wb_cyc && (ack_delay > 2))
			o_spi_sck_en <= 1'b1;
		else
			o_spi_sck_en <= 1'b0;
	initial begin
		o_spi_sck = 0;
		sck_counter = 0;
	end
	always @(posedge i_clk)
		if (i_reset) begin
			o_spi_sck <= 1'b0;
			sck_counter <= 0;
		end
		else begin
			if (sck_counter == SCK_COUNTER_MIDPOINT)
				o_spi_sck <= o_spi_sck_en;
			else if (sck_counter == SCK_COUNTER_ENDPOINT)
				o_spi_sck <= 1'b0;
			if (bus_request || user_request)
				sck_counter <= 0;
			else
				sck_counter <= sck_counter + 1;
		end
	assign shift_out_mosi = o_spi_sck_en & (sck_counter == SCK_COUNTER_ENDPOINT);
	assign shift_in_miso = o_spi_sck_en & (sck_counter == SCK_COUNTER_MIDPOINT);
	initial ack_delay = 0;
	always @(posedge i_clk)
		if (i_reset || !i_wb_cyc)
			ack_delay <= 0;
		else if (bus_request)
			ack_delay <= {7'd64, {SCK_DIV_NUM_BITS {1'b0}}};
		else if (user_request)
			ack_delay <= {7'd8, {SCK_DIV_NUM_BITS {1'b0}}};
		else if (ack_delay != 0)
			ack_delay <= ack_delay - 1'b1;
	initial wdata_pipe = 0;
	always @(posedge i_clk)
		if (!o_wb_stall)
			wdata_pipe[23:0] <= {i_wb_addr[21:0], 2'b00};
		else if (shift_out_mosi)
			wdata_pipe[23:0] <= {wdata_pipe[22:0], 1'b0};
	always @(posedge i_clk)
		if (i_wb_stb && !o_wb_stall)
			wdata_pipe[31:24] <= 8'h03;
		else if (i_cfg_stb && !o_wb_stall)
			wdata_pipe[31:24] <= {i_wb_data[7:0]};
		else if (shift_out_mosi)
			wdata_pipe[31:24] <= {wdata_pipe[30:23]};
	always @(*) o_spi_mosi = wdata_pipe[31];
	initial o_wb_ack = 0;
	always @(posedge i_clk)
		if (i_reset)
			o_wb_ack <= 0;
		else if (ack_delay == 1)
			o_wb_ack <= i_wb_cyc;
		else if ((i_wb_stb && !o_wb_stall) && !bus_request)
			o_wb_ack <= 1'b1;
		else if ((i_cfg_stb && !o_wb_stall) && !user_request)
			o_wb_ack <= 1'b1;
		else
			o_wb_ack <= 0;
	initial cfg_user_mode = 0;
	always @(posedge i_clk)
		if (i_reset)
			cfg_user_mode <= 0;
		else if ((i_cfg_stb && !o_wb_stall) && i_wb_we)
			cfg_user_mode <= !i_wb_data[8];
	always @(posedge i_clk) begin
		if (shift_in_miso) begin
			if (cfg_user_mode)
				o_wb_data <= {24'h000010, o_wb_data[6:0], i_spi_miso};
			else
				o_wb_data <= {o_wb_data[30:24], i_spi_miso, o_wb_data[22:16], o_wb_data[31], o_wb_data[14:8], o_wb_data[23], o_wb_data[6:0], o_wb_data[15]};
		end
		if (cfg_user_mode)
			o_wb_data[31:8] <= 24'h000010;
	end
	initial o_spi_cs_n = 1'b1;
	always @(posedge i_clk)
		if (i_reset)
			o_spi_cs_n <= 1'b1;
		else if (!i_wb_cyc && !cfg_user_mode)
			o_spi_cs_n <= 1'b1;
		else if (bus_request)
			o_spi_cs_n <= 1'b0;
		else if ((i_cfg_stb && !o_wb_stall) && i_wb_we)
			o_spi_cs_n <= i_wb_data[8];
		else if (cfg_user_mode)
			o_spi_cs_n <= 1'b0;
		else if ((ack_delay == 1) && !cfg_user_mode)
			o_spi_cs_n <= 1'b1;
	initial o_wb_stall = 1'b0;
	always @(posedge i_clk)
		if (i_reset || !i_wb_cyc)
			o_wb_stall <= 1'b0;
		else if (bus_request || user_request)
			o_wb_stall <= 1'b1;
		else
			o_wb_stall <= ack_delay > 1;
	wire [22:0] unused;
	assign unused = i_wb_data[31:9];
endmodule
