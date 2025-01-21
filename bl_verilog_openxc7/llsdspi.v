`default_nettype none
module llsdspi (
	i_clk,
	i_reset,
	i_speed,
	i_cs,
	i_stb,
	i_byte,
	o_cs_n,
	o_sclk,
	o_mosi,
	i_miso,
	o_stb,
	o_byte,
	o_idle,
	i_bus_grant
);
	parameter SPDBITS = 7;
	parameter STARTUP_CLOCKS = 150;
	parameter POWERUP_IDLE = 1000;
	parameter [0:0] OPT_SPI_ARBITRATION = 1'b0;
	localparam [0:0] CSN_ON_STARTUP = 1'b1;
	localparam [0:0] MOSI_INACTIVE_VALUE = 1'b1;
	parameter [0:0] OPT_CONTINUOUS_CLOCK = 1'b0;
	input wire i_clk;
	input wire i_reset;
	input wire [SPDBITS - 1:0] i_speed;
	input wire i_cs;
	input wire i_stb;
	input wire [7:0] i_byte;
	output reg o_cs_n;
	output reg o_sclk;
	output reg o_mosi;
	input wire i_miso;
	output reg o_stb;
	output reg [7:0] o_byte;
	output reg o_idle;
	input wire i_bus_grant;
	localparam [3:0] LLSDSPI_IDLE = 4'h0;
	localparam [3:0] LLSDSPI_HOTIDLE = 4'h1;
	localparam [3:0] LLSDSPI_WAIT = 4'h2;
	localparam [3:0] LLSDSPI_START = 4'h3;
	localparam [3:0] LLSDSPI_END = 4'hb;
	reg r_z_counter;
	reg [SPDBITS - 1:0] r_clk_counter;
	reg r_idle;
	reg [3:0] r_state;
	reg [7:0] r_byte;
	reg [7:0] r_ireg;
	wire byte_accepted;
	reg restart_counter;
	wire bus_grant;
	reg startup_hold;
	reg powerup_hold;
	assign bus_grant = (OPT_SPI_ARBITRATION ? i_bus_grant : 1'b1);
	generate
		if (POWERUP_IDLE > 0) begin : WAIT_FOR_POWERUP
			localparam POWERUP_BITS = $clog2(POWERUP_IDLE);
			reg [POWERUP_BITS - 1:0] powerup_counter;
			initial powerup_counter = POWERUP_IDLE[POWERUP_BITS - 1:0];
			initial powerup_hold = 1;
			always @(posedge i_clk)
				if (i_reset) begin
					powerup_counter <= POWERUP_IDLE;
					powerup_hold <= 1;
				end
				else if (powerup_hold) begin
					if (|powerup_counter)
						powerup_counter <= powerup_counter - 1;
					powerup_hold <= powerup_counter > 0;
				end
		end
		else begin : genblk1
			always @(*) powerup_hold = 0;
		end
		if (STARTUP_CLOCKS > 0) begin : WAIT_FOR_STARTUP
			localparam STARTUP_BITS = $clog2(STARTUP_CLOCKS);
			reg [STARTUP_BITS - 1:0] startup_counter;
			initial startup_counter = STARTUP_CLOCKS[STARTUP_BITS - 1:0];
			initial startup_hold = 1;
			always @(posedge i_clk)
				if (i_reset || powerup_hold) begin
					startup_counter <= STARTUP_CLOCKS;
					startup_hold <= 1;
				end
				else if ((startup_hold && r_z_counter) && !o_sclk) begin
					if (|startup_counter)
						startup_counter <= startup_counter - 1;
					startup_hold <= startup_counter > 0;
				end
		end
		else begin : genblk2
			always @(*) startup_hold = 0;
		end
	endgenerate
	assign byte_accepted = i_stb && o_idle;
	initial r_clk_counter = 0;
	initial r_z_counter = 1'b1;
	always @(*)
		if (OPT_CONTINUOUS_CLOCK || powerup_hold)
			restart_counter = !powerup_hold;
		else begin
			restart_counter = 1'b0;
			if (startup_hold || !i_cs)
				restart_counter = 1'b1;
			else if (!OPT_SPI_ARBITRATION && byte_accepted)
				restart_counter = 1'b1;
			else if (OPT_SPI_ARBITRATION && (r_state == LLSDSPI_IDLE))
				restart_counter = 1'b0;
			else if ((OPT_SPI_ARBITRATION && (r_state == LLSDSPI_WAIT)) && !bus_grant)
				restart_counter = 1'b0;
			else if (OPT_SPI_ARBITRATION && byte_accepted)
				restart_counter = 1'b1;
			else
				restart_counter = !r_idle;
		end
	always @(posedge i_clk)
		if (!r_z_counter) begin
			r_clk_counter <= r_clk_counter - 1;
			r_z_counter <= r_clk_counter == 1;
		end
		else if (restart_counter) begin
			r_clk_counter <= i_speed;
			r_z_counter <= i_speed == 0;
		end
	initial o_cs_n = CSN_ON_STARTUP;
	initial r_state = LLSDSPI_IDLE;
	always @(posedge i_clk)
		if (i_reset || (!CSN_ON_STARTUP && startup_hold)) begin
			o_cs_n <= CSN_ON_STARTUP;
			r_state <= LLSDSPI_IDLE;
		end
		else if (r_z_counter) begin
			if (!i_cs) begin
				r_state <= LLSDSPI_IDLE;
				o_cs_n <= 1'b1;
			end
			else if (r_state == LLSDSPI_IDLE) begin
				if (byte_accepted) begin
					o_cs_n <= 1'b0;
					if (OPT_SPI_ARBITRATION)
						r_state <= LLSDSPI_WAIT;
					else
						r_state <= LLSDSPI_START + (OPT_CONTINUOUS_CLOCK ? 1 : 0);
				end
			end
			else if (r_state == LLSDSPI_WAIT) begin
				if (bus_grant)
					r_state <= LLSDSPI_START;
			end
			else if (byte_accepted)
				r_state <= 4;
			else if (o_sclk && (r_state >= LLSDSPI_START)) begin
				r_state <= r_state + 1;
				if (r_state >= LLSDSPI_END)
					r_state <= LLSDSPI_HOTIDLE;
			end
			if (startup_hold)
				o_cs_n <= 1;
		end
	always @(posedge i_clk)
		if (r_z_counter && !o_sclk)
			r_ireg <= {r_ireg[6:0], i_miso};
	always @(posedge i_clk)
		if ((r_z_counter && o_sclk) && (r_state == LLSDSPI_END))
			o_byte <= r_ireg;
	initial r_idle = 0;
	always @(posedge i_clk)
		if (startup_hold || i_reset)
			r_idle <= 0;
		else if (r_z_counter) begin
			if (byte_accepted)
				r_idle <= 1'b0;
			else if ((r_state == LLSDSPI_END) || (r_state == LLSDSPI_HOTIDLE))
				r_idle <= 1'b1;
			else if (r_state == LLSDSPI_IDLE)
				r_idle <= 1'b1;
			else
				r_idle <= 1'b0;
		end
	initial o_sclk = 1;
	always @(posedge i_clk)
		if (i_reset)
			o_sclk <= 1;
		else if (r_z_counter) begin
			if (OPT_CONTINUOUS_CLOCK)
				o_sclk <= !o_sclk;
			else if (restart_counter && ((startup_hold || (i_cs && !o_cs_n)) || !o_sclk))
				o_sclk <= (r_state == LLSDSPI_WAIT) || !o_sclk;
		end
	initial r_byte = -1;
	initial o_mosi = MOSI_INACTIVE_VALUE;
	always @(posedge i_clk)
		if (i_reset) begin
			r_byte <= {8 {MOSI_INACTIVE_VALUE}};
			o_mosi <= MOSI_INACTIVE_VALUE;
		end
		else if (r_z_counter) begin
			if (byte_accepted) begin
				o_mosi <= MOSI_INACTIVE_VALUE;
				if (o_cs_n && !OPT_CONTINUOUS_CLOCK)
					r_byte <= i_byte[7:0];
				else begin
					r_byte <= {i_byte[6:0], MOSI_INACTIVE_VALUE};
					o_mosi <= i_byte[7];
				end
			end
			else if (o_sclk && (!OPT_SPI_ARBITRATION || (bus_grant && (r_state != LLSDSPI_WAIT)))) begin
				r_byte <= {r_byte[6:0], MOSI_INACTIVE_VALUE};
				if ((r_state >= LLSDSPI_START) && (r_state < LLSDSPI_END))
					o_mosi <= r_byte[7];
				else if (!i_cs)
					o_mosi <= MOSI_INACTIVE_VALUE;
			end
		end
	initial o_stb = 1'b0;
	always @(posedge i_clk)
		if ((((i_reset || startup_hold) || !i_cs) || !r_z_counter) || !o_sclk)
			o_stb <= 1'b0;
		else
			o_stb <= r_state >= LLSDSPI_END;
	always @(*)
		if (OPT_CONTINUOUS_CLOCK)
			o_idle = (r_idle && r_z_counter) && o_sclk;
		else
			o_idle = r_idle && r_z_counter;
endmodule
