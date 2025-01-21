module boxlambda_clk_gen (
	ext_clk_100,
	rst_n,
	clk_50,
	clk_100,
	clk_12,
	locked
);
	input wire ext_clk_100;
	input wire rst_n;
	output wire clk_50;
	output wire clk_100;
	output wire clk_12;
	output wire locked;
	wire locked_pll;
	wire io_clk_buf;
	wire clk_100_buf;
	wire clk_100_unbuf;
	wire clk_50_buf;
	wire clk_50_unbuf;
	wire clk_fb_buf;
	wire clk_fb_unbuf;
	wire clk_12_unbuf;
	wire clk_12_buf;
	IBUF io_clk_ibuf(
		.I(ext_clk_100),
		.O(io_clk_buf)
	);
	PLLE2_ADV #(
		.BANDWIDTH("OPTIMIZED"),
		.COMPENSATION("ZHOLD"),
		.STARTUP_WAIT("FALSE"),
		.DIVCLK_DIVIDE(1),
		.CLKFBOUT_MULT(12),
		.CLKFBOUT_PHASE(0.000),
		.CLKOUT0_DIVIDE(24),
		.CLKOUT0_PHASE(0.000),
		.CLKOUT0_DUTY_CYCLE(0.500),
		.CLKOUT1_DIVIDE(12),
		.CLKOUT1_PHASE(0.000),
		.CLKOUT1_DUTY_CYCLE(0.500),
		.CLKOUT2_DIVIDE(100),
		.CLKOUT2_PHASE(0.000),
		.CLKOUT2_DUTY_CYCLE(0.500),
		.CLKIN1_PERIOD(10)
	) pll(
		.CLKFBOUT(clk_fb_unbuf),
		.CLKOUT0(clk_50_unbuf),
		.CLKOUT1(clk_100_unbuf),
		.CLKOUT2(clk_12_unbuf),
		.CLKOUT3(),
		.CLKOUT4(),
		.CLKOUT5(),
		.CLKFBIN(clk_fb_buf),
		.CLKIN1(io_clk_buf),
		.CLKIN2(1'b0),
		.CLKINSEL(1'b1),
		.DADDR(7'h00),
		.DCLK(1'b0),
		.DEN(1'b0),
		.DI(16'h0000),
		.DO(),
		.DRDY(),
		.DWE(1'b0),
		.LOCKED(locked_pll),
		.PWRDWN(1'b0),
		.RST(1'b0)
	);
	BUFG clk_fb_bufg(
		.I(clk_fb_unbuf),
		.O(clk_fb_buf)
	);
	BUFG clk_50_bufg(
		.I(clk_50_unbuf),
		.O(clk_50_buf)
	);
	BUFG clk_100_bufg(
		.I(clk_100_unbuf),
		.O(clk_100_buf)
	);
	BUFG clk_12_bufg(
		.I(clk_12_unbuf),
		.O(clk_12_buf)
	);
	assign clk_50 = clk_50_buf;
	assign clk_100 = clk_100_buf;
	assign clk_12 = clk_12_buf;
	assign locked = locked_pll & rst_n;
endmodule
