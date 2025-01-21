module syncpls (
	t_clk,
	t_rst_n,
	t_pulse,
	r_clk,
	r_rst_n,
	r_pulse
);
	input wire t_clk;
	input wire t_rst_n;
	input wire t_pulse;
	input wire r_clk;
	input wire r_rst_n;
	output wire r_pulse;
	wire t_tgl;
	wire r_tgl;
	pls2tgl pls2tgl_inst(
		.tgl(t_tgl),
		.pulse(t_pulse),
		.clk(t_clk),
		.rst_n(t_rst_n)
	);
	sync3 sync3_inst(
		.q(r_tgl),
		.d(t_tgl),
		.clk(r_clk),
		.rst_n(r_rst_n)
	);
	tgl2pls tgl2pls_inst(
		.pulse(r_pulse),
		.q(),
		.d(r_tgl),
		.clk(r_clk),
		.rst_n(r_rst_n)
	);
endmodule
