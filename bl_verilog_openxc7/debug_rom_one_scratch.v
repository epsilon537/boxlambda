`default_nettype none
module debug_rom_one_scratch (
	clk_i,
	rst_ni,
	req_i,
	addr_i,
	rdata_o
);
	reg _sv2v_0;
	input wire clk_i;
	input wire rst_ni;
	input wire req_i;
	input wire [63:0] addr_i;
	output reg [63:0] rdata_o;
	localparam [31:0] RomSize = 14;
	wire [895:0] mem;
	assign mem = 896'h7b2000737b20247310802823f1402473aa5ff06f7b20247310002423001000737b20247310002c23fddff06ffc0414e30024741340044403f140247302041263001474134004440310802023f14024737b2410730ff0000f000000130380006f000000130580006f000000130180006f;
	wire [3:0] addr_d;
	reg [3:0] addr_q;
	assign addr_d = (req_i ? addr_i[6:3] : addr_q);
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			addr_q <= 1'sb0;
		else
			addr_q <= addr_d;
	function automatic [3:0] sv2v_cast_31EC5;
		input reg [3:0] inp;
		sv2v_cast_31EC5 = inp;
	endfunction
	always @(*) begin : p_outmux
		if (_sv2v_0)
			;
		rdata_o = 1'sb0;
		if (addr_q < sv2v_cast_31EC5(RomSize))
			rdata_o = mem[addr_q * 64+:64];
	end
	initial _sv2v_0 = 0;
endmodule
