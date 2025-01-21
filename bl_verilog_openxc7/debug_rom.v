`default_nettype none
module debug_rom (
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
	localparam [31:0] RomSize = 20;
	wire [1279:0] mem;
	assign mem = 1280'h7b2000737b2024737b30257310852823f1402473a79ff06f7b2024737b30257310052423001000737b2024737b30257310052c2300c5151300c5551300000517fd5ff06ffa0418e3002474134004440300a40433f140247302041c63001474134004440300a4043310852023f140247300c5151300c55513000005177b3510737b2410730ff0000f000000130500006f000000130840006f000000130180006f;
	wire [4:0] addr_d;
	reg [4:0] addr_q;
	assign addr_d = (req_i ? addr_i[7:3] : addr_q);
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			addr_q <= 1'sb0;
		else
			addr_q <= addr_d;
	function automatic [4:0] sv2v_cast_CA402;
		input reg [4:0] inp;
		sv2v_cast_CA402 = inp;
	endfunction
	always @(*) begin : p_outmux
		if (_sv2v_0)
			;
		rdata_o = 1'sb0;
		if (addr_q < sv2v_cast_CA402(RomSize))
			rdata_o = mem[addr_q * 64+:64];
	end
	initial _sv2v_0 = 0;
endmodule
