module wb_dp_ram (
	a_clk,
	a_adr_i,
	a_dat_i,
	a_dat_o,
	a_we_i,
	a_sel_i,
	a_stb_i,
	a_ack_o,
	a_cyc_i,
	b_clk,
	b_adr_i,
	b_dat_i,
	b_dat_o,
	b_we_i,
	b_sel_i,
	b_stb_i,
	b_ack_o,
	b_cyc_i
);
	parameter DATA_WIDTH = 32;
	parameter ADDR_WIDTH = 16;
	parameter SELECT_WIDTH = DATA_WIDTH / 8;
	parameter INIT_FILE = "";
	input wire a_clk;
	input wire [ADDR_WIDTH - 1:0] a_adr_i;
	input wire [DATA_WIDTH - 1:0] a_dat_i;
	output wire [DATA_WIDTH - 1:0] a_dat_o;
	input wire a_we_i;
	input wire [SELECT_WIDTH - 1:0] a_sel_i;
	input wire a_stb_i;
	output wire a_ack_o;
	input wire a_cyc_i;
	input wire b_clk;
	input wire [ADDR_WIDTH - 1:0] b_adr_i;
	input wire [DATA_WIDTH - 1:0] b_dat_i;
	output wire [DATA_WIDTH - 1:0] b_dat_o;
	input wire b_we_i;
	input wire [SELECT_WIDTH - 1:0] b_sel_i;
	input wire b_stb_i;
	output wire b_ack_o;
	input wire b_cyc_i;
	parameter VALID_ADDR_WIDTH = ADDR_WIDTH - $clog2(SELECT_WIDTH);
	parameter WORD_WIDTH = SELECT_WIDTH;
	parameter WORD_SIZE = DATA_WIDTH / WORD_WIDTH;
	reg [DATA_WIDTH - 1:0] a_dat_o_reg = {DATA_WIDTH {1'b0}};
	reg a_ack_o_reg = 1'b0;
	reg [DATA_WIDTH - 1:0] b_dat_o_reg = {DATA_WIDTH {1'b0}};
	reg b_ack_o_reg = 1'b0;
	reg [DATA_WIDTH - 1:0] mem [(2 ** VALID_ADDR_WIDTH) - 1:0];
	function automatic [VALID_ADDR_WIDTH - 1:0] sv2v_cast_CCF26;
		input reg [VALID_ADDR_WIDTH - 1:0] inp;
		sv2v_cast_CCF26 = inp;
	endfunction
	wire [VALID_ADDR_WIDTH - 1:0] a_adr_i_valid = sv2v_cast_CCF26(a_adr_i >> (ADDR_WIDTH - VALID_ADDR_WIDTH));
	wire [VALID_ADDR_WIDTH - 1:0] b_adr_i_valid = sv2v_cast_CCF26(b_adr_i >> (ADDR_WIDTH - VALID_ADDR_WIDTH));
	assign a_dat_o = a_dat_o_reg;
	assign a_ack_o = a_ack_o_reg;
	assign b_dat_o = b_dat_o_reg;
	assign b_ack_o = b_ack_o_reg;
	integer i;
	integer j;
	initial if (INIT_FILE != "")
		$readmemh(INIT_FILE, mem);
	always @(posedge a_clk) begin
		a_ack_o_reg <= 1'b0;
		for (i = 0; i < WORD_WIDTH; i = i + 1)
			if ((a_cyc_i & a_stb_i) & ~a_ack_o) begin
				if (a_we_i & a_sel_i[i])
					mem[a_adr_i_valid][WORD_SIZE * i+:WORD_SIZE] <= a_dat_i[WORD_SIZE * i+:WORD_SIZE];
				a_dat_o_reg[WORD_SIZE * i+:WORD_SIZE] <= mem[a_adr_i_valid][WORD_SIZE * i+:WORD_SIZE];
				a_ack_o_reg <= 1'b1;
			end
	end
	always @(posedge b_clk) begin
		b_ack_o_reg <= 1'b0;
		for (i = 0; i < WORD_WIDTH; i = i + 1)
			if ((b_cyc_i & b_stb_i) & ~b_ack_o) begin
				if (b_we_i & b_sel_i[i])
					mem[b_adr_i_valid][WORD_SIZE * i+:WORD_SIZE] <= b_dat_i[WORD_SIZE * i+:WORD_SIZE];
				b_dat_o_reg[WORD_SIZE * i+:WORD_SIZE] <= mem[b_adr_i_valid][WORD_SIZE * i+:WORD_SIZE];
				b_ack_o_reg <= 1'b1;
			end
	end
endmodule
