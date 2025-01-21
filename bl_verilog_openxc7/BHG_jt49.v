`default_nettype wire
module BHG_jt49 (
	rst_n,
	clk,
	clk_en,
	addr,
	cs_n,
	wr_n,
	din,
	sel,
	dout,
	sound,
	A,
	B,
	C,
	sample,
	IOA_in,
	IOA_out,
	IOB_in,
	IOB_out
);
	parameter DAC_BITS = 8;
	input wire rst_n;
	input wire clk;
	input wire clk_en;
	input wire [3:0] addr;
	input wire cs_n;
	input wire wr_n;
	input wire [7:0] din;
	input wire sel;
	output reg [7:0] dout;
	output reg [DAC_BITS + 1:0] sound;
	output reg [DAC_BITS - 1:0] A;
	output reg [DAC_BITS - 1:0] B;
	output reg [DAC_BITS - 1:0] C;
	output wire sample;
	input wire [7:0] IOA_in;
	output wire [7:0] IOA_out;
	input wire [7:0] IOB_in;
	output wire [7:0] IOB_out;
	localparam CLKDIV = 3;
	reg [7:0] regarray [15:0];
	wire [7:0] port_A;
	wire [7:0] port_B;
	wire [4:0] envelope;
	wire bitA;
	wire bitB;
	wire bitC;
	wire noise;
	reg Amix = 0;
	reg Bmix = 0;
	reg Cmix = 0;
	wire cen16;
	wire cen256;
	assign IOA_out = regarray[14];
	assign IOB_out = regarray[15];
	assign port_A = (!regarray[7][6] ? IOA_in : IOA_out);
	assign port_B = (!regarray[7][7] ? IOB_in : IOB_out);
	assign sample = cen16;
	jt49_cen #(.CLKDIV(CLKDIV)) u_cen(
		.clk(clk),
		.rst_n(rst_n),
		.cen(clk_en),
		.sel(sel),
		.cen16(cen16),
		.cen256(cen256)
	);
	jt49_div #(.W(12)) u_chA(
		.clk(clk),
		.rst_n(rst_n),
		.cen(cen16),
		.period({regarray[1][3:0], regarray[0][7:0]}),
		.div(bitA)
	);
	jt49_div #(.W(12)) u_chB(
		.clk(clk),
		.rst_n(rst_n),
		.cen(cen16),
		.period({regarray[3][3:0], regarray[2][7:0]}),
		.div(bitB)
	);
	jt49_div #(.W(12)) u_chC(
		.clk(clk),
		.rst_n(rst_n),
		.cen(cen16),
		.period({regarray[5][3:0], regarray[4][7:0]}),
		.div(bitC)
	);
	jt49_noise u_ng(
		.clk(clk),
		.cen(cen16),
		.rst_n(rst_n),
		.period(regarray[6][4:0]),
		.noise(noise)
	);
	wire eg_step;
	wire [15:0] eg_period = {regarray[4'hc], regarray[4'hb]};
	wire null_period = eg_period == 16'h0000;
	jt49_div #(.W(16)) u_envdiv(
		.clk(clk),
		.cen(cen256),
		.rst_n(rst_n),
		.period(eg_period),
		.div(eg_step)
	);
	reg eg_restart;
	jt49_eg u_env(
		.clk(clk),
		.cen(cen256),
		.step(eg_step),
		.rst_n(rst_n),
		.restart(eg_restart),
		.null_period(null_period),
		.ctrl(regarray[4'hd][3:0]),
		.env(envelope)
	);
	reg [4:0] logA;
	reg [4:0] logB;
	reg [4:0] logC;
	reg [4:0] log;
	initial begin
		logA = 5'd0;
		logB = 5'd0;
		logC = 5'd0;
		log = 5'd0;
	end
	wire [DAC_BITS - 1:0] lin;
	BHG_jt49_exp #(.DAC_BITS(DAC_BITS)) u_exp(
		.clk(clk),
		.din(log),
		.dout(lin)
	);
	wire [4:0] volA = {regarray[8][3:0], regarray[8][3]};
	wire [4:0] volB = {regarray[9][3:0], regarray[9][3]};
	wire [4:0] volC = {regarray[10][3:0], regarray[10][3]};
	wire use_envA = regarray[8][4];
	wire use_envB = regarray[9][4];
	wire use_envC = regarray[10][4];
	wire use_noA = regarray[7][3];
	wire use_noB = regarray[7][4];
	wire use_noC = regarray[7][5];
	reg [3:0] acc_st;
	always @(posedge clk)
		if (clk_en) begin
			Amix <= (noise | use_noA) & (bitA | regarray[7][0]);
			Bmix <= (noise | use_noB) & (bitB | regarray[7][1]);
			Cmix <= (noise | use_noC) & (bitC | regarray[7][2]);
			logA <= (!Amix ? 5'd0 : (use_envA ? envelope : volA));
			logB <= (!Bmix ? 5'd0 : (use_envB ? envelope : volB));
			logC <= (!Cmix ? 5'd0 : (use_envC ? envelope : volC));
		end
	reg [DAC_BITS + 1:0] acc;
	always @(posedge clk or negedge rst_n)
		if (!rst_n) begin
			acc_st <= 4'b0001;
			acc <= {DAC_BITS + 2 {1'b0}};
			A <= {DAC_BITS {1'b0}};
			B <= {DAC_BITS {1'b0}};
			C <= {DAC_BITS {1'b0}};
			sound <= {DAC_BITS + 2 {1'b0}};
			log <= 5'd0;
		end
		else if (clk_en) begin
			acc_st <= {acc_st[2:0], acc_st[3]};
			acc <= acc + {2'b00, lin};
			case (acc_st)
				4'b0001: begin
					log <= logA;
					acc <= {DAC_BITS + 2 {1'b0}};
					sound <= acc;
				end
				4'b0010: begin
					A <= lin;
					log <= logB;
				end
				4'b0100: begin
					B <= lin;
					log <= logC;
				end
				4'b1000: C <= lin;
				default:
					;
			endcase
		end
	reg [7:0] read_mask;
	always @(*)
		case (addr)
			4'h0, 4'h2, 4'h4, 4'h7, 4'hb, 4'hc, 4'he, 4'hf: read_mask = 8'hff;
			4'h1, 4'h3, 4'h5, 4'hd: read_mask = 8'h0f;
			4'h6, 4'h8, 4'h9, 4'ha: read_mask = 8'h1f;
		endcase
	wire write;
	reg last_write;
	wire wr_edge = write & ~last_write;
	assign write = !wr_n && !cs_n;
	always @(posedge clk or negedge rst_n)
		if (!rst_n) begin
			dout <= 8'd0;
			last_write <= 0;
			eg_restart <= 0;
			regarray[0] <= 8'd0;
			regarray[4] <= 8'd0;
			regarray[8] <= 8'd0;
			regarray[12] <= 8'd0;
			regarray[1] <= 8'd0;
			regarray[5] <= 8'd0;
			regarray[9] <= 8'd0;
			regarray[13] <= 8'd0;
			regarray[2] <= 8'd0;
			regarray[6] <= 8'd0;
			regarray[10] <= 8'd0;
			regarray[14] <= 8'd0;
			regarray[3] <= 8'd0;
			regarray[7] <= 8'd0;
			regarray[11] <= 8'd0;
			regarray[15] <= 8'd0;
		end
		else begin
			last_write <= write;
			case (addr)
				4'he: dout <= port_A;
				4'hf: dout <= port_B;
				default: dout <= regarray[addr] & read_mask;
			endcase
			if (write) begin
				regarray[addr] <= din;
				if ((addr == 4'hd) && wr_edge)
					eg_restart <= 1;
			end
			else
				eg_restart <= 0;
		end
endmodule
