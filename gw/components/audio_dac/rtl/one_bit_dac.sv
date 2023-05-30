/* Second order 1-bit delta-sigma DAC.*/
module one_bit_dac #(
	parameter W=16
	) (
    input wire clk,                 //e.g. 50MHz
    input wire clk_en,             //e.g. 12.5 MHz
    input wire signed [W-1:0] in,  // input q0.15
    output wire out         // one bit out modulated at clk_en rate
    );
`ifdef SECOND_ORDER_DAC
  reg signed [2:-W] acc1_r;  // first integrator, q2.16 (W=16)
  reg signed [4:-W] acc2_r;  // second integrator, q4.16 (W=16)
  
  initial begin
    acc1_r = 0;
    acc2_r = 0;
  end

  reg signed [4:0] feedback = out ? -1 : 1; //q4.0
  
  //Stage 1: q2.16 (W=16)
  wire signed [2:-W] x1 = {{3{in[W-1]}}, in};  // 16 bit q0.15 input -> q2.16 [-0.5 0.5] (W=16)
  wire signed [2:-W] s1 = {x1[2:0] + feedback[2:0], x1[-1:-W]};
  wire signed [2:-W] acc1 = acc1_r + s1;
  
  //Stage 2: q4.16 (W=16)
  wire signed [4:-W] x2 = {{2{acc1[2]}}, acc1};
  wire signed [4:-W] s2 = {x2[4:0] + feedback[4:0], x2[-1:-W]};
  wire signed [4:-W] acc2 = acc2_r + s2;
  
  always @(posedge clk) begin
    if (clk_en) begin
      acc1_r <= acc1; 
      acc2_r <= acc2; 
    end
  end
  
  assign out = acc2_r[4];
`endif
`ifdef FIRST_ORDER_DAC
  //This code is taken from the book 'FPGA prototyping by Systemverilog Examples' by Chu, Pong P.

  localparam BIAS = 2**(W-1);  //{1'b1, (W-2){1'b0}};
  logic [W:0] pcm_biased;
  logic [W:0] acc_next;
  logic [W:0] acc_reg;
  
  // shift the range from [-2^(W-1)-1, 2^(W-1)-1] to [0, 2^W-1)] 
  assign pcm_biased = {in[W - 1], in} + BIAS;
  // signal treated as unsgined number in delta-sigma modulation
  assign acc_next = {1'b0, acc_reg[W-1:0]} + pcm_biased;

  initial
    acc_reg = 0;

  // accumulation register
  always_ff @(posedge clk)
    if (clk_en)
      acc_reg <= acc_next;

  // output
  assign out = acc_reg[W];
`endif
endmodule
