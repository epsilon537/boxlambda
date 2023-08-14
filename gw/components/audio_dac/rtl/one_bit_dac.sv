/* 1-bit delta-sigma DAC.*/
module one_bit_dac #(
	parameter W=16
	) (
    input wire clk,                //e.g. 50MHz
    input wire clk_en,             //e.g. 12.5 MHz
    input wire signed [W-1:0] in,  // input q0.15
    output wire out                // one bit out modulated at clk_en rate
`ifdef VERILATOR
    ,output wire acc1_overflow    // in simulation we check for accumulator overflows
    ,output wire acc2_overflow
`endif
    );
`ifdef SECOND_ORDER_DAC
  //This was originally q2.16 and q4.16, but overflows were detected.
  //With q3.16 and q5.16 I no longer see any overflows. I'm sure this can also be established
  //through analysis rather than testing.
  reg signed [3:-W] acc1_r;  // first integrator, q3.16 (W=16)
  reg signed [5:-W] acc2_r;  // second integrator, q5.16 (W=16)
  
  initial begin
    acc1_r = 0;
    acc2_r = 0;
  end

  reg signed [5:0] feedback = out ? -1 : 1; //q5.0
  
  //Stage 1: q3.16 (W=16)
  wire signed [3:-W] x1 = {{4{in[W-1]}}, in};  // 16 bit q0.15 input -> q4.16 [-0.5 0.5] (W=16)
  wire signed [3:-W] s1 = {x1[3:0] + feedback[3:0], x1[-1:-W]};
  wire signed [3:-W] acc1 = acc1_r + s1;
`ifdef VERILATOR
  //Overflow detector: if both terms have same sign and result has opposite sign, we have an overflow.
  logic acc1_overflow_i;
  always_comb
    if( (acc1_r > 0 && s1 > 0 && acc1 < 0) 
        || 
        (acc1_r < 0 && s1 < 0 && acc1 > 0))
          acc1_overflow_i = 1'b1;
        else
          acc1_overflow_i = 1'b0;
  
  assign acc1_overflow = acc1_overflow_i;
 `endif

  //Stage 2: q5.16 (W=16)
  wire signed [5:-W] x2 = {{2{acc1[3]}}, acc1};
  wire signed [5:-W] s2 = {x2[5:0] + feedback[5:0], x2[-1:-W]};
  wire signed [5:-W] acc2 = acc2_r + s2;
 `ifdef VERILATOR
  //Overflow detector: if both terms have same sign and result has opposite sign, we have an overflow.
  logic acc2_overflow_i;
  always_comb
    if( (acc2_r > 0 && s2 > 0 && acc2 < 0) 
        || 
        (acc2_r < 0 && s2 < 0 && acc2 > 0))
          acc2_overflow_i = 1'b1;
        else
          acc2_overflow_i = 1'b0;
  
  assign acc2_overflow = acc2_overflow_i;
 `endif 

  always @(posedge clk) begin
    if (clk_en) begin
      acc1_r <= acc1; 
      acc2_r <= acc2; 
    end
  end
  
  assign out = acc2_r[4];

`else //FIRST_ORDER_DAC implementation:

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
