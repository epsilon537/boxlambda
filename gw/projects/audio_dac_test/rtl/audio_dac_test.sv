`default_nettype none

module audio_dac_test (
  input  wire       ext_clk, /*External clock: 100MHz on FPGA, 50MHz in simulation.*/
  input  wire       ext_rst_n, /*External reset, asynchronous, active-low.*/
  input  wire [3:0] sw,
  output wire [3:0] gpio0,
  output wire       audio_out,
  output wire       audio_gain,
  output wire       audio_shutdown_n
`ifdef VERILATOR
  ,output wire [15:0] pcm_out
  ,output wire acc1_overflow /*In simulation we check for accumulator overflows*/
  ,output wire acc2_overflow
`endif
);

//sys_clk is a 50MHz clock.  
logic sys_clk, sys_rst_n;

logic signed [15:0] audio_pcm;
logic [7:0] phase_r;
logic [8:0] sr_divider_r;
logic [3:0] gpio0_r;
logic [31:0] gpio_divider_r;
logic [1:0] cpt4_r;
logic signed [15:0] sin_rom_out;

logic unused = &{1'b0, sys_rst_n, 1'b0};

initial begin
    phase_r = 8'h0;
    sr_divider_r = 9'h0;
    gpio_divider_r = 0;
    gpio0_r = 0;
    cpt4_r = 2'b00;
end

assign audio_gain = sw[0];
assign audio_shutdown_n = sw[1];
assign gpio0 = gpio0_r;

`ifdef SYNTHESIS
  //This clkgen does a divide-by-2 of the 100MHz ext_clk => sys_clk runs at 50MHz.
  clkgen_xil7series clkgen (
    .IO_CLK     (ext_clk),
    .IO_RST_N   (ext_rst_n),
    .clk_sys    (sys_clk),
    .rst_sys_n  (sys_rst_n));
`else
  //In simulation ext_clk runs at 50MHz and sys_clk = ext_clk;
  assign sys_clk = ext_clk;
  assign sys_rst_n = ext_rst_n;
`endif //SYNTHESIS/No SYNTHESIS

sin_rom sin_rom_inst
   (.clk(sys_clk),
    .addr_r(phase_r),
    .dout(sin_rom_out)
   );

always_comb begin
  case (sw[3:2])
    2'b00: audio_pcm = sin_rom_out;
    2'b01: audio_pcm = {sin_rom_out[15], sin_rom_out[15:1]};
    2'b10: audio_pcm = {{2{sin_rom_out[15]}}, sin_rom_out[15:2]};
    2'b11: audio_pcm = {{3{sin_rom_out[15]}}, sin_rom_out[15:3]};
  endcase
end
//assign audio_pcm = phase_r > 127 ? (sw[2] ? 32767 : 16383) : (sw[2] ? -32767 : -16383); //16384 : -16384; //half scale amplitude square wave
`ifdef VERILATOR
assign pcm_out = audio_pcm;
`endif

one_bit_dac dac_inst (
  .clk(sys_clk),      // 50MHz clock
  .clk_en(cpt4_r==2'b00),   // 12.5MHz clock enable
  .in(audio_pcm),     // input
  .out(audio_out)     // one bit out modulated at 12.5MHz
`ifdef VERILATOR
  ,.acc1_overflow(acc1_overflow)
  ,.acc2_overflow(acc2_overflow)
`endif
  );

always_ff @(posedge sys_clk)
begin
    cpt4_r <= cpt4_r + 1;
    
    if (gpio_divider_r == 32'd10000000) begin
      gpio_divider_r <= 0;
      gpio0_r <= gpio0_r + 4'd1;
    end
    else begin
      gpio_divider_r <= gpio_divider_r + 1;
    end

    if (sr_divider_r == 9'd444) begin
        sr_divider_r <= 9'd0;
        phase_r <= phase_r + 8'h01;
    end
    else begin
        sr_divider_r <= sr_divider_r + 9'd01;
    end
end

endmodule

`resetall
