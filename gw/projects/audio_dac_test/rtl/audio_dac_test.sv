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
`endif
);

//sys_clk is a 50MHz clock.  
logic sys_clk, sys_rst_n;

logic signed [15:0] audio_pcm;
logic [7:0] phase_r;
logic [8:0] sr_divider_r;
logic [3:0] gpio0_r;
logic [31:0] gpio_divider_r;
logic unused = &{1'b0, sys_rst_n, sw[3], 1'b0};

initial begin
    phase_r = 8'h0;
    sr_divider_r = 9'h0;
    gpio_divider_r = 0;
    gpio0_r = 0;
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

assign audio_pcm = phase_r > 127 ? (sw[2] ? 32767 : 16383) : (sw[2] ? -32767 : -16383); //16384 : -16384; //half scale amplitude square wave
`ifdef VERILATOR
assign pcm_out = audio_pcm;
`endif
reg [1:0] cpt4;

one_bit_dac dac_inst (
  .clk(sys_clk),      // 50MHz clock
  .clk_en(cpt4==0),   // 12.5MHz clock enable
  .in(audio_pcm),     // input
  .out(audio_out)     // one bit out modulated at 12.5MHz
  );

always_ff @(posedge sys_clk)
begin
    cpt4 <= cpt4 - 1;
    gpio_divider_r <= gpio_divider_r - 1;

    if (gpio_divider_r == 0) begin
      gpio_divider_r <= 32'd10000000;
      gpio0_r <= gpio0_r + 4'd1;
    end

    if (sr_divider_r == 0) begin
        sr_divider_r <= 9'd111;
        phase_r <= phase_r + 8'h01;
    end
    else begin
        sr_divider_r <= sr_divider_r - 9'h01;
    end
end

endmodule

`resetall
