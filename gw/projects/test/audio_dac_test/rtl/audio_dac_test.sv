`default_nettype none

//A simple build to test the 1-bit audio DAC.
module audio_dac_test (
  input  wire       ext_clk, /*External clock: 100MHz on FPGA, 50MHz in simulation.*/
  input  wire       ext_rst_n, /*External reset, asynchronous, active-low.*/
  input  wire [3:0] sw, /*Switches, for rudimentary volume control.*/
  output wire [3:0] leds, /*Toggling leds, to indicate liveness.*/
  output wire       audio_out, /*1-bit audio output*/
  output wire       audio_gain, /*PmodAMP2 control signal. When high a 6dB gain is applied, when low a 12dB gain is applied.*/
  output wire       audio_shutdown_n /*PmodAMP2 control signal to put the amplifier in shutdown*/
`ifdef VERILATOR
  ,output wire [15:0] pcm_out /*16-bit audio output, to cross check against 1-bit output in simulation.*/
  ,output wire acc1_overflow /*In simulation we check for accumulator overflows.*/
  ,output wire acc2_overflow
`endif
);

//Advance sine ROM look-up phase angle once every PHASE_ADV_PERIOD clock ticks.
//The frequency of the generated sine wave is 50MHz/(256*PHASE_ADV_PERIOD).
localparam [8:0] PHASE_ADV_PERIOD = 9'd444;

//Toggle LED once every TOGGLE_LED_PERIOD clock ticks.
localparam [31:0] TOGGLE_LED_PERIOD = 32'd10000000;

//sys_clk is a 50MHz clock.
logic sys_clk, sys_rst_n;

logic signed [15:0] audio_pcm;
logic [7:0] phase_r; //Phase angle for sine ROM look-up.
logic [8:0] phase_adv_div_ctr; //Phase advance divider counter.
logic [3:0] led_state_r;
logic [31:0] toggle_led_div_ctr; //Toggle LED divider counter.
logic [1:0] div_by_4_ctr; //Divide-clock-by-4 divider counter.
logic signed [15:0] sin_rom_out; //Sine ROM output.

logic unused = &{1'b0, sys_rst_n, 1'b0};

initial begin
    phase_r = 8'h0;
    phase_adv_div_ctr = 9'h0;
    toggle_led_div_ctr = 0;
    led_state_r = 0;
    div_by_4_ctr = 2'b00;
end

assign audio_gain = sw[0]; //Switch 0 sets gain.
assign audio_shutdown_n = sw[1]; //Switch 1 controls shutdown.
assign leds = led_state_r;

logic pll_locked;

boxlambda_clk_gen clkgen (
    .ext_clk_100(ext_clk),  //100MHz external clock input.
    .rst_n(ext_rst_n),
    .clk_50(sys_clk),  //50MHz clock output
    .clk_100(),  //100MHz clock output
    .clk_12(),  //12 MHz USB clock output
    .locked(pll_locked) //PLL lock indication outpt.
);

assign sys_rst_n = ~pll_locked;

//256 entry sine ROM look-up table.
sin_rom sin_rom_inst
   (.clk(sys_clk),
    .addr_r(phase_r),
    .dout(sin_rom_out)
   );

//Uses switches 2 and 3 to scale down sine ROM output -> crude volume control.
always_comb begin
  case (sw[3:2])
    2'b00: audio_pcm = sin_rom_out;
    2'b01: audio_pcm = {sin_rom_out[15], sin_rom_out[15:1]};
    2'b10: audio_pcm = {{2{sin_rom_out[15]}}, sin_rom_out[15:2]};
    2'b11: audio_pcm = {{3{sin_rom_out[15]}}, sin_rom_out[15:3]};
  endcase
end

`ifdef VERILATOR
assign pcm_out = audio_pcm;
`endif

one_bit_dac dac_inst (
  .clk(sys_clk),      // 50MHz clock
  .clk_en(div_by_4_ctr==2'b00),   // 12.5MHz clock enable
  .in(audio_pcm),     // 16-bit audio PCM input
  .out(audio_out)     // one bit output, modulated at 12.5MHz
`ifdef VERILATOR
  ,.acc1_overflow(acc1_overflow) //On simulation, check the DAC accumulators for overflows.
  ,.acc2_overflow(acc2_overflow) //On simulation, check the DAC accumulators for overflows.
`endif
  );

always_ff @(posedge sys_clk)
begin
    div_by_4_ctr <= div_by_4_ctr + 1;

    if (toggle_led_div_ctr == TOGGLE_LED_PERIOD) begin
      toggle_led_div_ctr <= 0;
      led_state_r <= led_state_r + 4'd1;
    end
    else begin
      toggle_led_div_ctr <= toggle_led_div_ctr + 1;
    end

    if (phase_adv_div_ctr == PHASE_ADV_PERIOD) begin
        phase_adv_div_ctr <= 9'd0;
        phase_r <= phase_r + 8'h01;
    end
    else begin
        phase_adv_div_ctr <= phase_adv_div_ctr + 9'd01;
    end
end

endmodule

`resetall
