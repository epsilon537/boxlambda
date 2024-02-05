//
//  A simulator top-level wrapping around ibex_soc, instantiating SimJTAG.
//
module sim_main #(
	parameter int unsigned OPENOCD_PORT = 9999
	) (
    input logic      clk_100,
	input logic      clk_50,
	input logic      clk_6,
    input logic      rst_ni,
	// usb_device leds
	output wire  [7:0] ledg_0,
	output wire  [9:0] ledr_0,
	output wire  [7:0] ledg_1,
	output wire  [9:0] ledr_1,
	// // VGA interface
	output wire  [3:0] vga_r,       
	output wire  [3:0] vga_g,       
	output wire  [3:0] vga_b,       
	output wire        vga_hsync,   
	output wire        vga_vsync,
	// SDSPI interface
  	output wire  sdspi_cs_n, 
  	output wire  sdspi_sck, 
  	output wire  sdspi_mosi,
	input  wire	 sdspi_miso, 
  	input  wire  sdspi_card_detect,
	// Audio interface
	output wire       audio_out,
	output wire       audio_gain,
	output wire       audio_shutdown_n,
	output wire [15:0] pcm_out,
	output wire acc1_overflow,
    output wire acc2_overflow,
	input wire 	     uart_rx,
    output wire      uart_tx,  
	inout wire [7:0] gpio0,
    inout wire [3:0] gpio1
	// The USB interface is not added here. The USB HID host signals are connected directly to a USB HID device module below.
    );
   
   reg usb0_dm_host_i; 
   reg usb0_dp_host_i;
   
   reg usb0_dm_device_i; 
   reg usb0_dp_device_i;
   
   reg usb1_dm_host_i; 
   reg usb1_dp_host_i;
   
   reg usb1_dm_device_i; 
   reg usb1_dp_device_i;
   
   reg usb0_dm_host_o; 
   reg usb0_dp_host_o;
   reg usb0_oe_host;

   reg usb0_dm_device_o; 
   reg usb0_dp_device_o;
   reg usb0_oe_device;
   
   reg usb1_dm_host_o; 
   reg usb1_dp_host_o;
   reg usb1_oe_host;

   reg usb1_dm_device_o; 
   reg usb1_dp_device_o;
   reg usb1_oe_device;

   // jtag openocd bridge signals
   logic 	     sim_jtag_tck;
   logic 	     sim_jtag_tms;
   logic 	     sim_jtag_tdi;
   logic 	     sim_jtag_trstn;
   logic 	     sim_jtag_tdo;
   logic [31:0]  sim_jtag_exit;
   
   SimJTAG  #(
              .TICK_DELAY (1),
              .PORT(OPENOCD_PORT)
	      ) simJTAG_inst(
			     .clock                ( clk_100              ),
			     .reset                ( ~rst_ni              ),
			     .enable               ( 1'b1                 ),
			     .init_done            ( rst_ni               ),
			     .jtag_TCK             ( sim_jtag_tck         ),
			     .jtag_TMS             ( sim_jtag_tms         ),
			     .jtag_TDI             ( sim_jtag_tdi         ),
			     .jtag_TRSTn           ( sim_jtag_trstn       ),
			     .jtag_TDO_data        ( sim_jtag_tdo         ),
			     .jtag_TDO_driven      ( 1'b1                 ),
			     .exit                 ( sim_jtag_exit        )
			     );

   //Emulate USB low-speed pull-ups and pull-downs for USB port 0.
   always_comb begin
	 case ({usb0_oe_host, usb0_oe_device})
	 2'b00: begin
	  	usb0_dp_host_i = 1'b0;
	  	usb0_dm_host_i = 1'b1;
	  	usb0_dp_device_i = 1'b0;
	  	usb0_dm_device_i = 1'b1;
	 end
	 2'b01: begin
		usb0_dp_host_i = usb0_dp_device_o;
	  	usb0_dm_host_i = usb0_dm_device_o;
		usb0_dp_device_i = 1'b0;
	  	usb0_dm_device_i = 1'b1;
	 end
	 2'b10: begin
		usb0_dp_host_i = 1'b0;
	  	usb0_dm_host_i = 1'b1;
		usb0_dp_device_i = usb0_dp_host_o;
	  	usb0_dm_device_i = usb0_dm_host_o;
	 end
	 2'b11: begin
	 	$display("usb0 output enable conflict");
		usb0_dp_host_i = usb0_dp_device_o;
	  	usb0_dm_host_i = usb0_dm_device_o;
		usb0_dp_device_i = usb0_dp_host_o;
	  	usb0_dm_device_i = usb0_dm_host_o;
	 end
	 endcase
   end

   //Emulate USB low-speed pull-ups and pull-downs for USB port 1.
   always_comb begin
	 case ({usb1_oe_host, usb1_oe_device})
	 2'b00: begin
	  	usb1_dp_host_i = 1'b0;
	  	usb1_dm_host_i = 1'b1;
	  	usb1_dp_device_i = 1'b0;
	  	usb1_dm_device_i = 1'b1;
	 end
	 2'b01: begin
		usb1_dp_host_i = usb1_dp_device_o;
	  	usb1_dm_host_i = usb1_dm_device_o;
		usb1_dp_device_i = 1'b0;
	  	usb1_dm_device_i = 1'b1;
	 end
	 2'b10: begin
		usb1_dp_host_i = 1'b0;
	  	usb1_dm_host_i = 1'b1;
		usb1_dp_device_i = usb1_dp_host_o;
	  	usb1_dm_device_i = usb1_dm_host_o;
	 end
	 2'b11: begin
	 	$display("usb1 output enable conflict");
		usb1_dp_host_i = usb1_dp_device_o;
	  	usb1_dm_host_i = usb1_dm_device_o;
		usb1_dp_device_i = usb1_dp_host_o;
	  	usb1_dm_device_i = usb1_dm_host_o;
	 end
	 endcase
   end

   //USB HID device connected to USB port 0 is a simulated mouse.
   top_usb_device #(.J1_ROM_INIT_FILE("j1_mouse.hex")) usb_device_0 (
	.clk(clk_50),
	.usb_clk(clk_6), //This is actually a 6.25MHz clock. It should be 6MHz to maintain a correct low-speed USB data rate,
	                 //but it's fine, because in simulation, BoxLambda's USD HID host core runs at 12.5MHz instead of 12MHz,
					 //i.e. USB host and device are off by the same amount in simulation.
	.reset_in_n(rst_ni),
	.dm_i(usb0_dm_device_i),
	.dp_i(usb0_dp_device_i),
	.dm_o(usb0_dm_device_o),
	.dp_o(usb0_dp_device_o),
	.usb_oe(usb0_oe_device),
	.ledg(ledg_0),
	.ledr(ledr_0));
   
   //USB HID device connected to USB port 0 is a simulated keyboard.
   top_usb_device #(.J1_ROM_INIT_FILE("j1_keyboard.hex")) usb_device_1 (
	.clk(clk_50),
	.usb_clk(clk_6),
	.reset_in_n(rst_ni),
	.dm_i(usb1_dm_device_i),
	.dp_i(usb1_dp_device_i),
	.dm_o(usb1_dm_device_o),
	.dp_o(usb1_dp_device_o),
	.usb_oe(usb1_oe_device),
	.ledg(ledg_1),
	.ledr(ledr_1));

   top dut (
		.ext_clk_100(clk_100),
		.ext_rst_n(rst_ni),
		.tck(sim_jtag_tck),
		.trst_n(sim_jtag_trstn),
		.tms(sim_jtag_tms),
		.tdi(sim_jtag_tdi),
		.tdo(sim_jtag_tdo),
		.pll_locked_led(),
		.init_done_led(),
		.init_err_led(),
		// VGA interface
		.vga_r(vga_r),       
		.vga_g(vga_g),       
		.vga_b(vga_b),       
		.vga_hsync(vga_hsync),   
		.vga_vsync(vga_vsync),		 
		// SDSPI interface
		.sdspi_cs_n(sdspi_cs_n), 
		.sdspi_sck(sdspi_sck), 
		.sdspi_mosi(sdspi_mosi),
		.sdspi_miso(sdspi_miso), 
		.sdspi_card_detect_n(~sdspi_card_detect),

		// UART and GPIO
		.uart_rx(uart_rx),
		.uart_tx(uart_tx),
		.gpio0(gpio0),
		.gpio1(gpio1),		 

		.usb0_dm_i(usb0_dm_host_i), 
  		.usb0_dp_i(usb0_dp_host_i),
		.usb0_dm_o(usb0_dm_host_o), 
  		.usb0_dp_o(usb0_dp_host_o),
		.usb0_oe(usb0_oe_host),
  		.usb1_dm_i(usb1_dm_host_i), 
  		.usb1_dp_i(usb1_dp_host_i),
		.usb1_dm_o(usb1_dm_host_o), 
  		.usb1_dp_o(usb1_dp_host_o),
		.usb1_oe(usb1_oe_host),
		 
		// ym2149 interface
		.audio_out(audio_out),
  		.audio_gain(audio_gain),
  		.audio_shutdown_n(audio_shutdown_n),
  		.pcm_out(pcm_out),
		.acc1_overflow(acc1_overflow),
		.acc2_overflow(acc2_overflow)
		);
   
   always_comb begin : jtag_exit_handler
      if (|sim_jtag_exit)
        $finish(2); // print stats too
   end
endmodule
