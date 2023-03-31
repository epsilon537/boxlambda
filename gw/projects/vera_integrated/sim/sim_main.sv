//
//  A simulator top-level wrapping around ibex_soc, instantiating SimJTAG.
//
module sim_main #(
		  parameter int unsigned OPENOCD_PORT = 9999
		  ) 
   (
    input logic      clk_i,
    input logic      rst_ni,
    inout wire [7:0] gpio0,
    inout wire [3:0] gpio1,
    input wire 	     uart_rx,
    output wire      uart_tx,
	// VGA interface
	output wire  [3:0] vga_r,       
	output wire  [3:0] vga_g,       
	output wire  [3:0] vga_b,       
	output wire        vga_hsync,   
	output wire        vga_vsync   	
    );
   
   // jtag openocd bridge signals
   logic 	     sim_jtag_tck;
   logic 	     sim_jtag_tms;
   logic 	     sim_jtag_tdi;
   logic 	     sim_jtag_trstn;
   logic 	     sim_jtag_tdo;
   logic [31:0]      sim_jtag_exit;
   
   SimJTAG  #(
              .TICK_DELAY (1),
              .PORT(OPENOCD_PORT)
	      ) simJTAG_inst(
			     .clock                ( clk_i                ),
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

   vera_test_soc dut (
		 .ext_clk100(clk_i),
		 .ext_rst_n(rst_ni),
		 .gpio0(gpio0),
		 .gpio1(gpio1),
		 .uart_rx(uart_rx),
		 .uart_tx(uart_tx),
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
  		.vga_vsync(vga_vsync)   		 
		 );
   
   always_comb begin : jtag_exit_handler
      if (|sim_jtag_exit)
        $finish(2); // print stats too
   end
endmodule
