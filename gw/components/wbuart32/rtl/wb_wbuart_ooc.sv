//wb_uart wrapper for Out of Context synthesis.
module wb_wbuart_ooc
  ( input logic 	   i_clk, i_reset,
    // Wishbone inputs
    input logic 	   i_wb_cyc,
    input logic 	   i_wb_stb, i_wb_we,
    input logic [27:0] 	   i_wb_addr,
    input logic [31:0] 	   i_wb_data,
    input logic [3:0] 	   i_wb_sel,
    output logic           o_wb_err, 	   
    output logic 	   o_wb_stall,
    output logic 	   o_wb_ack,
    output logic [31:0]    o_wb_data,
    
    input  wire i_uart_rx,
    output wire o_uart_tx,
    
    input  wire i_cts_n,
    output wire o_rts_n,
    output wire o_uart_rx_int,
    output wire o_uart_tx_int,
    output wire o_uart_rxfifo_int,
    output wire o_uart_txfifo_int
   );

   wb_if wbs(.rst(i_reset),
	     .clk(i_clk));

   assign wbs.adr = i_wb_addr;
   assign wbs.cyc = i_wb_cyc;
   assign wbs.stb = i_wb_stb;
   assign wbs.we = i_wb_we;
   assign wbs.sel = i_wb_sel;
   assign wbs.dat_m = i_wb_data;

   assign o_wb_ack = wbs.ack;
   assign o_wb_stall = wbs.stall;
   assign o_wb_data = wbs.dat_s;
   assign o_wb_err = wbs.err;

   wb_wbuart_wrap wb_wbuart_wrap_inst(.wb(wbs),
				      .*);
   
   logic 	unused = &{1'b0,wbs.sel,wbs.adr,1'b0};
 endmodule
