//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 06/21/2022 06:15:36 PM
// Design Name: 
// Module Name: wb_gpio_wrapper
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module wb_gpio_wrapper #(
    parameter size = 32
  )(
    inout logic [size-1:0] gpio,
    input logic 	   i_clk, i_reset,
    // Wishbone inputs
    input logic 	   i_wb_cyc,
    input logic 	   i_wb_stb, i_wb_we,
    input logic [31:0] 	   i_wb_addr,
    input logic [31:0] 	   i_wb_data,
    input logic [3:0] 	   i_wb_sel,
    output logic           o_wb_err, 	   
    output logic 	   o_wb_stall,
    output logic 	   o_wb_ack,
    output logic [31:0]    o_wb_data
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
   
   wb_gpio #(.size(size)) wb_gpio_inst(
				       .gpio(gpio),
				       .wb(wbs)
				       );
   
endmodule
