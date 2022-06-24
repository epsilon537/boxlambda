module wbgpio #(
    parameter size = 32
  )(
    inout wire [size-1:0] gpio,
    input wire 		  i_clk, i_reset,
    // Wishbone inputs
    input wire 		  i_wb_cyc,
    input wire 		  i_wb_stb, i_wb_we,
    input wire [31:0] 	  i_wb_addr,
    input wire [31:0] 	  i_wb_data,
    input wire [3:0] 	  i_wb_sel,
    output wire           o_wb_err, 	  
    output wire 	  o_wb_stall,
    output wire 	  o_wb_ack,
    output wire [31:0] 	  o_wb_data
  );

   wb_gpio_wrapper #(.size(size)) 
   wb_gpio_wrapper_inst(
			.gpio(gpio),
			.i_clk(i_clk),
			.i_reset(i_reset),
			.i_wb_cyc(i_wb_cyc),
			.i_wb_stb(i_wb_stb),
			.i_wb_we(i_wb_we),
			.i_wb_addr(i_wb_addr),
			.i_wb_data(i_wb_data),
			.i_wb_sel(i_wb_sel),
			.o_wb_err(o_wb_err),
			.o_wb_stall(o_wb_stall),
			.o_wb_ack(o_wb_ack),
			.o_wb_data(o_wb_data)
			);
endmodule
