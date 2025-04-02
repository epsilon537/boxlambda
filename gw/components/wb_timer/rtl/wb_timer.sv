
//Wishbone wrapper for the RISCV Timer module.
module wb_timer (
    input  wire    clk_i,
    input  wire    rst_i,
    input  wire    wb_cyc_i,
    input  wire    wb_stb_i,
    input  wire    wb_we_i,
    input  wire  [7:0]  wb_addr_i,
    input  wire  [31:0] wb_data_i,
    input  wire  [3:0]  wb_sel_i,
    output  wire    wb_stall_o,
    output  wire    wb_ack_o,
    output  wire    wb_err_o,
    output  wire [31:0]  wb_data_o,
    output  wire    timer_irq_o
);

  assign wb_stall_o = 1'b0;

  /*Straight out of the Wishbone B4 spec. This is how you interface a classic slave to a pipelined master.
   *The stall signal ensures that the STB signal remains asserted until an ACK is received from the slave.*/
  assign wb_stall_o = !wb_cyc_i ? 1'b0 : !wb_ack_o;

  timer #(
      .DataWidth(32),
      .AddressWidth(10)
  ) timer_inst (
      .clk_i(clk_i),
      .rst_ni(~rst_i),
      .timer_req_i(wb_cyc_i & wb_stb_i),
      .timer_addr_i({wb_addr_i, 2'b00}),  //Word-to-Byte addressing
      .timer_we_i(wb_we_i),
      .timer_be_i(wb_sel_i),
      .timer_wdata_i(wb_data_i),
      .timer_rvalid_o(wb_ack_o),
      .timer_rdata_o(wb_data_o),
      .timer_err_o(wb_err_o),
      .timer_intr_o(timer_irq_o)
  );
endmodule

