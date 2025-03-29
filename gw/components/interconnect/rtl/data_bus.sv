//This is BoxLambda's data bus: a 3-to-1 arbiter connected to a 1-to-18
//mux.
module data_bus #(
    parameter DATA_WIDTH = 32,  // width of data bus in bits (8, 16, 32, or 64)
    parameter ADDR_WIDTH = 32,  // width of address bus in bits
    parameter SELECT_WIDTH = (DATA_WIDTH / 8),  // width of word select bus (1, 2, 4, or 8)
    parameter ARB_TYPE_ROUND_ROBIN = 0,  // select round robin arbitration
    parameter ARB_LSB_HIGH_PRIORITY = 1,  // LSB priority selection
    parameter ARB_BLOCK_ACK = 1,  // block ack generation
    parameter [18*ADDR_WIDTH-1:0] SLAVE_ADDRESSES,
    parameter [18*ADDR_WIDTH-1:0] SLAVE_ADDR_MASKS
) (
    input wire clk,
    input wire rst,
    wb_if.slave wbm[3],
    wb_if.master wbs[18]
);

  import wb_pkg::*;

  wb_if arbiter_to_mux_if (
      .rst(rst),
      .clk(clk)
  );

  wb_arbiter_3_wrapper #(
      .DATA_WIDTH(DATA_WIDTH),
      .ADDR_WIDTH(ADDR_WIDTH),
      .SELECT_WIDTH(SELECT_WIDTH),
      .ARB_TYPE_ROUND_ROBIN(ARB_TYPE_ROUND_ROBIN),
      .ARB_LSB_HIGH_PRIORITY(ARB_LSB_HIGH_PRIORITY),
      .ARB_BLOCK_ACK(ARB_BLOCK_ACK),
      .ARB_DEFAULT_TO_PORT_0(1)
  ) arbiter (
      .clk  (clk),
      .rst  (rst),
      .wbm_0(wbm[0]),
      .wbm_1(wbm[1]),
      .wbm_2(wbm[2]),
      .wbs  (arbiter_to_mux_if)
  );

  wb_mux_18_wrapper #(
      .DATA_WIDTH(DATA_WIDTH),
      .ADDR_WIDTH(ADDR_WIDTH),
      .SELECT_WIDTH(SELECT_WIDTH),
      .SLAVE_ADDRESSES(SLAVE_ADDRESSES),
      .SLAVE_ADDR_MASKS(SLAVE_ADDR_MASKS)
  ) mux (
      .clk(clk),
      .rst(rst),
      .wbm(arbiter_to_mux_if),
      .wbs(wbs)
  );

endmodule

