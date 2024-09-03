/*This is a J1B realization of a Reconfigurable Module 0. RM0 resides on the crossbar with a Wishbone master and slave port, and irq inputs and output. */
module rm0 (
    input logic sys_clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface.
    output logic [27:0] wbm_adr_o,
    output logic [31:0] wbm_dat_o,
    input logic [31:0] wbm_dat_i,
    output logic wbm_we_o,
    output logic [3:0] wbm_sel_o,
    output logic wbm_stb_o,
    input logic wbm_ack_i,
    input logic wbm_stall_i,
    output logic wbm_cyc_o,
    input logic wbm_err_i,

    //32-bit pipelined Wishbone slave interface.
    input logic [19:0] wbs_adr,
    input logic [31:0] wbs_dat_w,
    output logic [31:0] wbs_dat_r,
    input logic [3:0] wbs_sel,
    output logic wbs_stall,
    input logic wbs_cyc,
    input logic wbs_stb,
    output logic wbs_ack,
    input logic wbs_we,
    output logic wbs_err,

    //Input IRQs from the rest of the system.
    input wire [31:0] irq_in,
    //Output IRQ signal
    output wire irq_out
);

  localparam MHZ = 50;
  localparam integer MEM_SZ_WORDS = 8192;  //J1B Program and data memory size
  localparam integer WBS_REG_BASE_ADDR = MEM_SZ_WORDS;  //Register base address as seen by WB slave.

  //J1B signals
  logic [7:0] j1b_uart_rx_data;
  logic j1b_uart_rx_data_avl;
  logic [7:0] j1b_uart_tx_data;
  logic j1b_uart_tx_busy;
  logic [31:0] j1b_gp_in, j1b_gp_out;

  wire [15:0] j1b_mem_addr;
  wire [31:0] j1b_mem_din;
  wire j1b_wen;
  wire [31:0] j1b_dout;
  reg [31:0] j1b_io_din;

  wire [12:0] j1b_code_addr;
  wire [15:0] j1b_insn;

  wire j1b_io_rd, j1b_io_wr;

  //The J1B reset signal is controlled through the register interface. It's not tied
  //to the system reset register.
  logic        j1b_rst_n;

  //Three system registers: ctrl, irq-in and irq-out.
  logic [31:0] ctrl_reg;
  logic [31:0] irq_in_reg;
  logic [31:0] irq_out_reg, irq_out_next;

  logic do_ack_wbs;
  logic do_wbs_wr_mem, do_wbs_wr_reg;
  logic do_wbs_rd_reg;
  logic [31:0] wbs_dat_r_reg, wbs_dat_r_mem;

  logic unused = &{wbs_sel, wbm_err_i, wbm_dat_i, wbm_ack_i, wbm_stall_i};

  //WBM interface currently not used.
  assign wbm_adr_o = 28'b0;
  assign wbm_dat_o = 32'b0;
  assign wbm_we_o = 1'b0;
  assign wbm_sel_o = 4'b0;
  assign wbm_stb_o = 1'b0;
  assign wbm_cyc_o = 1'b0;

  //WB slave handshake
  assign do_wbs_rd_reg = wbs_cyc && wbs_stb && (~wbs_we) && (wbs_adr[13:0] >= 14'(WBS_REG_BASE_ADDR));
  assign do_wbs_wr_reg = wbs_cyc && wbs_stb && wbs_we && (wbs_adr[13:0] >= 14'(WBS_REG_BASE_ADDR));
  assign do_wbs_wr_mem = wbs_cyc && wbs_stb && wbs_we && (wbs_adr[13:0] < 14'(WBS_REG_BASE_ADDR));

  always_ff @(posedge sys_clk) begin
    do_ack_wbs <= 1'b0;
    if (wbs_stb) begin
      wbs_dat_r_mem <= j1b_mem_din;  //wbs_ram_register needed for timing closure.
      do_ack_wbs <= 1'b1;
    end
  end

  assign wbs_dat_r = (wbs_adr[13:0] < 14'(WBS_REG_BASE_ADDR)) ? wbs_dat_r_mem : wbs_dat_r_reg;
  assign wbs_ack   = do_ack_wbs & wbs_cyc;
  assign wbs_stall = 1'b0;
  assign wbs_err   = 1'b0;

  //Reset Control via ctrl register bit 0.
  assign j1b_rst_n = ctrl_reg[0];

  //IRQ handling
  assign irq_out   = |irq_out_reg;
  always_comb begin
    for (int i = 0; i < 32; i++) begin
      irq_out_next[i] = irq_out_reg[i];
      //Ack IRQ by writing 1 to corresponding bit in register 0 (IRQ_OUT)
      if (do_wbs_wr_reg && (wbs_adr[13:0] == 14'(WBS_REG_BASE_ADDR + 0)) && wbs_dat_w[i])
        irq_out_next[i] = 1'b0;
    end
    //No IRQ support currently.
    // for (int i = 0; i < 31; i++) begin
    //   if (reg_we && reg_valid && (reg_addr == J1B_REG_BASE_ADDR + 0) && reg_wdata[i])
    //     irq_out_next[i] = 1'b1;  //J1B can set IRQs by writing to irq_out register.
    // end
  end

  //Counter for J1B
  reg [63:0] counter;
  always @(posedge sys_clk) counter <= counter + 64'd1;

  reg [31:0] ms;
  reg [17:0] subms;
  localparam [17:0] lim = (MHZ * 1000) - 1;
  always @(posedge sys_clk) begin
    subms <= (subms == lim) ? 18'd0 : (subms + 18'd1);
    if (subms == lim) ms <= ms + 32'd1;
  end

  //The J1B processor
  j1 j1_inst (
      .clk(sys_clk),
      .resetq(j1b_rst_n),

      .io_rd(j1b_io_rd),
      .io_wr(j1b_io_wr),
      .mem_addr(j1b_mem_addr),
      .mem_wr(j1b_wen),
      .dout(j1b_dout),
      .mem_din(j1b_mem_din),

      .io_din(j1b_io_din),

      .code_addr(j1b_code_addr),
      .insn(j1b_insn)
  );

  //Program memory for the J1B processor.
  //When J1B is in reset, WBS has RAM access. When J1B is not in reset, it has RAM access.
  ram32k ram (
      .clk(sys_clk),
      .a_addr(j1b_rst_n ? j1b_mem_addr[14:0] : {wbs_adr[12:0], 2'b00}),
      .a_q(j1b_mem_din),
      .a_wr(j1b_rst_n ? j1b_wen : do_wbs_wr_mem),
      .a_d(j1b_rst_n ? j1b_dout : wbs_dat_w),
      .b_addr(j1b_code_addr),
      .b_q(j1b_insn)
  );

  reg j1b_io_wr_, j1b_io_rd_;
  reg [15:0] j1b_mem_addr_;
  reg [31:0] j1b_dout_;
  always @(posedge sys_clk)
    {j1b_io_wr_, j1b_io_rd_, j1b_mem_addr_, j1b_dout_} <= {
      j1b_io_wr, j1b_io_rd, j1b_mem_addr, j1b_dout
    };

  /*
    Memory Map as seen by J1B:

    READ            WRITE
    00xx  GPIO rd         GPIO wr

    1000  UART RX         UART TX
    2000  UART status

    1010  master freq     snapshot clock
    1014  clock[31:0]
    1018  clock[63:32]
    101c  millisecond uptime

  */

  logic [63:0] counter_;

  always @(posedge sys_clk) begin
    if (rst) begin
      j1b_uart_rx_data <= 8'b0;
      j1b_uart_rx_data_avl <= 1'b0;
      j1b_uart_tx_data <= 8'b0;
      j1b_uart_tx_busy <= 1'b0;
      j1b_gp_in <= 32'b0;
      j1b_gp_out <= 32'b0;

      ctrl_reg <= 32'b0;

      irq_in_reg <= 32'b0;
      irq_out_reg <= 32'b0;
    end else begin
      irq_in_reg  <= irq_in;
      irq_out_reg <= irq_out_next;

      //WBS register writes
      if (do_wbs_wr_reg) begin
        case (wbs_adr[13:0])  //wbs address is a word address.
          14'(WBS_REG_BASE_ADDR + 2): ctrl_reg <= wbs_dat_w;
          14'(WBS_REG_BASE_ADDR + 16):
          {j1b_uart_rx_data_avl, j1b_uart_rx_data} <= {1'b1, wbs_dat_w[7:0]};
          14'(WBS_REG_BASE_ADDR + 17): j1b_gp_in <= wbs_dat_w;
          default: ;
        endcase
      end

      //WBS register read
      if (do_wbs_rd_reg) begin
        case (wbs_adr[13:0])
          14'(WBS_REG_BASE_ADDR): wbs_dat_r_reg <= irq_out_reg;
          14'(WBS_REG_BASE_ADDR + 1): wbs_dat_r_reg <= irq_in_reg;
          14'(WBS_REG_BASE_ADDR + 2): wbs_dat_r_reg <= ctrl_reg;
          14'(WBS_REG_BASE_ADDR + 16):
          wbs_dat_r_reg <= {23'b0, j1b_uart_rx_data_avl, j1b_uart_rx_data};
          14'(WBS_REG_BASE_ADDR + 18): wbs_dat_r_reg <= {23'b0, j1b_uart_tx_busy, j1b_uart_tx_data};
          14'(WBS_REG_BASE_ADDR + 19): wbs_dat_r_reg <= j1b_gp_out;
          default: ;
        endcase

        if (wbs_adr[13:0] == 14'(WBS_REG_BASE_ADDR + 18)) j1b_uart_tx_busy <= 1'b0;
      end

      //J1B register reads
      casez (j1b_mem_addr)
        16'h00??: j1b_io_din <= {31'b0, j1b_gp_in[j1b_mem_addr[4:0]]};

        16'h1000: j1b_io_din <= {24'd0, j1b_uart_rx_data};
        16'h2000: j1b_io_din <= {30'd0, j1b_uart_rx_data_avl, !j1b_uart_tx_busy};

        16'h1010: j1b_io_din <= MHZ * 1000000;
        16'h1014: j1b_io_din <= counter_[31:0];
        16'h1018: j1b_io_din <= counter_[63:32];
        16'h101c: j1b_io_din <= ms;

        default: j1b_io_din <= 32'b0;
      endcase

      if (j1b_io_rd_ && (j1b_mem_addr_ == 16'h1000)) begin
        j1b_uart_rx_data_avl <= 1'b0;
      end

      //J1B register writes
      if (j1b_io_wr_) begin
        casez (j1b_mem_addr_)
          16'h00??: j1b_gp_out[j1b_mem_addr_[4:0]] <= j1b_dout_[0];
          16'h1000: {j1b_uart_tx_busy, j1b_uart_tx_data} <= {1'b1, j1b_dout_[7:0]};
          16'h1010: counter_ <= counter;
          default:  ;
        endcase
      end
    end
  end
endmodule

// A 32Kbyte RAM (8192x32) with two ports:
//   port a, 32 bits read/write
//   port b, 16 bits read-only, lower 16K only
module ram32k (
    input wire clk,

    input  wire [14:0] a_addr,
    output wire [31:0] a_q,
    input  wire [31:0] a_d,
    input  wire        a_wr,

    input  wire [12:0] b_addr,
    output wire [15:0] b_q
);

  logic unused = &{a_addr[1:0]};
  wire [31:0] insn32;

  bram_tdp #(
      .DATA(32),
      .ADDR(13)
  ) nram (
      .a_clk (clk),
      .a_wr  (a_wr),
      .a_addr(a_addr[14:2]),
      .a_din (a_d),
      .a_dout(a_q),

      .b_clk (clk),
      .b_wr  (1'b0),
      .b_addr({1'b0, b_addr[12:1]}),
      .b_din (32'd0),
      .b_dout(insn32)
  );

  reg ba_;
  always @(posedge clk) ba_ <= b_addr[0];
  assign b_q = ba_ ? insn32[31:16] : insn32[15:0];

endmodule

module bram_tdp #(
    parameter DATA = 72,
    parameter ADDR = 10
) (
    // Port A
    input  wire            a_clk,
    input  wire            a_wr,
    input  wire [ADDR-1:0] a_addr,
    input  wire [DATA-1:0] a_din,
    output reg  [DATA-1:0] a_dout,

    // Port B
    input  wire            b_clk,
    input  wire            b_wr,
    input  wire [ADDR-1:0] b_addr,
    input  wire [DATA-1:0] b_din,
    output reg  [DATA-1:0] b_dout
);

  // Shared memory
  reg [DATA-1:0] mem[(2**ADDR)-1:0];

  // Port A
  always @(posedge a_clk) begin
    a_dout <= mem[a_addr];
    if (a_wr) begin
      a_dout      <= a_din;
      mem[a_addr] <= a_din;
    end
  end

  // Port B
  always @(posedge b_clk) begin
    b_dout <= mem[b_addr];
    if (b_wr) begin
      b_dout      <= b_din;
      mem[b_addr] <= b_din;
    end
  end

endmodule

