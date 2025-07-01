/*
 * Wishbone dual port RAM wrapper around two implementations: In simulation, a generic implementations is used. On
 * FPGA, a Vivado XPM wrapper is used.
 * When Vivado synthesizes an XPM memory instance, it produces a .mmi file for that memory.
 * This .mmi file can be used for post-implementation updates of the memory contents in the FPGA bitstream.
 */
module wb_dp_ram_wrapper #(
    parameter ADDR_WIDTH = 14,  // width of word addressed address bus in bits
    parameter INIT_FILE  = ""   // This parameter used to be used only when building on verilator,
                                // Vivado builds relying stricly on .mmi based post-implementation
                                // memory updates. Now, INIT_FILEs can also be passed into Vivado
                                // builds at synthesis time. This allow for instance for a default
                                // memory image (e.g. a jump-to-flash stub) that can later,
                                // post-imp, be overwritten by a custom test build image.
) (
    input wire clk,
    input wire rst,

    // port A
    input  wire [ADDR_WIDTH-1:0] a_adr_i,    // ADR_I() address
    input  wire [          31:0] a_dat_i,    // DAT_I() data in
    output wire [          31:0] a_dat_o,    // DAT_O() data out
    input  wire                  a_we_i,     // WE_I write enable input
    input  wire [           3:0] a_sel_i,    // SEL_I() select input
    input  wire                  a_stb_i,    // STB_I strobe input
    output wire                  a_stall_o,  // STALL_O stall output
    output wire                  a_ack_o,    // ACK_O acknowledge output
    output wire                  a_err_o,    // ERR_O error output
    input  wire                  a_cyc_i,    // CYC_I cycle input

    // port B
    input  wire [ADDR_WIDTH-1:0] b_adr_i,    // ADR_I() address
    input  wire [          31:0] b_dat_i,    // DAT_I() data in
    output wire [          31:0] b_dat_o,    // DAT_O() data out
    input  wire                  b_we_i,     // WE_I write enable input
    input  wire [           3:0] b_sel_i,    // SEL_I() select input
    input  wire                  b_stb_i,    // STB_I strobe input
    output wire                  b_stall_o,  // STALL_O stall output
    output wire                  b_ack_o,    // ACK_O acknowledge output
    output wire                  b_err_o,    // ERR_O error output
    input  wire                  b_cyc_i     // CYC_I cycle input
);

`ifdef SYNTHESIS
  logic a_valid, b_valid;
  logic [3:0] a_ram_we, b_ram_we;
  logic a_ack, b_ack;

  /* Wishbone control */
  assign a_valid   = a_cyc_i & a_stb_i;
  assign b_valid   = b_cyc_i & b_stb_i;
  assign a_stall_o = 1'b0;
  assign a_err_o   = 1'b0;
  assign b_stall_o = 1'b0;
  assign b_err_o   = 1'b0;

  assign a_ram_we  = {4{a_we_i & a_valid}} & a_sel_i;
  assign b_ram_we  = {4{b_we_i & b_valid}} & b_sel_i;

  always_ff @(posedge clk) begin
    if (rst) begin
      a_ack <= 1'b0;
      b_ack <= 1'b0;
    end else begin
      a_ack <= a_valid & ~a_stall_o;
      b_ack <= b_valid & ~b_stall_o;
    end
  end

  assign a_ack_o = a_ack;
  assign b_ack_o = b_ack;

  xpm_memory_tdpram #(
      .ADDR_WIDTH_A(ADDR_WIDTH),  // DECIMAL
      .ADDR_WIDTH_B(ADDR_WIDTH),  // DECIMAL
      .AUTO_SLEEP_TIME(0),  // DECIMAL
      .BYTE_WRITE_WIDTH_A(8),  // DECIMAL
      .BYTE_WRITE_WIDTH_B(8),  // DECIMAL
      .CASCADE_HEIGHT(0),  // DECIMAL
      .CLOCKING_MODE("common_clock"),  // String
      .ECC_BIT_RANGE("7:0"),  // String
      .ECC_MODE("no_ecc"),  // String
      .ECC_TYPE("none"),  // String
      .IGNORE_INIT_SYNTH(0),  // DECIMAL
      .MEMORY_INIT_FILE(INIT_FILE == "" ? "none" : INIT_FILE),  // if an INIT_FILE is specified, used it.
      .MEMORY_INIT_PARAM(""),  // String
      .MEMORY_OPTIMIZATION("true"),  // String
      .MEMORY_PRIMITIVE("block"),  // String
      .MEMORY_SIZE((2 ** ADDR_WIDTH) * 32),  // MEMORY_SIZE in bits
      .MESSAGE_CONTROL(0),  // DECIMAL
      .RAM_DECOMP("power"),  // String
      .READ_DATA_WIDTH_A(32),  // DECIMAL
      .READ_DATA_WIDTH_B(32),  // DECIMAL
      .READ_LATENCY_A(1),  // DECIMAL
      .READ_LATENCY_B(1),  // DECIMAL
      .READ_RESET_VALUE_A("0"),  // String
      .READ_RESET_VALUE_B("0"),  // String
      .RST_MODE_A("SYNC"),  // String
      .RST_MODE_B("SYNC"),  // String
      .SIM_ASSERT_CHK(0),  // DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      .USE_EMBEDDED_CONSTRAINT(0),  // DECIMAL
      .USE_MEM_INIT(1),  // DECIMAL
      .USE_MEM_INIT_MMI(1),  // Enable MMI
      .WAKEUP_TIME("disable_sleep"),  // String
      .WRITE_DATA_WIDTH_A(32),  // DECIMAL
      .WRITE_DATA_WIDTH_B(32),  // DECIMAL
      .WRITE_MODE_A("no_change"),  // String
      .WRITE_MODE_B("no_change"),  // String
      .WRITE_PROTECT(1)  // DECIMAL
  ) xpm_memory_tdpram_inst (
      .dbiterra(),  // 1-bit output: Status signal to indicate double bit error occurrence
      // on the data output of port A.

      .dbiterrb(),  // 1-bit output: Status signal to indicate double bit error occurrence
      // on the data output of port A.

      .douta   (a_dat_o),  // READ_DATA_WIDTH_A-bit output: Data output for port A read operations.
      .doutb   (b_dat_o),  // READ_DATA_WIDTH_B-bit output: Data output for port B read operations.
      .sbiterra(),         // 1-bit output: Status signal to indicate single bit error occurrence
      // on the data output of port A.

      .sbiterrb(),  // 1-bit output: Status signal to indicate single bit error occurrence
      // on the data output of port B.

      .addra(a_adr_i),  // ADDR_WIDTH_A-bit input: Address for port A write and read operations.
      .addrb(b_adr_i),  // ADDR_WIDTH_B-bit input: Address for port B write and read operations.
      .clka (clk),      // 1-bit input: Clock signal for port A. Also clocks port B when
                        // parameter CLOCKING_MODE is "common_clock".

      .clkb(clk),  // 1-bit input: Clock signal for port B when parameter CLOCKING_MODE is
      // "independent_clock". Unused when parameter CLOCKING_MODE is
      // "common_clock".

      .dina(a_dat_i),  // WRITE_DATA_WIDTH_A-bit input: Data input for port A write operations.
      .dinb(b_dat_i),  // WRITE_DATA_WIDTH_B-bit input: Data input for port B write operations.
      .ena (a_valid),  // 1-bit input: Memory enable signal for port A. Must be high on clock
                       // cycles when read or write operations are initiated. Pipelined
                       // internally.

      .enb(b_valid),  // 1-bit input: Memory enable signal for port B. Must be high on clock
                      // cycles when read or write operations are initiated. Pipelined
                      // internally.

      .injectdbiterra(1'b0),  // 1-bit input: Controls double bit error injection on input data when
      // ECC enabled (Error injection capability is not available in
      // "decode_only" mode).

      .injectdbiterrb(1'b0),  // 1-bit input: Controls double bit error injection on input data when
      // ECC enabled (Error injection capability is not available in
      // "decode_only" mode).

      .injectsbiterra(1'b0),  // 1-bit input: Controls single bit error injection on input data when
      // ECC enabled (Error injection capability is not available in
      // "decode_only" mode).

      .injectsbiterrb(1'b0),  // 1-bit input: Controls single bit error injection on input data when
      // ECC enabled (Error injection capability is not available in
      // "decode_only" mode).

      .regcea(1'b1),  // 1-bit input: Clock Enable for the last register stage on the output
      // data path.

      .regceb(1'b1),  // 1-bit input: Clock Enable for the last register stage on the output
      // data path.

      .rsta(rst),  // 1-bit input: Reset signal for the final port A output register stage.
      // Synchronously resets output port douta to the value specified by
      // parameter READ_RESET_VALUE_A.

      .rstb(rst),  // 1-bit input: Reset signal for the final port B output register stage.
      // Synchronously resets output port doutb to the value specified by
      // parameter READ_RESET_VALUE_B.

      .sleep(1'b0),     // 1-bit input: sleep signal to enable the dynamic power saving feature.
      .wea  (a_ram_we), // WRITE_DATA_WIDTH_A/BYTE_WRITE_WIDTH_A-bit input: Write enable vector
                        // for port A input data port dina. 1 bit wide when word-wide writes are
                        // used. In byte-wide write configurations, each bit controls the
                        // writing one byte of dina to address addra. For example, to
                        // synchronously write only bits [15-8] of dina when WRITE_DATA_WIDTH_A
                        // is 32, wea would be 4'b0010.

      .web(b_ram_we)  // WRITE_DATA_WIDTH_B/BYTE_WRITE_WIDTH_B-bit input: Write enable vector
                      // for port B input data port dinb. 1 bit wide when word-wide writes are
                      // used. In byte-wide write configurations, each bit controls the
                      // writing one byte of dinb to address addrb. For example, to
                      // synchronously write only bits [15-8] of dinb when WRITE_DATA_WIDTH_B
                      // is 32, web would be 4'b0010.

  );

`else  /*SIMULATION:*/
  logic [ADDR_WIDTH+1:0] a_adr_i_byte = {a_adr_i, 2'b0};
  logic [ADDR_WIDTH+1:0] b_adr_i_byte = {b_adr_i, 2'b0};

  logic unused = rst;

  assign a_adr_i_byte = {a_adr_i, 2'b0};
  assign b_adr_i_byte = {b_adr_i, 2'b0};

  wb_dp_ram #(
      .DATA_WIDTH(32),
      .ADDR_WIDTH(ADDR_WIDTH + 2),  //Expects byte addresses.
      .SELECT_WIDTH(4),
      .INIT_FILE(INIT_FILE)
  ) wb_dp_ram_inst (
      .a_clk  (clk),
      .b_clk  (clk),
      .a_adr_i(a_adr_i_byte),
      .b_adr_i(b_adr_i_byte),
      .*
  );

  /*Straight out of the Wishbone B4 spec. This is how you interface a classic slave to a pipelined master.
    *The stall signal ensures that the STB signal remains asserted until an ACK is received from the slave.*/
  assign a_stall_o = !a_cyc_i ? 1'b0 : !a_ack_o;
  assign a_err_o   = 1'b0;
  assign b_stall_o = !b_cyc_i ? 1'b0 : !b_ack_o;
  assign b_err_o   = 1'b0;
`endif

endmodule
