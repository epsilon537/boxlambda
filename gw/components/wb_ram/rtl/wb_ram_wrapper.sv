/*
 * Wishbone single port RAM wrapper around two implementations: In simulation, a generic implementations is used. On
 * FPGA, a Vivado XPM wrapper is used.
 * When Vivado synthesizes an XPM memory instance, it produces a .mmi file for that memory.
 * This .mmi file can be used for post-implementation updates of the memory contents in the FPGA bitstream.
 */
module wb_ram_wrapper #(
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

    input  wire [ADDR_WIDTH-1:0] adr_i,    // ADR_I() address
    input  wire [          31:0] dat_i,    // DAT_I() data in
    output wire [          31:0] dat_o,    // DAT_O() data out
    input  wire                  we_i,     // WE_I write enable input
    input  wire [           3:0] sel_i,    // SEL_I() select input
    input  wire                  stb_i,    // STB_I strobe input
    output wire                  stall_o,  // STALL_O stall output
    output wire                  ack_o,    // ACK_O acknowledge output
    output wire                  err_o,    // ERR_O error output
    input  wire                  cyc_i     // CYC_I cycle input
);

`ifdef SYNTHESIS
  logic valid;
  logic [3:0] ram_we;
  logic ack;

  /* Wishbone control */
  assign valid   = cyc_i & stb_i;
  assign stall_o = 1'b0;
  assign err_o   = 1'b0;

  assign ram_we  = {4{we_i & valid}} & sel_i;

  always_ff @(posedge clk) begin
    if (rst) begin
      ack <= 1'b0;
    end else begin
      ack <= valid & ~stall_o;
    end
  end

  assign ack_o = ack;

  xpm_memory_spram #(
      .ADDR_WIDTH_A(ADDR_WIDTH),  // DECIMAL
      .AUTO_SLEEP_TIME(0),  // DECIMAL
      .BYTE_WRITE_WIDTH_A(8),  // DECIMAL
      .CASCADE_HEIGHT(0),  // DECIMAL
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
      .READ_LATENCY_A(1),  // DECIMAL
      .READ_RESET_VALUE_A("0"),  // String
      .RST_MODE_A("SYNC"),  // String
      .SIM_ASSERT_CHK(0),  // DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      .USE_MEM_INIT(1),  // DECIMAL
      .USE_MEM_INIT_MMI(1),  // DECIMAL
      .WAKEUP_TIME("disable_sleep"),  // String
      .WRITE_DATA_WIDTH_A(32),  // DECIMAL
      .WRITE_MODE_A("write_first"),  // String
      .WRITE_PROTECT(0)  // DECIMAL
  ) xpm_memory_spram_inst (
      .dbiterra(),  // 1-bit output: Status signal to indicate double bit error occurrence
      // on the data output of port A.

      .douta   (dat_o),  // READ_DATA_WIDTH_A-bit output: Data output for port A read operations.
      .sbiterra(),       // 1-bit output: Status signal to indicate single bit error occurrence
      // on the data output of port A.

      .addra(adr_i),  // ADDR_WIDTH_A-bit input: Address for port A write and read operations.
      .clka (clk),    // 1-bit input: Clock signal for port A. Also clocks port B when
                      // parameter CLOCKING_MODE is "common_clock".

      // "independent_clock". Unused when parameter CLOCKING_MODE is
      // "common_clock".

      .dina(dat_i),  // WRITE_DATA_WIDTH_A-bit input: Data input for port A write operations.
      .ena (valid),  // 1-bit input: Memory enable signal for port A. Must be high on clock
      // cycles when read or write operations are initiated. Pipelined
      // internally.

      .injectdbiterra(1'b0),  // 1-bit input: Controls double bit error injection on input data when
      // ECC enabled (Error injection capability is not available in
      // "decode_only" mode).

      .injectsbiterra(1'b0),  // 1-bit input: Controls single bit error injection on input data when
      // ECC enabled (Error injection capability is not available in
      // "decode_only" mode).

      .regcea(1'b1),  // 1-bit input: Clock Enable for the last register stage on the output
      // data path.

      .rsta(rst),  // 1-bit input: Reset signal for the final port A output register stage.
      // Synchronously resets output port douta to the value specified by
      // parameter READ_RESET_VALUE_A.

      .sleep(1'b0),   // 1-bit input: sleep signal to enable the dynamic power saving feature.
      .wea  (ram_we)  // WRITE_DATA_WIDTH_A/BYTE_WRITE_WIDTH_A-bit input: Write enable vector
      // for port A input data port dina. 1 bit wide when word-wide writes are
      // used. In byte-wide write configurations, each bit controls the
      // writing one byte of dina to address addra. For example, to
      // synchronously write only bits [15-8] of dina when WRITE_DATA_WIDTH_A
      // is 32, wea would be 4'b0010.
  );

`else  /*SIMULATION:*/
  logic [ADDR_WIDTH+1:0] adr_i_byte = {adr_i, 2'b0};

  logic unused = rst;

  assign adr_i_byte = {adr_i, 2'b0};

  wb_ram #(
      .DATA_WIDTH(32),
      .ADDR_WIDTH(ADDR_WIDTH + 2),  //Expects byte addresses.
      .SELECT_WIDTH(4),
      .INIT_FILE(INIT_FILE)
  ) wb_ram_inst (
      .clk  (clk),
      .adr_i(adr_i_byte),
      .*
  );

  /*Straight out of the Wishbone B4 spec. This is how you interface a classic slave to a pipelined master.
    *The stall signal ensures that the STB signal remains asserted until an ACK is received from the slave.*/
  assign stall_o = !cyc_i ? 1'b0 : !ack_o;
  assign err_o   = 1'b0;
`endif

endmodule
