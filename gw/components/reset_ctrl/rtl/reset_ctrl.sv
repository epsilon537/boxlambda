`ifdef __ICARUS__
`timescale 1 ns / 1 ps
`endif

/* BoxLambda Reset Controller.*/
module reset_ctrl (
    input logic sys_clk,
    input logic usb_clk,
    input logic sys_pll_locked_i, //input indicating system clock PLL achieved lock. Used for Power-On-Reset (POR).
    input logic usb_pll_locked_i, //input indicating USB clock PLL achieved lock. Used for Power-On-Reset (POR).
    input logic ndm_reset_i,  //Non-Debug-Module reset request. Typically comes from Debug Module.
    input logic ext_reset_i,  //Asynchronous external reset request.
    output logic ndm_reset_o, //Non-Debug-Module reset output signal, synchronous in sys_clk domain.
    output logic dm_reset_o,  //Debug-Module reset output signal, synchronous in sys_clk domain.
    output logic usb_reset_o,  //USB reset output signal, synchronous in usb_clk domain.
    output logic por_completed_o,  //indicates that Power On Reset has been completed.

    //32-bit pipelined Wishbone slave interface.
    input  logic        wb_adr,    //2 register addresses
    input  logic [31:0] wb_dat_w,
    output logic [31:0] wb_dat_r,
    input  logic [ 3:0] wb_sel,
    output logic        wb_stall,
    input  logic        wb_cyc,
    input  logic        wb_stb,
    output logic        wb_ack,
    input  logic        wb_we,
    output logic        wb_err
);

  //Reset reason values tracked by reset_reason_reg.
  localparam [5:0] RESET_REASON_POR = 1;
  localparam [5:0] RESET_REASON_WB_NDM = 2;
  localparam [5:0] RESET_REASON_WB_DM = 4;
  localparam [5:0] RESET_REASON_NDM = 8;
  localparam [5:0] RESET_REASON_EXT = 16;
  localparam [5:0] RESET_REASON_WB_USB = 32;

  //Registered ndm, dm, and usb reset outputs.
  logic ndm_reset_o_reg, dm_reset_o_reg, usb_reset_o_reg;

  //Power on Reset state machine states
  typedef enum {
    wait_for_pll_lock,
    wait_assert_por,
    assert_por,
    por_completed
  } por_state;
  por_state por_state_reg;
  logic [5:0] por_assert_counter;
  logic wb_por;  //Power-On Reset signal in WB clock domain
  logic usb_por;  //Power-On Reset signal in USB clock domain

  logic [5:0] reset_reason_reg, reset_reason_next;

  initial begin
    por_state_reg = wait_for_pll_lock;
    por_assert_counter = 6'b000000;
    reset_reason_reg = 6'b0;
    ndm_reset_o_reg = 1'b0;
    dm_reset_o_reg = 1'b0;
  end

  logic wb_usb_pll_locked;  //USB PLL locked signal in WB clock domain.

  //Synchronize the usb_pll_locked signal to the WB clock domain.
  sync3 usb_pll_locked_sync (
      .q(wb_usb_pll_locked),
      .d(usb_pll_locked_i),
      .clk(sys_clk),
      .rst_n(1'b1)
  );

  always_ff @(posedge sys_clk)
    case (por_state_reg)
      wait_for_pll_lock:
      //When PLL lock achieved, move to next state
      if (sys_pll_locked_i && wb_usb_pll_locked)
        por_state_reg <= wait_assert_por;
      wait_assert_por: begin
        //Wait 64 ticks before going to assert_por state
        if (por_assert_counter == 6'b111111) por_state_reg <= assert_por;
        //Wraps back to 0.
        por_assert_counter <= por_assert_counter + 6'b000001;
      end
      assert_por: begin
        //Stay in assert_por state for 64 ticks.
        if (por_assert_counter == 6'b111111) por_state_reg <= por_completed;
        por_assert_counter <= por_assert_counter + 6'b000001;
      end
      por_completed: ;
    endcase

  assign wb_por = (por_state_reg == assert_por);
  assign por_completed_o = (por_state_reg == por_completed);

  //Debounced external reset synchronized to WB clock domain.
  logic wb_ext_reset_conditioned;
  //Debounced external reset synchronized to USB clock domain.
  logic usb_ext_reset_conditioned;

  button_conditioner button_conditioner_instr (
      .clk(sys_clk),
      .btn(ext_reset_i),
      .out(wb_ext_reset_conditioned)
  );

  //WB handshake
  logic wb_adr_reg;
  logic do_ack_reg, do_wb_wr;
  logic unused = &{wb_sel, wb_adr, wb_dat_w[31:3]};

  initial wb_adr_reg = 1'b0;

  assign do_wb_wr = wb_cyc & wb_stb & wb_we;

  always_ff @(posedge sys_clk) begin
    do_ack_reg <= 1'b0;
    if (wb_stb) begin
      do_ack_reg <= 1'b1;
      wb_adr_reg <= wb_adr;  //Register the address too.
    end
  end

  //Always return the reset reason register contents
  assign wb_dat_r = {26'b0, reset_reason_reg};
  assign wb_ack   = do_ack_reg & wb_cyc;
  assign wb_stall = 1'b0;
  assign wb_err   = 1'b0;

  //Wishbone triggered ndm and/or dm reset
  logic wb_ndm_reset_stb, wb_dm_reset_stb, wb_usb_reset_stb;
  //Wishbone triggered usb reset in USB clock domain.
  logic usb_wb_reset_stb;

  assign wb_ndm_reset_stb = do_wb_wr & wb_dat_w[0];
  assign wb_dm_reset_stb  = do_wb_wr & wb_dat_w[1];
  assign wb_usb_reset_stb = do_wb_wr & wb_dat_w[2];

  //Keep track of the reset reason.
  always_comb begin
    reset_reason_next = reset_reason_reg;
    if (wb_por) reset_reason_next = reset_reason_next | RESET_REASON_POR;
    if (wb_ndm_reset_stb) reset_reason_next = reset_reason_next | RESET_REASON_WB_NDM;
    if (wb_dm_reset_stb) reset_reason_next = reset_reason_next | RESET_REASON_WB_DM;
    if (ndm_reset_i) reset_reason_next = reset_reason_next | RESET_REASON_NDM;
    if (wb_ext_reset_conditioned) reset_reason_next = reset_reason_next | RESET_REASON_EXT;
    if (wb_usb_reset_stb) reset_reason_next = reset_reason_next | RESET_REASON_WB_USB;
    //An access to the reset reason register resets it.
    if (wb_adr_reg && do_ack_reg) reset_reason_next = 6'b0;
  end

  //Register the ndm and dm reset outputs and update reset_reason state/
  always_ff @(posedge sys_clk) begin
    reset_reason_reg <= reset_reason_next;
    ndm_reset_o_reg  <= wb_por | wb_ext_reset_conditioned | ndm_reset_i | wb_ndm_reset_stb;
    dm_reset_o_reg   <= wb_por | wb_ext_reset_conditioned | wb_dm_reset_stb;
  end

  //Non-Debug Module reset output
  assign ndm_reset_o = ndm_reset_o_reg;

  //Debug Module reset output
  assign dm_reset_o  = dm_reset_o_reg;

  //Sync usb_reset_stb from WB domain to USB domain.
  syncpls usb_wb_reset_sync (
      .t_clk(sys_clk),  //transmitting clock.
      .t_rst_n(1'b1),  //reset in t_clk domain.
      .t_pulse(wb_usb_reset_stb),  //input pulse in t_clk domain.
      .r_clk(usb_clk),  //receiving clock.
      .r_rst_n(1'b1),  //reset in r_clk_domain.
      .r_pulse(usb_wb_reset_stb)
  );

  //Sync WB domain ext_reset to USB domain
  sync3 usb_ext_reset_sync (
      .q(usb_ext_reset_conditioned),
      .d(wb_ext_reset_conditioned),
      .clk(usb_clk),
      .rst_n(1'b1)
  );

  //Sync WB domain POR to USB domain.
  sync3 usb_por_sync (
      .q(usb_por),
      .d(wb_por),
      .clk(usb_clk),
      .rst_n(1'b1)
  );

  //Register the usb reset output
  initial usb_reset_o_reg = 1'b0;

  always_ff @(posedge usb_clk)
    usb_reset_o_reg <= usb_wb_reset_stb | usb_por | usb_ext_reset_conditioned;

  assign usb_reset_o = usb_reset_o_reg;
endmodule
