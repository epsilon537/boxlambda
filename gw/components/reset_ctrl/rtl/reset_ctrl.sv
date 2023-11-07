`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

/* BoxLambda Reset Controller.*/
module reset_ctrl (
    input logic clk,
    input logic pll_locked_i, //input indicating PLL achieved lock. Used for Power-On-Reset (POR).
    input logic ndm_reset_i, //Non-Debug-Module reset request. Typically comes from Debug Module.
    input logic ext_reset_i, //Asynchronous external reset request.
    output logic ndm_reset_o, //Non-Debug-Module reset output signal.
    output logic dm_reset_o, //Debug-Module reset output signal
    output logic por_completed_o, //indicates that Power On Reset has been completed.

    //32-bit pipelined Wishbone slave interface.
    input logic                                wb_adr, //2 register addresses
	input logic [31:0]                         wb_dat_w,
	output logic [31:0]                        wb_dat_r,
	input logic [3:0]                          wb_sel,
    output logic                               wb_stall,
	input logic                                wb_cyc,
	input logic                                wb_stb,
	output logic                               wb_ack,
	input logic                                wb_we,
	output logic                               wb_err
    );

    //Reset reason values tracked by reset_reason_reg.
    localparam [4:0] RESET_REASON_POR = 1;
    localparam [4:0] RESET_REASON_WB_NDM = 2;
    localparam [4:0] RESET_REASON_WB_DM = 4;
    localparam [4:0] RESET_REASON_NDM = 8;
    localparam [4:0] RESET_REASON_EXT = 16;

    //Power on Reset state machine states
    typedef enum {wait_for_pll_lock, wait_assert_por, assert_por, por_completed} por_state;
    por_state por_state_reg;
    logic [2:0] por_assert_counter;
    logic por;
    
    logic [4:0] reset_reason_reg, reset_reason_next;

    initial begin
        por_state_reg = wait_for_pll_lock;
        por_assert_counter = 3'b0;
        reset_reason_reg = 5'b0;
    end

    always_ff @(posedge clk)
        case (por_state_reg)
            wait_for_pll_lock:
                //When PLL lock achieved, move to next state
                if (pll_locked_i)
                    por_state_reg <= wait_assert_por;
            wait_assert_por:
                begin
                    //Wait 8 ticks before going to assert_por state
                    if (por_assert_counter == 3'b111)
                        por_state_reg <= assert_por;
                    //Wraps back to 0.
                    por_assert_counter <= por_assert_counter + 3'b001;
                end
            assert_por:
                begin
                    //Stay in assert_por state for 8 ticks.
                    if (por_assert_counter == 3'b111)
                        por_state_reg <= por_completed;
                    por_assert_counter <= por_assert_counter + 3'b001;
                end
            por_completed:;
        endcase

    assign por = (por_state_reg == assert_por);
    assign por_completed_o = (por_state_reg == por_completed);

    //Synchronize and debounce external reset
    logic ext_reset_conditioned;

    button_conditioner button_conditioner_instr (
        .clk(clk),
        .btn(ext_reset_i),
        .out(ext_reset_conditioned)
    );

    //WB handshake
    logic wb_adr_reg;
    logic do_ack_reg, do_wb_wr;
    logic unused = &{wb_sel, wb_adr, wb_dat_w[31:2]};

    initial begin
        wb_adr_reg = 1'b0;
    end

    assign do_wb_wr = wb_cyc & wb_stb & wb_we;

    always @(posedge clk) begin
        do_ack_reg <= 1'b0;
        if (wb_stb) begin
            do_ack_reg <= 1'b1;
            wb_adr_reg <= wb_adr; //Register the address too.
        end
    end

    //Always return the reset reason register contents
    assign wb_dat_r = {27'b0, reset_reason_reg};
    assign wb_ack = do_ack_reg & wb_cyc;
    assign wb_stall = 1'b0; //!wb_cyc ? 1'b0 : !wb_ack;
    assign wb_err = 1'b0;

    //Wishbone triggered ndm and/or dm reset
    logic wb_ndm_reset, wb_dm_reset;
    assign wb_ndm_reset = do_wb_wr & wb_dat_w[0];
    assign wb_dm_reset = do_wb_wr & wb_dat_w[1];
    
    //Keep track of the reset reason.
    always_comb begin
        reset_reason_next = reset_reason_reg;
        if (por)
            reset_reason_next = reset_reason_next | RESET_REASON_POR;
        if (wb_ndm_reset)
            reset_reason_next = reset_reason_next | RESET_REASON_WB_NDM;
        if (wb_dm_reset)
            reset_reason_next = reset_reason_next | RESET_REASON_WB_DM;
        if (ndm_reset_i)
            reset_reason_next = reset_reason_next | RESET_REASON_NDM;
        if (ext_reset_conditioned)
            reset_reason_next = reset_reason_next | RESET_REASON_EXT;
        //An access to the reset reason register resets it.
        if (wb_adr_reg && do_ack_reg)
            reset_reason_next = 5'b0;
    end

    always @(posedge clk) begin
            reset_reason_reg <= reset_reason_next;
    end

    //Non-Debug Module reset output
    assign ndm_reset_o = por | ext_reset_conditioned | ndm_reset_i | wb_ndm_reset;

    //Debug Module reset output
    assign dm_reset_o = por | ext_reset_conditioned | wb_dm_reset;

endmodule
