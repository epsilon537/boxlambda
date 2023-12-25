`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

/* BoxLambda Reset Controller.*/
module reset_ctrl (
    input logic sys_clk,
    input logic usb_clk,
    input logic sys_pll_locked_i, //input indicating system clock PLL achieved lock. Used for Power-On-Reset (POR).
    input logic usb_pll_locked_i, //input indicating USB clock PLL achieved lock. Used for Power-On-Reset (POR).
    input logic ndm_reset_i, //Non-Debug-Module reset request. Typically comes from Debug Module.
    input logic ext_reset_i, //Asynchronous external reset request.
    output logic ndm_reset_o, //Non-Debug-Module reset output signal, synchronous in sys_clk domain.
    output logic dm_reset_o, //Debug-Module reset output signal, synchronous in sys_clk domain.
    output logic usb_reset_o, //USB reset output signal, synchronous in usb_clk domain.
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
    localparam [5:0] RESET_REASON_POR = 1;
    localparam [5:0] RESET_REASON_WB_NDM = 2;
    localparam [5:0] RESET_REASON_WB_DM = 4;
    localparam [5:0] RESET_REASON_NDM = 8;
    localparam [5:0] RESET_REASON_EXT = 16;
    localparam [5:0] RESET_REASON_WB_USB = 32;

    //Registered ndm and dm outputs.
    logic ndm_reset_o_reg, dm_reset_o_reg;

    //Power on Reset state machine states
    typedef enum {wait_for_pll_lock, wait_assert_por, assert_por, por_completed} por_state;
    por_state por_state_reg;
    logic [5:0] por_assert_counter;
    logic por;
    
    logic [5:0] reset_reason_reg, reset_reason_next;
    
    //Transfer pipes for clock domain crossing.
    (* ASYNC_REG = "TRUE" *) logic [1:0] usb_pll_locked_xfer_pipe;
    logic usb_pll_locked_synced;
    (* ASYNC_REG = "TRUE" *) logic [1:0] usb_reset_xfer_pipe;
    logic usb_reset_synced;

    initial begin
        por_state_reg = wait_for_pll_lock;
        por_assert_counter = 6'b000000;
        reset_reason_reg = 6'b0;
        usb_pll_locked_synced = 1'b0;
        usb_pll_locked_xfer_pipe = 2'b00;
        usb_reset_synced = 1'b0;
        usb_reset_xfer_pipe = 2'b00; 
        ndm_reset_o_reg = 1'b0;
        dm_reset_o_reg = 1'b0;
    end

    //Synchronize the usb_pll_locked signal
    always @(posedge sys_clk) begin
	    {usb_pll_locked_synced, usb_pll_locked_xfer_pipe} <= {usb_pll_locked_xfer_pipe, usb_pll_locked_i};
    end

    always_ff @(posedge sys_clk)
        case (por_state_reg)
            wait_for_pll_lock:
                //When PLL lock achieved, move to next state
                if (sys_pll_locked_i && usb_pll_locked_synced)
                    por_state_reg <= wait_assert_por;
            wait_assert_por:
                begin
                    //Wait 64 ticks before going to assert_por state
                    if (por_assert_counter == 6'b111111)
                        por_state_reg <= assert_por;
                    //Wraps back to 0.
                    por_assert_counter <= por_assert_counter + 6'b000001;
                end
            assert_por:
                begin
                    //Stay in assert_por state for 64 ticks.
                    if (por_assert_counter == 6'b111111)
                        por_state_reg <= por_completed;
                    por_assert_counter <= por_assert_counter + 6'b000001;
                end
            por_completed:;
        endcase

    assign por = (por_state_reg == assert_por);
    assign por_completed_o = (por_state_reg == por_completed);

    //Synchronize and debounce external reset
    logic ext_reset_conditioned;

    button_conditioner button_conditioner_instr (
        .clk(sys_clk),
        .btn(ext_reset_i),
        .out(ext_reset_conditioned)
    );

    //WB handshake
    logic wb_adr_reg;
    logic do_ack_reg, do_wb_wr;
    logic unused = &{wb_sel, wb_adr, wb_dat_w[31:2]};

    initial wb_adr_reg = 1'b0;

    assign do_wb_wr = wb_cyc & wb_stb & wb_we;

    always @(posedge sys_clk) begin
        do_ack_reg <= 1'b0;
        if (wb_stb) begin
            do_ack_reg <= 1'b1;
            wb_adr_reg <= wb_adr; //Register the address too.
        end
    end

    //Always return the reset reason register contents
    assign wb_dat_r = {26'b0, reset_reason_reg};
    assign wb_ack = do_ack_reg & wb_cyc;
    assign wb_stall = 1'b0;
    assign wb_err = 1'b0;

    //Wishbone triggered ndm and/or dm reset
    logic wb_ndm_reset, wb_dm_reset, wb_usb_reset;
    logic [5:0] wb_usb_reset_count;
    logic wb_usb_reset_reg;

    initial begin
        wb_usb_reset_reg = 1'b0;
        wb_usb_reset_count = 6'b000000;
    end

    assign wb_ndm_reset = do_wb_wr & wb_dat_w[0];
    assign wb_dm_reset = do_wb_wr & wb_dat_w[1];
    assign wb_usb_reset = do_wb_wr & wb_dat_w[2];

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
        if (wb_usb_reset)
            reset_reason_next = reset_reason_next | RESET_REASON_WB_USB;
        //An access to the reset reason register resets it.
        if (wb_adr_reg && do_ack_reg)
            reset_reason_next = 6'b0;
    end

    always @(posedge sys_clk) begin
        reset_reason_reg <= reset_reason_next;

        if (!wb_usb_reset_reg) begin
            wb_usb_reset_reg <= wb_usb_reset;
        end
        else begin
            wb_usb_reset_count <= wb_usb_reset_count + 6'b000001;
            if (wb_usb_reset_count == 6'b111111)
                wb_usb_reset_reg <= 1'b0;
        end
    end

    //Register the ndm and dm reset outputs
    always @(posedge sys_clk) begin
        ndm_reset_o_reg <= por | ext_reset_conditioned | ndm_reset_i | wb_ndm_reset;
        dm_reset_o_reg <= por | ext_reset_conditioned | wb_dm_reset;
    end

    //Non-Debug Module reset output
    assign ndm_reset_o = ndm_reset_o_reg;

    //Debug Module reset output
    assign dm_reset_o = dm_reset_o_reg;

    //USB Module reset output, in usb_clk clock domain.
    //Synchronize the usb_reset signal
    always @(posedge usb_clk) begin
	    {usb_reset_synced, usb_reset_xfer_pipe} <= {usb_reset_xfer_pipe, por | ext_reset_conditioned | wb_usb_reset_reg};
    end

    assign usb_reset_o = usb_reset_synced;
endmodule
