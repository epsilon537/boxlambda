/* BoxLambda Reset Controller.*/
module reset_ctrl (
    input logic clk,
    input logic pll_locked_i,
    input logic ndm_reset_i,
    input logic ext_reset_i, //asynchronous external reset
    output logic ndm_reset_o,
    output logic dm_reset_o,
    output logic por_completed_o, //indicates that Power On Reset has been completed.

    //32-bit pipelined Wishbone slave interface.
    input logic                                wb_adr,
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

    //Power on Reset
    typedef enum {wait_for_pll_lock, wait_assert_por, assert_por, por_completed} por_state;
    por_state por_state_reg;
    logic [2:0] counter;
    logic por;

    initial begin
        por_state_reg = wait_for_pll_lock;
        counter = 3'b0;
    end

    always_ff @(posedge clk)
        case (por_state_reg)
            wait_for_pll_lock:
                if (pll_locked_i)
                    por_state_reg <= wait_assert_por;
            wait_assert_por:
                begin
                    if (counter == 3'b111)
                        por_state_reg <= assert_por;
                    counter <= counter + 3'b001;
                end
            assert_por:
                begin
                    if (counter == 3'b111)
                        por_state_reg <= por_completed;
                    counter <= counter + 3'b001;
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
    logic do_ack_reg, do_wb_wr;
    logic unused = &{wb_sel, wb_adr, wb_dat_w[31:2]};

    assign do_wb_wr = wb_cyc & wb_stb & wb_we;

    always @(posedge clk) begin
        do_ack_reg <= 1'b0;
        if (wb_stb) begin
            do_ack_reg <= 1'b1;
        end
    end

    assign wb_dat_r = 0;
    assign wb_ack = do_ack_reg & wb_cyc;
    assign wb_stall = !wb_cyc ? 1'b0 : !wb_ack;
    assign wb_err = 1'b0;

    //Wishbone triggered ndm and/or dm reset
    logic wb_ndm_reset, wb_dm_reset;
    assign wb_ndm_reset = do_wb_wr & wb_dat_w[0];
    assign wb_dm_reset = do_wb_wr & wb_dat_w[1];
    
    //Non-Debug Module reset
    assign ndm_reset_o = por | ext_reset_conditioned | ndm_reset_i | wb_ndm_reset;

    //Debug Module reset
    assign dm_reset_o = por | ext_reset_conditioned | wb_dm_reset;

endmodule
