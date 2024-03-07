//Wishbone Frontend for usb_hid_host module.
module wb_usb_hid_host (
    input  wire wb_clk,                     // Wishbone clock - assumed to be faster than usb_clock, e.g. 50MHz.
    input  wire wb_rst_n,                  // System clock domain active low reset
    output wire irq,     

    //32-bit pipelined Wishbone slave interface.
    input wire [3:0] wbs_adr,
	input wire [31:0] wbs_dat_w,
	output reg [31:0] wbs_dat_r,
	input wire [3:0] wbs_sel,
    output wire wbs_stall,
	input wire wbs_cyc,
	input wire wbs_stb,
	output wire wbs_ack,
	input wire wbs_we,
	output wire wbs_err,     

    input wire wb_usb_rst_n, //WB synchronous indication that USB clock domain is in reset

    input wire [1:0] usb_typ, //USB type
    input wire usb_report_stb, //USB report strobe indication
    input wire usb_conn_err, //USB connection error 

    // keyboard signals
    input wire [7:0] usb_key_modifiers,
    input wire [7:0] usb_key1, usb_key2, usb_key3, usb_key4,

    // mouse signals
    input wire [7:0] usb_mouse_btn,     // {5'bx, middle, right, left}
    input wire signed [7:0] usb_mouse_dx,      // signed 8-bit, cleared after `report` pulse
    input wire signed [7:0] usb_mouse_dy,      // signed 8-bit, cleared after `report` pulse

    // gamepad signals
    input wire usb_game_l, usb_game_r, usb_game_u, usb_game_d,  // left right up down
    input wire usb_game_a, usb_game_b, usb_game_x, usb_game_y, usb_game_sel, usb_game_sta,  // buttons

    input wire [63:0] usb_dbg_hid_report,	// last HID report

    // Pulse requesting to update keyboard LEDS.
    output reg update_leds_stb,
    output reg [3:0] leds, //Led bitmap

    // Indication that leds have been updated
    input wire ack_update_leds_stb
    
);

    reg [1:0] wb_usb_ien;  //IRQ enable register
    reg [1:0] wb_usb_isr;  //IRQ status register

    reg [1:0] wb_usb_typ;   //USB type register: 0: no device, 1: keyboard, 2: mouse, 3: gamepad
    reg wb_usb_conn_err;    //USB connection error register.

    // keyboard registers.
    reg [7:0] wb_usb_key_modifiers;
    reg [7:0] wb_usb_key1, wb_usb_key2, wb_usb_key3, wb_usb_key4;

    // mouse registers.
    reg [7:0] wb_usb_mouse_btn;            // {5'bx, middle, right, left}
    reg signed [7:0] wb_usb_mouse_dx;      // signed 8-bit, cleared after `report` pulse
    reg signed [7:0] wb_usb_mouse_dy;      // signed 8-bit, cleared after `report` pulse

    // gamepad registers.
    reg wb_usb_game_l, wb_usb_game_r, wb_usb_game_u, wb_usb_game_d;  // left right up down
    reg wb_usb_game_a, wb_usb_game_b, wb_usb_game_x, wb_usb_game_y, wb_usb_game_sel, wb_usb_game_sta;  // buttons

    reg [63:0] wb_usb_dbg_hid_report;	// last HID report register.

    // Wishbone
    reg do_ack_wbs;
    wire do_wbs_wr_reg;
    wire unused = &{wbs_sel, wbs_dat_w[31:4]};

    //IRQ signalling.
    assign irq = | (wb_usb_isr & wb_usb_ien);

    //WB slave handshake.
    assign do_wbs_wr_reg = wbs_cyc && wbs_stb && wbs_we;
    assign wbs_ack = do_ack_wbs & wbs_cyc;
    assign wbs_stall = 1'b0;
    assign wbs_err = 1'b0;

    always @(posedge wb_clk) begin
        if ((!wb_usb_rst_n) || (!wb_rst_n)) begin
            if (!wb_rst_n) begin
                wb_usb_ien <= 2'b0;
                do_ack_wbs <= 1'b0;
                update_leds_stb <= 1'b0;
                leds <= 4'b0;
            end

            wb_usb_isr <= 2'b00;
            wb_usb_typ <= 2'b00;
            wb_usb_conn_err <= 1'b0;
        end
        else begin
            //WBS ack on next clock cycle.
            do_ack_wbs <= 1'b0;
            if (wbs_stb) begin
                do_ack_wbs <= 1'b1;
            end

            update_leds_stb <= 1'b0;
            //WBS register writes
            //A write to WB register 9 triggers an update_led action to the usb_hid_host module.
            //The usb_hid_host module will send the new led bitmap to the USB device using a SetReport transaction.
            if (do_wbs_wr_reg) begin
                case (wbs_adr)
                    4'd0: wb_usb_ien <= wbs_dat_w[1:0];
                    4'd1: wb_usb_isr <= wb_usb_isr & ~wbs_dat_w[1:0];
                    4'd9: begin
                        update_leds_stb <= 1'b1;
                        leds <= wbs_dat_w[3:0];
                    end
                    default:;
                endcase
            end 

            //update_led acknowledge received. Signal ISR.
            if (ack_update_leds_stb) wb_usb_isr[1] <= 1'b1;

            //A USB report has been received.
            if (usb_report_stb) begin
                wb_usb_isr[0] <= 1'b1;
                {wb_usb_typ, wb_usb_conn_err} <= {usb_typ, usb_conn_err};
                {wb_usb_key_modifiers, wb_usb_key1, wb_usb_key2, wb_usb_key3, wb_usb_key4} <= {usb_key_modifiers, usb_key1, usb_key2, usb_key3, usb_key4};
                {wb_usb_mouse_btn, wb_usb_mouse_dx, wb_usb_mouse_dy} <= {usb_mouse_btn, usb_mouse_dx, usb_mouse_dy};
                {wb_usb_game_l, wb_usb_game_r, wb_usb_game_u, wb_usb_game_d, wb_usb_game_a, wb_usb_game_b, wb_usb_game_x, wb_usb_game_y, wb_usb_game_sel, wb_usb_game_sta}
                    <= {usb_game_l, usb_game_r, usb_game_u, usb_game_d, usb_game_a, usb_game_b, usb_game_x, usb_game_y, usb_game_sel, usb_game_sta}; 
                wb_usb_dbg_hid_report <= usb_dbg_hid_report;
            end
        end
    end

    //WBS register reads
    always @* begin
        case(wbs_adr) //wbs address is a word address.
            4'd0: wbs_dat_r = {30'b0, wb_usb_ien};
            4'd1: wbs_dat_r = {30'b0, wb_usb_isr};
            4'd2: wbs_dat_r = {29'b0, wb_usb_conn_err, wb_usb_typ};
            4'd3: wbs_dat_r = {24'b0, wb_usb_key_modifiers}; 
            4'd4: wbs_dat_r = {wb_usb_key4, wb_usb_key3, wb_usb_key2, wb_usb_key1};
            4'd5: wbs_dat_r = {8'b0, wb_usb_mouse_btn, wb_usb_mouse_dx, wb_usb_mouse_dy};
            4'd6: wbs_dat_r = {22'b0, wb_usb_game_l, wb_usb_game_r, wb_usb_game_u, wb_usb_game_d, wb_usb_game_a, wb_usb_game_b, wb_usb_game_x, wb_usb_game_y, wb_usb_game_sel, wb_usb_game_sta};
            4'd7: wbs_dat_r = wb_usb_dbg_hid_report[31:0];
            4'd8: wbs_dat_r = wb_usb_dbg_hid_report[63:32];
            4'd9: wbs_dat_r = {28'b0, leds};
            default: wbs_dat_r = 32'd0;
        endcase
    end
endmodule
