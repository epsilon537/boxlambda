module gpio_top (
	wb_clk_i,
	wb_rst_i,
	wb_cyc_i,
	wb_adr_i,
	wb_dat_i,
	wb_sel_i,
	wb_we_i,
	wb_stb_i,
	wb_dat_o,
	wb_ack_o,
	wb_err_o,
	wb_inta_o,
	ext_pad_i,
	ext_pad_o,
	ext_padoe_o,
	clk_pad_i
);
	parameter dw = 32;
	parameter aw = 4;
	parameter gw = 24;
	input wire wb_clk_i;
	input wire wb_rst_i;
	input wire wb_cyc_i;
	input wire [aw - 1:0] wb_adr_i;
	input wire [dw - 1:0] wb_dat_i;
	input wire [3:0] wb_sel_i;
	input wire wb_we_i;
	input wire wb_stb_i;
	output reg wb_ack_o;
	output reg wb_err_o;
	output reg wb_inta_o;
	output reg [dw - 1:0] wb_dat_o;
	input wire [gw - 1:0] ext_pad_i;
	input wire clk_pad_i;
	output reg [gw - 1:0] ext_pad_o;
	output wire [gw - 1:0] ext_padoe_o;
	reg [gw - 1:0] rgpio_in;
	reg [gw - 1:0] rgpio_out;
	reg [gw - 1:0] rgpio_oe;
	reg [gw - 1:0] rgpio_inte;
	reg [gw - 1:0] rgpio_ptrig;
	wire [gw - 1:0] rgpio_aux;
	reg [1:0] rgpio_ctrl;
	reg [gw - 1:0] rgpio_ints;
	reg [gw - 1:0] rgpio_eclk;
	reg [gw - 1:0] rgpio_nec;
	reg [gw - 1:0] sync;
	reg [gw - 1:0] ext_pad_s;
	wire rgpio_out_sel;
	wire rgpio_oe_sel;
	wire rgpio_inte_sel;
	wire rgpio_ptrig_sel;
	wire rgpio_aux_sel;
	wire rgpio_ctrl_sel;
	wire rgpio_ints_sel;
	wire rgpio_eclk_sel;
	wire rgpio_nec_sel;
	wire full_decoding;
	wire [gw - 1:0] in_muxed;
	wire wb_ack;
	wire wb_err;
	wire wb_inta;
	reg [dw - 1:0] wb_dat;
	wire [gw - 1:0] out_pad;
	wire [gw - 1:0] extc_in;
	wire [gw - 1:0] pext_clk;
	reg [gw - 1:0] pextc_sampled;
	assign wb_ack = (wb_cyc_i & wb_stb_i) & !wb_err_o;
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			wb_ack_o <= #(1) 1'b0;
		else
			wb_ack_o <= #(1) (wb_ack & ~wb_ack_o) & !wb_err;
	assign wb_err = (wb_cyc_i & wb_stb_i) & (wb_sel_i != 4'b1111);
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			wb_err_o <= #(1) 1'b0;
		else
			wb_err_o <= #(1) wb_err & ~wb_err_o;
	assign full_decoding = 1'b1;
	assign rgpio_out_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h1)) & full_decoding;
	assign rgpio_oe_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h2)) & full_decoding;
	assign rgpio_inte_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h3)) & full_decoding;
	assign rgpio_ptrig_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h4)) & full_decoding;
	assign rgpio_ctrl_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h6)) & full_decoding;
	assign rgpio_ints_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h7)) & full_decoding;
	assign rgpio_eclk_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h8)) & full_decoding;
	assign rgpio_nec_sel = ((wb_cyc_i & wb_stb_i) & (wb_adr_i[3:0] == 4'h9)) & full_decoding;
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_ctrl <= #(1) 2'b00;
		else if (rgpio_ctrl_sel && wb_we_i)
			rgpio_ctrl <= #(1) wb_dat_i[1:0];
		else if (rgpio_ctrl[0])
			rgpio_ctrl[1] <= #(1) rgpio_ctrl[1] | wb_inta_o;
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_out <= #(1) {gw {1'b0}};
		else if (rgpio_out_sel && wb_we_i)
			rgpio_out <= #(1) wb_dat_i[gw - 1:0];
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_oe <= #(1) {gw {1'b0}};
		else if (rgpio_oe_sel && wb_we_i)
			rgpio_oe <= #(1) wb_dat_i[gw - 1:0];
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_inte <= #(1) {gw {1'b0}};
		else if (rgpio_inte_sel && wb_we_i)
			rgpio_inte <= #(1) wb_dat_i[gw - 1:0];
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_ptrig <= #(1) {gw {1'b0}};
		else if (rgpio_ptrig_sel && wb_we_i)
			rgpio_ptrig <= #(1) wb_dat_i[gw - 1:0];
	assign rgpio_aux = 24'h000000;
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_eclk <= #(1) {gw {1'b0}};
		else if (rgpio_eclk_sel && wb_we_i)
			rgpio_eclk <= #(1) wb_dat_i[gw - 1:0];
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_nec <= #(1) {gw {1'b0}};
		else if (rgpio_nec_sel && wb_we_i)
			rgpio_nec <= #(1) wb_dat_i[gw - 1:0];
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i) begin
			sync <= #(1) {gw {1'b0}};
			ext_pad_s <= #(1) {gw {1'b0}};
		end
		else begin
			sync <= #(1) ext_pad_i;
			ext_pad_s <= #(1) sync;
		end
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_in <= #(1) {gw {1'b0}};
		else
			rgpio_in <= #(1) in_muxed;
	reg sync_clk;
	reg clk_s;
	reg clk_r;
	wire pedge;
	wire nedge;
	wire [gw - 1:0] pedge_vec;
	wire [gw - 1:0] nedge_vec;
	wire [gw - 1:0] in_lach;
	assign pedge = clk_s & !clk_r;
	assign nedge = !clk_s & clk_r;
	assign pedge_vec = {gw {pedge}};
	assign nedge_vec = {gw {nedge}};
	assign in_lach = (~rgpio_nec & pedge_vec) | (rgpio_nec & nedge_vec);
	assign extc_in = (in_lach & ext_pad_s) | (~in_lach & pextc_sampled);
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i) begin
			sync_clk <= #(1) 1'b0;
			clk_s <= #(1) 1'b0;
			clk_r <= #(1) 1'b0;
		end
		else begin
			sync_clk <= #(1) clk_pad_i;
			clk_s <= #(1) sync_clk;
			clk_r <= #(1) clk_s;
		end
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			pextc_sampled <= #(1) {gw {1'b0}};
		else
			pextc_sampled <= #(1) extc_in;
	assign in_muxed = (rgpio_eclk & pextc_sampled) | (~rgpio_eclk & ext_pad_s);
	always @(wb_adr_i or rgpio_in or rgpio_out or rgpio_oe or rgpio_inte or rgpio_ptrig or rgpio_aux or rgpio_ctrl or rgpio_ints or rgpio_eclk or rgpio_nec)
		case (wb_adr_i[3:0])
			4'h1: wb_dat[dw - 1:0] = rgpio_out;
			4'h2: wb_dat[dw - 1:0] = rgpio_oe;
			4'h3: wb_dat[dw - 1:0] = rgpio_inte;
			4'h4: wb_dat[dw - 1:0] = rgpio_ptrig;
			4'h9: wb_dat[dw - 1:0] = rgpio_nec;
			4'h8: wb_dat[dw - 1:0] = rgpio_eclk;
			4'h6: begin
				wb_dat[1:0] = rgpio_ctrl;
				wb_dat[dw - 1:2] = {dw - 2 {1'b0}};
			end
			4'h7: wb_dat[dw - 1:0] = rgpio_ints;
			default: wb_dat[dw - 1:0] = rgpio_in;
		endcase
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			wb_dat_o <= #(1) {dw {1'b0}};
		else
			wb_dat_o <= #(1) wb_dat;
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			rgpio_ints <= #(1) {gw {1'b0}};
		else if (rgpio_ints_sel && wb_we_i)
			rgpio_ints <= #(1) wb_dat_i[gw - 1:0];
		else if (rgpio_ctrl[0])
			rgpio_ints <= #(1) rgpio_ints | (((in_muxed ^ rgpio_in) & ~(in_muxed ^ rgpio_ptrig)) & rgpio_inte);
	assign wb_inta = (|rgpio_ints ? rgpio_ctrl[0] : 1'b0);
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			wb_inta_o <= #(1) 1'b0;
		else
			wb_inta_o <= #(1) wb_inta;
	assign ext_padoe_o = rgpio_oe;
	assign out_pad = rgpio_out;
	always @(posedge wb_clk_i or posedge wb_rst_i)
		if (wb_rst_i)
			ext_pad_o <= #(1) {gw {1'b0}};
		else
			ext_pad_o <= #(1) out_pad;
endmodule
