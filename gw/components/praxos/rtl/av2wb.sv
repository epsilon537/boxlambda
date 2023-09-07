`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

module av2wb (
	input wire clk,
	input wire rst_n,

	//Avalon Slave
	input wire [31:0] av_address,
	output wire [31:0] av_readdata,
	input wire [31:0] av_writedata,
	input wire [3:0] av_byteenable,
	input wire av_write,
	input wire av_read,
	output wire av_waitrequest,

	//WB Master
	output wire [29:0] wb_adr, //This is a word address
	output wire [31:0] wb_dat_w,
	input wire [31:0] wb_dat_r,
	output wire [3:0] wb_sel,
    input wire wb_stall,
	output wire wb_cyc,
	output wire wb_stb,
	input wire wb_ack,
	output wire wb_we,
	input wire wb_err
);

logic unused = &{av_address[31:30]}; //Can't use the top bits. wb_adr is 30-bits.

logic cyc_reg, cyc_next;
logic stb_reg, stb_next;
logic we_reg, we_next;

initial begin
	cyc_reg = 1'b0;
	stb_reg = 1'b0;
	we_reg = 1'b0;
end

always_comb begin
	stb_next = stb_reg;
	we_next = we_reg;
	cyc_next = cyc_reg;

	if (!cyc_reg) begin /*New transaction*/
		if (av_read || av_write) begin
			stb_next = 1'b1;
			we_next = av_write;
			cyc_next = 1'b1;
		end
	end
	else begin /*Ongoing transaction*/
		if (wb_ack||wb_err)
			cyc_next = 1'b0;

		if (stb_reg && !wb_stall) begin
			stb_next = 1'b0;
			we_next = 1'b0;
		end
	end
end

/*One transfer per WB CYC bus cycle.*/
always_ff @(posedge clk)
	if (!rst_n) begin
		cyc_reg <= 1'b0;
		stb_reg <= 1'b0;
		we_reg <= 1'b0;
	end
	else begin
		cyc_reg <= cyc_next;
		stb_reg <= stb_next;
		we_reg <= we_next;
	end

assign av_waitrequest = ~wb_ack;
assign av_readdata  = wb_dat_r;

assign wb_cyc = cyc_next | cyc_reg;
assign wb_stb = stb_next | stb_reg;
assign wb_adr = av_address[29:0];
assign wb_we = we_next | we_reg;
assign wb_sel = av_byteenable;
assign wb_dat_w  = av_writedata;

endmodule
