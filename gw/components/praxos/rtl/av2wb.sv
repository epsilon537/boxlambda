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
	output wire [31:0] wb_adr,
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

logic transaction_ongoing;
logic stb_reg, stb_next;

initial transaction_ongoing = 1'b0;
initial stb_reg = 1'b0;

always_comb begin
	stb_next = av_read || av_write;
	if (stb_reg && !wb_stall)
		stb_next = 1'b0;
	if (wb_ack || wb_err)
		stb_next = 1'b0;
end

/*One transfer per WB CYC bus cycle.*/
always_ff @(posedge clk)
	if (!rst_n) begin
		transaction_ongoing <= 1'b0;
		stb_reg <= 1'b0;
	end
	else begin
		stb_reg <= stb_next;
		if (transaction_ongoing) begin
			/*pipelined WB: During a transaction STB should only be high for one clock cycle 
			*after master has been granted access by WB slave or WB interconnect  (through !stall).
			*As long as we're stalled, STB remains asserted.*/
			//if (!wb_stall)
			//	stb <= 1'b0; 

			/*We don't do multiple transfers per transaction. A transaction ends when WB ack or
			*error is received, i.e. when the slave has accepted (or errored) the write data or
			*returned the read data.*/
			if (wb_ack || wb_err) begin
				transaction_ongoing <= 1'b0;
				//stb <= 1'b0;
			end
		end
		else begin
			/*av_read or av_write indicate the CPU wants to initiate a transaction.*/
			if (av_read || av_write) begin
			   	/*Register the relevant signals so they hold their data for as long as we want to
				*for the purpose of this adapter.*/
				transaction_ongoing <= 1'b1;
				//stb <= 1'b1;
			end
		end
	end

//read data is valid when we have a transaction ongoing and the slave asserts ack.
//av_waitrequest goes low in that case.
assign av_waitrequest = !(wb_ack & transaction_ongoing);
assign av_readdata  = wb_dat_r;

assign wb_cyc = av_read || av_write;
assign wb_stb = stb_reg;
assign wb_adr = av_address;
assign wb_we = av_write;
assign wb_sel = av_byteenable;
assign wb_dat_w  = av_writedata;

// dump signals
`ifdef COCOTB_SIM
initial begin
	$dumpfile ("av2wb.vcd");
	$dumpvars (0, av2wb);
end
`endif

endmodule
