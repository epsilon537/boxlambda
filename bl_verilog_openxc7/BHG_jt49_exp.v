`default_nettype wire
module BHG_jt49_exp (
	clk,
	din,
	dout
);
	parameter [5:0] DAC_BITS = 8;
	input clk;
	input [4:0] din;
	output reg [DAC_BITS - 1:0] dout;
	initial dout = 0;
	generate
		if ((DAC_BITS < 8) || (DAC_BITS > 12)) begin : genblk1
			initial begin
				$display("");
				$display("");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXXX                                              XXXXXX");
				$display("  XXXXXX   BrianHG's BHG_jt49_exp.v PARAMETER ERROR   XXXXXX");
				$display("  XXXXXX   https://github.com/BrianHGinc              XXXXXX");
				$display("  XXXXXX                                              XXXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXXX                                                             XXXXXX");
				$display("  XXXXXX   BHG_jt49_exp parameter .DAC_BITS(%d) is not supported.    XXXXXX", DAC_BITS);
				$display("  XXXXXX   Only numbers from 8 thru 12 are allowed.                  XXXXXX");
				$display("  XXXXXX                                                             XXXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("Error [%0t] /home/epsilon/work/boxlambda/gw/components/ym2149/../../../sub/ym2149_psg_system/BHG_jt49_exp.sv:53:1 - BHG_jt49_exp.genblk1.<unnamed_block>", $time);
				$stop;
			end
		end
	endgenerate
	reg [511:0] dlut_8 = 512'h1000200030004000500060008000a000c000e001100130017001a001f00230029002f0036003e00460050005c006800770087009a00af00c600e100ff;
	reg [511:0] dlut_9 = 512'h1000200030004000600070009000c000e001100150019001e0024002a0032003b00450051005f006f0082009700b100ce00f001170145017a01b701ff;
	reg [511:0] dlut_10 = 512'h100020003000500060008000b000e00110015001a002000270030003a0046005400650079009100ae00d000f80129016201a701f9025b02cf035a03ff;
	reg [511:0] dlut_11 = 512'h100020003000500070009000c00100014001a002100290033003f004e00600076009200b300dc010e014b019601f1026102e90391045d0557068907ff;
	reg [511:0] dlut_12 = 512'h10002000300050008000a000e00120018001f00280033004100520068008400a600d10108014c01a2020e0295033e0413051f066f08140a250cbd0fff;
	reg [6655:4096] dlut_sel = {dlut_8, dlut_9, dlut_10, dlut_11, dlut_12};
	reg [511:0] dlut = dlut_sel[16 * ((20 - DAC_BITS) * 32)+:512];
	always @(posedge clk) dout <= dlut[(31 - din) * 16+:16];
endmodule
