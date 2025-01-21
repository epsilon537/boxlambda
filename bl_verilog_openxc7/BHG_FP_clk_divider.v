`default_nettype wire
module BHG_FP_clk_divider (
	clk_in,
	rst_in,
	clk_out,
	clk_p0,
	clk_p180
);
	input wire clk_in;
	input wire rst_in;
	output reg clk_out;
	output reg clk_p0;
	output reg clk_p180;
	initial begin
		clk_out = 0;
		clk_p0 = 0;
		clk_p180 = 0;
	end
	parameter USE_FLOATING_DIVIDE = 1;
	parameter INPUT_CLK_HZ = 100000000;
	parameter OUTPUT_CLK_HZ = 3579545;
	generate
		if ((OUTPUT_CLK_HZ * 2) > INPUT_CLK_HZ) begin : genblk1
			initial begin
				$display("");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXX                                                       XXXXX");
				$display("  XXXXX   BHG_FP_clk_divider.v inoperable parameter error.    XXXXX");
				$display("  XXXXX   https://github.com/BrianHGinc                       XXXXX");
				$display("  XXXXX                                                       XXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("  XXXXX                                                                                                        XXXXX");
				$display("  XXXXX   BHG_FP_clk_divider.v can only generate an output clock at least 1/2 the source clock frequency.      XXXXX");
				$display("  XXXXX                                                                                                        XXXXX");
				$display("  XXXXX   Your current set parameters:                                                                         XXXXX");
				$display("  XXXXX                                                                                                        XXXXX");
				$display("  XXXXX     .INPUT_CLK_HZ  = %d Hz.                                                                   XXXXX", INPUT_CLK_HZ);
				$display("  XXXXX     .OUTPUT_CLK_HZ = %d Hz.                                                                   XXXXX", OUTPUT_CLK_HZ);
				$display("  XXXXX                                                                                                        XXXXX");
				$display("  XXXXX   .OUTPUT_CLK_HZ must be less than or equal to .INPUT_CLK_HZ/2 for BHG_FP_clk_divider.v to function.   XXXXX");
				$display("  XXXXX                                                                                                        XXXXX");
				$display("  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("Warning [%0t] /home/epsilon/work/boxlambda/gw/components/ym2149/../../../sub/ym2149_psg_system/BHG_FP_clk_divider.v:76:1 - BHG_FP_clk_divider.genblk1.<unnamed_block>\n msg: ", $time, "  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
				$display("Error [%0t] /home/epsilon/work/boxlambda/gw/components/ym2149/../../../sub/ym2149_psg_system/BHG_FP_clk_divider.v:77:1 - BHG_FP_clk_divider.genblk1.<unnamed_block>", $time);
				$stop;
			end
		end
	endgenerate
	localparam [63:0] clk_per_x65kX10 = (INPUT_CLK_HZ * 655360) / OUTPUT_CLK_HZ;
	localparam [63:0] clk_per_x65k = (clk_per_x65kX10 + 5) / 10;
	localparam [63:0] clk_per_round = clk_per_x65k + 32768;
	localparam [23:0] clk_per_int = (USE_FLOATING_DIVIDE ? clk_per_x65k[39:16] : clk_per_round[39:16]);
	localparam [15:0] clk_per_f = (USE_FLOATING_DIVIDE ? clk_per_x65k[15:0] : 16'd0);
	localparam [63:0] clk_tru_hzX100 = (INPUT_CLK_HZ * 6553600) / ((clk_per_int * 65536) + clk_per_f);
	localparam signed clk_dif_hzX100 = clk_tru_hzX100 - (OUTPUT_CLK_HZ * 100);
	localparam signed clk_ppm_x100 = (clk_dif_hzX100 * OUTPUT_CLK_HZ) / 1000000;
	localparam [63:0] clk_jitterx100 = (clk_per_f == 0 ? 0 : (38'sd100000000000 / INPUT_CLK_HZ) / 2);
	localparam real f_tru = clk_tru_hzX100;
	localparam real f_ppm = clk_ppm_x100;
	localparam real f_jit = clk_jitterx100;
	localparam real f_div = clk_per_f;
	localparam pmb = $clog2(clk_per_int);
	localparam mb = (pmb < 2 ? 2 : pmb);
	reg [mb - 1:0] clk_cnt_m;
	reg [16:0] clk_cnt_n = 17'd0;
	initial begin
		clk_cnt_m = {mb {1'b0}};
		clk_cnt_n = 17'd0;
	end
	always @(posedge clk_in)
		if (rst_in) begin
			clk_cnt_m <= {mb {1'b0}};
			clk_cnt_n <= 17'd0;
			clk_out <= 1'b0;
			clk_p0 <= 1'b0;
			clk_p180 <= 1'b0;
		end
		else begin
			if (clk_cnt_m == (clk_per_int[mb - 1:0] - !clk_cnt_n[16])) begin
				clk_cnt_m <= {mb {1'b0}};
				clk_p0 <= 1'b1;
				clk_out <= 1'b1;
				clk_cnt_n <= clk_cnt_n[15:0] + clk_per_f;
			end
			else begin
				clk_cnt_m <= 1'b1 + clk_cnt_m;
				clk_p0 <= 1'b0;
				if (clk_cnt_m[mb - 2:0] == (clk_per_int[mb - 1:1] - 1))
					clk_out <= 1'b0;
			end
			if (clk_cnt_m[mb - 2:0] == (clk_per_int[mb - 1:1] - 1))
				clk_p180 <= 1'b1;
			else
				clk_p180 <= 1'b0;
		end
endmodule
