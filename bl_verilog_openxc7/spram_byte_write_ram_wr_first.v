`default_nettype none
module spram_byte_write_ram_wr_first (
	addra,
	dina,
	clka,
	wea,
	douta
);
	parameter NB_COL = 4;
	parameter COL_WIDTH = 9;
	parameter RAM_DEPTH = 1024;
	parameter RAM_PERFORMANCE = "HIGH_PERFORMANCE";
	parameter INIT_FILE = "";
	function integer clogb2;
		input integer depth;
		for (clogb2 = 0; depth > 0; clogb2 = clogb2 + 1)
			depth = depth >> 1;
	endfunction
	input wire [clogb2(RAM_DEPTH - 1) - 1:0] addra;
	input wire [(NB_COL * COL_WIDTH) - 1:0] dina;
	input wire clka;
	input wire [NB_COL - 1:0] wea;
	output wire [(NB_COL * COL_WIDTH) - 1:0] douta;
	reg [(NB_COL * COL_WIDTH) - 1:0] BRAM [RAM_DEPTH - 1:0];
	reg [(NB_COL * COL_WIDTH) - 1:0] ram_data = {NB_COL * COL_WIDTH {1'b0}};
	genvar _gv_i_37;
	generate
		for (_gv_i_37 = 0; _gv_i_37 < NB_COL; _gv_i_37 = _gv_i_37 + 1) begin : byte_write
			localparam i = _gv_i_37;
			always @(posedge clka)
				if (wea[i]) begin
					BRAM[addra][((i + 1) * COL_WIDTH) - 1:i * COL_WIDTH] <= dina[((i + 1) * COL_WIDTH) - 1:i * COL_WIDTH];
					ram_data[((i + 1) * COL_WIDTH) - 1:i * COL_WIDTH] <= dina[((i + 1) * COL_WIDTH) - 1:i * COL_WIDTH];
				end
				else
					ram_data[((i + 1) * COL_WIDTH) - 1:i * COL_WIDTH] <= BRAM[addra][((i + 1) * COL_WIDTH) - 1:i * COL_WIDTH];
		end
		if (RAM_PERFORMANCE == "LOW_LATENCY") begin : no_output_register
			assign douta = ram_data;
		end
		else begin : output_register
			reg [(NB_COL * COL_WIDTH) - 1:0] douta_reg = {NB_COL * COL_WIDTH {1'b0}};
			always @(posedge clka) douta_reg <= ram_data;
			assign douta = douta_reg;
		end
	endgenerate
endmodule
