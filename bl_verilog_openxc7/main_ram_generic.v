module main_ram_generic (
	clk,
	bus_addr,
	bus_wrdata,
	bus_wrbytesel,
	bus_rddata,
	bus_write
);
	parameter VRAM_SIZE_BYTES = 131072;
	input wire clk;
	input wire [14:0] bus_addr;
	input wire [31:0] bus_wrdata;
	input wire [3:0] bus_wrbytesel;
	output wire [31:0] bus_rddata;
	input wire bus_write;
	spram_byte_write_ram_wr_first #(
		.NB_COL(4),
		.COL_WIDTH(8),
		.RAM_DEPTH(VRAM_SIZE_BYTES / 4),
		.RAM_PERFORMANCE("LOW_LATENCY"),
		.INIT_FILE("")
	) spram_inst(
		.addra(bus_addr),
		.dina(bus_wrdata),
		.clka(clk),
		.wea((bus_write ? bus_wrbytesel : 4'b0000)),
		.douta(bus_rddata)
	);
endmodule
