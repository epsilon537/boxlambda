module vram_if (
	clk,
	if0_addr,
	if0_wrdata,
	if0_rddata,
	if0_wrbytesel,
	if0_strobe,
	if0_write,
	if0_ack,
	if1_addr,
	if1_rddata,
	if1_strobe,
	if1_ack,
	if2_addr,
	if2_rddata,
	if2_strobe,
	if2_ack,
	if3_addr,
	if3_rddata,
	if3_strobe,
	if3_ack
);
	parameter VRAM_SIZE_BYTES = 131072;
	input wire clk;
	input wire [14:0] if0_addr;
	input wire [31:0] if0_wrdata;
	output wire [31:0] if0_rddata;
	input wire [3:0] if0_wrbytesel;
	input wire if0_strobe;
	input wire if0_write;
	output reg if0_ack;
	input wire [14:0] if1_addr;
	output wire [31:0] if1_rddata;
	input wire if1_strobe;
	output reg if1_ack;
	input wire [14:0] if2_addr;
	output wire [31:0] if2_rddata;
	input wire if2_strobe;
	output reg if2_ack;
	input wire [14:0] if3_addr;
	output wire [31:0] if3_rddata;
	input wire if3_strobe;
	output reg if3_ack;
	reg [14:0] ram_addr;
	wire [31:0] ram_wrdata;
	wire [3:0] ram_wrbytesel;
	wire [31:0] ram_rddata;
	wire ram_write;
	main_ram_generic #(.VRAM_SIZE_BYTES(VRAM_SIZE_BYTES)) main_ram(
		.clk(clk),
		.bus_addr(ram_addr),
		.bus_wrdata(ram_wrdata),
		.bus_wrbytesel(ram_wrbytesel),
		.bus_rddata(ram_rddata),
		.bus_write(ram_write)
	);
	reg if0_ack_next;
	reg if1_ack_next;
	reg if2_ack_next;
	reg if3_ack_next;
	reg [1:0] selector_reg;
	reg [1:0] selector_next;
	assign ram_wrdata = if0_wrdata;
	assign ram_write = (if0_strobe && if0_write) && (selector_reg == 2'h0);
	assign ram_wrbytesel = if0_wrbytesel;
	initial selector_reg = 2'h0;
	always @(*) begin
		selector_next = selector_reg + 1;
		ram_addr = 15'b000000000000000;
		if0_ack_next = 1'b0;
		if1_ack_next = 1'b0;
		if2_ack_next = 1'b0;
		if3_ack_next = 1'b0;
		case (selector_reg)
			2'h0:
				if (if0_strobe) begin
					ram_addr = if0_addr;
					if0_ack_next = 1'b1;
				end
			2'h1:
				if (if1_strobe) begin
					ram_addr = if1_addr;
					if1_ack_next = 1'b1;
				end
			2'h2:
				if (if2_strobe) begin
					ram_addr = if2_addr;
					if2_ack_next = 1'b1;
				end
			2'h3:
				if (if3_strobe) begin
					ram_addr = if3_addr;
					if3_ack_next = 1'b1;
				end
		endcase
	end
	always @(posedge clk) begin
		selector_reg <= selector_next;
		if0_ack <= if0_ack_next;
		if1_ack <= if1_ack_next;
		if2_ack <= if2_ack_next;
		if3_ack <= if3_ack_next;
	end
	assign if0_rddata = ram_rddata;
	assign if1_rddata = ram_rddata;
	assign if2_rddata = ram_rddata;
	assign if3_rddata = ram_rddata;
endmodule
