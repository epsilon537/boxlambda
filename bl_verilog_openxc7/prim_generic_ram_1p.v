module prim_generic_ram_1p (
	clk_i,
	req_i,
	write_i,
	addr_i,
	wdata_i,
	wmask_i,
	rdata_o,
	cfg_i
);
	parameter signed [31:0] Width = 32;
	parameter signed [31:0] Depth = 128;
	parameter signed [31:0] DataBitsPerMask = 1;
	parameter MemInitFile = "";
	localparam signed [31:0] Aw = $clog2(Depth);
	input wire clk_i;
	input wire req_i;
	input wire write_i;
	input wire [Aw - 1:0] addr_i;
	input wire [Width - 1:0] wdata_i;
	input wire [Width - 1:0] wmask_i;
	output reg [Width - 1:0] rdata_o;
	input wire [9:0] cfg_i;
	wire unused_cfg;
	assign unused_cfg = ^cfg_i;
	localparam signed [31:0] MaskWidth = Width / DataBitsPerMask;
	reg [Width - 1:0] mem [0:Depth - 1];
	wire [MaskWidth - 1:0] wmask;
	genvar _gv_k_5;
	generate
		for (_gv_k_5 = 0; _gv_k_5 < MaskWidth; _gv_k_5 = _gv_k_5 + 1) begin : gen_wmask
			localparam k = _gv_k_5;
			assign wmask[k] = &wmask_i[k * DataBitsPerMask+:DataBitsPerMask];
		end
	endgenerate
	always @(posedge clk_i)
		if (req_i) begin
			if (write_i) begin : sv2v_autoblock_1
				reg signed [31:0] i;
				for (i = 0; i < MaskWidth; i = i + 1)
					if (wmask[i])
						mem[addr_i][i * DataBitsPerMask+:DataBitsPerMask] <= wdata_i[i * DataBitsPerMask+:DataBitsPerMask];
			end
			else
				rdata_o <= mem[addr_i];
		end
	initial begin : sv2v_autoblock_2
		reg show_mem_paths;
		if (MemInitFile != "") begin : gen_meminit
			$display("Initializing memory %m from file '%s'.", MemInitFile);
			$readmemh(MemInitFile, mem);
		end
	end
endmodule
