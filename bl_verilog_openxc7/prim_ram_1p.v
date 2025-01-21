module prim_ram_1p (
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
	output wire [Width - 1:0] rdata_o;
	input wire [9:0] cfg_i;
	localparam integer Impl = 32'sd1;
	generate
		if (Impl == 32'sd2) begin : gen_badbit
			prim_badbit_ram_1p #(
				.DataBitsPerMask(DataBitsPerMask),
				.Depth(Depth),
				.MemInitFile(MemInitFile),
				.Width(Width)
			) u_impl_badbit(
				.clk_i(clk_i),
				.req_i(req_i),
				.write_i(write_i),
				.addr_i(addr_i),
				.wdata_i(wdata_i),
				.wmask_i(wmask_i),
				.rdata_o(rdata_o)
			);
		end
		else begin : gen_generic
			prim_generic_ram_1p #(
				.DataBitsPerMask(DataBitsPerMask),
				.Depth(Depth),
				.MemInitFile(MemInitFile),
				.Width(Width)
			) u_impl_generic(
				.clk_i(clk_i),
				.req_i(req_i),
				.write_i(write_i),
				.addr_i(addr_i),
				.wdata_i(wdata_i),
				.wmask_i(wmask_i),
				.rdata_o(rdata_o),
				.cfg_i(cfg_i)
			);
		end
	endgenerate
endmodule
