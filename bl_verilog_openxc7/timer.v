module timer (
	clk_i,
	rst_ni,
	timer_req_i,
	timer_addr_i,
	timer_we_i,
	timer_be_i,
	timer_wdata_i,
	timer_rvalid_o,
	timer_rdata_o,
	timer_err_o,
	timer_intr_o
);
	reg _sv2v_0;
	parameter [31:0] DataWidth = 32;
	parameter [31:0] AddressWidth = 32;
	input wire clk_i;
	input wire rst_ni;
	input wire timer_req_i;
	input wire [AddressWidth - 1:0] timer_addr_i;
	input wire timer_we_i;
	input wire [(DataWidth / 8) - 1:0] timer_be_i;
	input wire [DataWidth - 1:0] timer_wdata_i;
	output wire timer_rvalid_o;
	output wire [DataWidth - 1:0] timer_rdata_o;
	output wire timer_err_o;
	output wire timer_intr_o;
	localparam [31:0] TW = 64;
	localparam [31:0] ADDR_OFFSET = 10;
	localparam [9:0] MTIME_LOW = 0;
	localparam [9:0] MTIME_HIGH = 4;
	localparam [9:0] MTIMECMP_LOW = 8;
	localparam [9:0] MTIMECMP_HIGH = 12;
	wire timer_we;
	wire mtime_we;
	wire mtimeh_we;
	wire mtimecmp_we;
	wire mtimecmph_we;
	wire [DataWidth - 1:0] mtime_wdata;
	wire [DataWidth - 1:0] mtimeh_wdata;
	wire [DataWidth - 1:0] mtimecmp_wdata;
	wire [DataWidth - 1:0] mtimecmph_wdata;
	reg [63:0] mtime_q;
	wire [63:0] mtime_d;
	wire [63:0] mtime_inc;
	reg [63:0] mtimecmp_q;
	wire [63:0] mtimecmp_d;
	reg interrupt_q;
	wire interrupt_d;
	reg error_q;
	reg error_d;
	reg [DataWidth - 1:0] rdata_q;
	reg [DataWidth - 1:0] rdata_d;
	reg rvalid_q;
	assign timer_we = timer_req_i & timer_we_i;
	assign mtime_inc = mtime_q + 64'd1;
	genvar _gv_b_4;
	generate
		for (_gv_b_4 = 0; _gv_b_4 < (DataWidth / 8); _gv_b_4 = _gv_b_4 + 1) begin : gen_byte_wdata
			localparam b = _gv_b_4;
			assign mtime_wdata[b * 8+:8] = (timer_be_i[b] ? timer_wdata_i[b * 8+:8] : mtime_q[b * 8+:8]);
			assign mtimeh_wdata[b * 8+:8] = (timer_be_i[b] ? timer_wdata_i[b * 8+:8] : mtime_q[DataWidth + (b * 8)+:8]);
			assign mtimecmp_wdata[b * 8+:8] = (timer_be_i[b] ? timer_wdata_i[b * 8+:8] : mtimecmp_q[b * 8+:8]);
			assign mtimecmph_wdata[b * 8+:8] = (timer_be_i[b] ? timer_wdata_i[b * 8+:8] : mtimecmp_q[DataWidth + (b * 8)+:8]);
		end
	endgenerate
	assign mtime_we = timer_we & (timer_addr_i[9:0] == MTIME_LOW);
	assign mtimeh_we = timer_we & (timer_addr_i[9:0] == MTIME_HIGH);
	assign mtimecmp_we = timer_we & (timer_addr_i[9:0] == MTIMECMP_LOW);
	assign mtimecmph_we = timer_we & (timer_addr_i[9:0] == MTIMECMP_HIGH);
	assign mtime_d = {(mtimeh_we ? mtimeh_wdata : mtime_inc[63:32]), (mtime_we ? mtime_wdata : mtime_inc[31:0])};
	assign mtimecmp_d = {(mtimecmph_we ? mtimecmph_wdata : mtimecmp_q[63:32]), (mtimecmp_we ? mtimecmp_wdata : mtimecmp_q[31:0])};
	always @(posedge clk_i or negedge rst_ni)
		if (~rst_ni)
			mtime_q <= 'b0;
		else
			mtime_q <= mtime_d;
	always @(posedge clk_i or negedge rst_ni)
		if (~rst_ni)
			mtimecmp_q <= 'b0;
		else if (mtimecmp_we | mtimecmph_we)
			mtimecmp_q <= mtimecmp_d;
	assign interrupt_d = ((mtime_q >= mtimecmp_q) | interrupt_q) & ~(mtimecmp_we | mtimecmph_we);
	always @(posedge clk_i or negedge rst_ni)
		if (~rst_ni)
			interrupt_q <= 'b0;
		else
			interrupt_q <= interrupt_d;
	assign timer_intr_o = interrupt_q;
	always @(*) begin
		if (_sv2v_0)
			;
		rdata_d = 'b0;
		error_d = 1'b0;
		(* full_case, parallel_case *)
		case (timer_addr_i[9:0])
			MTIME_LOW: rdata_d = mtime_q[31:0];
			MTIME_HIGH: rdata_d = mtime_q[63:32];
			MTIMECMP_LOW: rdata_d = mtimecmp_q[31:0];
			MTIMECMP_HIGH: rdata_d = mtimecmp_q[63:32];
			default: begin
				rdata_d = 'b0;
				error_d = 1'b1;
			end
		endcase
	end
	always @(posedge clk_i)
		if (timer_req_i) begin
			rdata_q <= rdata_d;
			error_q <= error_d;
		end
	assign timer_rdata_o = rdata_q;
	always @(posedge clk_i or negedge rst_ni)
		if (!rst_ni)
			rvalid_q <= 1'b0;
		else
			rvalid_q <= timer_req_i;
	assign timer_rvalid_o = rvalid_q;
	assign timer_err_o = error_q;
	initial _sv2v_0 = 0;
endmodule
