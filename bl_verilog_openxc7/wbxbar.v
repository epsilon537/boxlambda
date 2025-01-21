`default_nettype none
module wbxbar (
	i_clk,
	i_reset,
	i_mcyc,
	i_mstb,
	i_mwe,
	i_maddr,
	i_mdata,
	i_msel,
	o_mstall,
	o_mack,
	o_mdata,
	o_merr,
	o_scyc,
	o_sstb,
	o_swe,
	o_saddr,
	o_sdata,
	o_ssel,
	i_sstall,
	i_sack,
	i_sdata,
	i_serr
);
	parameter NM = 4;
	parameter NS = 8;
	parameter AW = 32;
	parameter DW = 32;
	parameter [(NS * AW) - 1:0] SLAVE_ADDR = {3'b111, {AW - 3 {1'b0}}, 3'b110, {AW - 3 {1'b0}}, 3'b101, {AW - 3 {1'b0}}, 3'b100, {AW - 3 {1'b0}}, 3'b011, {AW - 3 {1'b0}}, 3'b010, {AW - 3 {1'b0}}, 4'b0010, {AW - 4 {1'b0}}, 4'b0000, {AW - 4 {1'b0}}};
	parameter [(NS * AW) - 1:0] SLAVE_MASK = (NS <= 1 ? 0 : {{NS - 2 {3'b111, {AW - 3 {1'b0}}}}, {2 {4'b1111, {AW - 4 {1'b0}}}}});
	parameter LGMAXBURST = 6;
	parameter OPT_TIMEOUT = 0;
	parameter [0:0] OPT_STARVATION_TIMEOUT = 1'd0;
	parameter [0:0] OPT_DBLBUFFER = 1'b0;
	parameter [0:0] OPT_LOWPOWER = 1'b1;
	parameter [0:0] OPT_ACK_INVALID_ADDR = 1'b0;
	parameter [31:0] ERROR_DATA_PATTERN = 32'hdeadbeef;
	input wire i_clk;
	input wire i_reset;
	input wire [NM - 1:0] i_mcyc;
	input wire [NM - 1:0] i_mstb;
	input wire [NM - 1:0] i_mwe;
	input wire [(NM * AW) - 1:0] i_maddr;
	input wire [(NM * DW) - 1:0] i_mdata;
	input wire [((NM * DW) / 8) - 1:0] i_msel;
	output wire [NM - 1:0] o_mstall;
	output wire [NM - 1:0] o_mack;
	output reg [(NM * DW) - 1:0] o_mdata;
	output wire [NM - 1:0] o_merr;
	output reg [NS - 1:0] o_scyc;
	output reg [NS - 1:0] o_sstb;
	output reg [NS - 1:0] o_swe;
	output reg [(NS * AW) - 1:0] o_saddr;
	output reg [(NS * DW) - 1:0] o_sdata;
	output reg [((NS * DW) / 8) - 1:0] o_ssel;
	input wire [NS - 1:0] i_sstall;
	input wire [NS - 1:0] i_sack;
	input wire [(NS * DW) - 1:0] i_sdata;
	input wire [NS - 1:0] i_serr;
	localparam TIMEOUT_WIDTH = $clog2(OPT_TIMEOUT);
	localparam LGNM = (NM > 1 ? $clog2(NM) : 1);
	localparam LGNS = $clog2(NS + 1);
	wire [NS:0] request [0:NM - 1];
	reg [NS - 1:0] requested [0:NM - 1];
	reg [NS:0] grant [0:NM - 1];
	reg [NM - 1:0] mgrant;
	reg [NS - 1:0] sgrant;
	wire [LGMAXBURST - 1:0] w_mpending [0:NM - 1];
	reg [NM - 1:0] mfull;
	reg [NM - 1:0] mnearfull;
	reg [NM - 1:0] mempty;
	wire [NM - 1:0] timed_out;
	localparam NMFULL = (NM > 1 ? 1 << LGNM : 1);
	localparam NSFULL = 1 << LGNS;
	wire [LGNS - 1:0] mindex [0:NMFULL - 1];
	wire [LGNM - 1:0] sindex [0:NSFULL - 1];
	wire [NMFULL - 1:0] m_cyc;
	wire [NMFULL - 1:0] m_stb;
	wire [NMFULL - 1:0] m_we;
	wire [AW - 1:0] m_addr [0:NMFULL - 1];
	wire [DW - 1:0] m_data [0:NMFULL - 1];
	wire [(DW / 8) - 1:0] m_sel [0:NMFULL - 1];
	reg [NM - 1:0] m_stall;
	wire [NSFULL - 1:0] s_stall;
	wire [DW - 1:0] s_data [0:NSFULL - 1];
	wire [NSFULL - 1:0] s_ack;
	wire [NSFULL - 1:0] s_err;
	wire [NM - 1:0] dcd_stb;
	localparam [0:0] OPT_BUFFER_DECODER = (NS != 1) || (SLAVE_MASK != 0);
	integer iN;
	integer iM;
	genvar _gv_N_1;
	genvar _gv_M_1;
	generate
		for (_gv_N_1 = 0; _gv_N_1 < NM; _gv_N_1 = _gv_N_1 + 1) begin : DECODE_REQUEST
			localparam N = _gv_N_1;
			wire skd_stb;
			wire skd_stall;
			wire skd_we;
			wire [AW - 1:0] skd_addr;
			wire [DW - 1:0] skd_data;
			wire [(DW / 8) - 1:0] skd_sel;
			wire [NS:0] decoded;
			wire iskd_ready;
			skidbuffer #(
				.DW(((1 + AW) + DW) + (DW / 8)),
				.OPT_OUTREG(0)
			) iskid(
				.i_clk(i_clk),
				.i_reset(i_reset || !i_mcyc[N]),
				.i_valid(i_mstb[N]),
				.o_ready(iskd_ready),
				.i_data({i_mwe[N], i_maddr[N * AW+:AW], i_mdata[N * DW+:DW], i_msel[(N * DW) / 8+:DW / 8]}),
				.o_valid(skd_stb),
				.i_ready(!skd_stall),
				.o_data({skd_we, skd_addr, skd_data, skd_sel})
			);
			assign o_mstall[N] = !iskd_ready;
			addrdecode #(
				.NS(NS),
				.AW(AW),
				.DW((DW + (DW / 8)) + 1),
				.SLAVE_ADDR(SLAVE_ADDR),
				.SLAVE_MASK(SLAVE_MASK),
				.OPT_REGISTERED(OPT_BUFFER_DECODER)
			) adcd(
				.i_clk(i_clk),
				.i_reset(i_reset),
				.i_valid(skd_stb && i_mcyc[N]),
				.o_stall(skd_stall),
				.i_addr(skd_addr),
				.i_data({skd_we, skd_data, skd_sel}),
				.o_valid(dcd_stb[N]),
				.i_stall(m_stall[N] && i_mcyc[N]),
				.o_decode(decoded),
				.o_addr(m_addr[N]),
				.o_data({m_we[N], m_data[N], m_sel[N]})
			);
			assign request[N] = (m_cyc[N] && dcd_stb[N] ? decoded : 0);
			assign m_cyc[N] = i_mcyc[N];
			assign m_stb[N] = (i_mcyc[N] && dcd_stb[N]) && !mfull[N];
		end
		for (_gv_N_1 = NM; _gv_N_1 < NMFULL; _gv_N_1 = _gv_N_1 + 1) begin : UNUSED_MASTER_SIGNALS
			localparam N = _gv_N_1;
			assign m_cyc[N] = 0;
			assign m_stb[N] = 0;
			assign m_we[N] = 0;
			assign m_addr[N] = 0;
			assign m_data[N] = 0;
			assign m_sel[N] = 0;
		end
	endgenerate
	always @(*)
		for (iM = 0; iM < NS; iM = iM + 1)
			begin
				requested[0][iM] = 0;
				for (iN = 1; iN < NM; iN = iN + 1)
					begin
						requested[iN][iM] = requested[iN - 1][iM];
						if (request[iN - 1][iM] && (grant[iN - 1][iM] || (!mgrant[iN - 1] || mempty[iN - 1])))
							requested[iN][iM] = 1;
					end
			end
	initial sgrant = 0;
	generate
		for (_gv_M_1 = 0; _gv_M_1 < NS; _gv_M_1 = _gv_M_1 + 1) begin : SLAVE_GRANT
			localparam M = _gv_M_1;
			reg drop_sgrant;
			always @(*) begin
				drop_sgrant = !m_cyc[sindex[M]];
				if ((!request[sindex[M]][M] && m_stb[sindex[M]]) && mempty[sindex[M]])
					drop_sgrant = 1;
				if (!sgrant[M])
					drop_sgrant = 0;
				if (i_reset)
					drop_sgrant = 1;
			end
			always @(posedge i_clk) begin
				sgrant[M] <= sgrant[M];
				for (iN = 0; iN < NM; iN = iN + 1)
					if (request[iN][M] && (!mgrant[iN] || mempty[iN]))
						sgrant[M] <= 1;
				if (drop_sgrant)
					sgrant[M] <= 0;
			end
			assign s_data[M] = i_sdata[M * DW+:DW];
			assign s_stall[M] = o_sstb[M] && i_sstall[M];
			assign s_ack[M] = o_scyc[M] && i_sack[M];
			assign s_err[M] = o_scyc[M] && i_serr[M];
		end
		for (_gv_M_1 = NS; _gv_M_1 < NSFULL; _gv_M_1 = _gv_M_1 + 1) begin : UNUSED_SLAVE_SIGNALS
			localparam M = _gv_M_1;
			assign s_data[M] = 0;
			assign s_stall[M] = 1;
			assign s_ack[M] = 0;
			assign s_err[M] = 1;
		end
	endgenerate
	initial mgrant = 0;
	generate
		for (_gv_N_1 = 0; _gv_N_1 < NM; _gv_N_1 = _gv_N_1 + 1) begin : ARBITRATE_REQUESTS
			localparam N = _gv_N_1;
			wire [NS:0] regrant;
			wire [LGNS - 1:0] reindex;
			reg stay_on_channel;
			reg requested_channel_is_available;
			always @(*) begin
				stay_on_channel = |(request[N] & grant[N]);
				if (mgrant[N] && !mempty[N])
					stay_on_channel = 1;
			end
			always @(*) begin
				requested_channel_is_available = |((request[N][NS - 1:0] & ~sgrant) & ~requested[N][NS - 1:0]);
				if (request[N][NS])
					requested_channel_is_available = 1;
				if (NM < 2)
					requested_channel_is_available = m_stb[N];
			end
			initial grant[N] = 0;
			always @(posedge i_clk)
				if (i_reset || !i_mcyc[N]) begin
					grant[N] <= 0;
					mgrant[N] <= 0;
				end
				else if (!stay_on_channel) begin
					if (requested_channel_is_available) begin
						mgrant[N] <= 1'b1;
						grant[N] <= request[N];
					end
					else if (m_stb[N]) begin
						mgrant[N] <= 1'b0;
						grant[N] <= 0;
					end
				end
			if (NS == 1) begin : MINDEX_ONE_SLAVE
				assign mindex[N] = 0;
				assign regrant = 0;
				assign reindex = 0;
			end
			else begin : MINDEX_MULTIPLE_SLAVES
				reg [LGNS - 1:0] r_mindex;
				reg [NS:0] r_regrant;
				reg [LGNS - 1:0] r_reindex;
				always @(*) begin
					r_regrant = 0;
					for (iM = 0; iM < NS; iM = iM + 1)
						begin
							if (grant[N][iM])
								r_regrant[iM] = 1'b1;
							else if (!sgrant[iM] && !requested[N][iM])
								r_regrant[iM] = 1'b1;
							if (!request[N][iM])
								r_regrant[iM] = 1'b0;
						end
					if (grant[N][NS])
						r_regrant[NS] = 1;
					if (!request[N][NS])
						r_regrant[NS] = 0;
					if (mgrant[N] && !mempty[N])
						r_regrant = 0;
				end
				always @(*) begin
					r_reindex = 0;
					for (iM = 0; iM <= NS; iM = iM + 1)
						if (r_regrant[iM])
							r_reindex = r_reindex | iM[LGNS - 1:0];
					if (regrant == 0)
						r_reindex = r_mindex;
				end
				always @(posedge i_clk) r_mindex <= reindex;
				assign reindex = r_reindex;
				assign regrant = r_regrant;
				assign mindex[N] = r_mindex;
			end
		end
		for (_gv_N_1 = NM; _gv_N_1 < NMFULL; _gv_N_1 = _gv_N_1 + 1) begin : UNUSED_MINDEXES
			localparam N = _gv_N_1;
			assign mindex[N] = 0;
		end
		for (_gv_M_1 = 0; _gv_M_1 < NS; _gv_M_1 = _gv_M_1 + 1) begin : GEN_SINDEX
			localparam M = _gv_M_1;
			if (NM <= 1) begin : SINDEX_SINGLE_MASTER
				assign sindex[M] = 0;
			end
			else begin : SINDEX_MORE_THAN_ONE_MASTER
				reg [LGNM - 1:0] r_sindex;
				reg [NM - 1:0] regrant;
				reg [LGNM - 1:0] reindex;
				always @(*) begin
					regrant = 0;
					for (iN = 0; iN < NM; iN = iN + 1)
						begin
							if (grant[iN][M])
								regrant[iN] = 1;
							else if (!sgrant[M] && !requested[iN][M])
								regrant[iN] = 1;
							if (!request[iN][M])
								regrant[iN] = 0;
							if (mgrant[iN] && !mempty[iN])
								regrant[iN] = 0;
						end
				end
				always @(*) begin
					reindex = 0;
					if (regrant == 0)
						reindex = sindex[M];
					else
						for (iN = 0; iN < NM; iN = iN + 1)
							if (regrant[iN])
								reindex = reindex | iN[LGNM - 1:0];
				end
				always @(posedge i_clk) r_sindex <= reindex;
				assign sindex[M] = r_sindex;
			end
		end
		for (_gv_M_1 = NS; _gv_M_1 < NSFULL; _gv_M_1 = _gv_M_1 + 1) begin : UNUSED_SINDEXES
			localparam M = _gv_M_1;
			assign sindex[M] = 0;
		end
	endgenerate
	initial o_scyc = 0;
	initial o_sstb = 0;
	generate
		for (_gv_M_1 = 0; _gv_M_1 < NS; _gv_M_1 = _gv_M_1 + 1) begin : GEN_CYC_STB
			localparam M = _gv_M_1;
			always @(posedge i_clk) begin
				if (sgrant[M]) begin
					if (!i_mcyc[sindex[M]]) begin
						o_scyc[M] <= 1'b0;
						o_sstb[M] <= 1'b0;
					end
					else begin
						o_scyc[M] <= 1'b1;
						if (!o_sstb[M] || !s_stall[M])
							o_sstb[M] <= request[sindex[M]][M] && !mfull[sindex[M]];
					end
				end
				else begin
					o_scyc[M] <= 1'b0;
					o_sstb[M] <= 1'b0;
				end
				if (i_reset || s_err[M]) begin
					o_scyc[M] <= 1'b0;
					o_sstb[M] <= 1'b0;
				end
			end
		end
		if ((NM == 1) && !OPT_LOWPOWER) begin : ONE_MASTER
			reg r_swe;
			reg [AW - 1:0] r_saddr;
			reg [DW - 1:0] r_sdata;
			reg [(DW / 8) - 1:0] r_ssel;
			always @(posedge i_clk) begin
				r_swe <= o_swe[0];
				r_saddr <= o_saddr[0+:AW];
				r_sdata <= o_sdata[0+:DW];
				r_ssel <= o_ssel[0+:DW / 8];
				if (sgrant[mindex[0]] && !s_stall[mindex[0]]) begin
					r_swe <= m_we[0];
					r_saddr <= m_addr[0];
					r_sdata <= m_data[0];
					r_ssel <= m_sel[0];
				end
			end
			for (_gv_M_1 = 0; _gv_M_1 < NS; _gv_M_1 = _gv_M_1 + 1) begin : FOREACH_SLAVE_PORT
				localparam M = _gv_M_1;
				always @(*) begin
					o_swe[M] = r_swe;
					o_saddr[M * AW+:AW] = r_saddr[AW - 1:0];
					o_sdata[M * DW+:DW] = r_sdata[DW - 1:0];
					o_ssel[(M * DW) / 8+:DW / 8] = r_ssel[(DW / 8) - 1:0];
				end
			end
		end
		else begin : genblk10
			for (_gv_M_1 = 0; _gv_M_1 < NS; _gv_M_1 = _gv_M_1 + 1) begin : GEN_DOWNSTREAM
				localparam M = _gv_M_1;
				always @(posedge i_clk)
					if (OPT_LOWPOWER && !sgrant[M]) begin
						o_swe[M] <= 1'b0;
						o_saddr[M * AW+:AW] <= 0;
						o_sdata[M * DW+:DW] <= 0;
						o_ssel[M * (DW / 8)+:DW / 8] <= 0;
					end
					else if (!s_stall[M]) begin
						o_swe[M] <= m_we[sindex[M]];
						o_saddr[M * AW+:AW] <= m_addr[sindex[M]];
						if (OPT_LOWPOWER && !m_we[sindex[M]])
							o_sdata[M * DW+:DW] <= 0;
						else
							o_sdata[M * DW+:DW] <= m_data[sindex[M]];
						o_ssel[M * (DW / 8)+:DW / 8] <= m_sel[sindex[M]];
					end
			end
		end
		if (OPT_DBLBUFFER) begin : DOUBLE_BUFFERRED_STALL
			reg [NM - 1:0] r_mack;
			reg [NM - 1:0] r_merr;
			initial r_mack = 0;
			initial r_merr = 0;
			for (_gv_N_1 = 0; _gv_N_1 < NM; _gv_N_1 = _gv_N_1 + 1) begin : FOREACH_MASTER_PORT
				localparam N = _gv_N_1;
				always @(*) begin
					if (grant[N][NS])
						m_stall[N] = 1;
					else if (mgrant[N] && request[N][mindex[N]])
						m_stall[N] = mfull[N] || s_stall[mindex[N]];
					else
						m_stall[N] = m_stb[N];
					if (o_merr[N])
						m_stall[N] = 0;
				end
				always @(posedge i_clk) begin
					r_mack[N] <= mgrant[N] && s_ack[mindex[N]];
					r_merr[N] <= mgrant[N] && s_err[mindex[N]];
					if (OPT_LOWPOWER && !mgrant[N])
						o_mdata[N * DW+:DW] <= 0;
					else
						o_mdata[N * DW+:DW] <= s_data[mindex[N]];
					if ((OPT_ACK_INVALID_ADDR && mgrant[N]) && s_err[mindex[N]])
						o_mdata[N * DW+:DW] <= ERROR_DATA_PATTERN;
					if (grant[N][NS] || (timed_out[N] && !o_mack[N])) begin
						r_mack[N] <= 1'b0;
						r_merr[N] <= !o_merr[N];
					end
					if ((i_reset || !i_mcyc[N]) || o_merr[N]) begin
						r_mack[N] <= 1'b0;
						r_merr[N] <= 1'b0;
					end
				end
				assign o_mack[N] = (OPT_ACK_INVALID_ADDR ? r_mack[N] || r_merr[N] : r_mack[N]);
				assign o_merr[N] = !OPT_ACK_INVALID_ADDR && ((!OPT_STARVATION_TIMEOUT || i_mcyc[N]) && r_merr[N]);
			end
		end
		else if (NS == 1) begin : SINGLE_SLAVE
			for (_gv_N_1 = 0; _gv_N_1 < NM; _gv_N_1 = _gv_N_1 + 1) begin : FOREACH_MASTER_PORT
				localparam N = _gv_N_1;
				reg r_mack;
				reg r_merr;
				always @(*) begin
					m_stall[N] = (!mgrant[N] || s_stall[0]) || (m_stb[N] && !request[N][0]);
					r_mack = mgrant[N] && i_sack[0];
					r_merr = mgrant[N] && i_serr[0];
					o_mdata[N * DW+:DW] = (!mgrant[N] && OPT_LOWPOWER ? 0 : i_sdata);
					if (mfull[N])
						m_stall[N] = 1'b1;
					if (timed_out[N] && !r_mack) begin
						m_stall[N] = 1'b0;
						r_mack = 1'b0;
						r_merr = 1'b1;
					end
					if (grant[N][NS] && m_stb[N]) begin
						m_stall[N] = 1'b0;
						r_mack = 1'b0;
						r_merr = 1'b1;
					end
					if (!m_cyc[N]) begin
						r_mack = 1'b0;
						r_merr = 1'b0;
					end
					if (OPT_ACK_INVALID_ADDR && r_merr)
						o_mdata[N * DW+:DW] = ERROR_DATA_PATTERN;
				end
				assign o_mack[N] = (OPT_ACK_INVALID_ADDR ? r_mack || r_merr : r_mack);
				assign o_merr[N] = !OPT_ACK_INVALID_ADDR && r_merr;
			end
		end
		else begin : SINGLE_BUFFER_STALL
			for (_gv_N_1 = 0; _gv_N_1 < NM; _gv_N_1 = _gv_N_1 + 1) begin : FOREACH_MASTER_PORT
				localparam N = _gv_N_1;
				reg r_mack;
				reg r_merr;
				always @(*) begin
					m_stall[N] = 1;
					r_mack = mgrant[N] && s_ack[mindex[N]];
					r_merr = mgrant[N] && s_err[mindex[N]];
					if (OPT_LOWPOWER && !mgrant[N])
						o_mdata[N * DW+:DW] = 0;
					else
						o_mdata[N * DW+:DW] = s_data[mindex[N]];
					if (mgrant[N])
						m_stall[N] = s_stall[mindex[N]] || !request[N][mindex[N]];
					if (mfull[N])
						m_stall[N] = 1'b1;
					if (grant[N][NS] || (timed_out[N] && !r_mack)) begin
						m_stall[N] = 1'b0;
						r_mack = 1'b0;
						r_merr = 1'b1;
					end
					if (!m_cyc[N]) begin
						r_mack = 1'b0;
						r_merr = 1'b0;
					end
					if (OPT_ACK_INVALID_ADDR && r_merr)
						o_mdata[N * DW+:DW] = ERROR_DATA_PATTERN;
				end
				assign o_mack[N] = (OPT_ACK_INVALID_ADDR ? r_mack || r_merr : r_mack);
				assign o_merr[N] = !OPT_ACK_INVALID_ADDR && r_merr;
			end
		end
	endgenerate
	initial mfull = 0;
	initial mnearfull = 0;
	initial mempty = ~0;
	generate
		for (_gv_N_1 = 0; _gv_N_1 < NM; _gv_N_1 = _gv_N_1 + 1) begin : COUNT_PENDING_TRANSACTIONS
			localparam N = _gv_N_1;
			reg [LGMAXBURST - 1:0] lclpending;
			initial lclpending = 0;
			always @(posedge i_clk)
				if ((i_reset || !i_mcyc[N]) || o_merr[N]) begin
					lclpending <= 0;
					mfull[N] <= 0;
					mempty[N] <= 1'b1;
					mnearfull[N] <= 0;
				end
				else
					case ({m_stb[N] && !m_stall[N], o_mack[N]})
						2'b01: begin
							lclpending <= lclpending - 1'b1;
							mnearfull[N] <= mfull[N];
							mfull[N] <= 1'b0;
							mempty[N] <= lclpending == 1;
						end
						2'b10: begin
							lclpending <= lclpending + 1'b1;
							mnearfull[N] <= &lclpending[LGMAXBURST - 1:2] && (lclpending[1:0] != 0);
							mfull[N] <= mnearfull[N];
							mempty[N] <= 1'b0;
						end
						default:
							;
					endcase
			assign w_mpending[N] = lclpending;
		end
		if (OPT_TIMEOUT > 0) begin : CHECK_TIMEOUT
			for (_gv_N_1 = 0; _gv_N_1 < NM; _gv_N_1 = _gv_N_1 + 1) begin : FOREACH_MASTER_PORT
				localparam N = _gv_N_1;
				reg [TIMEOUT_WIDTH - 1:0] deadlock_timer;
				reg r_timed_out;
				initial deadlock_timer = OPT_TIMEOUT;
				initial r_timed_out = 0;
				always @(posedge i_clk)
					if (((((i_reset || !i_mcyc[N]) || ((w_mpending[N] == 0) && !m_stb[N])) || (m_stb[N] && !m_stall[N])) || (o_mack[N] || o_merr[N])) || (!OPT_STARVATION_TIMEOUT && !mgrant[N])) begin
						deadlock_timer <= OPT_TIMEOUT;
						r_timed_out <= 0;
					end
					else if (deadlock_timer > 0) begin
						deadlock_timer <= deadlock_timer - 1;
						r_timed_out <= deadlock_timer <= 1;
					end
				assign timed_out[N] = r_timed_out;
			end
		end
		else begin : NO_TIMEOUT
			assign timed_out = 0;
		end
	endgenerate
	initial begin : PARAMETER_CONSISTENCY_CHECK
		if (NM == 0) begin
			$display("ERROR: At least one master must be defined");
			$stop;
		end
		if (NS == 0) begin
			$display("ERROR: At least one slave must be defined");
			$stop;
		end
		if ((OPT_STARVATION_TIMEOUT != 0) && (OPT_TIMEOUT == 0)) begin
			$display("ERROR: The starvation timeout is implemented as part of the regular timeout");
			$display("  Without a timeout, the starvation timeout will not work");
			$stop;
		end
	end
endmodule
