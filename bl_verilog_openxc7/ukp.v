`default_nettype none
module ukp (
	usbrst_n,
	usbclk,
	usb_dp_i,
	usb_dm_i,
	usb_dp_o,
	usb_dm_o,
	usb_oe,
	req_branch_stb,
	ack_req_branch_stb,
	outr0,
	outr1,
	outr2,
	ukprdy,
	ukpstb,
	ukpdat,
	save,
	save_r,
	save_b,
	connected,
	conerr
);
	input wire usbrst_n;
	input wire usbclk;
	input wire usb_dp_i;
	input wire usb_dm_i;
	output wire usb_dp_o;
	output wire usb_dm_o;
	output wire usb_oe;
	input wire req_branch_stb;
	output reg ack_req_branch_stb;
	input wire [7:0] outr0;
	input wire [7:0] outr1;
	input wire [7:0] outr2;
	output reg ukprdy;
	output wire ukpstb;
	output reg [7:0] ukpdat;
	output reg save;
	output reg [2:0] save_r;
	output reg [2:0] save_b;
	output reg connected;
	output wire conerr;
	parameter S_OPCODE = 0;
	parameter S_LDI0 = 1;
	parameter S_LDI1 = 2;
	parameter S_B0 = 3;
	parameter S_B1 = 4;
	parameter S_B2 = 5;
	parameter S_S0 = 6;
	parameter S_S1 = 7;
	parameter S_S2 = 8;
	parameter S_TOGGLE0 = 9;
	parameter S_TOGGLE1 = 10;
	reg dpi;
	reg dmi;
	reg ukprdyd;
	wire [4:0] inst;
	reg [4:0] insth;
	wire sample;
	reg inst_ready;
	reg up = 0;
	reg um = 0;
	reg cond;
	reg nak;
	reg dmis = 0;
	reg ug;
	reg nrzon;
	reg bank = 0;
	reg record1 = 0;
	reg [1:0] mbit;
	reg [3:0] state;
	reg [7:0] wk = 0;
	reg [7:0] sb = 0;
	reg [3:0] sadr;
	reg [13:0] pc;
	reg [13:0] wpc;
	reg [2:0] timing;
	reg [3:0] lb4 = 0;
	reg [3:0] lb4w;
	reg [13:0] interval = 0;
	reg [6:0] bitadr;
	reg [7:0] data = 0;
	reg [2:0] nrztxct;
	reg [2:0] nrzrxct;
	wire interval_cy = interval == 12001;
	wire next = ~((state == S_OPCODE) & (((((inst == 2) & dmi) | (((inst == 4) || (inst == 5)) & (timing != 0))) | ((inst == 13) & (~sample | ((dpi | dmi) & (wk != 1))))) | ((inst == 14) & ~interval_cy)));
	wire branch = (state == S_B1) & cond;
	wire retpc = ((state == S_OPCODE) && (inst == 7) ? 1 : 0);
	wire jmppc = ((state == S_OPCODE) && (inst == 15) ? 1 : 0);
	wire dbit = sb[7 - sadr[2:0]];
	wire record;
	reg req_branch_reg;
	reg dmid;
	reg [23:0] conct;
	assign conerr = conct[23] || ~usbrst_n;
	usb_hid_host_rom ukprom(
		.clk(usbclk),
		.adr(pc),
		.data(inst)
	);
	initial begin
		pc = 0;
		connected = 0;
		cond = 0;
		inst_ready = 0;
		state = S_OPCODE;
		timing = 0;
		mbit = 0;
		bitadr = 0;
		nak = 1;
		ug = 0;
		req_branch_reg = 0;
		ack_req_branch_stb = 0;
	end
	always @(posedge usbclk)
		if (~usbrst_n) begin
			pc <= 0;
			connected <= 0;
			cond <= 0;
			inst_ready <= 0;
			state <= S_OPCODE;
			timing <= 0;
			mbit <= 0;
			bitadr <= 0;
			nak <= 1;
			ug <= 0;
			req_branch_reg <= 0;
			ack_req_branch_stb <= 0;
		end
		else begin
			ack_req_branch_stb <= 1'b0;
			if (req_branch_stb)
				req_branch_reg <= 1'b1;
			dpi <= usb_dp_i;
			dmi <= usb_dm_i;
			save <= 0;
			if (inst_ready) begin
				case (state)
					S_OPCODE: begin
						insth <= inst;
						if (inst == 1)
							state <= S_LDI0;
						if (inst == 3) begin
							sadr <= 3;
							state <= S_S0;
						end
						if (inst == 4) begin
							ug <= 1'b1;
							up <= 0;
							um <= 0;
						end
						if (inst == 5)
							ug <= 0;
						if (inst == 6) begin
							sadr <= 7;
							state <= S_S0;
						end
						if (inst[4:2] == 3'b010) begin
							state <= S_B0;
							case (inst[1:0])
								2'b00: cond <= ~dmi;
								2'b01: cond <= connected;
								2'b10: cond <= nak;
								2'b11: cond <= wk != 1;
							endcase
						end
						if ((inst == 11) | ((inst == 13) & sample))
							wk <= wk - 8'd1;
						if (inst == 15) begin
							state <= S_B2;
							cond <= 1;
						end
						if (inst == 12)
							state <= S_TOGGLE0;
						if (inst == 16) begin
							state <= S_B0;
							cond <= req_branch_reg;
							req_branch_reg <= 1'b0;
							ack_req_branch_stb <= 1'b1;
						end
						if (inst == 17) begin
							sadr <= 7;
							state <= S_S2;
							sb <= outr0;
							mbit <= 1;
						end
						if (inst == 18) begin
							sadr <= 7;
							state <= S_S2;
							sb <= outr1;
							mbit <= 1;
						end
						if (inst == 19) begin
							sadr <= 7;
							state <= S_S2;
							sb <= outr2;
							mbit <= 1;
						end
					end
					S_LDI0: begin
						wk[3:0] <= inst[3:0];
						state <= S_LDI1;
					end
					S_LDI1: begin
						wk[7:4] <= inst[3:0];
						state <= S_OPCODE;
					end
					S_B2: begin
						lb4w <= inst[3:0];
						state <= S_B0;
					end
					S_B0: begin
						lb4 <= inst[3:0];
						state <= S_B1;
					end
					S_B1: state <= S_OPCODE;
					S_S0: begin
						sb[3:0] <= inst[3:0];
						state <= S_S1;
					end
					S_S1: begin
						sb[7:4] <= inst[3:0];
						state <= S_S2;
						mbit <= 1;
					end
					S_TOGGLE0: begin
						if (inst == 15)
							connected <= ~connected;
						else
							save_r <= inst[2:0];
						state <= S_TOGGLE1;
					end
					S_TOGGLE1: begin
						if (inst != 15) begin
							save_b <= inst[2:0];
							save <= 1;
						end
						state <= S_OPCODE;
					end
				endcase
				if (mbit == 0) begin
					if (jmppc)
						wpc <= pc + 4;
					if ((next | branch) | retpc) begin
						if (retpc)
							pc <= wpc;
						else if (branch) begin
							if (insth == 15)
								pc <= {inst[3:0], lb4, lb4w, 2'b00};
							else
								pc <= {4'b0000, inst[3:0], lb4, 2'b00};
						end
						else
							pc <= pc + 1;
						inst_ready <= 0;
					end
				end
			end
			else
				inst_ready <= 1;
			if ((mbit == 1) && (timing == 0)) begin
				if (ug == 0)
					nrztxct <= 0;
				else if (dbit)
					nrztxct <= nrztxct + 1;
				else
					nrztxct <= 0;
				if ((((insth == 5'd6) || (insth == 5'd17)) || (insth == 5'd18)) || (insth == 5'd19)) begin
					if (nrztxct != 6) begin
						up <= (dbit ? up : ~up);
						um <= (dbit ? ~up : up);
					end
					else begin
						up <= ~up;
						um <= up;
						nrztxct <= 0;
					end
				end
				else begin
					up <= sb[{1'b1, sadr[1:0]}];
					um <= sb[sadr[2:0]];
				end
				ug <= 1'b1;
				if (nrztxct != 6)
					sadr <= sadr - 4'd1;
				if (sadr == 0) begin
					mbit <= 0;
					state <= S_OPCODE;
				end
			end
			dmid <= dmi;
			if ((inst_ready & (state == S_OPCODE)) & (inst == 5'b00010)) begin
				bitadr <= 0;
				nak <= 1;
				nrzrxct <= 0;
			end
			else if ((ug == 0) && (dmi != dmid))
				timing <= 1;
			else
				timing <= timing + 1;
			if (sample) begin
				if (bitadr == 8)
					nak <= dmi;
				if (nrzrxct != 6) begin
					data[6:0] <= data[7:1];
					data[7] <= dmis ~^ dmi;
					bitadr <= bitadr + 1;
					nrzon <= 0;
				end
				else
					nrzon <= 1;
				dmis <= dmi;
				if (dmis ~^ dmi)
					nrzrxct <= nrzrxct + 1;
				else
					nrzrxct <= 0;
				if (~dmi && ~dpi)
					ukprdy <= 0;
			end
			if (ug == 0) begin
				if (bitadr == 24)
					ukprdy <= 1;
				if (bitadr == 88)
					ukprdy <= 0;
			end
			if (((bitadr > 11) & (bitadr[2:0] == 3'b000)) & (timing == 2))
				ukpdat <= data;
			interval <= (interval_cy ? 0 : interval + 1);
			record1 <= record;
			if (~record & record1)
				bank <= ~bank;
			ukprdyd <= ukprdy;
			if ((ukprdy && ~ukprdyd) || ((inst_ready && (state == S_OPCODE)) && (inst == 5'b00010)))
				conct <= 0;
			else if (conct[23:22] != 2'b11)
				conct <= conct + 1;
			else begin
				pc <= 0;
				conct <= 0;
			end
		end
	assign usb_dp_o = up;
	assign usb_dm_o = um;
	assign usb_oe = ug;
	assign sample = ((inst_ready & (state == S_OPCODE)) & (inst == 5'b01101)) & (timing == 4);
	assign record = connected & ~nak;
	assign ukpstb = ((~nrzon & ukprdy) & (bitadr[2:0] == 3'b100)) & (timing == 2);
endmodule
