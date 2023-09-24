`ifdef __ICARUS__
`timescale 1 ns/1 ps
`endif

module praxos_ctrl
(
    input                                     clk,
    input                                     rst_n,

    //32-bit pipelined Wishbone interface.
    input wire [4:0]                          wb_adr,
	input wire [31:0]                         wb_dat_w,
	output wire [31:0]                        wb_dat_r,
	input wire [3:0]                          wb_sel,
    output wire                               wb_stall,
	input wire                                wb_cyc,
	input wire                                wb_stb,
	output wire                               wb_ack,
	input wire                                wb_we,
	output wire                               wb_err,

    //IRQs
    input wire [31:0]                         irq_in,
    output wire                               irq_out,

    //Praxos PM access
    output wire                               praxos_rst_n,
    output reg [7:0]                          praxos_pm_wr_addr,
	output reg                                praxos_pm_wr,
	output reg [35:0]                         praxos_pm_wr_data,

    //Praxos Port I/O
    input wire [4:0]                          praxos_port_addr,
	input wire                                praxos_port_rd,
    input wire                                praxos_port_wr,
    input wire [31:0]                         praxos_port_wr_data,
    output wire [31:0]                        praxos_port_rd_data
);

logic [31:0] gp_reg[0:15];
logic [31:0] ctrl_reg;
logic [31:0] irq_in_reg;
logic [31:0] irq_out_reg, irq_out_next;
logic [31:0] wb_dat_r_i;
logic [31:0] praxos_port_rd_data_i;

//WB handshake
logic do_ack_reg;
logic do_wb_wr, do_wb_rd;

assign do_wb_wr = wb_cyc & wb_stb & wb_we;
assign do_wb_rd = wb_cyc & wb_stb & !wb_we;

logic unused = &{wb_sel};

always @(posedge clk) begin
    do_ack_reg <= 1'b0;
    if (wb_stb) begin
        do_ack_reg <= 1'b1;
    end
end

assign wb_ack = do_ack_reg & wb_cyc;
assign wb_stall = 1'b0; //!wb_cyc ? 1'b0 : !wb_ack;
assign wb_err = 1'b0;

//Reset Control
assign praxos_rst_n = ctrl_reg[0];

//IRQ handling
assign irq_out = |irq_out_reg;
always_comb begin
    for (int i=0; i<32; i++) begin
        irq_out_next[i] = irq_out_reg[i];
        //Ack IRQ by writing to WB register 0
        if (do_wb_wr && (wb_adr[4:0]==5'd0) && wb_dat_w[i])
            irq_out_next[i] = 1'b0;
        if (praxos_port_wr && (praxos_port_addr[4:0]==5'd0) && praxos_port_wr_data[i])
            irq_out_next[i] = 1'b1;
    end   
end

//Register writes, both WB, Praxos and IRQs.
always_ff @(posedge clk) begin
    if( !rst_n ) begin
        gp_reg[0] <= 32'b0;
        gp_reg[1] <= 32'b0;
        gp_reg[2] <= 32'b0;
        gp_reg[3] <= 32'b0;
        gp_reg[4] <= 32'b0;
        gp_reg[5] <= 32'b0;
        gp_reg[6] <= 32'b0;
        gp_reg[7] <= 32'b0;
        gp_reg[8] <= 32'b0;
        gp_reg[9] <= 32'b0;
        gp_reg[10] <= 32'b0;
        gp_reg[11] <= 32'b0;
        gp_reg[12] <= 32'b0;
        gp_reg[13] <= 32'b0;
        gp_reg[14] <= 32'b0;
        gp_reg[15] <= 32'b0;

        ctrl_reg <= 32'b0;

        praxos_pm_wr_addr <= 8'b0;
        praxos_pm_wr_data <= 36'b0;

        irq_in_reg <= 32'b0;
        irq_out_reg <= 32'b0;

        praxos_pm_wr <= 1'b0;
    end
    else begin
        irq_in_reg <= irq_in;
        irq_out_reg <= irq_out_next;
        praxos_pm_wr <= 1'b0;

        if (do_wb_wr) begin
            case(wb_adr)
                5'd2: praxos_pm_wr_data[31:0] <= wb_dat_w;
                5'd3: praxos_pm_wr_data[35:32] <= wb_dat_w[3:0];
                5'd4: praxos_pm_wr_addr <= wb_dat_w[7:0];
                5'd5: praxos_pm_wr <= 1'b1;
                5'd6: ctrl_reg <= wb_dat_w;
                5'd16: gp_reg[0] <= wb_dat_w;
                5'd17: gp_reg[1] <= wb_dat_w;
                5'd18: gp_reg[2] <= wb_dat_w;
                5'd19: gp_reg[3] <= wb_dat_w;
                5'd20: gp_reg[4] <= wb_dat_w;
                5'd21: gp_reg[5] <= wb_dat_w;
                5'd22: gp_reg[6] <= wb_dat_w;
                5'd23: gp_reg[7] <= wb_dat_w;
                5'd24: gp_reg[8] <= wb_dat_w;
                5'd25: gp_reg[9] <= wb_dat_w;
                5'd26: gp_reg[10] <= wb_dat_w;
                5'd27: gp_reg[11] <= wb_dat_w;
                5'd28: gp_reg[12] <= wb_dat_w;
                5'd29: gp_reg[13] <= wb_dat_w;
                5'd30: gp_reg[14] <= wb_dat_w;
                5'd31: gp_reg[15] <= wb_dat_w;
                default:;
            endcase
        end
        
        if (praxos_port_wr) begin
            case(praxos_port_addr)
                5'd16: gp_reg[0] <= praxos_port_wr_data;
                5'd17: gp_reg[1] <= praxos_port_wr_data;
                5'd18: gp_reg[2] <= praxos_port_wr_data;
                5'd19: gp_reg[3] <= praxos_port_wr_data;
                5'd20: gp_reg[4] <= praxos_port_wr_data;
                5'd21: gp_reg[5] <= praxos_port_wr_data;
                5'd22: gp_reg[6] <= praxos_port_wr_data;
                5'd23: gp_reg[7] <= praxos_port_wr_data;
                5'd24: gp_reg[8] <= praxos_port_wr_data;
                5'd25: gp_reg[9] <= praxos_port_wr_data;
                5'd26: gp_reg[10] <= praxos_port_wr_data;
                5'd27: gp_reg[11] <= praxos_port_wr_data;
                5'd28: gp_reg[12] <= praxos_port_wr_data;
                5'd29: gp_reg[13] <= praxos_port_wr_data;
                5'd30: gp_reg[14] <= praxos_port_wr_data;
                5'd31: gp_reg[15] <= praxos_port_wr_data;
                default:;
            endcase
        end
    end
end

//WB and Praxos register reads
always_comb begin
    if (do_wb_rd) begin
        case(wb_adr)
            5'd0: wb_dat_r_i = irq_out_reg;
            5'd1: wb_dat_r_i = irq_in_reg;
            5'd2: wb_dat_r_i = praxos_pm_wr_data[31:0];
            5'd3: wb_dat_r_i = {28'd0, praxos_pm_wr_data[35:32]};
            5'd4: wb_dat_r_i = {24'b0, praxos_pm_wr_addr[7:0]};
            5'd6: wb_dat_r_i = ctrl_reg;
            5'd16: wb_dat_r_i = gp_reg[0];
            5'd17: wb_dat_r_i = gp_reg[1];
            5'd18: wb_dat_r_i = gp_reg[2];
            5'd19: wb_dat_r_i = gp_reg[3];
            5'd20: wb_dat_r_i = gp_reg[4];
            5'd21: wb_dat_r_i = gp_reg[5];
            5'd22: wb_dat_r_i = gp_reg[6];
            5'd23: wb_dat_r_i = gp_reg[7];
            5'd24: wb_dat_r_i = gp_reg[8];
            5'd25: wb_dat_r_i = gp_reg[9];
            5'd26: wb_dat_r_i = gp_reg[10];
            5'd27: wb_dat_r_i = gp_reg[11];
            5'd28: wb_dat_r_i = gp_reg[12];
            5'd29: wb_dat_r_i = gp_reg[13];
            5'd30: wb_dat_r_i = gp_reg[14];
            5'd31: wb_dat_r_i = gp_reg[15];
            default: wb_dat_r_i = 32'd0;
        endcase
    end
    if (praxos_port_rd) begin
        case(praxos_port_addr)
            5'd1: praxos_port_rd_data_i = irq_in_reg;
            5'd16: praxos_port_rd_data_i = gp_reg[0]; 
            5'd17: praxos_port_rd_data_i = gp_reg[1]; 
            5'd18: praxos_port_rd_data_i = gp_reg[2]; 
            5'd19: praxos_port_rd_data_i = gp_reg[3]; 
            5'd20: praxos_port_rd_data_i = gp_reg[4]; 
            5'd21: praxos_port_rd_data_i = gp_reg[5]; 
            5'd22: praxos_port_rd_data_i = gp_reg[6]; 
            5'd23: praxos_port_rd_data_i = gp_reg[7]; 
            5'd24: praxos_port_rd_data_i = gp_reg[8]; 
            5'd25: praxos_port_rd_data_i = gp_reg[9]; 
            5'd26: praxos_port_rd_data_i = gp_reg[10]; 
            5'd27: praxos_port_rd_data_i = gp_reg[11]; 
            5'd28: praxos_port_rd_data_i = gp_reg[12]; 
            5'd29: praxos_port_rd_data_i = gp_reg[13]; 
            5'd30: praxos_port_rd_data_i = gp_reg[14]; 
            5'd31: praxos_port_rd_data_i = gp_reg[15];
            default: praxos_port_rd_data_i = 32'd0; 
        endcase
    end
end

assign wb_dat_r = wb_dat_r_i;
assign praxos_port_rd_data = praxos_port_rd_data_i;

endmodule
