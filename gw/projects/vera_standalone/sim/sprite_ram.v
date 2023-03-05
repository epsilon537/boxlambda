//`default_nettype none

module sprite_ram(
    input  wire        rst_i,
    input  wire        wr_clk_i,
    input  wire        rd_clk_i,
    input  wire        wr_clk_en_i,
    input  wire        rd_en_i,
    input  wire        rd_clk_en_i,
    input  wire        wr_en_i,
    input  wire  [3:0] ben_i,
    input  wire [31:0] wr_data_i,
    input  wire  [7:0] wr_addr_i,
    input  wire  [7:0] rd_addr_i,
    output reg  [31:0] rd_data_o);

    reg [31:0] mem[0:255];

    always @(posedge wr_clk_i) begin
        if (wr_en_i) begin
            if (ben_i[3]) mem[wr_addr_i][31:24] <= wr_data_i[31:24];
            if (ben_i[2]) mem[wr_addr_i][23:16] <= wr_data_i[23:16];
            if (ben_i[1]) mem[wr_addr_i][15:8]  <= wr_data_i[15:8];
            if (ben_i[0]) mem[wr_addr_i][7:0]   <= wr_data_i[7:0];
        end
    end

    always @(posedge rd_clk_i) begin
        rd_data_o <= mem[rd_addr_i];
    end

    initial begin: INIT
        integer i;

        for (i=0; i<256; i=i+1) begin
            mem[i] = 0;
        end

        for (i=(256-2*128); i<256; i=i+2) begin
            mem[i][11:0]  = 'h100;   // addr
            mem[i][15]    = 1;       // mode: 8bpp
            mem[i][25:16] = 10'(32*i);  // x
            mem[i+1][9:0]   = 10'd3;   // y
            mem[i+1][16]    = 0;       // hflip
            mem[i+1][17]    = 0;       // vflip
            mem[i+1][19:18] = 2'd3;    // z
            mem[i+1][23:20] = 0;       // collision mask
            mem[i+1][27:24] = 0;       // palette offset
            mem[i+1][29:28] = 2'd0;    // width: 8
            mem[i+1][31:30] = 2'd0;    // height: 8
        end

    end

endmodule
