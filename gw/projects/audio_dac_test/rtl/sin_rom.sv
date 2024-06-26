module sin_rom
   #(
    parameter DATA_WIDTH = 16, // number of bits
              ADDR_WIDTH = 8   // number of address bits
   )
   (
    input  logic clk,
    input  logic [ADDR_WIDTH-1:0] addr_r,
    output logic [DATA_WIDTH-1:0] dout
   );

   // signal declaration
   logic [DATA_WIDTH-1:0] ram [0:2**ADDR_WIDTH-1];  // ascednding range
   logic [DATA_WIDTH-1:0] data_reg;
   
   // sin_table.mem specifies the sin() lookup table
   initial 
      $readmemh("sin_table.mem", ram);
      
   // body
   always_ff @(posedge clk)
   begin
      data_reg <= ram[addr_r];
   end
   
   // read operation
   assign dout = data_reg;
endmodule

// content of sin_table.mem
// sin((2*pi/256)*n) * 2^16 where n = 0, ..., 255
// 0000 0324 0648 096b 0c8c 0fab 12c8 15e2 18f9 1c0c 1f1a 2224 2528 2827 2b1f 2e11 
// 30fc 33df 36ba 398d 3c57 3f17 41ce 447b 471d 49b4 4c40 4ec0 5134 539b 55f6 5843 
// 5a82 5cb4 5ed7 60ec 62f2 64e9 66d0 68a7 6a6e 6c24 6dca 6f5f 70e3 7255 73b6 7505 
// 7642 776c 7885 798a 7a7d 7b5d 7c2a 7ce4 7d8a 7e1e 7e9d 7f0a 7f62 7fa7 7fd9 7ff6 
// 7fff 7ff6 7fd9 7fa7 7f62 7f0a 7e9d 7e1e 7d8a 7ce4 7c2a 7b5d 7a7d 798a 7885 776c 
// 7642 7505 73b6 7255 70e3 6f5f 6dca 6c24 6a6e 68a7 66d0 64e9 62f2 60ec 5ed7 5cb4 
// 5a82 5843 55f6 539b 5134 4ec0 4c40 49b4 471d 447b 41ce 3f17 3c57 398d 36ba 33df
// 30fc 2e11 2b1f 2827 2528 2224 1f1a 1c0c 18f9 15e2 12c8 0fab 0c8c 096b 0648 0324 
// 0000 fcdc f9b8 f695 f374 f055 ed38 ea1e e707 e3f4 e0e6 dddc dad8 d7d9 d4e1 d1ef 
// cf04 cc21 c946 c673 c3a9 c0e9 be32 bb85 b8e3 b64c b3c0 b140 aecc ac65 aa0a a7bd 
// a57e a34c a129 9f14 9d0e 9b17 9930 9759 9592 93dc 9236 90a1 8f1d 8dab 8c4a 8afb 
// 89be 8894 877b 8676 8583 84a3 83d6 831c 8276 81e2 8163 80f6 809e 8059 8027 800a 
// 8001 800a 8027 8059 809e 80f6 8163 81e2 8276 831c 83d6 84a3 8583 8676 877b 8894 
// 89be 8afb 8c4a 8dab 8f1d 90a1 9236 93dc 9592 9759 9930 9b17 9d0e 9f14 a129 a34c
// a57e a7bd aa0a ac65 aecc b140 b3c0 b64c b8e3 bb85 be32 c0e9 c3a9 c673 c946 cc21 
// cf04 d1ef d4e1 d7d9 dad8 dddc e0e6 e3f4 e707 ea1e ed38 f055 f374 f695 f9b8 fcdc       
