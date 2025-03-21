//A top-level of Out-of-Context synthesis of a wbxbar module.
module wbxbar_ooc #(
    parameter NM = 6,
    NS = 8,
    parameter AW = 28,
    DW = 32,
    parameter [NS*AW-1:0] SLAVE_ADDR = {
      {3'b111, {(AW - 3) {1'b0}}},
      {3'b110, {(AW - 3) {1'b0}}},
      {3'b101, {(AW - 3) {1'b0}}},
      {3'b100, {(AW - 3) {1'b0}}},
      {3'b011, {(AW - 3) {1'b0}}},
      {3'b010, {(AW - 3) {1'b0}}},
      {4'b0010, {(AW - 4) {1'b0}}},
      {4'b0000, {(AW - 4) {1'b0}}}
    },
    parameter [NS*AW-1:0] SLAVE_MASK = {
      {(NS - 2) {3'b111, {(AW - 3) {1'b0}}}}, {(2) {4'b1111, {(AW - 4) {1'b0}}}}
    }
) (
    input logic i_clk,
    i_reset,

    input    logic    [NM-1:0]    i_mcyc,
    i_mstb,
    i_mwe,
    input    logic    [NM*AW-1:0]    i_maddr,
    input    logic    [NM*DW-1:0]    i_mdata,
    input    logic    [NM*DW/8-1:0]    i_msel,

    output    logic    [NM-1:0]    o_mstall,
    output    logic    [NM-1:0]    o_mack,
    output    logic    [NM*DW-1:0]    o_mdata,
    output    logic    [NM-1:0]    o_merr,

    output    logic    [NS-1:0]    o_scyc,
    o_sstb,
    o_swe,
    output    logic    [NS*AW-1:0]    o_saddr,
    output    logic    [NS*DW-1:0]    o_sdata,
    output    logic    [NS*DW/8-1:0]    o_ssel,

    input    logic    [NS-1:0]    i_sstall,
    i_sack,
    input    logic    [NS*DW-1:0]    i_sdata,
    input    logic    [NS-1:0]    i_serr
);

  wbxbar #(
      .NM(NM),
      .NS(NS),
      .AW(AW),
      .DW(DW),
      .SLAVE_ADDR(SLAVE_ADDR),
      .SLAVE_MASK(SLAVE_MASK),
      .OPT_DBLBUFFER(1'b0),
      .OPT_LOWPOWER(1'b0)
  ) wbxbar_inst (
      .*
  );
endmodule
