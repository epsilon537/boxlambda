-- Copyright 1986-2022 Xilinx, Inc. All Rights Reserved.
-- Copyright 2022-2023 Advanced Micro Devices, Inc. All Rights Reserved.
-- --------------------------------------------------------------------------------
-- Tool Version: Vivado v.2023.1 (lin64) Build 3865809 Sun May  7 15:04:56 MDT 2023
-- Date        : Mon Sep  9 15:14:51 2024
-- Host        : asustuf running 64-bit Ubuntu 22.04.4 LTS
-- Command     : write_vhdl -force -mode synth_stub /home/epsilon/work/boxlambda/gw/dfx/ila/ila_0/ila_0_stub.vhdl
-- Design      : ila_0
-- Purpose     : Stub declaration of top-level module interface
-- Device      : xc7a100ticsg324-1L
-- --------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity ila_0 is
  Port ( 
    clk : in STD_LOGIC;
    probe0 : in STD_LOGIC_VECTOR ( 63 downto 0 );
    probe1 : in STD_LOGIC_VECTOR ( 63 downto 0 )
  );

end ila_0;

architecture stub of ila_0 is
attribute syn_black_box : boolean;
attribute black_box_pad_pin : string;
attribute syn_black_box of stub : architecture is true;
attribute black_box_pad_pin of stub : architecture is "clk,probe0[63:0],probe1[63:0]";
attribute X_CORE_INFO : string;
attribute X_CORE_INFO of stub : architecture is "ila,Vivado 2023.1";
begin
end;
