import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

#Cocotb-based syncpls unit test program.

@cocotb.test()
async def slow_clk_to_fast_clk(dut):
    dut.t_clk.value = 0
    dut.t_rst_n.value = 1
    dut.t_pulse.value = 0
    dut.r_clk.value = 0
    dut.r_rst_n.value = 1
    dut.r_pulse.value = 0

    cocotb.start_soon(Clock(dut.t_clk, 8, units="ns").start())
    cocotb.start_soon(Clock(dut.r_clk, 1, units="ns").start())

    await Timer(16, units="ns")
    dut.t_rst_n.value = 0
    dut.r_rst_n.value = 0
    await Timer(16, units="ns")
    dut.t_rst_n.value = 1
    dut.r_rst_n.value = 1
    await Timer(16, units="ns")

    await RisingEdge(dut.t_clk)

    dut.t_pulse.value = 1

    await FallingEdge(dut.t_clk)
    await RisingEdge(dut.t_clk)

    dut.t_pulse.value = 0

    count = 0

    while count < 100:
        count += 1
        await RisingEdge(dut.r_clk)
        if dut.r_pulse.value == 1:
            await FallingEdge(dut.r_clk)
            await RisingEdge(dut.r_clk)
            assert dut.r_pulse.value == 0
            break
        
    assert count < 100

@cocotb.test()
async def fast_clk_to_slow_clk(dut):
    dut.t_clk.value = 0
    dut.t_rst_n.value = 1
    dut.t_pulse.value = 0
    dut.r_clk.value = 0
    dut.r_rst_n.value = 1
    dut.r_pulse.value = 0

    cocotb.start_soon(Clock(dut.t_clk, 1, units="ns").start())
    cocotb.start_soon(Clock(dut.r_clk, 8, units="ns").start())

    await Timer(16, units="ns")
    dut.t_rst_n.value = 0
    dut.r_rst_n.value = 0
    await Timer(16, units="ns")
    dut.t_rst_n.value = 1
    dut.r_rst_n.value = 1
    await Timer(16, units="ns")
    
    await RisingEdge(dut.t_clk)

    dut.t_pulse.value = 1

    await FallingEdge(dut.t_clk)
    await RisingEdge(dut.t_clk)

    dut.t_pulse.value = 0

    count = 0

    while count < 100:
        count += 1
        await RisingEdge(dut.r_clk)
        if dut.r_pulse.value == 1:
            await FallingEdge(dut.r_clk)
            await RisingEdge(dut.r_clk)
            assert dut.r_pulse.value == 0
            break
        
    assert count < 100

if __name__ == "__main__":
    #Test Runner setup. Pass in the verilog sources and top-level.
    #The runner discovers the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../rtl/pls2tgl.sv",
                       proj_path / "../rtl/sync3.sv",
                       proj_path / "../rtl/tgl2pls.sv",
                       proj_path / "../rtl/syncpls.sv"]
    #Defined in scripts/cocotb_boxlambda.py
    test_runner(verilog_sources=verilog_sources, 
                test_module_filename=__file__, 
                top="syncpls")
