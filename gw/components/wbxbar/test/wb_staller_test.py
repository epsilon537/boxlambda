import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

@cocotb.test()
async def wb_staller_test(dut):

    #Initial value of input signals
    dut.clk.value = 0
    dut.rst.value = 1

    dut.wbm_adr_i.value = 0
    dut.wbm_dat_i.value = 0
    dut.wbm_we_i.value = 0
    dut.wbm_sel_i.value = 0
    dut.wbm_cyc_i.value = 0
    dut.wbm_stb_i.value = 0

    dut.wbs_dat_i.value = 0
    dut.wbs_ack_i.value = 0
    dut.wbs_err_i.value = 0
    dut.wbs_stall_i.value = 0

    await Timer(10, units="ns")  # wait 10 clocks

    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())

    #Take the system out of reset
    dut.rst.value = 0

    await Timer(10, units="ns")  # wait 10 clocks

    await RisingEdge(dut.clk)

    #Transaction 1:
    #Word read
    dut.wbm_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm_we_i.value = 1
    dut.wbm_sel_i.value = 0xf
    dut.wbm_cyc_i.value = 1
    dut.wbm_stb_i.value = 1

    #This should stall the master right away
    await with_timeout(RisingEdge(dut.wbm_stall_o), 500, "ps")
    assert dut.wbs_cyc_o.value == 0
    assert dut.wbs_stb_o.value == 0

    await RisingEdge(dut.clk)
    await with_timeout(FallingEdge(dut.wbm_stall_o), 500, "ps")

    #Transaction should now appear on slave side
    assert dut.wbs_cyc_o.value == 1
    assert dut.wbs_stb_o.value == 1
    assert dut.wbs_dat_o.value == dut.wbm_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm_sel_i.value

    await RisingEdge(dut.clk)
    #Deassert input STB
    dut.wbm_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 500, "ps")
    assert dut.wbm_stall_o.value == 0
    assert dut.wbm_ack_o.value == 0

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm_ack_o), 500, "ps")
    #assert dut.wbm_dat_o.value == dut.wbs_dat_i.value
    #assert dut.wbm_err_o.value == dut.wbs_err_i.value
    #assert dut.wbm_stall_o.value == dut.wbs_stall_i.value

    #Deassert ack one cycle later
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 0

    #Transaction 2:
    #Word read
    dut.wbm_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm_we_i.value = 1
    dut.wbm_sel_i.value = 0xf
    dut.wbm_cyc_i.value = 1
    dut.wbm_stb_i.value = 1

    #This should stall the master right away
    await with_timeout(RisingEdge(dut.wbm_stall_o), 500, "ps")
    assert dut.wbs_cyc_o.value == 0
    assert dut.wbs_stb_o.value == 0

    await RisingEdge(dut.clk)
    await with_timeout(FallingEdge(dut.wbm_stall_o), 500, "ps")

    #Transaction should now appear on slave side
    assert dut.wbs_cyc_o.value == 1
    assert dut.wbs_stb_o.value == 1
    assert dut.wbs_dat_o.value == dut.wbm_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm_sel_i.value

    await RisingEdge(dut.clk)
    #Deassert input STB
    dut.wbm_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 500, "ps")
    assert dut.wbm_stall_o.value == 0
    assert dut.wbm_ack_o.value == 0

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm_ack_o), 500, "ps")
    #assert dut.wbm_dat_o.value == dut.wbs_dat_i.value
    #assert dut.wbm_err_o.value == dut.wbs_err_i.value
    #assert dut.wbm_stall_o.value == dut.wbs_stall_i.value

    #Deassert ack one cycle later
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 0

    #Transaction 3: stalling slave
    #Word read
    dut.wbm_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm_we_i.value = 1
    dut.wbm_sel_i.value = 0xf
    dut.wbm_cyc_i.value = 1
    dut.wbm_stb_i.value = 1
    dut.wbs_stall_i.value = 1

    #This should stall the master right away
    await with_timeout(RisingEdge(dut.wbm_stall_o), 500, "ps")
    assert dut.wbs_cyc_o.value == 0
    assert dut.wbs_stb_o.value == 0

    #After a few clocks, un-stall the slave
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    dut.wbs_stall_i.value = 0
    await with_timeout(FallingEdge(dut.wbm_stall_o), 500, "ps")

    #Transaction should now appear on slave side
    assert dut.wbs_cyc_o.value == 1
    assert dut.wbs_stb_o.value == 1
    assert dut.wbs_dat_o.value == dut.wbm_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm_sel_i.value

    await RisingEdge(dut.clk)
    #Deassert input STB
    dut.wbm_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 500, "ps")
    assert dut.wbm_stall_o.value == 0
    assert dut.wbm_ack_o.value == 0

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm_ack_o), 500, "ps")
    #assert dut.wbm_dat_o.value == dut.wbs_dat_i.value
    #assert dut.wbm_err_o.value == dut.wbs_err_i.value
    #assert dut.wbm_stall_o.value == dut.wbs_stall_i.value

    #Deassert ack one cycle later
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 0

if __name__ == "__main__":
    #Test Runner setup. Pass in the verilog sources and top-level.
    #The runner discovers the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [
      proj_path / "../rtl/wb_staller.sv"
    ]

    #Defined in scripts/cocotb_boxlambda.py
    test_runner(verilog_sources=verilog_sources,
                test_module_filename=__file__,
                top="wb_staller")

