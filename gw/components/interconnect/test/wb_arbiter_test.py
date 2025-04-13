import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

#A unit test of the wb_arbiter_2 module. Non-stalling slave.
@cocotb.test()
async def wb_arbiter_2_test(dut):

    #Initial value of input signals
    dut.clk.value = 0
    dut.rst.value = 1

    dut.wbm0_adr_i.value = 0
    dut.wbm0_dat_i.value = 0
    dut.wbm0_we_i.value = 0
    dut.wbm0_sel_i.value = 0
    dut.wbm0_cyc_i.value = 0
    dut.wbm0_stb_i.value = 0

    dut.wbm1_adr_i.value = 0
    dut.wbm1_dat_i.value = 0
    dut.wbm1_we_i.value = 0
    dut.wbm1_sel_i.value = 0
    dut.wbm1_cyc_i.value = 0
    dut.wbm1_stb_i.value = 0

    dut.wbs_dat_i.value = 0
    dut.wbs_ack_i.value = 0
    dut.wbs_err_i.value = 0
    dut.wbs_stall_i.value = 0

    await Timer(10, unit="ns")  # wait 10 clocks

    cocotb.start_soon(Clock(dut.clk, 1, unit="ns").start())

    #Take the system out of reset
    dut.rst.value = 0

    await Timer(10, unit="ns")  # wait 10 clocks

    await RisingEdge(dut.clk)

    #Transaction 1:
    #Word read from bus 0
    dut.wbm0_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm0_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm0_we_i.value = 1
    dut.wbm0_sel_i.value = 0xf
    dut.wbm0_cyc_i.value = 1
    dut.wbm0_stb_i.value = 1

    #Port 1 should initially be stalling.
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should initially not be stalling.
    assert dut.wbm0_stall_o.value == 0

    #This should appear on slave side right away
    await with_timeout(Combine(RisingEdge(dut.wbs_cyc_o), RisingEdge(dut.wbs_stb_o)), 50, "ps")

    assert dut.wbs_dat_o.value == dut.wbm0_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm0_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm0_sel_i.value

    await RisingEdge(dut.clk)
    #Port 1 should still be stalling
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling
    assert dut.wbm0_stall_o.value == 0

    #Deassert input STB
    dut.wbm0_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 50, "ps")

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm0_ack_o), 50, "ps")

    await RisingEdge(dut.clk)
    assert dut.wbm0_dat_o.value == dut.wbs_dat_i.value
    assert dut.wbm0_err_o.value == dut.wbs_err_i.value

    #Port 1 should still be stalling
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling
    assert dut.wbm0_stall_o.value == 0

    #Deassert ack one cycle later
    dut.wbs_ack_i.value = 0
    #Deassert CYC
    dut.wbm0_cyc_i.value = 0

    #Transaction 2:
    #Word read from bus 1
    dut.wbm1_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm1_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm1_we_i.value = 1
    dut.wbm1_sel_i.value = 0xf
    dut.wbm1_cyc_i.value = 1
    dut.wbm1_stb_i.value = 1

    #Port 1 should initially be stalling.
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should initially not be stalling.
    assert dut.wbm0_stall_o.value == 0

    #The transaction should appear on slave side one cycle later
    await RisingEdge(dut.clk)
    await Timer(5, unit="ps")

    assert dut.wbs_cyc_o.value == 1
    assert dut.wbs_stb_o.value == 1
    assert dut.wbs_dat_o.value == dut.wbm1_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm1_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm1_sel_i.value

    #Port 1 should now no longer be stalling.
    assert dut.wbm1_stall_o.value == 0
    #Port 0 should now be stalling.
    assert dut.wbm0_stall_o.value == 1

    await RisingEdge(dut.clk)
    #Port 1 should not be stalling.
    assert dut.wbm1_stall_o.value == 0
    #Port 0 should be stalling.
    assert dut.wbm0_stall_o.value == 1

    #Deassert input STB
    dut.wbm1_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 50, "ps")
    assert dut.wbm1_ack_o.value == 0

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm1_ack_o), 50, "ps")

    await RisingEdge(dut.clk)
    assert dut.wbm1_dat_o.value == dut.wbs_dat_i.value
    assert dut.wbm1_err_o.value == dut.wbs_err_i.value

    #Port 1 should still not be stalling
    assert dut.wbm1_stall_o.value == 0
    #Port 0 should be stalling
    assert dut.wbm0_stall_o.value == 1

    #Deassert ack one cycle later
    dut.wbs_ack_i.value = 0
    #Deassert CYC
    dut.wbm1_cyc_i.value = 0

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

    #Transaction 3:
    #Word read from bus 0 again
    dut.wbm0_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm0_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm0_we_i.value = 1
    dut.wbm0_sel_i.value = 0xf
    dut.wbm0_cyc_i.value = 1
    dut.wbm0_stb_i.value = 1

    #Port 1 should be stalling.
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling.
    assert dut.wbm0_stall_o.value == 0

    #This should appear on slave side right away
    await with_timeout(Combine(RisingEdge(dut.wbs_cyc_o), RisingEdge(dut.wbs_stb_o)), 50, "ps")

    assert dut.wbs_dat_o.value == dut.wbm0_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm0_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm0_sel_i.value

    await RisingEdge(dut.clk)
    #Port 1 should still be stalling
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling
    assert dut.wbm0_stall_o.value == 0

    #Deassert input STB
    dut.wbm0_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 50, "ps")
    assert dut.wbm0_ack_o.value == 0

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm0_ack_o), 50, "ps")

    await RisingEdge(dut.clk)
    assert dut.wbm0_dat_o.value == dut.wbs_dat_i.value
    assert dut.wbm0_err_o.value == dut.wbs_err_i.value

    #Port 1 should still be stalling
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling
    assert dut.wbm0_stall_o.value == 0

    #Deassert ack one cycle later
    dut.wbs_ack_i.value = 0
    #Deassert CYC
    dut.wbm0_cyc_i.value = 0

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

    #Transaction 4:
    #Word read from bus 1 and bus 0 simultaneously
    dut.wbm0_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm0_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm0_we_i.value = 1
    dut.wbm0_sel_i.value = 0xf
    dut.wbm0_cyc_i.value = 1
    dut.wbm0_stb_i.value = 1
    dut.wbm1_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm1_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm1_we_i.value = 1
    dut.wbm1_sel_i.value = 0xf
    dut.wbm1_cyc_i.value = 1
    dut.wbm1_stb_i.value = 1

    #Port 1 should be stalling.
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling.
    assert dut.wbm0_stall_o.value == 0

    #This should appear on slave side right away
    await with_timeout(Combine(RisingEdge(dut.wbs_cyc_o), RisingEdge(dut.wbs_stb_o)), 50, "ps")

    assert dut.wbs_dat_o.value == dut.wbm0_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm0_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm0_sel_i.value

    await RisingEdge(dut.clk)
    #Port 1 should still be stalling
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling
    assert dut.wbm0_stall_o.value == 0

    #Deassert input STB
    dut.wbm0_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 50, "ps")
    assert dut.wbm0_ack_o.value == 0

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm0_ack_o), 50, "ps")

    await RisingEdge(dut.clk)
    assert dut.wbm0_dat_o.value == dut.wbs_dat_i.value
    assert dut.wbm0_err_o.value == dut.wbs_err_i.value

    #Port 1 should still be stalling
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling
    assert dut.wbm0_stall_o.value == 0

    #Deassert ack one cycle later
    dut.wbs_ack_i.value = 0
    #Deassert CYC
    dut.wbm0_cyc_i.value = 0

    #The port 1 transaction should appear on slave side one cycle later
    await RisingEdge(dut.clk)
    await Timer(5, unit="ps")

    assert dut.wbs_cyc_o.value == 1
    assert dut.wbs_stb_o.value == 1
    assert dut.wbs_dat_o.value == dut.wbm1_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm1_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm1_sel_i.value

    #Port 1 should now no longer be stalling.
    assert dut.wbm1_stall_o.value == 0
    #Port 0 should now be stalling.
    assert dut.wbm0_stall_o.value == 1

    await RisingEdge(dut.clk)
    #Port 1 should not be stalling.
    assert dut.wbm1_stall_o.value == 0
    #Port 0 should be stalling.
    assert dut.wbm0_stall_o.value == 1

    #Deassert input STB
    dut.wbm1_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 50, "ps")
    assert dut.wbm1_ack_o.value == 0

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm1_ack_o), 50, "ps")

    await RisingEdge(dut.clk)
    assert dut.wbm1_dat_o.value == dut.wbs_dat_i.value
    assert dut.wbm1_err_o.value == dut.wbs_err_i.value

    #Port 1 should still not be stalling
    assert dut.wbm1_stall_o.value == 0
    #Port 0 should be stalling
    assert dut.wbm0_stall_o.value == 1

    #Deassert ack one cycle later
    dut.wbs_ack_i.value = 0
    #Deassert CYC
    dut.wbm1_cyc_i.value = 0

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

    #Transaction 5:
    #Word read from bus 1 followed by bus 0 one clock cycle later.
    dut.wbm1_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm1_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm1_we_i.value = 1
    dut.wbm1_sel_i.value = 0xf
    dut.wbm1_cyc_i.value = 1
    dut.wbm1_stb_i.value = 1

    #Port 1 should be stalling.
    assert dut.wbm1_stall_o.value == 1
    #Port 0 should not be stalling.
    assert dut.wbm0_stall_o.value == 0

    #The port 1 transaction should appear on slave side one cycle later
    await RisingEdge(dut.clk)
    await Timer(5, unit="ps")

    assert dut.wbs_cyc_o.value == 1
    assert dut.wbs_stb_o.value == 1
    assert dut.wbs_dat_o.value == dut.wbm1_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm1_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm1_sel_i.value

    #Now port 0 requests the bus. It'll have to wait.
    dut.wbm0_adr_i.value = random.randint(0,0xffffffff)
    dut.wbm0_dat_i.value = random.randint(0, 0xffffffff)
    dut.wbm0_we_i.value = 1
    dut.wbm0_sel_i.value = 0xf
    dut.wbm0_cyc_i.value = 1
    dut.wbm0_stb_i.value = 1

    #Port 0 should be stalling
    assert dut.wbm0_stall_o.value == 1
    #Port 1 should not be stalling
    assert dut.wbm1_stall_o.value == 0

    #Deassert input STB
    dut.wbm1_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 50, "ps")

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm1_ack_o), 50, "ps")

    await RisingEdge(dut.clk)
    assert dut.wbm1_dat_o.value == dut.wbs_dat_i.value
    assert dut.wbm1_err_o.value == dut.wbs_err_i.value

    #Port 0 should still be stalling
    assert dut.wbm0_stall_o.value == 1
    #Port 1 should not be stalling
    assert dut.wbm1_stall_o.value == 0

    #Deassert ack one cycle later
    dut.wbs_ack_i.value = 0
    #Deassert CYC
    dut.wbm1_cyc_i.value = 0

    #The port 0 transaction should appear on slave side one cycle later
    await RisingEdge(dut.clk)
    await Timer(5, unit="ps")

    assert dut.wbs_cyc_o.value == 1
    assert dut.wbs_stb_o.value == 1
    assert dut.wbs_dat_o.value == dut.wbm0_dat_i.value
    assert dut.wbs_we_o.value == dut.wbm0_we_i.value
    assert dut.wbs_sel_o.value == dut.wbm0_sel_i.value

    #Port 0 should now no longer be stalling.
    assert dut.wbm0_stall_o.value == 0
    #Port 1 should now be stalling.
    assert dut.wbm1_stall_o.value == 1

    await RisingEdge(dut.clk)
    #Port 0 should not be stalling.
    assert dut.wbm0_stall_o.value == 0
    #Port 1 should be stalling.
    assert dut.wbm1_stall_o.value == 1

    #Deassert input STB
    dut.wbm0_stb_i.value = 0
    assert dut.wbs_cyc_o.value == 1

    #Slave stb output should follow master input
    await with_timeout(FallingEdge(dut.wbs_stb_o), 50, "ps")

    #Let's ACK
    await RisingEdge(dut.clk)
    dut.wbs_ack_i.value = 1
    dut.wbs_dat_i.value = random.randint(0,0xffffffff)

    #Should appear on master side right away
    await with_timeout(RisingEdge(dut.wbm0_ack_o), 50, "ps")

    await RisingEdge(dut.clk)
    assert dut.wbm0_dat_o.value == dut.wbs_dat_i.value
    assert dut.wbm0_err_o.value == dut.wbs_err_i.value

    #Port 0 should still not be stalling
    assert dut.wbm0_stall_o.value == 0
    #Port 1 should be stalling
    assert dut.wbm1_stall_o.value == 1

    #Deassert ack one cycle later
    dut.wbs_ack_i.value = 0
    #Deassert CYC
    dut.wbm0_cyc_i.value = 0

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

if __name__ == "__main__":
    #Test Runner setup. Pass in the verilog sources and top-level.
    #The runner discovers the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [
      proj_path / "../../../../sub/verilog-wishbone/rtl/priority_encoder.v",
      proj_path / "../../../../sub/verilog-wishbone/rtl/arbiter.v",
      proj_path / "../../../../sub/verilog-wishbone/rtl/wb_arbiter_2.v"
    ]

    #Defined in scripts/cocotb_boxlambda.py
    test_runner(verilog_sources=verilog_sources,
                test_module_filename=__file__,
                top="wb_arbiter_2",
                parameters={'ARB_BLOCK_ACK': 0, 'ARB_DEFAULT_TO_PORT_0': 1})

