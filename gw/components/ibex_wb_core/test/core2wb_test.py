import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

stb_width = 1

#This task monitors the core_gnt_o signal
async def core_gnt_o(dut):
    while True:
        await RisingEdge(dut.core_gnt_o)
        assert dut.core_req_i.value == 1

#This task monitors the rvalid_o signal
async def core_rvalid_o(dut):
    while True:
        await RisingEdge(dut.core_rvalid_o)
        #only on reads
        assert dut.core_we_i.value == 0
        #valid_o must remain asserted for one cycle
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        assert dut.core_rvalid_o.value == 0

#This task monitors the wb_stb_o signal
async def wb_stb_o(dut):
    global stb_width

    while True:
        await RisingEdge(dut.wb_stb_o)
        t = cocotb.utils.get_sim_time(units='ps')
        dut._log.info("stb rising edge at %d ps", t)

        #wb_stb_o must remain asserted for stb_width cycles
        for _ in range(stb_width):
            await RisingEdge(dut.clk)
            t = cocotb.utils.get_sim_time(units='ps')
            dut._log.info("stb in loop: %d at %d ps", dut.wb_stb_o.value, t)
            assert dut.wb_stb_o.value == 1

        await RisingEdge(dut.clk)

        t = cocotb.utils.get_sim_time(units='ps')
        dut._log.info("stb after loop: %d at %d ps", dut.wb_stb_o.value, t)
        assert dut.wb_stb_o.value == 0

@cocotb.test()
async def core2wb_test(dut):

    global stb_width

    #Initial value of input signals
    dut.clk.value = 0
    dut.rst.value = 1

    dut.core_req_i.value = 0
    dut.core_we_i.value = 0
    dut.core_be_i.value = 0
    dut.core_addr_i.value = 0
    dut.core_wdata_i.value = 0

    dut.wb_ack_i.value = 0
    dut.wb_stall_i.value = 0
    dut.wb_err_i.value = 0
    dut.wb_dat_s_i.value = 0

    await Timer(10, units="ns")  # wait 10 clocks

    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())

    #Take the system out of reset
    dut.rst.value = 0

    await Timer(10, units="ns")  # wait 10 clocks

    core_gnt_o_task = cocotb.start_soon(core_gnt_o(dut))
    rvalid_o_task = cocotb.start_soon(core_rvalid_o(dut))
    wb_stb_o_task = cocotb.start_soon(wb_stb_o(dut))

    await RisingEdge(dut.clk)

    #Transaction 1:
    #Word read
    dut.core_req_i.value = 1
    dut.core_we_i.value = 0
    dut.core_be_i.value = 0xf
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_sel_o.value == 0xf
    assert dut.wb_we_o.value == 0
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    dut.wb_dat_s_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.core_rvalid_o), 1, "ns")
    assert dut.core_rdata_o.value == dut.core_rdata_o.value

    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await with_timeout(FallingEdge(dut.core_rvalid_o), 1, "ns")
    await RisingEdge(dut.clk)

    #Transaction 2:
    #Word write
    dut.core_req_i.value = 1
    dut.core_we_i.value = 1
    dut.core_be_i.value = 0xf
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address
    dut.core_wdata_i.value = random.randint(0,0xffffffff)

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_we_o.value == 1
    assert dut.wb_sel_o.value == 0xf
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address
    assert dut.wb_dat_m_o.value == dut.core_wdata_i.value

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await RisingEdge(dut.clk)

    #Transaction 3:
    #Lower Half Word write
    dut.core_req_i.value = 1
    dut.core_we_i.value = 1
    dut.core_be_i.value = 0x3
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address
    dut.core_wdata_i.value = random.randint(0,0xffffffff)

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_we_o.value == 1
    assert dut.wb_sel_o.value == 0x3
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address
    assert dut.wb_dat_m_o.value == dut.core_wdata_i.value

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await RisingEdge(dut.clk)

    #Transaction 4:
    #Upper Half Word write
    dut.core_req_i.value = 1
    dut.core_we_i.value = 1
    dut.core_be_i.value = 0xc
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address
    dut.core_wdata_i.value = random.randint(0,0xffffffff)

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_we_o.value == 1
    assert dut.wb_sel_o.value == 0xc
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address
    assert dut.wb_dat_m_o.value == dut.core_wdata_i.value

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await RisingEdge(dut.clk)

    #Transaction 5:
    #Lower Byte write
    dut.core_req_i.value = 1
    dut.core_we_i.value = 1
    dut.core_be_i.value = 0x1
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address
    dut.core_wdata_i.value = random.randint(0,0xffffffff)

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_we_o.value == 1
    assert dut.wb_sel_o.value == 0x1
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address
    assert dut.wb_dat_m_o.value == dut.core_wdata_i.value

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await RisingEdge(dut.clk)

    #Transaction 6:
    #Upper Byte write
    dut.core_req_i.value = 1
    dut.core_we_i.value = 1
    dut.core_be_i.value = 0x8
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address
    dut.core_wdata_i.value = random.randint(0,0xffffffff)

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_we_o.value == 1
    assert dut.wb_sel_o.value == 0x8
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address
    assert dut.wb_dat_m_o.value == dut.core_wdata_i.value

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0

    #Transactions 7 and 8:
    #Back-to-Back Word read
    dut.core_req_i.value = 1
    dut.core_we_i.value = 0
    dut.core_be_i.value = 0xf
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_sel_o.value == 0xf
    assert dut.wb_we_o.value == 0
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    #The core req data is allowed to change now. Let's use another address and keep core_req_i asserted
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address

    dut.wb_ack_i.value = 1
    dut.wb_dat_s_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.core_rvalid_o), 1, "ns")
    assert dut.core_rdata_o.value == dut.core_rdata_o.value

    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await with_timeout(FallingEdge(dut.core_rvalid_o), 1, "ns")

    #core_req_i is still asserted at this point so the next transaction start
    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_sel_o.value == 0xf
    assert dut.wb_we_o.value == 0
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    dut.wb_dat_s_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.core_rvalid_o), 1, "ns")
    assert dut.core_rdata_o.value == dut.core_rdata_o.value

    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await with_timeout(FallingEdge(dut.core_rvalid_o), 1, "ns")
    await RisingEdge(dut.clk)

    #Transaction 9:
    #Word read with stall
    dut.core_req_i.value = 1
    dut.core_we_i.value = 0
    dut.core_be_i.value = 0xf
    dut.core_addr_i.value = random.randint(0,0x03ffffff)*4 #Word-aligned byte address
    dut.wb_stall_i.value = 1
    #Let' stall for 3 cycles
    stb_width = 4

    #It should be granted right away
    await with_timeout(RisingEdge(dut.core_gnt_o), 1, "ns")

    assert dut.wb_cyc_o.value == 1
    assert dut.wb_stb_o.value == 1
    assert dut.wb_sel_o.value == 0xf
    assert dut.wb_we_o.value == 0
    assert dut.wb_adr_o.value == int(dut.core_addr_i.value)>>2 #Byte to Word address

    for _ in range(stb_width-1):
        await RisingEdge(dut.clk)

    dut.wb_stall_i.value = 0

    #Let's ack on the next cycle
    await RisingEdge(dut.clk)
    dut.core_req_i.value =0
    dut.wb_ack_i.value = 1
    dut.wb_dat_s_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.core_rvalid_o), 1, "ns")
    assert dut.core_rdata_o.value == dut.core_rdata_o.value

    await RisingEdge(dut.clk)
    dut.wb_ack_i.value = 0
    await with_timeout(FallingEdge(dut.core_rvalid_o), 1, "ns")
    await RisingEdge(dut.clk)

    core_gnt_o_task.kill()
    rvalid_o_task.kill()
    wb_stb_o_task.kill()

if __name__ == "__main__":
    #Test Runner setup. Pass in the verilog sources and top-level.
    #The runner discovers the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [
      proj_path / "../../../../sub/ibex_wb/rtl/core2wb_no_ifs.sv"
    ]

    #Defined in scripts/cocotb_boxlambda.py
    test_runner(verilog_sources=verilog_sources,
                test_module_filename=__file__,
                top="core2wb_no_ifs")

