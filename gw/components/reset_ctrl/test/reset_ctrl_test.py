import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

#Cocotb-based reset_ctrl unit test program.

#Async task waiting for NDM reset
async def wait_for_ndm_reset(dut):
    await RisingEdge(dut.ndm_reset_o)
    startTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("ndm_reset asserted")
    await FallingEdge(dut.ndm_reset_o)
    endTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("ndm_reset deasserted")

#Async task waiting for DM reset
async def wait_for_dm_reset(dut):
    await RisingEdge(dut.dm_reset_o)
    startTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("dm_reset asserted")
    await FallingEdge(dut.dm_reset_o)
    endTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("dm_reset deasserted")

#Async task cause test failure if DM reset is asserted.
async def make_sure_no_dm_reset(dut):
    await RisingEdge(dut.dm_reset_o)
    assert False

#Async task causing test failure if NDM reset is asserted more than once.
async def make_sure_no_double_ndm_reset(dut):
    await RisingEdge(dut.ndm_reset_o)
    dut._log.info("ndm_reset asserted (1)")
    await FallingEdge(dut.ndm_reset_o)
    dut._log.info("ndm_reset deasserted (1)")
    await RisingEdge(dut.ndm_reset_o)
    dut._log.info("ndm_reset asserted (2)")
    assert 0

#Async task causing test failure if DM reset is asserted more than once.
async def make_sure_no_double_dm_reset(dut):
    await RisingEdge(dut.dm_reset_o)
    dut._log.info("dm_reset asserted (1)")
    await FallingEdge(dut.dm_reset_o)
    dut._log.info("dm_reset deasserted (1)")
    await RisingEdge(dut.dm_reset_o)
    dut._log.info("dm_reset asserted (2)")
    assert 0

#Async task causing test failure if POR completed goes to 0.
async def make_sure_por_stays_completed(dut):
    await FallingEdge(dut.por_completed_o)
    assert 0

#Async task extending STB signal until slave stops stalling
async def wb_stall_check(dut):
    await RisingEdge(dut.clk)
    if dut.wb_stall.value == 1:
        await FallingEdge(dut.wb_stall)
    dut.wb_stb.value = 0

#Wishbone word read transaction task.
async def wb_read(dut, addr):
    await RisingEdge(dut.clk)
        
    dut.wb_adr.value = addr
    dut.wb_sel.value = 0xf
    dut.wb_cyc.value = 1
    dut.wb_stb.value = 1
    dut.wb_we.value = 0
    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.clk)
        if dut.wb_ack.value == 1:
            #dut._log.info("WBS: ack detected")
            ackDetected = True

    stall_check_task.kill()
    assert dut.wb_err.value == 0
    res = dut.wb_dat_r.value
    
    await RisingEdge(dut.clk)
    dut.wb_cyc.value = 0
    
    return res

#Wishbone word write transaction task.
async def wb_write(dut, addr, val):
    await RisingEdge(dut.clk)
    dut.wb_adr.value = addr
    dut.wb_dat_w.value = val
    dut.wb_sel.value = 0xf
    dut.wb_cyc.value = 1
    dut.wb_stb.value = 1
    dut.wb_we.value = 1

    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.clk)
        if dut.wb_ack.value == 1:
            #dut._log.info("WBS: ack detected")
            ackDetected = True

    stall_check_task.kill()
    assert dut.wb_err.value == 0
    
    await RisingEdge(dut.clk)
    dut.wb_cyc.value = 0

@cocotb.test()
async def por_followed_by_ndm_reset_followed_by_ext_reset_followed_by_WB_reset_test(dut):
    #Initial values of input signals
    dut.wb_adr.value = 0
    dut.wb_dat_w.value = 0
    dut.wb_sel.value = 0
    dut.wb_cyc.value = 0
    dut.wb_stb.value = 0
    dut.wb_we.value = 0
	
    dut.pll_locked_i.value = 0
    dut.ext_reset_i.value = 0
    dut.ndm_reset_i.value = 0

    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())
    startTime = cocotb.utils.get_sim_time(units='ns')

    dut._log.info("POR test.")

    await Timer(10, units="ns")  # wait 10 clocks
    await FallingEdge(dut.clk)  # wait for falling edge/"negedge"

    #We shouldn't see any reset yet.
    assert dut.ndm_reset_o.value == 0
    assert dut.dm_reset_o.value == 0

    ndm_reset_task = cocotb.start_soon(wait_for_ndm_reset(dut))
    dm_reset_task = cocotb.start_soon(wait_for_dm_reset(dut))
    dut.pll_locked_i.value = 1; #Indicate lock achieved.

    await Timer(3, units="ns")  # wait 3 clocks

    #We still shouldn't see any resets.
    assert dut.ndm_reset_o.value == 0
    assert dut.por_completed_o == 0

    await Timer(30, units="ns")  # wait 30 clocks

    #Both tasks should have completed by now
    assert ndm_reset_task.done()
    assert dm_reset_task.done()
    assert dut.por_completed_o == 1

    #Read the  reset-reason register
    res = await with_timeout(wb_read(dut, 1), 30, 'ns')
    assert res == 1 #1 indicates POR

    check_por_completed_task = cocotb.start_soon(make_sure_por_stays_completed(dut))

    await Timer(10, units="ns")  # wait 10 clocks

    #Now let's trigger an NDM reset
    dut._log.info("NDM reset test.")
    assert dut.ndm_reset_o.value == 0
    assert dut.dm_reset_o.value == 0
    
    ndm_reset_task = cocotb.start_soon(wait_for_ndm_reset(dut))
    dm_reset_task = cocotb.start_soon(make_sure_no_dm_reset(dut))

    dut.ndm_reset_i.value = 1
    await Timer(10, units="ns")  # wait 10 clocks
    dut.ndm_reset_i.value = 0
    await Timer(10, units="ns")  # wait 10 clocks

    #ndm reset task should have completed by now
    assert ndm_reset_task.done()
    #dm task should not have completed by now
    dm_reset_task.kill()

    #Read the reset-reason register
    res = await with_timeout(wb_read(dut, 1), 30, 'ns')
    assert res == 8 #8 indicates NDM reset

    await Timer(10, units="ns")  # wait 10 clocks

    #Now let's trigger an external reset
    dut._log.info("Ext. reset test.")
    assert dut.ndm_reset_o.value == 0
    assert dut.dm_reset_o.value == 0
    
    ndm_reset_task = cocotb.start_soon(wait_for_ndm_reset(dut))
    dm_reset_task = cocotb.start_soon(wait_for_dm_reset(dut))

    double_ndm_reset_task = cocotb.start_soon(make_sure_no_double_ndm_reset(dut))
    double_dm_reset_task = cocotb.start_soon(make_sure_no_double_dm_reset(dut))

    #Toggle a bit to simulate bouncing. Debouncer should absorb this.
    dut.ext_reset_i.value = 1
    await Timer(50, units="ns")
    dut.ext_reset_i.value = 0
    await Timer(200, units="ns")

    dut.ext_reset_i.value = 1
    await Timer(100, units="ns")
    dut.ext_reset_i.value = 0
    await Timer(100, units="ns")

    dut.ext_reset_i.value = 1
    await Timer(5000, units="ns")
    dut.ext_reset_i.value = 0
    await Timer(5000, units="ns")

    #Both tasks should have completed by now
    assert ndm_reset_task.done()
    assert dm_reset_task.done()

    #Read the register
    res = await with_timeout(wb_read(dut, 1), 30, 'ns')
    assert res == 16 #16 indicates External Reset.

    double_ndm_reset_task.kill()
    double_dm_reset_task.kill()

    #Wishbone triggered reset
    dut._log.info("WB reset test.")
    assert dut.ndm_reset_o == 0
    assert dut.dm_reset_o == 0

    ndm_reset_task = cocotb.start_soon(wait_for_ndm_reset(dut))
    dm_reset_task = cocotb.start_soon(wait_for_dm_reset(dut))

    await Timer(50, units="ns")

    await with_timeout(wb_write(dut, 0, 3), 30, 'ns')

    await Timer(50, units="ns")

    await with_timeout(wb_write(dut, 0, 0), 30, 'ns')

    await Timer(50, units="ns")

    #Both tasks should have completed by now
    assert ndm_reset_task.done()
    assert dm_reset_task.done()

    #Read the register
    res = await with_timeout(wb_read(dut, 1), 30, 'ns')
    assert res == 4+2 #4 and 2 indicate WB triggered NDM and DM reset.

    #Read it again to check read-reset behavior
    res = await with_timeout(wb_read(dut, 1), 30, 'ns')
    assert res == 0

    endTime = cocotb.utils.get_sim_time(units='ns')

    dut._log.info("startTime: %d, endTime %d", startTime, endTime)

if __name__ == "__main__":
    #Test Runner setup. Pass in the verilog sources and top-level.
    #The runner discovers the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../rtl/button_conditioner.sv",
                       proj_path / "../rtl/reset_ctrl.sv"]
    #Defined in scripts/cocotb_boxlambda.py
    test_runner(verilog_sources=verilog_sources, 
                test_module_filename=__file__, 
                top="reset_ctrl")
