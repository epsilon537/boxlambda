# test_my_design.py (extended)

import cocotb
from cocotb.triggers import FallingEdge, RisingEdge, Timer
from cocotb.clock import Clock
import random

async def wait_for_ndm_reset(dut):
    await RisingEdge(dut.ndm_reset_o)
    startTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("ndm_reset asserted")
    await FallingEdge(dut.ndm_reset_o)
    endTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("ndm_reset deasserted")
    assert (endTime-startTime) > 7

async def wait_for_dm_reset(dut):
    await RisingEdge(dut.dm_reset_o)
    startTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("dm_reset asserted")
    await FallingEdge(dut.dm_reset_o)
    endTime = cocotb.utils.get_sim_time(units='ns')
    dut._log.info("dm_reset deasserted")
    assert (endTime-startTime) > 7

async def make_sure_no_dm_reset(dut):
    await RisingEdge(dut.dm_reset_o)
    assert False

async def make_sure_no_double_ndm_reset(dut):
    await RisingEdge(dut.ndm_reset_o)
    dut._log.info("ndm_reset asserted (1)")
    await FallingEdge(dut.ndm_reset_o)
    dut._log.info("ndm_reset deasserted (1)")
    await RisingEdge(dut.ndm_reset_o)
    dut._log.info("ndm_reset asserted (2)")
    assert 0

async def make_sure_no_double_dm_reset(dut):
    await RisingEdge(dut.dm_reset_o)
    dut._log.info("dm_reset asserted (1)")
    await FallingEdge(dut.dm_reset_o)
    dut._log.info("dm_reset deasserted (1)")
    await RisingEdge(dut.dm_reset_o)
    dut._log.info("dm_reset asserted (2)")
    assert 0

async def make_sure_por_stays_completed(dut):
    await FallingEdge(dut.por_completed_o)
    assert 0

@cocotb.test()
async def por_followed_by_ndm_reset_followed_by_ext_reset_followed_by_WB_reset_test(dut):
    """Try accessing the design."""

    #Wishbone
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

    assert dut.ndm_reset_o.value == 0
    assert dut.dm_reset_o.value == 0

    ndm_reset_task = cocotb.start_soon(wait_for_ndm_reset(dut))
    dm_reset_task = cocotb.start_soon(wait_for_dm_reset(dut))
    dut.pll_locked_i.value = 1;

    await Timer(3, units="ns")  # wait 3 clocks
    assert dut.ndm_reset_o.value == 0
    assert dut.por_completed_o == 0

    await Timer(30, units="ns")  # wait 30 clocks

    #Both tasks should have completed by now
    assert ndm_reset_task.done()
    assert dm_reset_task.done()
    assert dut.por_completed_o == 1

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

    await Timer(10, units="ns")  # wait 10 clocks

    #Now let's trigger an external reset
    dut._log.info("Ext. reset test.")
    assert dut.ndm_reset_o.value == 0
    assert dut.dm_reset_o.value == 0
    
    ndm_reset_task = cocotb.start_soon(wait_for_ndm_reset(dut))
    dm_reset_task = cocotb.start_soon(wait_for_dm_reset(dut))

    double_ndm_reset_task = cocotb.start_soon(make_sure_no_double_ndm_reset(dut))
    double_dm_reset_task = cocotb.start_soon(make_sure_no_double_dm_reset(dut))

    #Toggle a bit to simulate bouncing
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

    double_ndm_reset_task.kill()
    double_dm_reset_task.kill()

    #Wishbone triggered reset
    dut._log.info("WB reset test.")
    assert dut.ndm_reset_o == 0
    assert dut.dm_reset_o == 0

    ndm_reset_task = cocotb.start_soon(wait_for_ndm_reset(dut))
    dm_reset_task = cocotb.start_soon(wait_for_dm_reset(dut))

    await Timer(50, units="ns")
    await RisingEdge(dut.clk)
    dut.wb_adr.value = 0
    dut.wb_dat_w.value = 3
    dut.wb_cyc.value = 1
    dut.wb_stb.value = 1
    dut.wb_we.value = 1

    await RisingEdge(dut.wb_ack)
    await RisingEdge(dut.clk)
    dut.wb_adr.value = 0
    dut.wb_dat_w.value = 0
    dut.wb_cyc.value = 0
    dut.wb_stb.value = 0
    dut.wb_we.value = 0
    
    await Timer(50, units="ns")

    assert dut.wb_ack.value == 0

    await Timer(50, units="ns")

    #Both tasks should have completed by now
    assert ndm_reset_task.done()
    assert dm_reset_task.done()

    endTime = cocotb.utils.get_sim_time(units='ns')

    dut._log.info("startTime: %d, endTime %d", startTime, endTime)

