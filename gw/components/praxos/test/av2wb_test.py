# test_my_design.py (extended)

import cocotb
from cocotb.triggers import FallingEdge, RisingEdge, Timer
from cocotb.clock import Clock
import random

async def wait_for_response(dut, responseValue):
    await FallingEdge(dut.av_waitrequest)
    assert dut.av_readdata.value == responseValue, "Read response mismatch"

@cocotb.test()
async def av_read_test(dut):
    """Try accessing the design."""

    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())

    await Timer(1, units="ns")  # wait 1 clock
    await FallingEdge(dut.clk)  # wait for falling edge/"negedge"

    address = random.randint(0, 0xffffffff)
    dut.av_address.value = address
    dut.av_read.value = 1
    dut.av_write.value = 0
    dut.wb_stall.value = 0
    dut.wb_ack.value = 0
    dut.wb_err.value = 0

    responseValue = random.randint(0, 0xffffffff)
    await cocotb.start(wait_for_response(dut, responseValue))  # run the clock "in the background"
    startTime = cocotb.utils.get_sim_time(units='ns')

    await RisingEdge(dut.wb_stb)
    stbStartTime = cocotb.utils.get_sim_time(units='ns')

    await FallingEdge(dut.wb_stb)
    stbEndTime = cocotb.utils.get_sim_time(units='ns')

    dut._log.info("startTime: %d, stbStartTime: %d, stbEndTime %d", startTime, stbStartTime, stbEndTime)

    #await Timer(1, units="ns")  # wait 1 clock
    #await RisingEdge(dut.clk)  # wait for rising edge/"posedge"
    #await FallingEdge(dut.clk)  # wait for falling edge/"negedge"
    #await RisingEdge(dut.clk)  # wait for rising edge/"posedge"
    #await FallingEdge(dut.clk)  # wait for falling edge/"negedge"

    assert (stbEndTime-stbStartTime) == 1, "wb_stb asserted too long"
    assert dut.wb_adr.value == address, "dut.wb_adr.value does not match av_address"
    assert dut.wb_cyc == 1, "wb_cyc not asserted on av_read"

    dut.wb_dat_r.value = responseValue
    dut.wb_ack.value = 1

    await Timer(1, units="ns")  # wait 1 clock
    dut.wb_ack.value = 0

    await Timer(10, units="ns")  # wait 10 clocks

    #assert dut.wb_cyc == 1, "wb_cyc no longer asserted on av_read 2nd clock"
    #assert dut.wb_stb == 0, "wb_stb still asserted on av_read 2nd clock"