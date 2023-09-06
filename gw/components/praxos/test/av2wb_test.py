# test_my_design.py (extended)

import cocotb
from cocotb.triggers import FallingEdge, RisingEdge, Timer
from cocotb.clock import Clock
import random
from pathlib import Path
from cocotb.runner import *
from cocotb_genDumpModule import *

async def init(dut):
    #For simplicity's sake, pretend we have a 1ns clock period.
    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())

    dut.rst_n.value = 0
    await Timer(1, units="ns")  # wait 1 clock
    dut.rst_n.value = 1
    dut.av_address.value = 0
    dut.av_read.value = 0
    dut.av_write.value = 0
    dut.av_byteenable.value = 0
    dut.wb_stall.value = 0
    dut.wb_ack.value = 0
    dut.wb_err.value = 0
    await Timer(1, units="ns")  # wait 1 clock

async def timeout_check(dut):
    await Timer(30, units="ns")  # wait 30 clocks
    #We should never reach this point
    assert False, "Transaction timeout!"

async def wait_for_read_response(dut):
    while True:
        await RisingEdge(dut.clk)
        dut._log.info("AV: read = %s, write = %s, waitrequest = %s", dut.av_read.value, dut.av_write.value, dut.av_waitrequest.value)
        if ((dut.av_read.value == 1) or (dut.av_write.value == 1)) and (dut.av_waitrequest.value == 0):
            dut._log.info("AV: response received.")
            dut.av_read.value = 0
            dut.av_write.value = 0
            assert dut.av_readdata.value == dut.wb_dat_r.value, "Read response mismatch"
            break

async def wait_for_write_response(dut):
    while True:
        await RisingEdge(dut.clk)
        dut._log.info("AV: read = %s, write = %s, waitrequest = %s", dut.av_read.value, dut.av_write.value, dut.av_waitrequest.value)
        if ((dut.av_read.value == 1) or (dut.av_write.value == 1)) and (dut.av_waitrequest.value == 0):
            dut._log.info("AVL response received.")
            dut.av_read.value = 0
            dut.av_write.value = 0
            break

async def wb_slave_emulator(dut):
    while True:
        await RisingEdge(dut.clk)
        if dut.wb_stb.value == 1:
            dut._log.info("WB: stb detected")
            assert dut.wb_cyc.value == 1, 'WB: cyc not asserted at start of transaction'
            assert dut.wb_sel.value == dut.av_byteenable.value, 'WB: byte enable mismatch'
            assert dut.wb_adr.value == dut.av_address.value, 'WB: address mismatch'

            if dut.wb_we.value == 1:
                assert dut.wb_dat_w.value == dut.av_writedata.value, 'WB: write data mismatch'        
            else:
                dut.wb_dat_r.value = random.randint(0, 0xffffffff)

            dut.wb_stall.value = 1    
            responseDelay = random.randint(1, 10)
            dut._log.info("WB: stalling %d ns", responseDelay)
            await Timer(responseDelay, units="ns")  # wait a random amount of time before responding
            await RisingEdge(dut.clk)
            dut._log.info("WB: signalling ACK, clearing stall.")
            dut.wb_ack.value = 1 #ACK
            dut.wb_stall.value = 0
            while True:
                await RisingEdge(dut.clk)
                if dut.wb_stb.value == 0:
                    dut._log.info("WB: stb deassert detected")
                    dut.wb_ack.value = 0
                    await Timer(1, units="ns")
                    return 

@cocotb.test()
async def av_read_test(dut):
    await init(dut)

    await RisingEdge(dut.clk)  # wait for falling edge/"negedge"
    
    timeout_task = cocotb.start_soon(timeout_check(dut))
    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut))
    av_read_response_task = cocotb.start_soon(wait_for_read_response(dut))

    dut._log.info("Test: Initiating AV read...")
    dut.av_address.value = random.randint(0, 0xffffffff)
    dut.av_read.value = 1
    dut.av_write.value = 0
    dut.av_byteenable.value = random.randint(1, 0xf)

    await wb_slave_task
    dut._log.info("Test: wb slave_task completed.")

    await av_read_response_task
    dut._log.info("Test: av read response task completed.")

    timeout_task.kill()

@cocotb.test()
async def av_write_test(dut):
    await init(dut)

    await RisingEdge(dut.clk)  # wait for falling edge/"negedge"

    timeout_task = cocotb.start_soon(timeout_check(dut))
    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut))
    av_write_response_task = cocotb.start_soon(wait_for_write_response(dut))

    dut._log.info("Test: Initiating AV write...")
    dut.av_address.value = random.randint(0, 0xffffffff)
    dut.av_writedata.value = random.randint(0, 0xffffffff)
    dut.av_read.value = 0
    dut.av_write.value = 1
    dut.av_byteenable.value = random.randint(1, 0xf)

    await wb_slave_task
    dut._log.info("Test: wb slave_task completed.")
    await av_write_response_task
    dut._log.info("Test: av write response task completed.")

    timeout_task.kill()

def av2wb_test_runner():
    hdl_toplevel_lang = "verilog"
    sim = "icarus"
    build_dir= 'av2wb_sim_build'
    proj_path = Path(__file__).resolve().parent
    top= "av2wb"

    verilog_sources = [proj_path / "../rtl/av2wb.sv", 
                       genDumpModule(build_dir, top)]

    runner = get_runner(sim)
    runner.build(
        verilog_sources=verilog_sources,
        vhdl_sources= [],
        hdl_toplevel= top,
        always=True,
        build_dir=build_dir
    )

    res = runner.test(hdl_toplevel=top, test_module="av2wb_test,", plusargs=['-fst'])
    check_results_file(res)

if __name__ == "__main__":
    av2wb_test_runner()