import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb.runner import *
from cocotb_genDumpModule import *
import importlib
import time

wb_transactions = []

async def init(dut):
    #For simplicity's sake, pretend we have a 1ns clock period.
    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())

    dut.rst.value = 1
    await Timer(1, units="ns")  # wait 1 clock
    dut.rst.value = 0
    await Timer(1, units="ns")  # wait 1 clock

    dut.wb_s_adr.value = 0
    dut.wb_s_dat_w.value = 0
    dut.wb_s_sel.value = 0
    dut.wb_s_cyc.value = 0
    dut.wb_s_stb.value = 0
    dut.wb_s_we.value = 0
    dut.irq_in.value = 0

    dut.wb_m_dat_r.value = 0
    dut.wb_m_stall.value = 0
    dut.wb_m_ack.value = 0
    dut.wb_m_err.value = 0

async def timeout_check(dut):
    await Timer(20000, units="ns")
    #We should never reach this point
    assert False, "Transaction timeout!"

async def wb_stall_check(dut):
    await RisingEdge(dut.clk)
    if dut.wb_s_stall.value == 1:
        await FallingEdge(dut.wb_s_stall)
    dut.wb_s_stb.value = 0

async def wb_read(dut, addr):
    await RisingEdge(dut.clk)
        
    dut.wb_s_adr.value = addr
    dut.wb_s_sel.value = 0xf
    dut.wb_s_cyc.value = 1
    dut.wb_s_stb.value = 1
    dut.wb_s_we.value = 0
    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    await RisingEdge(dut.wb_s_ack)
    stall_check_task.kill()
    assert dut.wb_s_err.value == 0
    res = dut.wb_s_dat_r.value
    
    await RisingEdge(dut.clk)
    dut.wb_s_cyc.value = 0
    
    return res

async def wb_write(dut, addr, val):
    await RisingEdge(dut.clk)
    dut.wb_s_adr.value = addr
    dut.wb_s_dat_w.value = val
    dut.wb_s_sel.value = 0xf
    dut.wb_s_cyc.value = 1
    dut.wb_s_stb.value = 1
    dut.wb_s_we.value = 1

    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    await RisingEdge(dut.wb_s_ack)
    stall_check_task.kill()
    assert dut.wb_s_err.value == 0
    
    await RisingEdge(dut.clk)
    dut.wb_s_cyc.value = 0

async def wb_slave_emulator(dut):
    while True:
        await RisingEdge(dut.clk)
        if dut.wb_m_stb.value == 1:
            if dut.wb_m_we.value == 1:
                print("WB: write addr: {:08X} sel: {} data: {:08X}".format(int(dut.wb_m_adr.value), int(dut.wb_m_sel.value), int(dut.wb_m_dat_w.value)))       
            else:
                if dut.wb_m_sel.value == 1:
                    dat_r = 0x00000044
                elif dut.wb_m_sel.value == 2:
                    dat_r = 0x00003300
                elif dut.wb_m_sel.value == 4:
                    dat_r = 0x00220000
                else:
                    dat_r = 0x11000000

                dut.wb_m_dat_r.value = dat_r
                print("WB: read addr: {:08X} sel: {} data: {:08X}".format(int(dut.wb_m_adr.value), int(dut.wb_m_sel.value), int(dat_r)))

            dut.wb_m_stall.value = 1    
            responseDelay = random.randint(1, 10)
            await Timer(responseDelay, units="ns")  # wait a random amount of time before responding
            await RisingEdge(dut.clk)
            dut.wb_m_ack.value = 1 #ACK
            dut.wb_m_stall.value = 0
            stdDeassertDetected = False
            while not stdDeassertDetected:
                await RisingEdge(dut.clk)
                if dut.wb_m_stb.value == 0:
                    dut.wb_m_ack.value = 0
                    await Timer(1, units="ns")
                    stdDeassertDetected = True

async def monitor_task(dut):
    count = 0
    while count < 8000:
        count += 1
        await RisingEdge(dut.clk)
        acc = dut.praxos_cpu_inst.acc
        index = dut.praxos_cpu_inst.index
        pc = dut.praxos_cpu_inst.pc

        try:
            acc = "{:08X}".format(int(acc))
        except:
            acc = 'xxxxxxxx'
        
        try:
            index = "{:08X}".format(int(index))
        except:
            index = 'xxxxxxxx'

        try:
            pc = "{:02X}".format(int(pc))
        except:
            pc = 'xx'

        print("MON {}: acc={} index={} pc={}".format(count, acc, index, pc))

async def pre_start_actions(dut):
    srcAddr = random.randint(0, 0x70000000)
    print("srcAddr = 0x%x"%(srcAddr))
    await with_timeout(wb_write(dut, 16, srcAddr), 30, 'ns')
    dstAddr = random.randint(0, 0x70000000)
    print("dstAddr = 0x%x"%(dstAddr))
    await with_timeout(wb_write(dut, 17, dstAddr), 30, 'ns')
    numBytes = 32
    print("numBytes = 0x%x"%(numBytes))
    await with_timeout(wb_write(dut, 18, numBytes), 30, 'ns')

async def post_start_actions(dut):
    pass
    #Ask Praxos to copy a number of words
    # numWords = random.randint(1, 16)
    # srcAddr = random.randint(0, 0x70000000) & ~3
    # dstAddr = random.randint(0x80000000, 0xf0000000) & ~3

    # dut._log.info("Test: Configuring DMA request.")
    # dut._log.info("Test: numWords = %d, srcAddr = 0x%x, dstAddr = 0x%x", numWords, srcAddr, dstAddr)

    # await with_timeout(wb_write(dut, 16, srcAddr), 30, 'ns')
    # await with_timeout(wb_write(dut, 17, dstAddr), 30, 'ns')
    # await with_timeout(wb_write(dut, 18, numWords), 30, 'ns')

    # dut._log.info("Test: Kicking off DMA.")
    # #Kick off the copy
    # await with_timeout(wb_write(dut, 19, 1), 30, 'ns')

@cocotb.test()
async def praxos_monitor(dut):
    pm_data = importlib.import_module("praxos_prog").PRAXOS_PROG

    await init(dut)
    
    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut))

    #Praxos is initially in reset
    assert dut.praxos_rst_n.value == 0

    print("Loading program.")
    #Load the program into memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, 2, pm_data[ii] & 0xffffffff), 30, 'ns') #data low
        await with_timeout(wb_write(dut, 3, pm_data[ii] >> 32), 30, 'ns') #data high
        await with_timeout(wb_write(dut, 4, ii), 30, 'ns') #addr
        await with_timeout(wb_write(dut, 5, 0), 30, 'ns') #commit

    print("Pre-start actions...")
    await pre_start_actions(dut)

    print("Taking Praxos out of reset.")
    #Take Praxos out of reset
    #Write the register
    await with_timeout(wb_write(dut, 6, 1), 30, 'ns')

    print("Launching monitor...")
    mon_task = cocotb.start_soon(monitor_task(dut))

    print("Post-start actions...")
    cocotb.start_soon(post_start_actions(dut))
    
    await mon_task

def praxos_monitor_test_runner():
    hdl_toplevel_lang = "verilog"
    sim = "icarus"
    build_dir= 'praxos_monitor_sim_build'
    proj_path = Path(__file__).resolve().parent
    top = "praxos_top"
    test_module = "praxos_monitor,"

    verilog_sources = [proj_path / "../rtl/praxos_ctrl.sv", 
                       proj_path / "../rtl/av2wb.sv", 
                       proj_path / "../rtl/praxos_generated.v",
                       proj_path / "../rtl/praxos_top.sv", 
                       genDumpModule(build_dir, top)]

    runner = get_runner(sim)
    runner.build(
        verilog_sources=verilog_sources,
        vhdl_sources= [],
        hdl_toplevel= top,
        always=True,
        build_dir=build_dir
    )

    res = runner.test(hdl_toplevel=top, test_module=test_module, plusargs=['-fst'])

if __name__ == "__main__":
    praxos_monitor_test_runner()
