from praxos_wordcopy import *
import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

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

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.clk)
        if dut.wb_s_ack.value == 1:
            dut._log.info("WBS: ack detected")
            ackDetected = True

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

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.clk)
        if dut.wb_s_ack.value == 1:
            dut._log.info("WBS: ack detected")
            ackDetected = True

    stall_check_task.kill()
    assert dut.wb_s_err.value == 0
    
    await RisingEdge(dut.clk)
    dut.wb_s_cyc.value = 0

async def wb_slave_emulator(dut):
    while True:
        await RisingEdge(dut.clk)
        if dut.wb_m_stb.value == 1:
            dut._log.info("WB: stb detected")
        
            if dut.wb_m_we.value == 1:
                dut._log.info("WB write, addr: 0x%x, data: 0x%x", int(dut.wb_m_adr.value), int(dut.wb_m_dat_w.value))
                wb_transactions.append(('write', int(dut.wb_m_adr.value), int(dut.wb_m_dat_w.value)))       
            else:
                dat_r = random.randint(0, 0xffffffff)
                dut.wb_m_dat_r.value = dat_r
                dut._log.info("WB read, addr: 0x%x, data: 0x%x", int(dut.wb_m_adr.value), dat_r)
                wb_transactions.append(('read', int(dut.wb_m_adr.value), dat_r))

            dut.wb_m_stall.value = 1    
            responseDelay = random.randint(1, 10)
            dut._log.info("WB: stalling %d ns", responseDelay)
            await Timer(responseDelay, units="ns")  # wait a random amount of time before responding
            await RisingEdge(dut.clk)
            dut._log.info("WB: signalling ACK, clearing stall.")
            dut.wb_m_ack.value = 1 #ACK
            dut.wb_m_stall.value = 0
            stdDeassertDetected = False
            while not stdDeassertDetected:
                await RisingEdge(dut.clk)
                if dut.wb_m_stb.value == 0:
                    dut._log.info("WB: stb deassert detected")
                    dut.wb_m_ack.value = 0
                    await Timer(1, units="ns")
                    stdDeassertDetected = True

#gp reg access: wb write - praxos read
@cocotb.test()
async def praxos_wordcopy_test(dut):
    await init(dut)

    pm_data = PRAXOS_WORDCOPY
    
    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut))

    #Praxos is initially in reset
    assert dut.praxos_rst_n.value == 0

    dut._log.info("Test: Loading program.")
    #Load the program into memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, 2, pm_data[ii] & 0xffffffff), 30, 'ns') #data low
        await with_timeout(wb_write(dut, 3, pm_data[ii] >> 32), 30, 'ns') #data high
        await with_timeout(wb_write(dut, 4, ii), 30, 'ns') #addr
        await with_timeout(wb_write(dut, 5, 0), 30, 'ns') #commit

    dut._log.info("Test: Taking Praxos out of reset.")
    #Take Praxos out of reset
    #Write the register
    await with_timeout(wb_write(dut, 6, 1), 30, 'ns')
    
    #Read the register
    res = await with_timeout(wb_read(dut, 6), 30, 'ns')
    assert res == 1
    
    #Ask Praxos to copy a number of words
    numWords = random.randint(1, 16)
    srcAddr = random.randint(0, 0x70000000) & ~3
    dstAddr = random.randint(0x80000000, 0xf0000000) & ~3

    dut._log.info("Test: Configuring DMA request.")
    dut._log.info("Test: numWords = %d, srcAddr = 0x%x, dstAddr = 0x%x", numWords, srcAddr, dstAddr)

    await with_timeout(wb_write(dut, 16, srcAddr), 30, 'ns')
    await with_timeout(wb_write(dut, 17, dstAddr), 30, 'ns')
    await with_timeout(wb_write(dut, 18, numWords), 30, 'ns')

    dut._log.info("Test: Kicking off DMA.")
    #Kick off the copy
    await with_timeout(wb_write(dut, 19, 1), 30, 'ns')

    timeout_task = cocotb.start_soon(timeout_check(dut))

    #Wait for completion
    res = 1
    while res != 0:
        resNew = await with_timeout(wb_read(dut, 19), 30, 'ns')
        if resNew != res:
            res = resNew
            dut._log.info("gp_reg[3] = %s", res)
    
    #Check the recorded transactions
    assert len(wb_transactions) == numWords*2
    for ii in range(0, len(wb_transactions), 2):
        rw, addr, dat_r = wb_transactions[ii]
        assert rw == 'read'
        assert addr == (srcAddr>>2)+ (ii/2)
        
        rw, addr, dat_w = wb_transactions[ii+1]
        assert rw == 'write'
        assert addr == (dstAddr>>2)+ (ii/2)
        assert dat_w == dat_r

if __name__ == "__main__":
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../rtl/praxos_ctrl.sv", 
                       proj_path / "../rtl/av2wb.sv", 
                       proj_path / "../rtl/praxos_generated.v",
                       proj_path / "../rtl/praxos_top.sv"]
    
    test_runner(verilog_sources=verilog_sources, 
                test_module_filename=__file__, 
                top="praxos_top")
