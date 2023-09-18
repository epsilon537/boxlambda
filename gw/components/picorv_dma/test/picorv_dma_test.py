import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *
import struct

async def init(dut):
    #For simplicity's sake, pretend we have a 1ns clock period.
    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())

    dut.rst.value = 1
    await Timer(10, units="ns")  # wait 10 clocks
    dut.rst.value = 0
    await Timer(1, units="ns")  # wait 1 clock

    dut.wbs_adr.value = 0
    dut.wbs_dat_w.value = 0
    dut.wbs_sel.value = 0
    dut.wbs_cyc.value = 0
    dut.wbs_stb.value = 0
    dut.wbs_we.value = 0
    dut.irq_in.value = 0
    dut.wbm_dat_i.value = 0
    dut.wbm_ack_i.value = 0
    dut.wbm_stall_i.value = 0

    await Timer(1, units="ns")  # wait 1 clock

def loadBinaryIntoWords(binfile):
    words=[]

    with open(binfile, mode='rb') as file:
        while True:
            bytes = file.read(4)
            if not bytes:
                break
            word = struct.unpack("I", bytes)[0]
            words.append(word)
    
    return words

async def wb_stall_check(dut):
    await RisingEdge(dut.clk)
    if dut.wbs_stall.value == 1:
        await FallingEdge(dut.wbs_stall)
    dut.wbs_stb.value = 0

async def wb_read(dut, addr):
    await RisingEdge(dut.clk)
        
    dut.wbs_adr.value = addr
    dut.wbs_sel.value = 0xf
    dut.wbs_cyc.value = 1
    dut.wbs_stb.value = 1
    dut.wbs_we.value = 0
    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.clk)
        if dut.wbs_ack.value == 1:
            #dut._log.info("WBS: ack detected")
            ackDetected = True

    stall_check_task.kill()
    assert dut.wbs_err.value == 0
    res = dut.wbs_dat_r.value
    
    await RisingEdge(dut.clk)
    dut.wbs_cyc.value = 0
    
    return res

async def wb_write(dut, addr, val):
    await RisingEdge(dut.clk)
    dut.wbs_adr.value = addr
    dut.wbs_dat_w.value = val
    dut.wbs_sel.value = 0xf
    dut.wbs_cyc.value = 1
    dut.wbs_stb.value = 1
    dut.wbs_we.value = 1

    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.clk)
        if dut.wbs_ack.value == 1:
            #dut._log.info("WBS: ack detected")
            ackDetected = True

    stall_check_task.kill()
    assert dut.wbs_err.value == 0
    
    await RisingEdge(dut.clk)
    dut.wbs_cyc.value = 0

@cocotb.test()
async def picorv_reset(dut):
    await init(dut)

    #Initially picorv should be held in reset
    assert dut.picorv_rst_n.value == 0

    #Read the register
    res = await with_timeout(wb_read(dut, 0x402), 30, 'ns')
    assert res == 0
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    assert dut.picorv_rst_n.value == 1
    
    #Read the register
    res = await with_timeout(wb_read(dut, 0x402), 30, 'ns')
    assert res == 1

    #Put praxos back in reset
    #Write the register
    await with_timeout(wb_write(dut, 0x402, 0), 30, 'ns')
    assert dut.picorv_rst_n.value == 0

    #Read the register
    res = await with_timeout(wb_read(dut, 0x402), 30, 'ns')
    assert res == 0

#WBS access to program memory
@cocotb.test()
async def wbs_access_to_program_memory(dut):
    await init(dut)

    pm_data = [0]*1024

    #Write PM memory
    for ii in range(1024):
        pm_data[ii] = random.randint(0, 0xffffffff)
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Read back from PM memory
    for ii in range(1024):
        res = await with_timeout(wb_read(dut, ii), 30, 'ns')
        dut._log.info("WB read, addr: 0x%x, data: 0x%x, ref: 0x%x", ii, res, pm_data[ii])
        assert int(res) == pm_data[ii]

#Pico write - WBS read access to GP registers.        
@cocotb.test()
async def pico_wr_wbs_rd(dut):
    await init(dut)

    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_wr_gp_regs.picobin")

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Read the last GP register
    gp15val=0
    retryCount = 0
    while gp15val == 0:
        retryCount += 1
        assert retryCount < 100
        await ClockCycles(dut.clk, 100)
        gp15val = await with_timeout(wb_read(dut, 0x410+15), 30, 'ns')

    #Now read all other GP register and see if the patterns match
    for ii in range(15):
        gpval = await with_timeout(wb_read(dut, 0x410+ii), 30, 'ns')
        assert gpval == 0x11111111*(ii+1)

#WBS write - pico read access to GP registers.
@cocotb.test()
async def wbs_wr_pico_rd(dut):
    await init(dut)

    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_gp_regs_copy.picobin")

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    gp_reg_vals = [0]*16 

    #Write pattern to even registers
    for ii in range(0, 16, 2):
        gp_reg_vals[ii] = random.randint(0, 0xffffffff)
        await with_timeout(wb_write(dut, 0x410+ii, gp_reg_vals[ii]), 30, 'ns')

    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Read the last GP register to see if it becomes non-zero
    gp15val=0
    retryCount = 0
    while gp15val == 0:
        retryCount += 1
        assert retryCount < 100
        await ClockCycles(dut.clk, 100)
        gp15val = await with_timeout(wb_read(dut, 0x410+15), 30, 'ns')

    #Now read all odd GP register and see if the patterns match
    for ii in range(1, 16, 2):
        res = await with_timeout(wb_read(dut, 0x410+ii), 30, 'ns')
        dut._log.info("WB read, addr: 0x%x, data: 0x%x, ref: 0x%x", 0x410+ii, res, gp_reg_vals[ii-1])
        assert int(res) == gp_reg_vals[ii-1]

#IRQ in, out, ack
@cocotb.test()
async def irq_in_out_ack(dut):
    await init(dut)

    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_irq_in_out.picobin")

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Wait some time
    await ClockCycles(dut.clk, random.randint(0,100))

    #Set input interrupts
    irqval = random.randint(1,0x7fffffff)
    dut.irq_in = irqval

    #Wait for output interrupt
    await with_timeout(RisingEdge(dut.irq_out), 300, 'ns')

    #Acknowledge irq by writing to reg 0
    await with_timeout(wb_write(dut, 0x400, irqval), 30, 'ns')

    await ClockCycles(dut.clk, 1)

    #irq out should be low now.
    assert dut.irq_out.value == 0

#WBM R/W word access

#WBM R/W byte access

if __name__ == "__main__":
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../../../../sub/picorv32/picosoc/picosoc_mem.v",
                       proj_path / "../../../../sub/picorv32/picorv32.v",
                       proj_path / "../rtl/picorv_dma_top.sv"]
    test_runner(verilog_sources=verilog_sources, 
                test_module_filename=__file__, 
                top="picorv_dma_top")
