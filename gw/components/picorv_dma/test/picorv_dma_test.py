import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *
import struct

wb_transactions = []

async def init(dut):
    global wb_transactions

    wb_transactions = []

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

def selToMask(sel):
    mask = 0
    if sel&1:
        mask |= 0xff
    if sel&2:
        mask |= 0xff00
    if sel&4:
        mask |= 0xff0000
    if sel&8:
        mask |= 0xff000000
    
    return mask

async def timeout_check(dut):
    await Timer(20000, units="ns")
    #We should never reach this point
    assert False, "Transaction timeout!"

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

async def wb_slave_emulator(dut):
    while True:
        await RisingEdge(dut.clk)
        if dut.wbm_stb_o.value == 1:
            dut._log.info("WB: stb detected")
        
            if dut.wbm_we_o.value == 1:
                dut._log.info("WB write, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm_adr_o.value), int(dut.wbm_dat_o.value), int(dut.wbm_sel_o.value))
                wb_transactions.append(('write', int(dut.wbm_adr_o.value), int(dut.wbm_dat_o.value), int(dut.wbm_sel_o)))       
            else:
                dat_r = random.randint(0, 0xffffffff)
                dut.wbm_dat_i.value = dat_r
                dut._log.info("WB read, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm_adr_o.value), dat_r, int(dut.wbm_sel_o))
                wb_transactions.append(('read', int(dut.wbm_adr_o.value), dat_r, int(dut.wbm_sel_o)))

            dut.wbm_stall_i.value = 1    
            responseDelay = random.randint(1, 10)
            dut._log.info("WB: stalling %d ns", responseDelay)
            await Timer(responseDelay, units="ns")  # wait a random amount of time before responding
            await RisingEdge(dut.clk)
            dut._log.info("WB: signalling ACK, clearing stall.")
            dut.wbm_ack_i.value = 1 #ACK
            dut.wbm_stall_i.value = 0
            stdDeassertDetected = False
            while not stdDeassertDetected:
                await RisingEdge(dut.clk)
                if dut.wbm_stb_o.value == 0:
                    dut._log.info("WB: stb deassert detected")
                    dut.wbm_ack_i.value = 0
                    await Timer(1, units="ns")
                    stdDeassertDetected = True

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
@cocotb.test()
async def wordcopy_test(dut):
    await init(dut)

    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_wordcopy.picobin")

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut))

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Ask Praxos to copy a number of words
    numWords = random.randint(1, 16)
    srcAddr = random.randint(0x00004000, 0x70000000) & ~3
    dstAddr = random.randint(0x80000000, 0xf0000000) & ~3

    dut._log.info("Test: Configuring DMA request.")
    dut._log.info("Test: numWords = %d, srcAddr = 0x%x, dstAddr = 0x%x", numWords, srcAddr, dstAddr)

    await with_timeout(wb_write(dut, 0x410, srcAddr), 30, 'ns')
    await with_timeout(wb_write(dut, 0x411, dstAddr), 30, 'ns')
    await with_timeout(wb_write(dut, 0x412, numWords), 30, 'ns')

    dut._log.info("Test: Kicking off DMA.")
    #Kick off the copy
    await with_timeout(wb_write(dut, 0x413, 1), 30, 'ns')

    timeout_task = cocotb.start_soon(timeout_check(dut))

    #Wait for completion
    res = 1
    while res != 0:
        resNew = await with_timeout(wb_read(dut, 0x413), 30, 'ns')
        if resNew != res:
            res = resNew
            dut._log.info("gp_reg[3] = %s", res)
    
    #Check the recorded transactions
    assert len(wb_transactions) == numWords*2
    for ii in range(0, len(wb_transactions), 2):
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == (srcAddr>>2)+ (ii/2)
        assert sel == 0xf

        rw, addr, dat_w, sel = wb_transactions[ii+1]
        assert rw == 'write'
        assert addr == (dstAddr>>2)+ (ii/2)
        assert dat_w == dat_r
        assert sel == 0xf

#WBM R/W byte access
@cocotb.test()
async def bytecopy_test(dut):
    await init(dut)

    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_bytecopy.picobin")

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut))

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Ask Praxos to copy a number of words
    numBytes = random.randint(1, 16)
    srcAddr = random.randint(0x00004000, 0x70000000)
    dstAddr = random.randint(0x80000000, 0xf0000000)

    dut._log.info("Test: Configuring DMA request.")
    dut._log.info("Test: numWords = %d, srcAddr = 0x%x, dstAddr = 0x%x", numBytes, srcAddr, dstAddr)

    await with_timeout(wb_write(dut, 0x410, srcAddr), 30, 'ns')
    await with_timeout(wb_write(dut, 0x411, dstAddr), 30, 'ns')
    await with_timeout(wb_write(dut, 0x412, numBytes), 30, 'ns')

    dut._log.info("Test: Kicking off DMA.")
    #Kick off the copy
    await with_timeout(wb_write(dut, 0x413, 1), 30, 'ns')

    timeout_task = cocotb.start_soon(timeout_check(dut))

    #Wait for completion
    res = 1
    while res != 0:
        resNew = await with_timeout(wb_read(dut, 0x413), 30, 'ns')
        if resNew != res:
            res = resNew
            dut._log.info("gp_reg[3] = %s", res)
    
    #Check the recorded transactions
    assert len(wb_transactions) == numBytes*2
    for ii in range(0, len(wb_transactions), 2):
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert sel == 0xf
        assert addr == (srcAddr + (ii>>1))>>2
        
        rw, addr, dat_w, sel = wb_transactions[ii+1]
        assert rw == 'write'
        assert sel == (1<<(((dstAddr + (ii>>1))%4)))
        assert addr == (dstAddr + (ii>>1))>>2

if __name__ == "__main__":
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../../../../sub/picorv32/picosoc/picosoc_mem.v",
                       proj_path / "../../../../sub/picorv32/picorv32.v",
                       proj_path / "../rtl/picorv_dma_top.sv"]
    test_runner(verilog_sources=verilog_sources, 
                test_module_filename=__file__, 
                top="picorv_dma_top")
