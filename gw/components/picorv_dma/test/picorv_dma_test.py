import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *
import struct

#Cocotb-based unit testcases for picorv_dma

wb0_transactions = []
wb1_transactions = []

async def sys_clk_div(dut):
    while True:
        await RisingEdge(dut.sys_clkx2)
        dut.sys_clk.value = not dut.sys_clk.value 

async def init(dut):
    global wb0_transactions, wb1_transactions

    #Clear the wishbone transaction lists.
    wb0_transactions = []
    wb1_transactions = []

    dut.sys_clk.value = 0
    dut.sys_clkx2.value = 0

    #For simplicity's sake, pretend we have a 1ns clock period.
    cocotb.start_soon(Clock(dut.sys_clkx2, 1, units="ns").start())
    cocotb.start_soon(sys_clk_div(dut))

    #Assert reset
    dut.rst.value = 1
    await Timer(10, units="ns")  # wait 10 clocks
    dut.rst.value = 0
    await Timer(1, units="ns")  # wait 1 clock

    #Initial values for input signals
    dut.wbs_adr.value = 0
    dut.wbs_dat_w.value = 0
    dut.wbs_sel.value = 0
    dut.wbs_cyc.value = 0
    dut.wbs_stb.value = 0
    dut.wbs_we.value = 0
    dut.irq_in.value = 0
    dut.wbm0_dat_i.value = 0
    dut.wbm0_ack_i.value = 0
    dut.wbm0_stall_i.value = 0
    dut.wbm0_err_i.value = 0
    dut.wbm1_dat_i.value = 0
    dut.wbm1_ack_i.value = 0
    dut.wbm1_stall_i.value = 0
    dut.wbm1_err_i.value = 0

    await Timer(1, units="ns")  # wait 1 clock

#Function loading given binary file into a list of 32-bit words.
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

#Asynchronous timeout task. The test fails if this task isn't killed before the timeout is reached.
async def timeout_check(dut):
    await Timer(20000, units="ns")
    #We should never reach this point
    assert False, "Transaction timeout!"

#Asynchronous task deasserting WB STB signal when slave no longer stalls.
async def wb_stall_check(dut):
    await RisingEdge(dut.sys_clk)
    if dut.wbs_stall.value == 1:
        await FallingEdge(dut.wbs_stall)
    dut.wbs_stb.value = 0

#Wishbone word read transaction
async def wb_read(dut, addr):
    await RisingEdge(dut.sys_clk)
        
    dut.wbs_adr.value = addr
    dut.wbs_sel.value = 0xf
    dut.wbs_cyc.value = 1
    dut.wbs_stb.value = 1
    dut.wbs_we.value = 0
    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.sys_clk)
        if dut.wbs_ack.value == 1:
            #dut._log.info("WBS: ack detected")
            ackDetected = True

    stall_check_task.kill()
    assert dut.wbs_err.value == 0
    res = dut.wbs_dat_r.value
    
    await RisingEdge(dut.sys_clk)
    dut.wbs_cyc.value = 0
    
    return res

#Wishone word write transaction
async def wb_write(dut, addr, val):
    await RisingEdge(dut.sys_clk)
    dut.wbs_adr.value = addr
    dut.wbs_dat_w.value = val
    dut.wbs_sel.value = 0xf
    dut.wbs_cyc.value = 1
    dut.wbs_stb.value = 1
    dut.wbs_we.value = 1

    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    ackDetected = False
    while not ackDetected:
        await RisingEdge(dut.sys_clk)
        if dut.wbs_ack.value == 1:
            #dut._log.info("WBS: ack detected")
            ackDetected = True

    stall_check_task.kill()
    assert dut.wbs_err.value == 0
    
    await RisingEdge(dut.sys_clk)
    dut.wbs_cyc.value = 0

async def wb0_slave_stall(dut):
    while True:
        dut.wbm0_stall_i.value = 1    
        stallDelay = random.randint(1, 10) #Randomly stall 1-10 ticks
        dut._log.info("WB0: stalling %d ns", stallDelay)
        await Timer(stallDelay, units="ns")
        dut.wbm0_stall_i.value = 0 
        stallDelay = random.randint(1, 10) #Randomly stall 1-10 ticks
        dut._log.info("WB0: not stalling %d ns", stallDelay)
        await Timer(stallDelay, units="ns")

async def wb1_slave_stall(dut):
    while True:
        dut.wbm1_stall_i.value = 1    
        stallDelay = random.randint(1, 10) #Randomly stall 1-10 ticks
        dut._log.info("WB1: stalling %d ns", stallDelay)
        await Timer(stallDelay, units="ns")
        dut.wbm1_stall_i.value = 0 
        stallDelay = random.randint(1, 10) #Randomly stall 1-10 ticks
        dut._log.info("WB1: not stalling %d ns", stallDelay)
        await Timer(stallDelay, units="ns")

#Asynchronous task emulating a Wishbone slave. Received transactions are recorded in a
#wb_transactions list.
async def wb0_slave_emulator(dut, delay_ack):
    while True:
        await RisingEdge(dut.sys_clk)
        if dut.wbm0_stb_o.value == 1:
            dut._log.info("WB: stb detected")
        
            if dut.wbm0_we_o.value == 1:
                dut._log.info("WB write, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm0_adr_o.value), int(dut.wbm0_dat_o.value), int(dut.wbm0_sel_o.value))
                wb0_transactions.append(('write', int(dut.wbm0_adr_o.value), int(dut.wbm0_dat_o.value), int(dut.wbm0_sel_o)))       
            else:
                #Return random data to read transactions
                dat_r = random.randint(0, 0xffffffff)
                dut.wbm0_dat_i.value = dat_r
                dut._log.info("WB read, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm0_adr_o.value), dat_r, int(dut.wbm0_sel_o))
                wb0_transactions.append(('read', int(dut.wbm0_adr_o.value), dat_r, int(dut.wbm0_sel_o)))

            await RisingEdge(dut.sys_clk)
            assert dut.wbm0_stb_o.value == 0
            dut._log.info("WB: stb deassert detected.")

            if delay_ack:       
                responseDelay = random.randint(0, 10) #Randomly delay ack 0-10 ticks
                dut._log.info("WB: delaying ack %d ns", responseDelay)
                await ClockCycles(dut.sys_clk, responseDelay)

            dut._log.info("WB: signalling ACK.")
            dut.wbm0_ack_i.value = 1 #ACK
            
            await RisingEdge(dut.sys_clk)
            
            dut.wbm0_ack_i.value = 0
            await Timer(1, units="ns")

#Asynchronous task emulating a Wishbone slave. Received transactions are recorded in a
#wb_transactions list.
async def wb1_slave_emulator(dut, delay_ack):
    while True:
        await RisingEdge(dut.sys_clk)
        if dut.wbm1_stb_o.value == 1:
            dut._log.info("WB: stb detected")
        
            if dut.wbm1_we_o.value == 1:
                dut._log.info("WB write, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm1_adr_o.value), int(dut.wbm1_dat_o.value), int(dut.wbm1_sel_o.value))
                wb1_transactions.append(('write', int(dut.wbm1_adr_o.value), int(dut.wbm1_dat_o.value), int(dut.wbm1_sel_o)))       
            else:
                #Return random data to read transactions
                dat_r = random.randint(0, 0xffffffff)
                dut.wbm1_dat_i.value = dat_r
                dut._log.info("WB read, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm1_adr_o.value), dat_r, int(dut.wbm1_sel_o))
                wb1_transactions.append(('read', int(dut.wbm1_adr_o.value), dat_r, int(dut.wbm1_sel_o)))

            await RisingEdge(dut.sys_clk)
            assert dut.wbm1_stb_o.value == 0
            dut._log.info("WB: stb deassert detected.")

            if delay_ack:       
                responseDelay = random.randint(0, 10) #Randomly delay ack 0-10 ticks
                dut._log.info("WB: delaying ack %d ns", responseDelay)
                await ClockCycles(dut.sys_clk, responseDelay)

            dut._log.info("WB: signalling ACK.")
            dut.wbm1_ack_i.value = 1 #ACK
            
            await RisingEdge(dut.sys_clk)
            
            dut.wbm1_ack_i.value = 0
            await Timer(1, units="ns")

#Test PicoRV reset
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

#Test WB slave access to program memory
@cocotb.test()
async def wbs_access_to_program_memory(dut):
    await init(dut)

    pm_data = [0]*1024

    #Write PM memory
    for ii in range(1024):
        #Fill with random data
        pm_data[ii] = random.randint(0, 0xffffffff)
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Read back from PM memory
    for ii in range(1024):
        res = await with_timeout(wb_read(dut, ii), 30, 'ns')
        dut._log.info("WB read, addr: 0x%x, data: 0x%x, ref: 0x%x", ii, res, pm_data[ii])
        assert int(res) == pm_data[ii]

#Pico write - WBS read access to HIR (Host Interface Registers).        
@cocotb.test()
async def pico_wr_wbs_rd(dut):
    await init(dut)

    #Load PicoRV program that writes 0x11111111, 0x2222222... to hir regs 0, 1...
    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_wr_hir_regs.picobin")

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Read the last hir register until it becomes non-zero, or until we give up.
    hir15val=0
    retryCount = 0
    while hir15val == 0:
        retryCount += 1
        assert retryCount < 100
        await ClockCycles(dut.sys_clk, 100)
        hir15val = await with_timeout(wb_read(dut, 0x410+15), 30, 'ns')

    #Now read all other hir register and see if the patterns match
    for ii in range(15):
        hirval = await with_timeout(wb_read(dut, 0x410+ii), 30, 'ns')
        assert hirval == 0x11111111*(ii+1)

#WB slave write - pico read access to HIR (Host Interface Registers).
@cocotb.test()
async def wbs_wr_pico_rd(dut):
    await init(dut)

    #Load PicoRV program that copies hir0 to hir1, hir2 to hir3, etc.
    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_hir_regs_copy.picobin")

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    hir_reg_vals = [0]*16 

    #Write pattern to even registers
    for ii in range(0, 16, 2):
        hir_reg_vals[ii] = random.randint(0, 0xffffffff)
        await with_timeout(wb_write(dut, 0x410+ii, hir_reg_vals[ii]), 30, 'ns')

    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Keep reading the last hir register until it becomes non-zero, or until we give up.
    hir15val=0
    retryCount = 0
    while hir15val == 0:
        retryCount += 1
        assert retryCount < 100
        await ClockCycles(dut.sys_clk, 100)
        hir15val = await with_timeout(wb_read(dut, 0x410+15), 30, 'ns')

    #Now read all odd hir register and see if the patterns match
    for ii in range(1, 16, 2):
        res = await with_timeout(wb_read(dut, 0x410+ii), 30, 'ns')
        dut._log.info("WB read, addr: 0x%x, data: 0x%x, ref: 0x%x", 0x410+ii, res, hir_reg_vals[ii-1])
        assert int(res) == hir_reg_vals[ii-1]

#Test IRQ in, IRQ out and IRQ ack
@cocotb.test()
async def irq_in_out_ack(dut):
    await init(dut)

    #Load PicoRV program that waits for input IRQs, then copies them to output IRQ register
    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_irq_in_out.picobin")

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Wait some time
    await ClockCycles(dut.sys_clk, random.randint(0,100))

    #Set some input interrupts
    irqval = random.randint(1,0x7fffffff)
    dut.irq_in.value = irqval

    #Wait for output interrupt
    await with_timeout(RisingEdge(dut.irq_out), 300, 'ns')

    #Read output IRQ register
    irqOutReg = await with_timeout(wb_read(dut, 0x400), 30, 'ns')
    assert irqOutReg == irqval

    #Acknowledge irq by writing to reg 0
    await with_timeout(wb_write(dut, 0x400, irqval), 30, 'ns')

    await ClockCycles(dut.sys_clk, 1)

    #irq out should be low now.
    assert dut.irq_out.value == 0

async def wb_to_wb_wordcopy_test_helper(dut, numWords, srcAddr, dstAddr, srcTransactions, dstTransactions, offset=0):
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
            dut._log.info("hir_reg[3] = %s", res)
    
    #Check the recorded transactions
    assert len(srcTransactions) == numWords
    assert len(dstTransactions) == numWords

    srcByteList=[]
    dstByteList=[]

    for ii in range(0, len(srcTransactions)):
        #Word Read...
        rw, addr, dat_r, sel = srcTransactions[ii]
        assert rw == 'read'
        assert addr == (srcAddr>>2) + ii
        assert sel == 0xf
        srcByteList.append(dat_r&0xff)
        srcByteList.append((dat_r>>8)&0xff)
        srcByteList.append((dat_r>>16)&0xff)
        srcByteList.append((dat_r>>24)&0xff)

        #Word write.
        rw, addr, dat_w, sel = dstTransactions[ii]
        assert rw == 'write'
        assert addr == (dstAddr>>2) + ii
        assert sel == 0xf
        dstByteList.append(dat_r&0xff)
        dstByteList.append((dat_r>>8)&0xff)
        dstByteList.append((dat_r>>16)&0xff)
        dstByteList.append((dat_r>>24)&0xff)

    #Remove last <offset> items from the byte lista.
    for ii in range(offset):
        srcByteList.pop(-1)
        dstByteList.pop(-1)

    assert srcByteList == dstByteList

    timeout_task.kill()

#WB Master R/W word access
@cocotb.test()
async def wb0_to_wb1_wordcopy_test(dut):
    global wb0_transactions, wb1_transactions

    await init(dut)

    #Load PicoRV program that copies a configurable number of words from 
    #a configurable source address to a configurable destination address.
    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_wordcopy.picobin")

    wb0_slave_stall_task = cocotb.start_soon(wb0_slave_stall(dut))
    wb1_slave_stall_task = cocotb.start_soon(wb1_slave_stall(dut))
    
    wb0_slave_task = cocotb.start_soon(wb0_slave_emulator(dut, delay_ack=True))
    wb1_slave_task = cocotb.start_soon(wb1_slave_emulator(dut, delay_ack=True))

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Ask Praxos to copy a number of words
    numWords = random.randint(1, 16)
    #Generate word aligned address values
    srcAddr = random.randint(0x10004000, 0x47ffffff) & ~3
    dstAddr = random.randint(0x58000000, 0x5fffffff) & ~3

    wb0_transactions = []
    wb1_transactions = []
    await wb_to_wb_wordcopy_test_helper(dut, numWords, srcAddr, dstAddr, wb0_transactions, wb1_transactions)

    #For good measure, test the other directions as well
    wb1_transactions = []
    wb0_transactions = []
    await wb_to_wb_wordcopy_test_helper(dut, numWords, dstAddr, srcAddr, wb1_transactions, wb0_transactions)

    wb0_slave_stall_task.kill()
    wb1_slave_stall_task.kill()
    wb0_slave_task.kill()
    wb1_slave_task.kill()

async def wb_to_wb_bytecopy_test_helper(dut, numBytes, srcAddr, dstAddr, srcTransactions, dstTransactions):
    dut._log.info("Test: Configuring DMA request.")
    dut._log.info("Test: numBytes = %d, srcAddr = 0x%x, dstAddr = 0x%x", numBytes, srcAddr, dstAddr)

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
            dut._log.info("hir_reg[3] = %s", res)
    
    #Check the recorded transactions
    assert len(srcTransactions) == numBytes
    assert len(dstTransactions) == numBytes
    for ii in range(0, len(srcTransactions)):
        #Byte Read...
        rw, addr, dat_r, sel = srcTransactions[ii]
        assert rw == 'read'
        assert sel == 0xf
        assert addr == (srcAddr + ii)>>2
        #Byte write.
        rw, addr, dat_w, sel = dstTransactions[ii]
        assert rw == 'write'
        assert sel == (1<<(((dstAddr + ii)%4)))
        assert addr == (dstAddr + ii)>>2

    timeout_task.kill()

#WB Master R/W byte access
@cocotb.test()
async def wb_to_wb_bytecopy_test(dut):
    global wb0_transactions, wb1_transactions

    await init(dut)

    #Load PicoRV program that copies a configurable number of bytes from 
    #a configurable source address to a configurable destination address.
    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_bytecopy.picobin")

    wb0_slave_stall_task = cocotb.start_soon(wb0_slave_stall(dut))
    wb1_slave_stall_task = cocotb.start_soon(wb1_slave_stall(dut))
    
    wb0_slave_task = cocotb.start_soon(wb0_slave_emulator(dut, delay_ack=True))
    wb1_slave_task = cocotb.start_soon(wb1_slave_emulator(dut, delay_ack=True))

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    #Ask Praxos to copy a number of bytes
    numBytes = random.randint(1, 16)
    srcAddr = random.randint(0x10004000, 0x4fffffff)
    dstAddr = random.randint(0x50000000, 0x5fffffff)

    wb0_transactions = []
    wb1_transactions = []
    await wb_to_wb_bytecopy_test_helper(dut, numBytes, srcAddr, dstAddr, wb0_transactions, wb1_transactions)

    #For good measure, do it the other way around as well
    wb0_transactions = []
    wb1_transactions = []
    await wb_to_wb_bytecopy_test_helper(dut, numBytes, dstAddr, srcAddr, wb1_transactions, wb0_transactions)

    wb0_slave_stall_task.kill()
    wb1_slave_stall_task.kill()
    wb0_slave_task.kill()
    wb1_slave_task.kill()

#Test PicoRV data access to Program Memory
@cocotb.test()
async def picorv_progmem_data_access(dut):
    await init(dut)

    #Load PicoRV program that reads a word from hir0, copies it to a location in Program Memory,
    #reads the words back from Program Memory, and finally stores it in hir1.
    #One extra ../ because the test runs from the sim_build subdirectory
    pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_progmem_data_access.picobin")

    #Write PM memory
    for ii in range(len(pm_data)):
        await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
    
    #Write the register taking picorv out of reset
    await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
    
    dut._log.info("Test: Configuring value to store in progrmem.")
    hir0val = random.randint(1, 0xffffffff)
    await with_timeout(wb_write(dut, 0x410, hir0val), 30, 'ns')

    dut._log.info("Test: Kicking off the program.")
    await with_timeout(wb_write(dut, 0x413, 1), 30, 'ns')

    timeout_task = cocotb.start_soon(timeout_check(dut))

    #Wait for completion
    res = 1
    while res != 0:
        resNew = await with_timeout(wb_read(dut, 0x413), 30, 'ns')
        if resNew != res:
            res = resNew
            dut._log.info("hir_reg[3] = %s", res)
    
    #Check the transaction
    hir1val = await with_timeout(wb_read(dut, 0x411), 30, 'ns')

    assert(hir1val == hir0val)

    timeout_task.kill()


#WB Master R/W burst word access
@cocotb.test()
async def wb0_to_wb1_wordcopy_burst_test(dut):
    global wb0_transactions, wb1_transactions

    for offset in range(4):
        await init(dut)

        #Load PicoRV program that copies a configurable number of words from 
        #a configurable source address to a configurable destination address.
        #One extra ../ because the test runs from the sim_build subdirectory
        pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_wordcopy_burst.picobin")

        wb0_slave_stall_task = cocotb.start_soon(wb0_slave_stall(dut))
        wb1_slave_stall_task = cocotb.start_soon(wb1_slave_stall(dut))

        wb0_slave_task = cocotb.start_soon(wb0_slave_emulator(dut, delay_ack=True))
        wb1_slave_task = cocotb.start_soon(wb1_slave_emulator(dut, delay_ack=True))

        #Write PM memory
        for ii in range(len(pm_data)):
            await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
        
        #Write the register taking picorv out of reset
        await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
        
        #Ask Praxos to copy a number of words
        numWords = random.randint(1, 16)*16
        #Generate word aligned address values
        srcAddr = random.randint(0x10004000, 0x47ffffff) & ~3
        dstAddr = offset + (random.randint(0x58000000, 0x5fffffff) & ~3)

        wb0_transactions = []
        wb1_transactions = []
        await wb_to_wb_wordcopy_test_helper(dut, numWords, srcAddr, dstAddr, wb0_transactions, wb1_transactions, offset)

        wb0_slave_stall_task.kill()
        wb1_slave_stall_task.kill()
        wb0_slave_task.kill()
        wb1_slave_task.kill()
        
#WB Master R/W burst word access, without stalls and delays in slave
@cocotb.test()
async def wb0_to_wb1_wordcopy_burst_test_fast(dut):
    global wb0_transactions, wb1_transactions

    for offset in range(4):
        await init(dut)

        #Load PicoRV program that copies a configurable number of words from 
        #a configurable source address to a configurable destination address.
        #One extra ../ because the test runs from the sim_build subdirectory
        pm_data = loadBinaryIntoWords("../../../../sw/components/picorv_dma/test/picorv_wordcopy_burst.picobin")

        wb0_slave_task = cocotb.start_soon(wb0_slave_emulator(dut, delay_ack=False))
        wb1_slave_task = cocotb.start_soon(wb1_slave_emulator(dut, delay_ack=False))

        #Write PM memory
        for ii in range(len(pm_data)):
            await with_timeout(wb_write(dut, ii, pm_data[ii]), 30, 'ns')
        
        #Write the register taking picorv out of reset
        await with_timeout(wb_write(dut, 0x402, 1), 30, 'ns')
        
        #Ask Praxos to copy a number of words
        numWords = random.randint(1, 16)*16
        #Generate word aligned address values
        srcAddr = random.randint(0x10004000, 0x47ffffff) & ~3
        dstAddr = offset + (random.randint(0x58000000, 0x5fffffff) & ~3)

        wb0_transactions = []
        wb1_transactions = []
        await wb_to_wb_wordcopy_test_helper(dut, numWords, srcAddr, dstAddr, wb0_transactions, wb1_transactions, offset)

        wb0_slave_task.kill()
        wb1_slave_task.kill()
        

if __name__ == "__main__":
    #Cocotb Test Runner setup: pass in the verilog sources and the top-level.
    #The runner discovers and executes the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../../../../sub/picorv32/picosoc/picosoc_mem.v",
                       proj_path / "../../../../sub/picorv32/picorv32.v",
                       proj_path / "../rtl/picorv_dma_top.sv",
                       proj_path / "../rtl/picorv_burst_fsm.sv"]
    #Wrapper function defined in scripts/cocotb_lambda.py
    test_runner(verilog_sources=verilog_sources, 
                test_module_filename=__file__, 
                top="picorv_dma_top")
