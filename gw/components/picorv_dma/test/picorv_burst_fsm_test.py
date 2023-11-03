import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *
import struct
import pdb

OFFSET_BURST_REG_OFFSET = 5
BURST_REG_4_OFFSET = 4
BURST_REG_3_OFFSET = 3
BURST_REG_2_OFFSET = 2
BURST_REG_1_OFFSET = 1
BURST_REG_0_OFFSET = 0

#Cocotb-based unit testcases for picorv_dma

wb_transactions = []

async def init(dut):
    global wb_transactions

    #Clear the wishbone transaction list.
    wb_transactions = []

    #For simplicity's sake, pretend we have a 1ns clock period.
    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())
    
    #Assert reset
    dut.rst.value = 1
    await Timer(10, units="ns")  # wait 10 clocks
    dut.rst.value = 0
    await Timer(1, units="ns")  # wait 1 clock

    #Initial values for input signals
    dut.picorv_valid_i.value = 0
    dut.picorv_addr_i.value = 0
    dut.picorv_wdata_i.value = 0
    dut.picorv_wstrb_i.value = 0

    dut.wbm_dat_i.value = 0
    dut.wbm_ack_i.value = 0
    dut.wbm_stall_i.value = 0
    dut.wbm_err_i.value = 0

    await Timer(1, units="ns")  # wait 1 clock

#Asynchronous timeout task. The test fails if this task isn't killed before the timeout is reached.
async def timeout_check(dut):
    await Timer(20000, units="ns")
    #We should never reach this point
    assert False, "Transaction timeout!"

#async def wb_slave_stall(dut):
#    while True:
#        if dut.wbm_cyc_o.value == 1:
#            dyt.wbm_stall_i.value = dut.wbm_ack_i.value
#        else:
#            dut.wbm_stall_i.value = 0

#Asynchronous task emulating a Wishbone slave. Received transactions are recorded in a
#wb_transactions list.
async def wb_slave_emulator(dut, delay_ack):
    while True:
        await RisingEdge(dut.clk)
        if dut.wbm_stb_o.value == 1:
            dut._log.info("WB: stb detected")
            if dut.wbm_we_o.value == 1:
                dut._log.info("WB write, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm_adr_o.value), int(dut.wbm_dat_o.value), int(dut.wbm_sel_o.value))
                wb_transactions.append(('write', int(dut.wbm_adr_o.value), int(dut.wbm_dat_o.value), int(dut.wbm_sel_o)))       
            else:
                #Return random data to read transactions
                dat_r = random.randint(0, 0xffffffff)
                dut.wbm_dat_i.value = dat_r
                dut._log.info("WB read, addr: 0x%x, data: 0x%x, sel: 0x%x", 
                              int(dut.wbm_adr_o.value), dat_r, int(dut.wbm_sel_o))
                wb_transactions.append(('read', int(dut.wbm_adr_o.value), dat_r, int(dut.wbm_sel_o)))

            await RisingEdge(dut.clk)
            
            if delay_ack:       
                responseDelay = random.randint(0, 10) #Randomly delay ack 0-10 ticks
                dut._log.info("WB: delaying ack %d ns", responseDelay)
                await ClockCycles(dut.clk, responseDelay)

            dut._log.info("WB: signalling ACK.")
            dut.wbm_ack_i.value = 1 #ACK

            await RisingEdge(dut.clk)
            
            dut.wbm_ack_i.value = 0
            await Timer(1, units="ns")

#PicoRV word read transaction
async def picorv_read(dut, addr):
    await RisingEdge(dut.clk)
        
    dut.picorv_addr_i.value = addr
    dut.picorv_wstrb_i.value = 0
    dut.picorv_valid_i.value = 1

    
    rdyDetected = False
    while not rdyDetected:
        await RisingEdge(dut.clk)
        if dut.picorv_rdy_o.value == 1:
            #dut._log.info("picorv ready detected")
            rdyDetected = True

    res = dut.picorv_rdata_o.value
    dut.picorv_valid_i.value = 0

    await RisingEdge(dut.clk)

    return res

#Wishone word write transaction
async def picorv_write(dut, addr, val):
    await RisingEdge(dut.clk)

    dut.picorv_addr_i.value = addr
    dut.picorv_wstrb_i.value = 0xf
    dut.picorv_valid_i.value = 1
    dut.picorv_wdata_i.value = val

    rdyDetected = False
    while not rdyDetected:
        await RisingEdge(dut.clk)
        if dut.picorv_rdy_o.value == 1:
            #dut._log.info("picorv ready detected")
            rdyDetected = True

    dut.picorv_valid_i.value = 0

    await RisingEdge(dut.clk)
    

@cocotb.test()
async def picorv_read_single_word_test_w_stalls_and_delays(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))

    numWords = random.randint(1, 10)
    wb_transactions = []

    read_words=[]
    addr_list=[]
    for w in range(numWords):
        #Generate word aligned address value
        addr = random.randint(0x20000000, 0x4fffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        addr_list.append(addr)
        res = await with_timeout(picorv_read(dut, addr), 30, 'ns')
        read_words.append(res)

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_single_word_test_wo_stalls_and_delays(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=False))

    numWords = random.randint(1, 10)
    wb_transactions = []

    read_words=[]
    addr_list=[]
    for w in range(numWords):
        #Generate word aligned address value
        addr = random.randint(0x20000000, 0x4fffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        addr_list.append(addr)
        res = await with_timeout(picorv_read(dut, addr), 30, 'ns')
        read_words.append(res)

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_write_single_word_test_w_stalls_and_delays(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))

    numWords = random.randint(1, 10)
    wb_transactions = []

    addr_list=[]
    written_words=[]

    for w in range(numWords):
        #Generate word aligned address value
        addr = random.randint(0x20000000, 0x4fffffff) >> 2
        val = random.randint(0x0,0xffffffff)
        written_words.append(val)
        dut._log.info("write word addr 0x%x", addr)
        addr_list.append(addr)
        await with_timeout(picorv_write(dut, addr, val), 30, 'ns')
        
    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        rw, addr, dat_w, sel = wb_transactions[ii]
        assert rw == 'write'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert written_words[ii] == dat_w

    wb_slave_task.kill()

@cocotb.test()
async def picorv_write_single_word_test_wo_stalls_and_delays(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=False))

    numWords = random.randint(1, 10)
    wb_transactions = []

    addr_list=[]
    written_words=[]

    for w in range(numWords):
        #Generate word aligned address value
        addr = random.randint(0x20000000, 0x4fffffff) >> 2
        val = random.randint(0x0,0xffffffff)
        written_words.append(val)
        dut._log.info("write word addr 0x%x", addr)
        addr_list.append(addr)
        await with_timeout(picorv_write(dut, addr, val), 30, 'ns')
        
    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        rw, addr, dat_w, sel = wb_transactions[ii]
        assert rw == 'write'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert written_words[ii] == dat_w

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_w_stalls_and_delays_offset_0(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 0), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        
        read_words.append(burst_reg_0)
        read_words.append(burst_reg_1)
        read_words.append(burst_reg_2)
        read_words.append(burst_reg_3)

    
    #Do one more write to a burst register to ensure the last burst is entirely completed out before
    #We start checking
    await with_timeout(picorv_write(dut, (0x10002020>>2) + BURST_REG_4_OFFSET, 0), 30*4, 'ns')

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_w_stalls_and_delays_offset_1(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 1), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        burst_reg_4 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 4), 30*4, 'ns')

        read_words.append((burst_reg_0>>8) | ((burst_reg_1&0xff)<<24))
        read_words.append((burst_reg_1>>8) | ((burst_reg_2&0xff)<<24))
        read_words.append((burst_reg_2>>8) | ((burst_reg_3&0xff)<<24))
        read_words.append((burst_reg_3>>8) | ((burst_reg_4&0xff)<<24))
    
    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_w_stalls_and_delays_offset_2(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 2), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        burst_reg_4 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 4), 30*4, 'ns')

        read_words.append((burst_reg_0>>16) | ((burst_reg_1&0xffff)<<16))
        read_words.append((burst_reg_1>>16) | ((burst_reg_2&0xffff)<<16))
        read_words.append((burst_reg_2>>16) | ((burst_reg_3&0xffff)<<16))
        read_words.append((burst_reg_3>>16) | ((burst_reg_4&0xffff)<<16))

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_w_stalls_and_delays_offset_3(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 3), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        burst_reg_4 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 4), 30*4, 'ns')

        read_words.append((burst_reg_0>>24) | ((burst_reg_1&0xffffff)<<8))
        read_words.append((burst_reg_1>>24) | ((burst_reg_2&0xffffff)<<8))
        read_words.append((burst_reg_2>>24) | ((burst_reg_3&0xffffff)<<8))
        read_words.append((burst_reg_3>>24) | ((burst_reg_4&0xffffff)<<8))

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_wo_stalls_and_delays_offset_0(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=False))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 0), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])

        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        
        read_words.append(burst_reg_0)
        read_words.append(burst_reg_1)
        read_words.append(burst_reg_2)
        read_words.append(burst_reg_3)

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_wo_stalls_and_delays_offset_1(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=False))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 1), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])

        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        burst_reg_4 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 4), 30*4, 'ns')

        read_words.append((burst_reg_0>>8) | ((burst_reg_1&0xff)<<24))
        read_words.append((burst_reg_1>>8) | ((burst_reg_2&0xff)<<24))
        read_words.append((burst_reg_2>>8) | ((burst_reg_3&0xff)<<24))
        read_words.append((burst_reg_3>>8) | ((burst_reg_4&0xff)<<24))
    
    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_wo_stalls_and_delays_offset_2(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=False))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 2), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        
        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        burst_reg_4 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 4), 30*4, 'ns')

        read_words.append((burst_reg_0>>16) | ((burst_reg_1&0xffff)<<16))
        read_words.append((burst_reg_1>>16) | ((burst_reg_2&0xffff)<<16))
        read_words.append((burst_reg_2>>16) | ((burst_reg_3&0xffff)<<16))
        read_words.append((burst_reg_3>>16) | ((burst_reg_4&0xffff)<<16))

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii]
        assert sel == 0xf
        assert read_words[ii] == dat_r

    wb_slave_task.kill()

@cocotb.test()
async def picorv_read_burst_test_wo_stalls_and_delays_offset_3(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=False))

    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    read_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, 3), 30*4, 'ns')

    for w in range(numBursts):
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("read word addr 0x%x", addr)
        await with_timeout(picorv_read(dut, addr), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        
        burst_reg_0 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 0), 30*4, 'ns')
        burst_reg_1 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 1), 30*4, 'ns')
        burst_reg_2 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 2), 30*4, 'ns')
        burst_reg_3 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 3), 30*4, 'ns')
        burst_reg_4 = await with_timeout(picorv_read(dut, (0x10002020>>2) + 4), 30*4, 'ns')

        read_words.append((burst_reg_0>>24) | ((burst_reg_1&0xffffff)<<8))
        read_words.append((burst_reg_1>>24) | ((burst_reg_2&0xffffff)<<8))
        read_words.append((burst_reg_2>>24) | ((burst_reg_3&0xffffff)<<8))
        read_words.append((burst_reg_3>>24) | ((burst_reg_4&0xffffff)<<8))

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat_r, sel = wb_transactions[ii]
        assert rw == 'read'
        assert addr == addr_list[ii], "ii=%d"%ii
        assert sel == 0xf
        assert read_words[ii] == dat_r, "ii=%d"%ii

    wb_slave_task.kill()

@cocotb.test()
async def picorv_burst_reg_write_read(dut):
    
    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))

    wr_data_list=[]

    for ii in range(5):
        data = random.randint(0x80000000, 0xffffffff)
        wr_data_list.append(data)
        dut._log.info("write to burst reg %d: 0x%x", ii, data)
        await with_timeout(picorv_write(dut, (0x10002020>>2) + ii, data), 30*4, 'ns')
    
    for ii in range(5):
        data = await with_timeout(picorv_read(dut, (0x10002020>>2) + ii), 30*4, 'ns')
        dut._log.info("read from burst reg %d: 0x%x", ii, data)
        assert data == wr_data_list[ii]

    assert len(wb_transactions) == 0

    wb_slave_task.kill()

@cocotb.test()
async def picorv_write_burst_test_w_stalls_and_delays(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=True))
  
    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    written_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, random.randint(0,0xffffffff)), 30*4, 'ns')

    for w in range(numBursts):
        wr_data_list=[]

        for ii in range(5):
            data = random.randint(0x80000000, 0xffffffff)
            wr_data_list.append(data)
            dut._log.info("write to burst reg %d: 0x%x", ii, data)
            await with_timeout(picorv_write(dut, (0x10002020>>2) + ii, data), 30*4, 'ns')
  
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("write word addr 0x%x", addr)
        await with_timeout(picorv_write(dut, addr, 0), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        written_words.append(wr_data_list[0])
        written_words.append(wr_data_list[1])
        written_words.append(wr_data_list[2])
        written_words.append(wr_data_list[3])
    
    #Do one more write to a burst register to ensure the burst is entirely written out before
    #We start checking
    await with_timeout(picorv_write(dut, (0x10002020>>2) + BURST_REG_4_OFFSET, 0), 30*4, 'ns')

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat, sel = wb_transactions[ii]
        assert rw == 'write'
        assert addr == addr_list[ii], "ii={}".format(ii)
        assert sel == 0xf
        assert written_words[ii] == dat

    wb_slave_task.kill()

@cocotb.test()
async def picorv_write_burst_test_wo_stalls_and_delays(dut):
    global wb_transactions

    await init(dut)

    wb_slave_task = cocotb.start_soon(wb_slave_emulator(dut, delay_ack=False))
  
    numBursts = random.randint(1, 10)
    numWords = 4*numBursts
    wb_transactions = []

    written_words=[]
    addr_list=[]

    await with_timeout(picorv_write(dut, (0x10002020>>2) + OFFSET_BURST_REG_OFFSET, random.randint(0,0xffffffff)), 30*4, 'ns')

    for w in range(numBursts):
        wr_data_list=[]

        for ii in range(5):
            data = random.randint(0x80000000, 0xffffffff)
            wr_data_list.append(data)
            dut._log.info("write to burst reg %d: 0x%x", ii, data)
            await with_timeout(picorv_write(dut, (0x10002020>>2) + ii, data), 30*4, 'ns')
  
        #Generate word aligned address value
        addr = random.randint(0x80000000, 0xffffffff) >> 2
        dut._log.info("write word addr 0x%x", addr)
        await with_timeout(picorv_write(dut, addr, 0), 30*4, 'ns')
        addr &= (0x7fffffff>>2)
        addr_list.extend([addr, addr+1, addr+2, addr+3])
        written_words.append(wr_data_list[0])
        written_words.append(wr_data_list[1])
        written_words.append(wr_data_list[2])
        written_words.append(wr_data_list[3])
    
    #Do one more write to a burst register to ensure the burst is entirely written out before
    #We start checking
    await with_timeout(picorv_write(dut, (0x10002020>>2) + BURST_REG_4_OFFSET, 0), 30*4, 'ns')

    assert len(wb_transactions) == numWords

    for ii in range(0, numWords):
        #Word Read...
        rw, addr, dat, sel = wb_transactions[ii]
        assert rw == 'write'
        assert addr == addr_list[ii], "ii={}".format(ii)
        assert sel == 0xf
        assert written_words[ii] == dat

    wb_slave_task.kill()

if __name__ == "__main__":
    #Cocotb Test Runner setup: pass in the verilog sources and the top-level.
    #The runner discovers and executes the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../rtl/picorv_burst_fsm.sv"]
    #Wrapper function defined in scripts/cocotb_lambda.py
    test_runner(verilog_sources=verilog_sources, 
                test_module_filename=__file__, 
                top="picorv_burst_fsm")
