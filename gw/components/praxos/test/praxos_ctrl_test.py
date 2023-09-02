import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb.runner import get_runner

async def init(dut):
    #For simplicity's sake, pretend we have a 1ns clock period.
    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())

    dut.rst_n.value = 0
    await Timer(1, units="ns")  # wait 1 clock
    dut.rst_n.value = 1
    await Timer(1, units="ns")  # wait 1 clock

    dut.wb_adr.value = 0
    dut.wb_dat_w.value = 0
    dut.wb_sel.value = 0
    dut.wb_cyc.value = 0
    dut.wb_stb.value = 0
    dut.wb_we.value = 0
    dut.irq_in.value = 0
    dut.praxos_port_addr.value = 0
    dut.praxos_port_rd.value = 0
    dut.praxos_port_wr.value = 0
    dut.praxos_port_wr_data.value = 0

async def wb_stall_check(dut):
    await RisingEdge(dut.clk)
    if dut.wb_stall.value == 1:
        await FallingEdge(dut.wb_stall)
    dut.wb_stb.value = 0

async def wb_read(dut, addr):
    await RisingEdge(dut.clk)
        
    dut.wb_adr.value = addr
    dut.wb_sel.value = 0xf
    dut.wb_cyc.value = 1
    dut.wb_stb.value = 1
    dut.wb_we.value = 0
    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    await RisingEdge(dut.wb_ack)
    stall_check_task.kill()
    assert dut.wb_err.value == 0
    res = dut.wb_dat_r.value
    
    await RisingEdge(dut.clk)
    dut.wb_cyc.value = 0
    
    return res

async def wb_write(dut, addr, val):
    await RisingEdge(dut.clk)
    dut.wb_adr.value = addr
    dut.wb_dat_w.value = val
    dut.wb_sel.value = 0xf
    dut.wb_cyc.value = 1
    dut.wb_stb.value = 1
    dut.wb_we.value = 1

    stall_check_task = cocotb.start_soon(wb_stall_check(dut))

    await RisingEdge(dut.wb_ack)
    stall_check_task.kill()
    assert dut.wb_err.value == 0
    
    await RisingEdge(dut.clk)
    dut.wb_cyc.value = 0

#gp reg access: wb write - praxos read
@cocotb.test()
async def gp_reg_access_wb_wr_praxos_rd(dut):
    await init(dut)

    gp_reg_values = [0]*16

    for ii in range(16):
        gp_reg_values[ii] = random.randint(0, 0xffffffff)
        await with_timeout(wb_write(dut, 16+ii, gp_reg_values[ii]), 30, 'ns')
        
    for ii in range(16):
        await RisingEdge(dut.clk)
        dut.praxos_port_addr.value = 16+ii
        dut.praxos_port_rd.value = 1
        await RisingEdge(dut.clk)
        assert dut.praxos_port_rd_data.value == gp_reg_values[ii]

    dut.praxos_port_rd.value = 0
    await Timer(1, units="ns")

#gp reg access: praxos write - wb read
@cocotb.test()
async def gp_reg_access_praxos_wr_wb_rd(dut):
    await init(dut)

    gp_reg_values = [0]*16

    for ii in range(16):
        await RisingEdge(dut.clk)
        gp_reg_values[ii] = random.randint(0, 0xffffffff)
        dut.praxos_port_addr.value = 16+ii
        dut.praxos_port_wr.value = 1
        dut.praxos_port_wr_data.value = gp_reg_values[ii]
        await RisingEdge(dut.clk)

    dut.praxos_port_wr.value = 0
    await Timer(1, units="ns")

    for ii in range(16):
        res = await with_timeout(wb_read(dut, 16+ii), 30, 'ns')
        assert res == gp_reg_values[ii]

#ctrl reg access
@cocotb.test()
async def ctrl_reg_access(dut):
    await init(dut)

    #Initially praxos should be held in reset
    assert dut.praxos_rst_n.value == 0

    #Read the register
    res = await with_timeout(wb_read(dut, 6), 30, 'ns')
    assert res == 0
    
    #Write the register taking praxos out of reset
    await with_timeout(wb_write(dut, 6, 1), 30, 'ns')
    assert dut.praxos_rst_n.value == 1
    
    #Read the register
    res = await with_timeout(wb_read(dut, 6), 30, 'ns')
    assert res == 1

    #Put praxos back in reset
    #Write the register
    await with_timeout(wb_write(dut, 6, 0), 30, 'ns')
    assert dut.praxos_rst_n.value == 0

    #Read the register
    res = await with_timeout(wb_read(dut, 6), 30, 'ns')
    assert res == 0

async def pm_prog_check(dut, pm_data):
    ii=0
    while ii<256:
        await RisingEdge(dut.clk)
        if dut.praxos_pm_wr == 1:
            assert dut.praxos_pm_wr_addr.value == ii
            assert dut.praxos_pm_wr_data.value == pm_data[ii]
            ii += 1
        
#pm access
@cocotb.test()
async def pm_access(dut):
    await init(dut)

    pm_data = [0]*256
    for ii in range(256):
        pm_data[ii] = random.randint(0, 0xfffffffff)

    pm_prog_check_task = cocotb.start_soon(pm_prog_check(dut, pm_data))

    for ii in range(256):
        await with_timeout(wb_write(dut, 2, pm_data[ii] & 0xffffffff), 30, 'ns') #data low
        await with_timeout(wb_write(dut, 3, pm_data[ii] >> 32), 30, 'ns') #data high
        await with_timeout(wb_write(dut, 4, ii), 30, 'ns') #addr
        await with_timeout(wb_write(dut, 5, 0), 30, 'ns') #commit

    #Make dure the prog check tasks completes in time
    await with_timeout(pm_prog_check_task, 10, 'ns')

#irq_in reg access
#gp reg access: wb write - praxos read
@cocotb.test()
async def irq_in_test(dut):
    await init(dut)

    irq_value = random.randint(0, 0xffffffff)
    dut.irq_in.value = irq_value

    await Timer(1, units="ns")

    res = await with_timeout(wb_read(dut, 1), 30, 'ns')
    assert res == irq_value

    await RisingEdge(dut.clk)
    dut.praxos_port_addr.value = 1
    dut.praxos_port_rd.value = 1
    await RisingEdge(dut.clk)
    assert dut.praxos_port_rd_data.value == irq_value

    dut.praxos_port_rd.value = 0
    await Timer(1, units="ns")

#irq_out reg access
@cocotb.test()
async def irq_out_test(dut):
    await init(dut)

    assert dut.irq_out.value == 0

    await RisingEdge(dut.clk)
    irq_test_bit = 1<<random.randint(0, 31)
    irq_value = random.randint(1, 0xffffffff) | irq_test_bit

    dut.praxos_port_addr.value = 0
    dut.praxos_port_wr.value = 1
    dut.praxos_port_wr_data.value = irq_value
    await RisingEdge(dut.clk)

    dut.praxos_port_wr.value = 0
    await Timer(1, units="ns")

    assert dut.irq_out.value == 1

    res = await with_timeout(wb_read(dut, 0), 30, 'ns')
    assert res == irq_value

    #Just acknowledge the irq_test_bit
    await with_timeout(wb_write(dut, 0, irq_test_bit), 30, 'ns')
    assert dut.irq_out.value == 1 #Should still be set
    res = await with_timeout(wb_read(dut, 0), 30, 'ns')
    assert res == irq_value & ~irq_test_bit

    #ack all irqs
    await with_timeout(wb_write(dut, 0, irq_value), 30, 'ns') 
    assert dut.irq_out.value == 0
    res = await with_timeout(wb_read(dut, 0), 30, 'ns')
    assert res == 0

def praxos_ctrl_test_runner():
    hdl_toplevel_lang = "verilog"
    sim ="icarus"

    proj_path = Path(__file__).resolve().parent

    verilog_sources = []
    vhdl_sources = []

    verilog_sources = [proj_path / "../rtl/praxos_ctrl.sv"]
    
    runner = get_runner(sim)
    runner.build(
        verilog_sources=verilog_sources,
        vhdl_sources=vhdl_sources,
        hdl_toplevel="praxos_ctrl",
        always=True,
        build_dir='praxos_ctrl_sim_build'
    )

    runner.test(hdl_toplevel="praxos_ctrl", test_module="praxos_ctrl_test,")

if __name__ == "__main__":
    praxos_ctrl_test_runner()
