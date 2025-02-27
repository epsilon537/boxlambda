import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

instr_req_may_deassert = False
mid_transaction_branch = False

#This task monitors the valid_o signal
async def valid_o(dut):
    global mid_transaction_branch

    while True:
        await RisingEdge(dut.valid_o)
        assert not mid_transaction_branch
        #valid_o must remain asserted for one cycle
        await RisingEdge(dut.clk_i)
        await RisingEdge(dut.clk_i)
        assert dut.valid_o.value == 0

#This task monitors the instr_req_o signal
async def instr_req_o(dut):
    global instr_req_may_deassert

    while True:
        await RisingEdge(dut.instr_req_o)
        assert (dut.ready_i.value==1) or (dut.branch_i.value==1)

        await FallingEdge(dut.instr_req_o)
        assert instr_req_may_deassert
        instr_req_may_deassert = False

@cocotb.test()
async def ibex_no_prefetch_test(dut):
    global instr_req_may_deassert
    global mid_transaction_branch

    #Initial value of input signals
    dut.clk_i.value = 0
    dut.rst_ni.value = 1
    dut.req_i.value = 0
    dut.branch_i.value = 0
    dut.addr_i.value = 0
    dut.ready_i.value = 0
    dut.instr_gnt_i.value = 0
    dut.instr_rdata_i.value = 0
    dut.instr_err_i.value = 0
    dut.instr_rvalid_i.value = 0

    await Timer(10, unit="ns")  # wait 10 clocks

    cocotb.start_soon(Clock(dut.clk_i, 1, unit="ns").start())

    #Take the system out of reset and assert req_i
    dut.rst_ni.value = 1

    await Timer(10, unit="ns")  # wait 10 clocks

    #Two small signal monitoring tasks
    instr_req_o_task = cocotb.start_soon(instr_req_o(dut))
    valid_o_task = cocotb.start_soon(valid_o(dut))

    #First transaction:
    #Regular non-branch transaction
    dut.req_i.value = 1
    dut.ready_i.value = 1
    dut.branch_i.value = 0
    await with_timeout(RisingEdge(dut.instr_req_o), 1, "ns")

    #grant right away
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #Data valid
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == 4 #We started at 0 and increment the instruction address by 4.
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Second transaction back to back. req and reqdy_i are still asserted.
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0 #Deassert the data valid of the previous transaction
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == 8 #Previous transaction address + 4.
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Third transaction back to back: branch. req and ready_i are still asserted as well.
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0 #Deassert the data valid of the previous transaction
    dut.branch_i.value = 1
    branchAddr = random.randint(0,0x3fffffff)*4 #Word-aligned byte address
    dut.addr_i.value = branchAddr
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant right away
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 0 #the branch request and address are only set for 1 clock cycle
    dut.addr_i.value = 0
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == branchAddr #the output address should be the requested branch address.
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Fourth transaction back to back: regular non-branch instruction
    #req and ready_i are still asserted.
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == branchAddr + 4 #the address increments by 4 relative to previous transaction
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Fifth transaction: after a non-ready delay
    #Regular non-branch transaction
    await RisingEdge(dut.clk_i)
    dut.ready_i.value = 0
    dut.instr_rvalid_i.value = 0
    await with_timeout(FallingEdge(dut.valid_o), 1, "ns")

    #One more instruction should still be fetched, but not propagated up until ready_i is reasserted.
    assert dut.instr_req_o.value == 1

    #grant right away
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)

    await RisingEdge(dut.clk_i)
    await RisingEdge(dut.clk_i)
    await RisingEdge(dut.clk_i)

    #Resuming releasing fetched instructions upwards.
    dut.ready_i.value = 1
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == branchAddr + 8
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Sixth transaction back to back: regular non-branch transaction interrupted by a branch between gnt and valid
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #Here's the interrupting branch request
    dut.branch_i.value = 1
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = branchAddr
    mid_transaction_branch = True

    #return valid w 1 cycle delay - this valid doesn't not propagate to valid_o.
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 0
    dut.addr_i.value = 0
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.instr_req_o), 2, "ns")
    dut.instr_rvalid_i.value = 0

    #grant right away
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #return valid w 1 cycle delay - this one is in response to the interrupting branch request
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    mid_transaction_branch = False
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == branchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Seventh transaction back to back: regular non-branch transaction interrupted by a branch request right on valid
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #return valid w 1 cycle delay, assert branch_i at the same time
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 1
    oldBranchAddr = branchAddr
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    dut.addr_i.value = branchAddr
    #This valid does propagate to valid_o. This is not the interrupting branch yet.
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == oldBranchAddr + 4
    assert dut.rdata_o.value == dut.instr_rdata_i.value
    await with_timeout(RisingEdge(dut.instr_req_o), 2, "ns")
    dut.instr_rvalid_i.value = 0
    dut.branch_i.value = 0
    dut.addr_i.value = 0

    #grant right away
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    #This is the interrupting branch response:
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == branchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Eigth transaction: regular branch transaction interrupted by a branch between gnt and valid
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    dut.branch_i.value = 1
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = branchAddr
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True
    dut.branch_i.value = 1
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = branchAddr
    mid_transaction_branch = True

    #return valid w 1 cycle delay - this valid doesn't not propagate to valid_o
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 0
    dut.addr_i.value = 0
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.instr_req_o), 2, "ns")
    dut.instr_rvalid_i.value = 0

    #grant right away
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    mid_transaction_branch = False
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == branchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Ninth transaction: after a non-ready delay
    #an IRQ branch transaction
    await RisingEdge(dut.clk_i)
    dut.ready_i.value = 0
    dut.instr_rvalid_i.value = 0
    await with_timeout(FallingEdge(dut.valid_o), 1, "ns")

    #One more instruction should still be fetched, but not propagated up until ready_i is reasserted.
    assert dut.instr_req_o.value == 1

    #grant right away
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)

    await RisingEdge(dut.clk_i)
    await RisingEdge(dut.clk_i)
    await RisingEdge(dut.clk_i)

    #Resuming releasing fetched instructions upwards and assert branch_i.
    dut.branch_i.value = 1
    newBranchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = newBranchAddr
    dut.ready_i.value = 1
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    #This is the instruction that's getting interrupted
    assert dut.addr_o.value == branchAddr + 4
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 0
    dut.instr_rvalid_i.value = 0
    await with_timeout(FallingEdge(dut.valid_o), 1, "ns")

    #Request the instruction belonging to the branch address
    assert dut.instr_req_o.value == 1
    #assert dut.instr_addr_o.value == newBranchAddr

    #Grant the request
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    instr_req_may_deassert = True

    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)

    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    assert dut.addr_o.value == newBranchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    instr_req_o_task.kill()
    valid_o_task.kill()

if __name__ == "__main__":
    #Test Runner setup. Pass in the verilog sources and top-level.
    #The runner discovers the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../ibex_out/src/lowrisc_ibex_ibex_core_0.1/rtl/ibex_single_prefetch_buffer.sv"]
    #Defined in scripts/cocotb_boxlambda.py
    test_runner(verilog_sources=verilog_sources,
                test_module_filename=__file__,
                top="ibex_single_prefetch_buffer")

