import cocotb
from cocotb.triggers import *
from cocotb.clock import Clock
import random
import os
from pathlib import Path
from cocotb_boxlambda import *

instr_req_may_deassert = False
valid_o_must_not_assert = False

#This task monitors the valid_o signal
async def valid_o(dut):
    global valid_o_must_not_assert

    while True:
        await RisingEdge(dut.valid_o)
        assert not valid_o_must_not_assert
        #valid_o must remain asserted for one cycle
        await RisingEdge(dut.clk_i)
        assert dut.valid_o.value == 1
        await RisingEdge(dut.clk_i)
        assert dut.valid_o.value == 0

#This task monitors the instr_req_o signal
async def instr_req_o(dut):
    global instr_req_may_deassert

    while True:
        sim_time_now = cocotb.utils.get_sim_time('ns')
        await RisingEdge(dut.instr_req_o)
        await Timer(1, "ns")
        sim_time_now = cocotb.utils.get_sim_time('ns')
        await FallingEdge(dut.instr_req_o)
        await Timer(1, "ns")
        sim_time_now = cocotb.utils.get_sim_time('ns')
        assert instr_req_may_deassert
        instr_req_may_deassert = False

@cocotb.test()
async def ibex_no_prefetch_test(dut):
    global instr_req_may_deassert
    global valid_o_must_not_assert

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

    await Timer(10, "ns")  # wait 10 clocks

    cocotb.start_soon(Clock(dut.clk_i, 10, "ns").start())

    #Take the system out of reset
    dut.rst_ni.value = 1

    await Timer(100, unit="ns")  # wait 10 clocks

    #Two small signal monitoring tasks
    instr_req_o_task = cocotb.start_soon(instr_req_o(dut))
    valid_o_task = cocotb.start_soon(valid_o(dut))

    #First transaction:
    #Regular non-branch transaction
    dut.req_i.value = 1 #req_i will remain asserted throughout the test sequence.
    dut.ready_i.value = 1 #prefetch will start (for 1 instruction) even if the IF stage is not ready.
    dut.branch_i.value = 0
    await with_timeout(RisingEdge(dut.instr_req_o), 1, "ns")

    #grant right away
    instr_req_may_deassert = True #indicate that instr_req_o may deassert on the next rising clock edge.
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0 #grant pulses for just one cycle.

    #Set return data valid
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    #Let it propagate to the output
    await Timer(1, "ns")
    assert dut.valid_o.value == 1
    assert dut.addr_o.value == 0 #We started at 0
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Second transaction back to back. req and reqdy_i remain asserted.
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0 #Deassert the data valid pulse of the previous transaction
    #valid_o should follow and the next instruction request should be issued.
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0 #one clock cycle wide pulse

    #Let's return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    #Let it propagate to the output
    await Timer(1, "ns")
    assert dut.valid_o.value == 1
    assert dut.addr_o.value == 4 #Previous transaction address + 4.
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Third transaction back to back: branch. req and ready_i remain asserted as well.
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0 #Deassert the data valid of the previous transaction
    dut.branch_i.value = 1 #Request to branch to addr_i value
    branchAddr = random.randint(0,0x3fffffff)*4 #Word-aligned byte address
    dut.addr_i.value = branchAddr
    #valid_o should fall and the next instruction request should be issued.
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant right away
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 0 #the branch request and address are only set for 1 clock cycle
    dut.addr_i.value = 0
    dut.instr_gnt_i.value = 0

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    #Let it propagate to the output
    await Timer(1, "ns")
    assert dut.valid_o.value == 1
    assert dut.addr_o.value == branchAddr #the output address should be the requested branch address.
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Fourth transaction back to back: a regular non-branch instruction.
    #req and ready_i remain asserted.
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    #valid_o should follow rvalid_i and the next instruction request should be issued.
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await Timer(1, "ns")
    assert dut.valid_o.value == 1
    #the address increments by 4 relative to previous transaction
    assert dut.addr_o.value == branchAddr + 4
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Fifth transaction: after a non-ready delay, get the next instruction.
    await RisingEdge(dut.clk_i)
    dut.ready_i.value = 0
    dut.instr_rvalid_i.value = 0
    await Timer(1, "ns")
    assert dut.valid_o.value == 0

    #One more instruction should still be fetched, but not propagated up until ready_i is reasserted.
    assert dut.instr_req_o.value == 1

    #grant right away
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    dut.instr_rvalid_i.value = 1
    valid_o_must_not_assert = True #indicate to the valid_o monitoring task that valid_o must not assert
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)

    await RisingEdge(dut.clk_i)
    await RisingEdge(dut.clk_i)
    await RisingEdge(dut.clk_i)

    #Resuming releasing fetched instructions upwards.
    dut.ready_i.value = 1
    valid_o_must_not_assert = False #valid_o is allowed to assert again
    await with_timeout(RisingEdge(dut.valid_o), 1, "ns")
    #We're now at 2 instuctions since the last branch.
    assert dut.addr_o.value == branchAddr + 8
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Sixth transaction back to back: regular non-branch transaction interrupted by a branch between grant and valid
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    #Here's the interrupting branch request
    dut.branch_i.value = 1
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = branchAddr
    valid_o_must_not_assert = True #indicate to the valid_o monitoring task that valid_o must not assert

    #return instr_rvalid w 1 cycle delay - this valid doesn't not propagate to valid_o.
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 0
    dut.addr_i.value = 0
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await RisingEdge(dut.clk_i)
    await Timer(1, "ns")
    assert dut.instr_req_o.value == 1
    dut.instr_rvalid_i.value = 0

    #grant right away
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    #return valid w 1 cycle delay - this one is in response to the interrupting branch request
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    valid_o_must_not_assert = False #valid_o is allowed to assert again
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await Timer(1, "ns")
    assert dut.valid_o.value == 1
    assert dut.addr_o.value == branchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Seventh transaction back to back: regular non-branch transaction interrupted by a branch request right on valid
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    #return valid w 1 cycle delay, assert branch_i at the same time
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 1
    #save the old branch address before generating a new one.
    oldBranchAddr = branchAddr
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    dut.addr_i.value = branchAddr
    #This valid does not propagate to valid_o. We have an interrupting branch
    #pending.
    assert dut.valid_o.value == 0
    await RisingEdge(dut.clk_i)
    await Timer(1, "ns")
    assert dut.instr_req_o.value == 1
    dut.instr_rvalid_i.value = 0
    dut.branch_i.value = 0
    dut.addr_i.value = 0

    #grant right away
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    #This is the interrupting branch response:
    await Timer(1, "ns")
    assert dut.valid_o.value == 1
    assert dut.addr_o.value == branchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Eighth transaction: regular branch transaction interrupted by a branch between gnt and valid
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0
    dut.branch_i.value = 1
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = branchAddr
    await with_timeout(Combine(FallingEdge(dut.valid_o), RisingEdge(dut.instr_req_o)), 1, "ns")
    dut.branch_i.value = 0

    #grant one the next cycle
    await RisingEdge(dut.clk_i)
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    dut.branch_i.value = 1
    branchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = branchAddr
    #We have an interrupting branch request in the middle of an ongoing transaction.
    #When the ongoing transaction completes, it must not propagate to valid_o.
    valid_o_must_not_assert = True

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.branch_i.value = 0
    dut.addr_i.value = 0
    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await RisingEdge(dut.clk_i)
    await Timer(1, "ns")
    assert dut.instr_req_o.value == 1
    assert dut.instr_addr_o.value == branchAddr
    dut.instr_rvalid_i.value = 0

    #grant right away
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    #return valid w 1 cycle delay
    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 1
    valid_o_must_not_assert = False #valid_o may assert now
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)
    await Timer(1, "ns")
    assert dut.valid_o.value == 1
    assert dut.addr_o.value == branchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value

    #Ninth transaction: after a non-ready delay
    #an IRQ branch transaction occurs
    await RisingEdge(dut.clk_i)
    dut.ready_i.value = 0
    dut.instr_rvalid_i.value = 0
    await Timer(1, "ns")
    assert dut.valid_o.value == 0

    #One more instruction should still be fetched, but not propagated up until ready_i is reasserted.
    assert dut.instr_req_o.value == 1

    #grant right away
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1

    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)

    await RisingEdge(dut.clk_i)
    dut.instr_rvalid_i.value = 0 #instr_rvalid_i pulse is one cycle long.
    await RisingEdge(dut.clk_i)
    await RisingEdge(dut.clk_i)

    #assert branch_i together with ready_i
    dut.branch_i.value = 1
    newBranchAddr = random.randint(0,0x3fffffff)*4
    dut.addr_i.value = newBranchAddr
    dut.ready_i.value = 1
    await RisingEdge(dut.clk_i)
    #IF-stage is ready but we've received a branch transaction request.
    #Don't return data to IF-stage until we've received the branch transaction
    #data.
    assert dut.valid_o.value == 0
    dut.branch_i.value = 0 #a branch_i pulse is one clock cycle long.
    await RisingEdge(dut.clk_i)
    #Request the instruction belonging to the branch address
    assert dut.instr_req_o.value == 1
    assert dut.instr_addr_o.value == newBranchAddr
    assert dut.valid_o.value == 0

    #Grant the request
    await RisingEdge(dut.clk_i)
    instr_req_may_deassert = True
    dut.instr_gnt_i.value = 1
    await RisingEdge(dut.clk_i)
    dut.instr_gnt_i.value = 0

    dut.instr_rvalid_i.value = 1
    dut.instr_rdata_i.value = random.randint(0, 0xffffffff)

    await RisingEdge(dut.clk_i)
    assert dut.valid_o.value == 1
    assert dut.addr_o.value == newBranchAddr
    assert dut.rdata_o.value == dut.instr_rdata_i.value
    dut.instr_rvalid_i.value = 0

    await RisingEdge(dut.clk_i)
    assert dut.valid_o.value == 0

    instr_req_o_task.kill()
    valid_o_task.kill()

if __name__ == "__main__":
    #Test Runner setup. Pass in the verilog sources and top-level.
    #The runner discovers the testcases.
    proj_path = Path(__file__).resolve().parent
    verilog_sources = [proj_path / "../../../../build/sim-a7-100/codegen/ibex/src/lowrisc_ibex_ibex_core_0.1/rtl/ibex_single_prefetch_buffer.sv"]
    #Defined in scripts/cocotb_boxlambda.py
    test_runner(verilog_sources=verilog_sources,
                test_module_filename=__file__,
                top="ibex_single_prefetch_buffer")

