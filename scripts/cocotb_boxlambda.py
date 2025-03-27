import os
import sys
import cocotb
from pathlib import Path
from cocotb_tools.runner import get_runner
from cocotb_tools.check_results import get_results

#This is a simple wrapper around CocoTB's Test Runner.
#It takes care of some boilerplate code.

#Pass in list of verilog source, the filename of the CocoTB Python test module and the top-level module. Optionally a specific testcase to run (by default all tests in the test module will be run) and module instantiation parameters.
def test_runner(verilog_sources, test_module_filename, top, testcase=None, parameters={}):
    hdl_toplevel_lang = "verilog"
    sim = "icarus"
    test_module = os.path.basename(os.path.splitext(test_module_filename)[0])
    build_dir= test_module+"_sim_build"

    print("verilog sources:")
    print(verilog_sources)

    runner = get_runner(sim)
    runner.build(
        verilog_sources=verilog_sources,
        vhdl_sources= [],
        parameters = parameters,
        hdl_toplevel= top,
        always=True,
        build_dir=build_dir,
        waves=True
    )

    res = runner.test(hdl_toplevel=top, test_module=test_module+",", testcase=testcase, waves=True, plusargs=['-fst'])

    try:
      (num_tests, num_failed) = get_results(res)
    except RuntimeError as e:
      print("%s", e.args[0])
      sys.exit(2)
    else:
      if num_failed:
        print("ERROR: Failed %d of %d tests.", num_failed, num_tests)
        sys.exit(1)
