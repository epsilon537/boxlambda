import os
import cocotb
from pathlib import Path
from cocotb.runner import *

#This is a simple wrapper around CocoTB's Test Runner.
#It takes care of some boilerplate code and makes sure waveforms are always generated.
#(The waves argument of the test method doesn't seem to work.)

def genDumpModule(buildDir, top):
    os.makedirs(buildDir, exist_ok=True)

    with open(buildDir+"/dumpWaves.v", 'w') as f:
        f.write('module cocotb_iverilog_dump();\n')
        f.write('initial begin\n')
        f.write('   $dumpfile("waves.fst");\n')
        f.write('   $dumpvars;\n')
        f.write('end\n')
        f.write('endmodule\n')

    return buildDir+"/dumpWaves.v"

#Pass in list of verilog source, the filename of the CocoTB Python test module, the top-level module
#and optionally a specific testcase to run (by default all tests in the test module will be run). 
def test_runner(verilog_sources, test_module_filename, top, testcase=None):
    hdl_toplevel_lang = "verilog"
    sim = "icarus"
    test_module = os.path.basename(os.path.splitext(test_module_filename)[0])
    build_dir= test_module+"_sim_build"

    #Add the generated dumpWaves.v module to the source list.
    verilog_sources.append(genDumpModule(build_dir, top))

    runner = get_runner(sim)
    runner.build(
        verilog_sources=verilog_sources,
        vhdl_sources= [],
        hdl_toplevel= top,
        always=True,
        build_dir=build_dir
    )

    res = runner.test(hdl_toplevel=top, test_module=test_module+",", testcase=testcase, plusargs=['-fst'])

    #Check results is needed to have this test script return pass or fail.
    #Without it, the script always returns success, even if some or all of the testcases themselves fail.
    check_results_file(res)
