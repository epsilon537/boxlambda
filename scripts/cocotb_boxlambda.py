import os
import cocotb
from pathlib import Path
from cocotb.runner import *

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

def test_runner(verilog_sources, test_module_filename, top, testcase=None):
    hdl_toplevel_lang = "verilog"
    sim = "icarus"
    test_module = os.path.basename(os.path.splitext(test_module_filename)[0])
    build_dir= test_module+"_sim_build"

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
    check_results_file(res)
