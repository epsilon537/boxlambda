import os

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