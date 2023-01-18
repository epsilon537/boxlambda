project(
    'boxlambda', 
    'c')

specs = '--specs='+ meson.source_root() / 'sw/picolibc-install/picolibc.specs'

add_project_arguments(
    specs,
    language: 'c')

if get_option('sim')
    add_project_arguments('-DSIM', language: 'c')
endif

add_project_link_arguments(
    [specs,
     '-Wl,--gc-sections'],
    language: 'c')

link_internal_mem_ld = meson.source_root() / 'sw/components/bootstrap/link_internal_mem.ld'

internal_link_args = [
    '-T', 
    link_internal_mem_ld
    ]

hex2vmem_prog = find_program(
    'hex2vmem.pl', 
    dirs: meson.source_root() / 'sub/ibex_wb/scripts')

hex2vmem_cmd = [
    hex2vmem_prog,
    '@INPUT@'
    ]

objcopy = find_program('objcopy')

elf2hex_cmd = [
    objcopy, 
    '-O', 
    'verilog', 
    '--interleave-width=4', 
    '--interleave=4', 
    '--byte=0', 
    '@INPUT@', 
    '@OUTPUT@'
    ]

script_dir = meson.source_root() / 'scripts'
lint_check = find_program('verilator_lint_check.sh', dirs: script_dir)
bender_get_vlts = find_program('bender_get_vlts.sh', dirs: script_dir)
bender_get_cpp_files = find_program('bender_get_cpp_files.sh', dirs: script_dir)
bender_gen_verilator_script = find_program('bender_gen_verilator_script.sh', dirs: script_dir)
bender_gen_vivado_script = find_program('bender_gen_vivado_script.sh', dirs: script_dir)
bender_gen_constraints_script = find_program('bender_gen_constraints_script.sh', dirs: script_dir)
gen_mem_file_script = find_program('gen_mem_file_script.sh', dirs: script_dir)
vivado_cmd = find_program('vivado_cmd.sh', dirs: script_dir)
verilator_sim = find_program('verilator_sim.sh', dirs: script_dir)
shell = find_program('sh')
vivado_load_bitstream = find_program('vivado_load_bitstream.sh', dirs: script_dir)

part = 'xc7a35ticsg324-1L'

test_suite_sim = 'sim'
test_suite_fpga = 'fpga'

subdir('sw/components/bootstrap')
subdir('sw/projects/hello_world')
subdir('sw/projects/picolibc_test')
subdir('sw/projects/ddr_test')

subdir('gw/components/ibex')
subdir('gw/components/ibex_wb_core')
subdir('gw/components/wb_gpio')
subdir('gw/components/wbuart32')
subdir('gw/components/riscv_dbg')
subdir('gw/components/litedram')

subdir('gw/projects/hello_world')
