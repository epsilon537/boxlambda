top_module = 'ibex_soc'
proj_name = 'hello_world'

#Compile and link flag for Verilator testbench code.
verilator_cpp_flags = '-I' + meson.source_root() / 'sub/wbuart32/bench/cpp/'
verilator_ld_flags = '-lncurses'

vlt_files_file = custom_target(
    proj_name+'_vlts',
    build_always_stale: true,
    output: 'vlts.txt',
    command: [
        bender_get_vlts, 
        meson.current_source_dir(),
        '@OUTPUT@'
        ]
    )

verilator_script = custom_target(
    proj_name+'_verilator_script',
    build_always_stale: true,
    output: 'verilator_script.txt',
    command: [
        bender_gen_verilator_script, 
        meson.current_source_dir(),
        '@OUTPUT@'
        ]
    )
    
run_target(
    proj_name+'_lint',
    command: [
        lint_check,
        top_module,
        vlt_files_file.full_path(),
        'waiver.vlt',
        verilator_script.full_path(),
        verilator_cpp_flags,
        verilator_ld_flags,
        ],
    depends: [vlt_files_file, verilator_script]
    ) 

vivado_script = custom_target(
    proj_name+'_vivado_script',
    build_always_stale: true,
    output: 'vivado_script.txt',
    command: [
        bender_gen_vivado_script, 
        meson.current_source_dir(),
        '@OUTPUT@', 
        ooc_target
        ]
    )

constraints_script = custom_target(
    proj_name+'_constraints_script',
    build_always_stale: true,
    output: 'constraints.tcl',
    command: [
        bender_gen_constraints_script, 
        meson.current_source_dir(),
        '@OUTPUT@'
        ]
    )

mem_file_script = custom_target(
    proj_name+'_mem_file_script',
    build_always_stale: true,
    output: 'mem_files.tcl',
    input: hello_world_mem,
    command: [
        gen_mem_file_script, 
        '@INPUT@',
        '@OUTPUT@'
        ]
    )

custom_target(
    proj_name+'_synth',
    build_always_stale: true,
    output: proj_name+'_synth',
    input: [vivado_script, constraints_script, mem_file_script],
    command: [
        vivado_cmd,
        'synth', 
        part,
        '@INPUT0@',
        '@INPUT1@', 
        '@INPUT2@',
        meson.current_build_dir(),
        top_module
        ]
    )

custom_target(
    proj_name+'_bit',
    build_always_stale: true,
    output: proj_name+'_bit',
    input: [vivado_script, constraints_script, mem_file_script],
    command: [
        vivado_cmd,
        'impl', 
        part,
        '@INPUT0@',
        '@INPUT1@', 
        '@INPUT2@',
        meson.current_build_dir(),
        top_module
        ]
    )

cpp_files_file = custom_target(
    proj_name+'_cpp_files',
    build_always_stale: true,
    output: 'cpp_files.txt',
    command: [
        bender_get_cpp_files, 
        meson.current_source_dir(),
        '@OUTPUT@'
        ]
    )

sim = custom_target(
    proj_name+'_sim',
    build_always_stale: true,
    output: 'Vmodel',
    input: [verilator_script, vlt_files_file, cpp_files_file],
    depends: mem_file_script,
    command: [
        verilator_sim,
        '@INPUT0@',
        '@INPUT1@', 
        '@INPUT2@',
        top_module,
        verilator_cpp_flags,
        verilator_ld_flags,
        meson.current_build_dir(),
        ]
    )

test(
    proj_name+'_test',
    shell,
    args: ['-c', './Vmodel'],
    depends: sim,
    workdir: meson.current_build_dir(),
    suite: test_suite_sim
    )

test(
    proj_name+'_run',
    vivado_load_bitstream,
    workdir: meson.current_build_dir(),
    verbose: true,
    suite: test_suite_fpga
    )
