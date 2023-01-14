top_module = 'wb_wbuart_wrap_wrap'
ooc_target = 'wbuart32'
verilator_cpp_flags = ''
verilator_ld_flags = ''

vlt_files_file = custom_target(
    ooc_target+'_vlts',
    build_always_stale: true,
    output: 'vlts.txt',
    command: [
        bender_get_vlts, 
        meson.current_source_dir(),
        '@OUTPUT@'
        ]
    )

verilator_script = custom_target(
    ooc_target+'_verilator_script',
    build_always_stale: true,
    output: 'verilator_script.txt',
    command: [
        bender_gen_verilator_script, 
        meson.current_source_dir(),
        '@OUTPUT@', 
        ooc_target
        ]
    )

run_target(
    ooc_target+'_lint',
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
    ooc_target+'_vivado_script',
    build_always_stale: true,
    output: 'vivado_script.txt',
    command: [
        bender_gen_vivado_script, 
        meson.current_source_dir(),
        '@OUTPUT@', 
        ooc_target
        ]
    )

synth = custom_target(
    ooc_target+'_synth',
    build_always_stale: true,
    output: 'project.xpr',
    input: vivado_script,
    command: [
        vivado_cmd,
        'synth', 
        part,
        '@INPUT@',
        '', 
        '',
        meson.current_build_dir(),
        top_module
        ]
    )
