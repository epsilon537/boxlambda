picolibc_test_srcs = files('picolibc_test.c')

picolibc_test_inc_dirs = include_directories(
    '../../components/bootstrap',
    '../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc')

picolibc_test_link_args = internal_link_args + [
    '-Wl,--Map,' + meson.current_build_dir() / 'out.map'
    ]

picolibc_test_elf = executable(
    'picolibc_test_elf',
    picolibc_test_srcs,
    include_directories: picolibc_test_inc_dirs,
    link_whole: bootstrap_lib,
    link_args: picolibc_test_link_args,
    link_depends: link_internal_mem_ld
    )

picolibc_test_hex = custom_target(
    'picolibc_test_hex',
    input : picolibc_test_elf,
    output : 'picolibc_test.hex',
    command: elf2hex_cmd
    )

picolibc_test_mem = custom_target(
    'picolibc_test_mem',
    input: picolibc_test_hex,
    output: 'spram.mem',
    command: hex2vmem_cmd,
    capture: true)
