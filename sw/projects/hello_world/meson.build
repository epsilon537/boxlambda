hello_world_inc_dirs = [
	'../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc'
    ]

hello_world_c_args = [
    ]

hello_world_srcs = files(
    '../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello/hello.c',
	'../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c',
	'../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c'
    )

hello_world_link_args = internal_link_args + [
    '-Wl,--Map,' + meson.current_build_dir() / 'out.map'
    ]

hello_world_elf = executable(
    'hello_world_elf',
    hello_world_srcs,
    include_directories: hello_world_inc_dirs,
    c_args: hello_world_c_args,
    link_whole: bootstrap_lib,
    link_args: hello_world_link_args,
    link_depends: link_internal_mem_ld
    )

hello_world_hex = custom_target(
    'hello_world_hex',
    input : hello_world_elf,
    output : 'hello_world.hex',
    command: elf2hex_cmd
    )

hello_world_mem = custom_target(
    'hello_world_mem',
    input: hello_world_hex,
    output: 'spram.mem',
    command: hex2vmem_cmd,
    capture: true)
