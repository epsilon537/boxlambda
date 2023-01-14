ddr_test_inc_dirs = [
    '../../components/bootstrap',
	'../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc',
    '../../../sub/litex/litex/soc/software/liblitedram',
	'../../../sub/litex/litex/soc/software/include',
	'../../../sub/litex/litex/soc/software',
	'../../../sub/litex/litex/soc/software/libbase',
    '../../../sub/litex/litex/soc/cores/cpu/ibex'
    ]

ddr_test_c_args = [
    '-DCONFIG_CPU_NOP="nop"',
    '-DMAIN_RAM_BASE=0x40000000UL',
    '-DMAIN_RAM_BASE2=0x50000000UL',
    '-DCSR_BASE=0x10030000',
    '-DSDRAM_TEST_DISABLE',
    ]

if get_option('sim')
    ddr_test_inc_dirs += [
	    '../../../gw/components/litedram/sim/sw/include'
        ]

    ddr_test_c_args += ['-DMEMTEST_SIZE=32768']
else
    ddr_test_inc_dirs += [
        '../../../gw/components/litedram/arty/sw/include'
        ]

    ddr_test_c_args += ['-DMEMTEST_SIZE=(256*1024*1024)']
endif

ddr_test_srcs = files(
    'ddr_test.c',
    'memtest.c',
	'../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c',
	'../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c',
	'../../../sub/litex/litex/soc/software/liblitedram/sdram.c'
    )

ddr_test_link_args = internal_link_args + [
    '-Wl,--Map,' + meson.current_build_dir() / 'out.map'
    ]

ddr_test_elf = executable(
    'ddr_test_elf',
    ddr_test_srcs,
    include_directories: ddr_test_inc_dirs,
    c_args: ddr_test_c_args,
    link_whole: bootstrap_lib,
    link_args: ddr_test_link_args,
    link_depends: link_internal_mem_ld
    )

ddr_test_hex = custom_target(
    'ddr_test_hex',
    input : ddr_test_elf,
    output : 'ddr_test.hex',
    command: elf2hex_cmd
    )

ddr_test_mem = custom_target(
    'ddr_test_mem',
    input: ddr_test_hex,
    output: 'spram.mem',
    command: hex2vmem_cmd,
    capture: true)
