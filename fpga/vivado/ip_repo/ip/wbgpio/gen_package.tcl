create_project project -part xc7a35ticsg324-1L -force

add_files {\
	       wbgpio.v \
	       wb_gpio_wrapper.sv \
	       ../../../../ibex_wb/soc/fpga/arty-a7-35/rtl/wb_gpio.sv \
	       ../../../../ibex_wb/rtl/wb_if.sv \
	       ../../../../ibex_wb/rtl/wb_pkg.sv \
}

add_files -fileset constrs_1 ooc.xdc
set_property USED_IN {synthesis out_of_context} [get_files ooc.xdc]
								
import_files -force
update_compile_order -fileset sources_1

if {($::argc > 0) && ($::argv=="synth")} {
    set_property -name {STEPS.SYNTH_DESIGN.ARGS.MORE OPTIONS} -value {-mode out_of_context} -objects [get_runs synth_1]
    reset_run synth_1
    launch_runs synth_1 -jobs 6
    wait_on_run synth_1
    puts "Synthesis done!"
    file mkdir reports
    open_run synth_1 -name synth_1
    report_timing_summary -file reports/post_synth_timing_summary.rpt
    report_utilization -file reports/post_synth_util.rpt
} else {
    ipx::package_project -root_dir project.srcs -vendor localdomain -library user -taxonomy /UserIP -force
    ipx::remove_bus_interface i_reset [ipx::current_core]
    ipx::remove_bus_interface i_clk [ipx::current_core]
    set_property core_revision 2 [ipx::current_core]
    ipx::create_xgui_files [ipx::current_core]
    ipx::update_checksums [ipx::current_core]
    ipx::check_integrity [ipx::current_core]
    ipx::save_core [ipx::current_core]
}
close_project
