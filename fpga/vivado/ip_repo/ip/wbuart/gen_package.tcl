create_project project -part xc7a35ticsg324-1L -force

add_files {\
	../../../../wbuart32/rtl/rxuart.v \
	../../../../wbuart32/rtl/txuart.v \
	../../../../wbuart32/rtl/ufifo.v \
	../../../../wbuart32/rtl/wbuart.v\
}

import_files -force
update_compile_order -fileset sources_1
ipx::package_project -root_dir project.srcs -vendor localdomain -library user -taxonomy /UserIP -force
ipx::remove_bus_interface i_reset [ipx::current_core]
ipx::remove_bus_interface i_clk [ipx::current_core]
set_property core_revision 2 [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::check_integrity [ipx::current_core]
ipx::save_core [ipx::current_core]

close_project
