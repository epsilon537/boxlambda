set part "xc7a100ticsg324-1L"
set inst boxlambda_soc_inst/rm0_inst
create_project -in_memory -part $part
add_files -norecurse /home/epsilon/work/boxlambda/build/arty-a7-100/gw/projects/dfx_test/project.runs/synth_1/boxlambda_top.dcp
add_files -fileset constrs_1 -norecurse /home/epsilon/work/boxlambda/gw/projects/boxlambda_top/constrs/boxlambda_soc.xdc
add_files -norecurse /home/epsilon/work/boxlambda/build/arty-a7-100/gw/components/rm0_j1b/project.runs/synth_1/rm0.dcp
import_ip ./dfx_controller/dfx_controller_0/dfx_controller_0.xci
set_property SCOPED_TO_CELLS {boxlambda_soc_inst/rm0_inst} [get_files /home/epsilon/work/boxlambda/build/arty-a7-100/gw/components/rm0_j1b/project.runs/synth_1/rm0.dcp]
link_design -mode default -reconfig_partitions {boxlambda_soc_inst/rm0_inst} -part $part -top boxlambda_top
write_checkpoint -force ./Checkpoint/boxlambda_top_link_rm0_j1b.dcp
create_pblock pblock_rm0_inst
add_cells_to_pblock [get_pblocks pblock_rm0_inst] [get_cells -quiet [list boxlambda_soc_inst/rm0_inst]]
resize_pblock [get_pblocks pblock_rm0_inst] -add {SLICE_X6Y100:SLICE_X45Y149}
resize_pblock [get_pblocks pblock_rm0_inst] -add {DSP48_X0Y40:DSP48_X0Y59}
resize_pblock [get_pblocks pblock_rm0_inst] -add {RAMB18_X0Y40:RAMB18_X0Y59}
resize_pblock [get_pblocks pblock_rm0_inst] -add {RAMB36_X0Y20:RAMB36_X0Y29}
set_property RESET_AFTER_RECONFIG true [get_pblocks pblock_rm0_inst]
set_property SNAPPING_MODE ON [get_pblocks pblock_rm0_inst]
opt_design
place_design
route_design
update_design -cell boxlambda_soc_inst/rm0_inst -black_box
lock_design -level routing
write_checkpoint -force Checkpoint/static_route_design.dcp
close_project
create_project -in_memory -part $part
add_files ./Checkpoint/static_route_design.dcp
add_files ../../build/arty-a7-100/gw/components/rm0_j1b/project.runs/synth_1/rm0.dcp
set_property SCOPED_TO_CELLS {boxlambda_soc_inst/rm0_inst} [get_files ../../build/arty-a7-100/gw/components/rm0_j1b/project.runs/synth_1/rm0.dcp]
link_design -mode default -reconfig_partitions {boxlambda_soc_inst/rm0_inst} -part $part -top boxlambda_top
opt_design
place_design
route_design
write_checkpoint -force ./Checkpoint/boxlambda_top_route_design_w_rm0_j1b.dcp
write_checkpoint -force -cell boxlambda_soc_inst/rm0_inst ./Checkpoint/rm0_j1b_route_design.dcp
close_project
create_project -in_memory -part $part
add_files ./Checkpoint/static_route_design.dcp
add_files ../../build/arty-a7-100/gw/components/rm0_stub/project.runs/synth_1/rm0.dcp
set_property SCOPED_TO_CELLS {boxlambda_soc_inst/rm0_inst} [get_files ../../build/arty-a7-100/gw/components/rm0_stub/project.runs/synth_1/rm0.dcp]
link_design -mode default -reconfig_partitions {boxlambda_soc_inst/rm0_inst} -part $part -top boxlambda_top
opt_design
place_design
route_design
write_checkpoint -force ./Checkpoint/boxlambda_top_route_design_w_rm0_stub.dcp
write_checkpoint -force -cell boxlambda_soc_inst/rm0_inst ./Checkpoint/rm0_stub_route_design.dcp
close_project
pr_verify ./Checkpoint/boxlambda_top_route_design_w_rm0_stub.dcp ./Checkpoint/boxlambda_top_route_design_w_rm0_j1b.dcp
open_checkpoint ./Checkpoint/boxlambda_top_route_design_w_rm0_j1b.dcp
write_bitstream -force ./Bitstreams/Config_rm0_j1b.bit
close_project
open_checkpoint ./Checkpoint/boxlambda_top_route_design_w_rm0_stub.dcp
write_bitstream -force ./Bitstreams/Config_rm0_stub.bit
close_project


