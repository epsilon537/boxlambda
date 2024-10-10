create_pblock pblock_vs0
add_cells_to_pblock [get_pblocks pblock_vs0] [get_cells -quiet [list boxlambda_soc_inst/vs0_inst]]
resize_pblock [get_pblocks pblock_vs0] -add {SLICE_X6Y100:SLICE_X45Y149}
resize_pblock [get_pblocks pblock_vs0] -add {DSP48_X0Y40:DSP48_X0Y59}
resize_pblock [get_pblocks pblock_vs0] -add {RAMB18_X0Y40:RAMB18_X0Y59}
resize_pblock [get_pblocks pblock_vs0] -add {RAMB36_X0Y20:RAMB36_X0Y29}
set_property RESET_AFTER_RECONFIG true [get_pblocks pblock_vs0]
set_property SNAPPING_MODE ON [get_pblocks pblock_vs0]
