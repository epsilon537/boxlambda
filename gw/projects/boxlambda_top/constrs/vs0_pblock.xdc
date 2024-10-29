#DFX (Partial FPGA Reconfiguration) constraints file for Virtual Socket 0 (VS0).
#These constraints define the location and dimensions of VS0's partition block on the FPGA's
#floorplan.

#Partition block name:
create_pblock pblock_vs0

#The module instance to associate this partition block with:
add_cells_to_pblock [get_pblocks pblock_vs0] [get_cells -quiet [list boxlambda_soc_inst/vs0_inst]]

#Region location:
resize_pblock [get_pblocks pblock_vs0] -add {SLICE_X6Y100:SLICE_X45Y149}
resize_pblock [get_pblocks pblock_vs0] -add {DSP48_X0Y40:DSP48_X0Y59}
resize_pblock [get_pblocks pblock_vs0] -add {RAMB18_X0Y40:RAMB18_X0Y59}
resize_pblock [get_pblocks pblock_vs0] -add {RAMB36_X0Y20:RAMB36_X0Y29}

#Indicate that the DFX controller must perform a reset of VS0 after reconfiguration.
set_property RESET_AFTER_RECONFIG true [get_pblocks pblock_vs0]

#Indicate that the partition block boundaries of VS0 given above must snap to the proper grid boundaries
#for 7 series FPGAs.
set_property SNAPPING_MODE ON [get_pblocks pblock_vs0]
