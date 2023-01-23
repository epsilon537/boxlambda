#Only relevant for CMAKE_BUILD_TYPE=fpga: -DBL_TARGET_FPGA="arty-a7-35" or -DBL_TARGET_FPGA="arty-a7-100".
set(BL_TARGET_FPGA "arty-a7-35" CACHE STRING "FPGA Target: arty-a7-35 or arty-a7-100")
set_property(CACHE BL_TARGET_FPGA PROPERTY STRINGS "arty-a7-35" "arty-a7-100")

#Map to full part number as expected by Vivado.
if(BL_TARGET_FPGA STREQUAL "arty-a7-35")
    set(PART xc7a35ticsg324-1L)
elseif(BL_TARGET_FPGA STREQUAL "arty-a7-100")
    set(PART xc7a100ticsg324-1L)
else()
    message(FATAL_ERROR "Invalid part number: ${BL_TARGET_FPGA}.")
endif()

#A top-level target aggregating all lint targets.
add_custom_target(lint_all ALL)

add_subdirectory(components)
add_subdirectory(projects)
