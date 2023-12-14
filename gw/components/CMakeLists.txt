#Function grouping gateware component build rules.
#TOP_MODULE: name of top module.
#COMPONENT_NAME: component name, e.g. wbuart32
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty. 
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty. 
function (gw_component_rules)
    set(options)
    set(oneValueArgs TOP_MODULE COMPONENT_NAME VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
    set(multiValueArgs)
    
    cmake_parse_arguments(gw_component_rules
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

    set(_top_module ${gw_component_rules_TOP_MODULE}) 
    set(_comp_name ${gw_component_rules_COMPONENT_NAME})
    set(_verilator_cpp_flags ${gw_component_rules_VERILATOR_CPP_FLAGS})
    set(_verilator_ld_flags ${gw_componeny_rules_VERILATOR_LD_FLAGS})

    #Forward to gw_common_rules() function. gw_common_rules() build logic is shared between 
    #gw components and projects.
    gw_common_rules(
        TOP_MODULE
            ${_top_module}
        PRJ_COMP_NAME
            ${_comp_name}
        VERILATOR_CPP_FLAGS
            ${_verilator_cpp_flags}
        VERILATOR_LD_FLAGS
            ${_verilator_ld_flags}
    )
endfunction()

#Gateware components:
add_subdirectory(audio_dac)
add_subdirectory(ym2149)
add_subdirectory(sdspi)
add_subdirectory(vera)
add_subdirectory(wbuart32)
add_subdirectory(riscv-dbg)
add_subdirectory(wb_gpio)
add_subdirectory(litedram)
add_subdirectory(ibex_wb_core)
add_subdirectory(ibex)
add_subdirectory(reset_ctrl)
add_subdirectory(praxos)
add_subdirectory(picorv_dma)
add_subdirectory(boxlambda_clk_gen)
add_subdirectory(wbxbar)
add_subdirectory(wb_dp_ram)
add_subdirectory(boxlambda_soc)
add_subdirectory(blackbox)





