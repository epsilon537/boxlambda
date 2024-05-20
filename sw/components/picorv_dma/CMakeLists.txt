add_library(picorv_dma)

#picorv_dma hardware access layer.
target_sources(picorv_dma
    PRIVATE
    picorv_dma_hal.c
)

target_compile_options(picorv_dma
 PRIVATE
  -O2 -g
)

#Paths to include files users of picorv_dma are expected to use.
target_include_directories(picorv_dma
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
