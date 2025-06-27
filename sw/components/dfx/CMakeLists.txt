add_library(dfx)

target_sources(dfx
    PRIVATE
    dfx_controller_hal.cpp
)

target_compile_options(dfx
 PRIVATE
  -O2 -g
)

target_include_directories(dfx
    PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
    ${PROJECT_SOURCE_DIR}/registers/generated/
)

target_link_libraries(dfx timer)
