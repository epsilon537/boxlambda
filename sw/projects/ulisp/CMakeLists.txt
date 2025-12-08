#
# ulisp
#
add_executable(ulisp
 ulisp-riscv-comments.cpp
 main.cpp
)

target_compile_options(ulisp
 PRIVATE
  -O2
)

target_compile_definitions(ulisp
 PRIVATE
  -DBOARD_BOXLAMBDA
)

# target_include_directories(ulisp
#   PRIVATE
#   ${PROJECT_SOURCE_DIR}/sw/components/i2c/
# )

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ulisp
${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)

target_link_libraries(ulisp arduino gpio riscv vera bootstrap timer
interrupts sdram)

add_flash_sw_target(ulisp)
