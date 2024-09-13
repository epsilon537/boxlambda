#Ymodem library

add_library(ymodem)

target_sources(ymodem
    PRIVATE
    ymodem.c
)

target_compile_options(ymodem
 PRIVATE
  -O2 -g
)

#Paths to include files users of ymodem are expected to use.
target_include_directories(ymodem
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(ymodem uart timer)
