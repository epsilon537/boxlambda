add_library(usb_hid)

#picorv_dma hardware access layer.
target_sources(usb_hid
    PRIVATE
    usb_hid_hal.c
)

target_compile_options(usb_hid
	PRIVATE
		-O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(usb_hid
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
