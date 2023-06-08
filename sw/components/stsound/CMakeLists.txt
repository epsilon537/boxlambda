add_library(stsound)

target_sources(stsound
    PRIVATE
    YmLoadBL.cpp
    YmMusicBL.cpp 
    Ym2149ExBL.cpp
    ../../../sub/StSound/StSoundLibrary/YmUserInterface.cpp 
    ../../../sub/StSound/StSoundLibrary/LZH/LzhLib.cpp    
)

#target_compile_definitions(stsound
#	PRIVATE
#		-D...
#)

#We have to turn on optimizations or we won't fit in the on-chip RAM link map.
target_compile_options(stsound
	PRIVATE
		-O2 -g
)

target_include_directories(stsound
    PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
    ${PROJECT_SOURCE_DIR}/sub/StSound/StSoundLibrary
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/StSound/StSoundLibrary/LZH
)

target_link_libraries(stsound fatfs)
