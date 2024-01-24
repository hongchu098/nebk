if(WIN32)
        set(EXECUTABLE_EXT ".exe")
elseif(LINUX)
        set(EXECUTABLE_EXT "")
else()
        message(FATAL_ERROR "unsupported platform")
endif()
# set(CMAKE_ASM_NASM_OBJECT_FORMAT bin)
# find_file(CMAKE_DD dd REQUIRED)
# message(STATUS "dd program: ${CMAKE_DD}")
find_file(CMAKE_TRUNCATE truncate REQUIRED)
message(STATUS "truncate program: ${CMAKE_TRUNCATE}")
find_file(CMAKE_MTOOLS_MCOPY "mcopy${EXECUTABLE_EXT}" PATHS CMAKE_MTOOLS_DIR REQUIRED)
message(STATUS "mcopy program: ${CMAKE_MTOOLS_MCOPY}")

add_custom_command(
        OUTPUT loader.bin
        COMMAND ${CMAKE_ASM_NASM_COMPILER} -f bin ${CMAKE_CURRENT_SOURCE_DIR}/loader.asm -Ox -o ${CMAKE_CURRENT_BINARY_DIR}/loader.bin
        DEPENDS loader.asm
        COMMENT "Builing loader"
        VERBATIM)

add_library(loader-pe.obj OBJECT loader_pe.asm)
add_executable(loader)
target_link_libraries(loader loader-pe.obj)
target_link_options(loader PRIVATE -T ${CMAKE_SOURCE_DIR}/config/loader-linker.ld -nostdlib)
target_link_options(loader PRIVATE --image-base 0x10000)

add_custom_command(
        OUTPUT floppy.img
        COMMAND ${CMAKE_ASM_NASM_COMPILER} -f bin ${CMAKE_CURRENT_SOURCE_DIR}/floppy.asm -Ox -o ${CMAKE_CURRENT_BINARY_DIR}/floppy.img
        COMMAND ${CMAKE_TRUNCATE} --size=1440k ${CMAKE_CURRENT_BINARY_DIR}/floppy.img
        COMMAND ${CMAKE_MTOOLS_MCOPY} -p -m -v -i ${CMAKE_CURRENT_BINARY_DIR}/floppy.img ${CMAKE_CURRENT_BINARY_DIR}/loader.exe ::
        DEPENDS floppy.asm loader
        COMMENT "Builing floppy booter"
        VERBATIM)
add_custom_target(floppy_booter ALL DEPENDS floppy.img)
