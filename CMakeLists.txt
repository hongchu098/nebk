cmake_minimum_required(VERSION 3.16)

set(CMAKE_ASM_NASM_OBJECT_FORMAT win64)

project(nebk VERSION 0.1 LANGUAGES ASM_NASM)

# Only do these if this is the main project, and not if it is included through add_subdirectory
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    # Let's ensure -std=c++xx instead of -std=g++xx
    set(CMAKE_CXX_EXTENSIONS OFF)
    
    # Let's nicely support folders in IDEs
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
endif()

set(CMAKE_ASM_NASM_LINK_EXECUTABLE "<CMAKE_LINKER> <FLAGS> <CMAKE_ASM_NASM_LINK_FLAGS> <LINK_FLAGS> <OBJECTS>  -o <TARGET> <LINK_LIBRARIES>")

add_subdirectory(src/bootloader)

# message(STATUS ${COMPILE_FLAGS})

#add_subdirectory(test output-test)

#enable_testing()

#add_test(NAME xxx-test COMMAND xxx-test)