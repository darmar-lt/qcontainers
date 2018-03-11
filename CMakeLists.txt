
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12)
PROJECT(qcontainers C)

SET(SRC_SUBPATHS
        containers/*.c
        internal/*.c
        utilities/*.c
        wrap_util_c/*.c)

SET(SRC_PATHS src)

INCLUDE_DIRECTORIES( .
        containers
        internal
        utilities
        wrap_util_c)

FILE(GLOB_RECURSE SRC_LIB
		${SRC_SUBPATHS})

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    #message(STATUS "Setting build type to 'Release' as none was specified.")
    SET(CMAKE_BUILD_TYPE Release)
    # Set the possible values to build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()
        
SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

ADD_LIBRARY(qcontainers STATIC ${SRC_LIB})


