cmake_minimum_required(VERSION 3.5)
# set(CMAKE_CXX_COMPILER"clang++")
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

project(DR_plan)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(cotire)

include_directories(src)

set(SOURCE_FILES
        src/Enumeration.h
        src/interpolation.cpp
        src/interpolation.hpp
        src/main.cpp
        src/Node.cpp
        src/Node.h
        src/Polynomial.cpp
        src/Polynomial.hpp
        src/spline.cpp
        src/spline.hpp
        src/Tree.cpp
        src/Tree.h
        src/Factory.cpp
        src/Factory.h
        src/stdafx.cpp
        src/stdafx.h)

add_executable(DR_plan ${SOURCE_FILES})

find_package(GSL REQUIRED)
include_directories(${GSL_INCLUDE_DIRS})
link_directories(${GSL_LIBRARY_DIRS})
target_link_libraries(DR_plan GSL::gsl GSL::gslcblas)

set(SPLINTER_LIB CACHE FILEPATH "Path to Splinter library")
include_directories(${CMAKE_SOURCE_DIR}/Splinter/include)
target_link_libraries(DR_plan ${SPLINTER_LIB})

set_target_properties(DR_plan PROPERTIES COTIRE_CXX_PREFIX_HEADER_INIT "${CMAKE_SOURCE_DIR}/src/stdafx.h")
cotire(DR_plan)
