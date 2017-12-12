cmake_minimum_required(VERSION 3.5)
set(CMAKE_CXX_COMPILER"clang++")
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
add_compile_options(-std=c++11)

project(DR_plan)

include(cmake/PrecompiledHeader.cmake)

include_directories(Splinter/include)
link_directories(Splinter/x86-64/Debug)

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
        src/Factory.h)

set(pch_source src/stdafx.cpp)

find_package(GSL REQUIRED)
include_directories(${GSL_INCLUDE_DIRS})
link_directories(${GSL_LIBRARY_DIRS})

add_executable(DR_plan ${SOURCE_FILES} src/stdafx.h ${pch_source})

set_target_properties(DR_plan PROPERTIES
    CXX_STANDARD 11
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
)

add_precompiled_header(DR_plan src/stdafx.h SOURCE_CXX src/stdafx.cpp FORCEINCLUDE)
