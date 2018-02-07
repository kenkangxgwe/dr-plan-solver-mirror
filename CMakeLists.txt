cmake_minimum_required(VERSION 3.5)
project(DR_plan)

#set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_CXX_STANDARD 11)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(cotire)

include_directories(src)

set(SOURCE_FILES
        src/stdafx.cpp
        src/stdafx.h
        src/Enumeration.h
        src/RootFinder.cpp
        src/RootFinder.h
        src/Polynomial.cpp
        src/Polynomial.hpp
        #  src/Tree.cpp
        src/Tree.h
        #  src/Factory.cpp
        src/Factory.h
        src/main.cpp
        src/Node.cpp
        src/Node.h
        src/RootFinder.cpp
        src/RootFinder.h)

add_executable(DR_plan ${SOURCE_FILES})

#find_package(GSL REQUIRED)
#include_directories(${GSL_INCLUDE_DIRS})
#link_directories(${GSL_LIBRARY_DIRS})
#target_link_libraries(DR_plan GSL::gsl GSL::gslcblas)

#find_package(Boost REQUIRED)

set(SPLINTER_LIB CACHE FILEPATH "Path to Splinter library")
include_directories(${CMAKE_SOURCE_DIR}/Splinter/include)
target_link_libraries(DR_plan ${SPLINTER_LIB}) # ${Boost_LIBRARIES})
# target_include_directories(DR_plan PRIVATE ${Boost_INCLUDE_DIRS})

set_target_properties(DR_plan PROPERTIES COTIRE_CXX_PREFIX_HEADER_INIT "${CMAKE_SOURCE_DIR}/src/stdafx.h")
cotire(DR_plan)
