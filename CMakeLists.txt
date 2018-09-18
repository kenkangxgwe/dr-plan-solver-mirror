cmake_minimum_required(VERSION 3.5)
project(DRPLAN)

#set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
#include(cotire)

set(SOURCE_FILES
        src/stdafx.cpp
        src/TwoTree.cpp
        src/RootFinder.cpp
        src/Tree.cpp
        src/Node.cpp
        src/TwoTreeUtils.cpp
        src/main.cpp)

# Include Boost Graph Library
find_package(Boost COMPONENTS program_options graph REQUIRED)

include_directories(
        ${CMAKE_SOURCE_DIR}/include
        ${Boost_INCLUDE_DIRS})

# Set Splinter library
set(SPLINTER_LIB CACHE FILEPATH "Path to Splinter library")
set(GLPK_LIB CACHE FILEPATH "Path to GLPK library")

add_executable(${PROJECT_NAME} ${SOURCE_FILES})
target_link_libraries(${PROJECT_NAME} PRIVATE
        ${SPLINTER_LIB}
        ${GLPK_LIB}
        ${Boost_LIBRARIES})
target_include_directories(${PROJECT_NAME} PRIVATE ${Boost_INCLUDE_DIRS})

# Cotire for pre-compiled header
#set_target_properties(${PROJECT_NAME} PROPERTIES COTIRE_CXX_PREFIX_HEADER_INIT "include/stdafx.h")
#cotire(${PROJECT_NAME})

