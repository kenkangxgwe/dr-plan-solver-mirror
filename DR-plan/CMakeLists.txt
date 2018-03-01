cmake_minimum_required(VERSION 3.5)
project(DR_plan)

#set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_CXX_STANDARD 11)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(cotire)

set(SOURCE_FILES
        stdafx.cpp
        stdafx.h
        RootFinder.cpp
        RootFinder.h
        Tree.h
        Factory.h
        Node.cpp
        Node.h
        RootFinder.cpp
        RootFinder.h)

include_directories(${CMAKE_SOURCE_DIR})

add_library(${PROJECT_NAME} SHARED ${SOURCE_FILES})

set(SPLINTER_LIB CACHE FILEPATH "Path to Splinter library")
target_link_libraries(${PROJECT_NAME} ${SPLINTER_LIB})
set_target_properties(${PROJECT_NAME} PROPERTIES COTIRE_CXX_PREFIX_HEADER_INIT "stdafx.h")
cotire(${PROJECT_NAME})

set(${PROJECT_NAME}_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}
    CACHE INTERNAL "${PROJECT_NAME}: Include Directories" FORCE)
