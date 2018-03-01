cmake_minimum_required(VERSION 3.5)
project(TwoTree)

#set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_CXX_STANDARD 11)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(cotire)

set(SOURCE_FILES
        stdafx.cpp
        stdafx.h
        TwoTree.cpp
        TwoTree.h)

find_package(Boost REQUIRED graph)
include_directories(${CMAKE_SOURCE_DIR}
        ${Boost_INCLUDE_DIRS})

add_library(TwoTree SHARED ${SOURCE_FILES})
target_link_libraries(${PROJECT_NAME} PRIVATE ${Boost_LIBRARIES})
target_include_directories(${PROJECT_NAME} PRIVATE ${Boost_INCLUDE_DIRS})
set_target_properties(${PROJECT_NAME} PROPERTIES COTIRE_CXX_PREFIX_HEADER_INIT "stdafx.h")
cotire(${PROJECT_NAME})

set(${PROJECT_NAME}_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}
                                 ${Boost_INCLUDE_DIRS}
    CACHE INTERNAL "${PROJECT_NAME}: Include Directories" FORCE)
