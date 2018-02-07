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
        main.cpp
        Node.cpp
        Node.h
        RootFinder.cpp
        RootFinder.h)

include_directories(${Utils_INCLUDE_DIR}
        ${CMAKE_SOURCE_DIR}/Splinter/include)

add_library(${PROJECT_NAME} STATIC ${SOURCE_FILES})

set(SPLINTER_LIB CACHE FILEPATH "Path to Splinter library")
target_link_libraries(DR_plan Utils ${SPLINTER_LIB})
set_target_properties(DR_plan PROPERTIES COTIRE_CXX_PREFIX_HEADER_INIT "stdafx.h")
cotire(${PROJECT_NAME})

set(${PROJECT_NAME}_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}
    CACHE INTERNAL "${PROJECT_NAME}: Include Directories" FORCE)
