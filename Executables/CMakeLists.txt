cmake_minimum_required(VERSION 3.5)
project(Executable)

#set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES main.cpp)

include_directories(${CMAKE_SOURCE_DIR}
        ${TwoTree_INCLUDE_DIRS}
        ${DR_plan_INCLUDE_DIRS})

add_executable(${PROJECT_NAME} ${SOURCE_FILES})

target_link_libraries(${PROJECT_NAME} DR_plan TwoTree)
