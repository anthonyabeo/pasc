cmake_minimum_required(VERSION 3.20)

project(
    Pasc

    VERSION 0.1.0
    LANGUAGES C CXX
)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CXX_FLAGS "-Wall")
set(CMAKE_CXX_FLAGS "${CXX_FLAGS}")


add_subdirectory(src)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIR})
link_directories(${LLVM_LIB_DIR})
add_definitions(${LLVM_DEFINITIONS})

add_executable(Pasc main.cc)
target_link_libraries(
    Pasc

    PRIVATE Deserializer
    PRIVATE IRCodeGenVisitor
)

target_include_directories(
    Pasc

    INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/include"
    INTERFACE "${LLVM_INCLUDE_DIR}"
)