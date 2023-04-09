cmake_minimum_required(VERSION 3.20)

project(
    Pasc

    VERSION 0.1.0
    LANGUAGES CXX
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CXX_FLAGS "-Wall")
set(CMAKE_CXX_FLAGS "${CXX_FLAGS}")

set(CMAKE_BUILD_TYPE Debug)

add_subdirectory(deserialize)
# add_subdirectory(llvm_ir)

add_executable(Pasc main.cc)
target_link_libraries(
    Pasc

    PRIVATE Deserializer
    PRIVATE CodeGen
)

target_include_directories(
    Pasc

    PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}"
)