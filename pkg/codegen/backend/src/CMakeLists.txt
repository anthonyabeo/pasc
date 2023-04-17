find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIR})
link_directories(${LLVM_LIB_DIR})
add_definitions(${LLVM_DEFINITIONS})


get_filename_component(PARENT_DIR ../ ABSOLUTE)
include_directories("${PARENT_DIR}/include")
include_directories(/usr/local/include)

add_subdirectory(deserialize)
add_subdirectory(llvm_ir)