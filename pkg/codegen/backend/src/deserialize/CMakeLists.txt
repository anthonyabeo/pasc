find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIR})
link_directories(${LLVM_LIB_DIR})
add_definitions(${LLVM_DEFINITIONS})


add_library(
    Deserializer

    Deserializer.cc
    Program.cc
    Block.cc
    Declaration.cc
    Expr.cc
    Statement.cc
    Type.cc
    ../llvm_ir/SymbolTable.cc
    ../../include/program.pb.cc
)

target_include_directories(
    Deserializer

#    INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}"
    INTERFACE ../../include

    PRIVATE "${LLVM_INCLUDE_DIR}"
    PRIVATE "/usr/local/include"
)

target_link_libraries(
    Deserializer

    -lprotobuf
    -L/usr/local/lib
)