find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIR})
link_directories(${LLVM_LIB_DIR})
add_definitions(${LLVM_DEFINITIONS})


add_library(
    IRCodeGenVisitor

    IRCodeGenVisitor.cc
    ExprCodeGen.cc
    StatementCodeGen.cc
    TypeCodeGen.cc)

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_include_directories(
    IRCodeGenVisitor

    INTERFACE ../../include/
    PRIVATE "${LLVM_INCLUDE_DIR}"
    PRIVATE "/usr/local/include"
)

target_link_libraries(
    IRCodeGenVisitor

    "${llvm_libs}"
)