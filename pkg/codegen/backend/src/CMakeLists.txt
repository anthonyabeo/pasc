find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIR})
link_directories(${LLVM_LIB_DIR})
add_definitions(${LLVM_DEFINITIONS})


get_filename_component(PARENT_DIR ../ ABSOLUTE)
include_directories("${PARENT_DIR}/include")
include_directories(/usr/local/include)

add_library(
    Deserializer

    Deserializer.cc
    Program.cc
    Block.cc
    Declaration.cc
    Expr.cc
    Statement.cc
    Type.cc
    SymbolTable.cc
    "${PARENT_DIR}/include/program.pb.cc"
)

target_include_directories(
    Deserializer

    INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}"

    PRIVATE "${LLVM_INCLUDE_DIR}"
    PRIVATE "/usr/local/include"
)

target_link_libraries(
    Deserializer

    -lprotobuf
    -L/usr/local/lib
)


###########################
# IRCODEGEN
###########################
add_library(
    IRCodeGenVisitor

    IRCodeGenVisitor.cc
    ExprCodeGen.cc
    StatementCodeGen.cc
)

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_include_directories(
    IRCodeGenVisitor

    INTERFACE "${PARENT_DIR}"
    PRIVATE "${LLVM_INCLUDE_DIR}"
    PRIVATE "/usr/local/include"
)

target_link_libraries(
    IRCodeGenVisitor

    "${llvm_libs}"
)