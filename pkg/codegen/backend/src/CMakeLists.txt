find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIR})
link_directories(${LLVM_LIB_DIR})
add_definitions(${LLVM_DEFINITIONS})


get_filename_component(PARENT_DIR ../ ABSOLUTE)
include_directories("${PARENT_DIR}/include")
include_directories(/usr/local/include)

add_subdirectory(deserialize)
add_subdirectory(llvm_ir)
#add_library(
#    Deserializer
#
#    deserialize/Deserializer.cc
#    deserialize/Program.cc
#    deserialize/Block.cc
#    deserialize/Declaration.cc
#    deserialize/Expr.cc
#    deserialize/Statement.cc
#    deserialize/Type.cc
#    llvm_ir/SymbolTable.cc
#    "${PARENT_DIR}/include/program.pb.cc"
#)
#
#target_include_directories(
#    Deserializer
#
#    INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}"
#
#    PRIVATE "${LLVM_INCLUDE_DIR}"
#    PRIVATE "/usr/local/include"
#)

#target_link_libraries(
#    Deserializer
#
#    -lprotobuf
#    -L/usr/local/lib
#)


###########################
# IR CODEGEN
###########################
#add_library(
#    IRCodeGenVisitor
#
#    llvm_ir/IRCodeGenVisitor.cc
#    llvm_ir/ExprCodeGen.cc
#    llvm_ir/StatementCodeGen.cc
#)
#
#llvm_map_components_to_libnames(llvm_libs support core irreader)
#
#target_include_directories(
#    IRCodeGenVisitor
#
#    INTERFACE "${PARENT_DIR}"
#    PRIVATE "${LLVM_INCLUDE_DIR}"
#    PRIVATE "/usr/local/include"
#)
#
#target_link_libraries(
#    IRCodeGenVisitor
#
#    "${llvm_libs}"
#)