add_library(
    Deserializer

    Deserializer.cc
    program.pb.cc
    Program.cc
    Block.cc
    Declaration.cc
    Expr.cc
    Statement.cc
    Type.cc
)

target_include_directories(
    Deserializer

    INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}"
    PUBLIC "/usr/local/include"
)

target_link_libraries(
    Deserializer

    -lprotobuf
    -L/usr/local/lib
)


add_library(
    CodeGen

    IRCodeGenVisitor.cc
)

target_include_directories(
    CodeGen

    INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}"
    PUBLIC "/usr/local/include"
)

target_link_libraries(
    CodeGen
    
    -L/usr/local/lib
)
