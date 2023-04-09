add_library(
    CodeGen

    IRCodeGenVisitor.cc
)

get_filename_component(DIR_ONE_ABOVE ../ ABSOLUTE)
message(STATUS ${DIR_ONE_ABOVE})

target_include_directories(
    CodeGen

    INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}"
    INTERFACE "${DIR_ONE_ABOVE}/deserialize"

    PUBLIC "/usr/local/include"
)

target_link_libraries(
    CodeGen

    -L/usr/local/lib
)