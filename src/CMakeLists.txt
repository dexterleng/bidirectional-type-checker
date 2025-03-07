add_library(core
        core.cpp
        ast.h
        type.h
        type_inference.h
        type_constraint.h
        ast_visitor.h
        union_find.h
)

add_executable(bidirectional_type_checker main.cpp)
target_link_libraries(bidirectional_type_checker core)
