add_library(core
        core.cpp
        expr.h
        type.h
        type_inference.h
        type_constraint.h
        ast_visitor.h
        union_find.h
        error.h
        ast_pretty_printer.h
        stmt.h
)

add_executable(bidirectional_type_checker main.cpp)
target_link_libraries(bidirectional_type_checker core)
