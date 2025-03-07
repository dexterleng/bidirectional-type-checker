cmake_minimum_required(VERSION 3.30)
project(bidirectional_type_checker)

set(CMAKE_CXX_STANDARD 20)

add_library(bidirectional_type_checker_lib
        lib.cpp
        ast.h
        type.h
        type_inference.h
        type_constraint.h
        ast_visitor.h
        union_find.h)

add_executable(bidirectional_type_checker main.cpp)
target_link_libraries(bidirectional_type_checker bidirectional_type_checker_lib)

add_subdirectory(tests)
