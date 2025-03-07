cmake_minimum_required(VERSION 3.30)
project(bidirectional_type_checker)

set(CMAKE_CXX_STANDARD 20)

add_executable(bidirectional_type_checker main.cpp
        ast.h
        type.h
        type_inference.h
        type_constraint.h
        ast_visitor.h
        union_find.h)
