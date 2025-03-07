cmake_minimum_required(VERSION 3.30)
project(bidirectional_type_checker)

set(CMAKE_CXX_STANDARD 20)

include(FetchContent)
FetchContent_Declare(
        googletest
        # Specify the commit you depend on and update it regularly.
        URL https://github.com/google/googletest/archive/5376968f6948923e2411081fd9372e71a59d8e77.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# Now simply link against gtest or gtest_main as needed. Eg
add_executable(bidirectional_type_checker main.cpp
        ast.h
        type.h
        type_inference.h
        type_constraint.h
        ast_visitor.h
        union_find.h)
target_link_libraries(bidirectional_type_checker gtest_main)
add_test(NAME bidirectional_type_checker_test COMMAND bidirectional_type_checker)
