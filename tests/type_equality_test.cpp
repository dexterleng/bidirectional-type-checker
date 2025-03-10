#include <gtest/gtest.h>
#include "type.h"

TEST(TypeEqualityTest, IntegerEquality) {
  EXPECT_EQ(IntegerType(), IntegerType());
}

TEST(TypeEqualityTest, VariableEquality) {
  EXPECT_EQ(VariableType(1), VariableType(1));
}

TEST(TypeEqualityTest, VariableInequality) {
  EXPECT_NE(VariableType(1), VariableType(2));
}

TEST(TypeEqualityTest, IntegerVariableInequality) {
  EXPECT_NE(VariableType(1), IntegerType());
}

TEST(TypeEqualityTest, IntegerFunctionInequality) {
  EXPECT_NE(
    VariableType(1),
    FunctionType(std::make_shared<VariableType>(1), std::make_shared<VariableType>(2))
  );
}

TEST(TypeEqualityTest, DoubleEquality) {
  EXPECT_EQ(DoubleType(), DoubleType());
}

TEST(TypeEqualityTest, DoubleIntegerInequality) {
  EXPECT_NE(DoubleType(), IntegerType());
}

TEST(TypeEqualityTest, FunctionWithDoubleEquality) {
  FunctionType func1(std::make_shared<DoubleType>(), std::make_shared<IntegerType>());
  FunctionType func2(std::make_shared<DoubleType>(), std::make_shared<IntegerType>());
  EXPECT_EQ(func1, func2);
}

TEST(TypeEqualityTest, FunctionStructuralEquality) {
  FunctionType func1(std::make_shared<VariableType>(1), std::make_shared<VariableType>(2));
  FunctionType func2(std::make_shared<VariableType>(1), std::make_shared<VariableType>(2));
  EXPECT_EQ(func1, func2);
}

TEST(TypeEqualityTest, FunctionReferenceEquality) {
  auto varX = std::make_shared<VariableType>(1);
  auto varY = std::make_shared<VariableType>(2);
  auto func1 = std::make_shared<FunctionType>(varX, varY);
  auto func2 = std::make_shared<FunctionType>(varX, varY);
  EXPECT_EQ(*func1, *func2);
}

TEST(TypeEqualityTest, NestedFunctionEquality) {
  auto nestedFunc1 = std::make_shared<FunctionType>(
    std::make_shared<FunctionType>(std::make_shared<VariableType>(1), std::make_shared<VariableType>(2)),
    std::make_shared<FunctionType>(std::make_shared<VariableType>(3), std::make_shared<VariableType>(4))
  );
  auto nestedFunc2 = std::make_shared<FunctionType>(
    std::make_shared<FunctionType>(std::make_shared<VariableType>(1), std::make_shared<VariableType>(2)),
    std::make_shared<FunctionType>(std::make_shared<VariableType>(3), std::make_shared<VariableType>(4))
  );
  EXPECT_EQ(*nestedFunc1, *nestedFunc2);
}
