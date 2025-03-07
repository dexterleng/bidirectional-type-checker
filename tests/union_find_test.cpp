#include <gtest/gtest.h>
#include "union_find.h"

TEST(UnionFindTest, InsertAndFind) {
  UnionFind uf;
  TypeVar var1 = uf.insert(std::nullopt);
  TypeVar var2 = uf.insert(std::nullopt);
  EXPECT_EQ(uf.find(var1), var1);
  EXPECT_EQ(uf.find(var2), var2);
}

TEST(UnionFindTest, UnionSimple) {
  UnionFind uf;
  TypeVar var1 = uf.insert(std::nullopt);
  TypeVar var2 = uf.insert(std::nullopt);
  uf.join(var1, var2);
  EXPECT_EQ(uf.find(var1), uf.find(var2));
}

TEST(UnionFindTest, UnionWithTypes) {
  UnionFind uf;
  auto type1 = std::make_shared<IntegerType>();
  auto type2 = std::make_shared<IntegerType>();
  TypeVar var1 = uf.insert(type1);
  TypeVar var2 = uf.insert(type2);
  uf.join(var1, var2);
  EXPECT_EQ(uf.find(var1), uf.find(var2));
  EXPECT_TRUE(uf.getType(var1).has_value());
  EXPECT_TRUE(uf.getType(var2).has_value());
  EXPECT_EQ(*uf.getType(var1).value(), *type1);
}

TEST(UnionFindTest, UnionDifferentTypes) {
  UnionFind uf;
  auto type1 = std::make_shared<IntegerType>();
  auto type2 = std::make_shared<VariableType>(1);
  TypeVar var1 = uf.insert(type1);
  TypeVar var2 = uf.insert(type2);
  EXPECT_THROW(uf.join(var1, var2), std::runtime_error);
}

TEST(UnionFindTest, MergeIntWithNullopt) {
  UnionFind uf;
  auto type1 = std::make_shared<IntegerType>();
  TypeVar var1 = uf.insert(type1);
  TypeVar var2 = uf.insert(std::nullopt);
  uf.join(var1, var2);
  EXPECT_EQ(uf.find(var1), uf.find(var2));
  EXPECT_TRUE(uf.getType(var1).has_value());
  EXPECT_EQ(*uf.getType(var1).value(), *type1);
}

TEST(UnionFindTest, MergeNulloptWithInt) {
  UnionFind uf;
  TypeVar var1 = uf.insert(std::nullopt);
  auto type2 = std::make_shared<IntegerType>();
  TypeVar var2 = uf.insert(type2);
  uf.join(var1, var2);
  EXPECT_EQ(uf.find(var1), uf.find(var2));
  EXPECT_TRUE(uf.getType(var1).has_value());
  EXPECT_EQ(*uf.getType(var1).value(), *type2);
}

TEST(UnionFindTest, MergeEquivalentFunctions) {
  UnionFind uf;
  auto func1 = std::make_shared<FunctionType>(
      std::make_shared<VariableType>(1),
      std::make_shared<VariableType>(2));
  auto func2 = std::make_shared<FunctionType>(
      std::make_shared<VariableType>(1),
      std::make_shared<VariableType>(2));
  TypeVar var1 = uf.insert(func1);
  TypeVar var2 = uf.insert(func2);
  uf.join(var1, var2);
  EXPECT_EQ(uf.find(var1), uf.find(var2));
  EXPECT_TRUE(uf.getType(var1).has_value());
  EXPECT_EQ(*uf.getType(var1).value(), *func1);
}

TEST(UnionFindTest, MergeDifferentFunctions) {
  UnionFind uf;
  auto func1 = std::make_shared<FunctionType>(
      std::make_shared<VariableType>(1),
      std::make_shared<VariableType>(2));
  auto func2 = std::make_shared<FunctionType>(
      std::make_shared<VariableType>(3),
      std::make_shared<VariableType>(4));
  TypeVar var1 = uf.insert(func1);
  TypeVar var2 = uf.insert(func2);
  EXPECT_THROW(uf.join(var1, var2), std::runtime_error);
}
