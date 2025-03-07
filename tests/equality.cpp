#include <gtest/gtest.h>

// A sample function to test (replace with real implementation)
int add(int a, int b) {
  return a + b;
}

// Test cases
TEST(TypeInferenceTest, AdditionWorks) {
  EXPECT_EQ(add(2, 3), 5);
  EXPECT_EQ(add(-1, 1), 0);
}

TEST(TypeInferenceTest, ZeroCase) {
  EXPECT_EQ(add(0, 0), 0);
  EXPECT_EQ(add(5, 0), 5);
}

TEST(TypeInferenceTest, NegativeNumbers) {
  EXPECT_EQ(add(-5, -3), -8);
  EXPECT_EQ(add(-2, 2), 0);
}