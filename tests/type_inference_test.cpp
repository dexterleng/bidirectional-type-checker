#include <gtest/gtest.h>
#include "type_inference.h"
#include "expr.h"
#include "type.h"

class TypeInferenceTest : public ::testing::Test {
protected:
    // Helper methods to create AST nodes more easily
    std::shared_ptr<IntegerExpr> makeInt(const std::string& value) {
        return std::make_shared<IntegerExpr>(value);
    }

    std::shared_ptr<DoubleExpr> makeDouble(const std::string& value) {
        return std::make_shared<DoubleExpr>(value);
    }

    std::shared_ptr<VariableExpr> makeVar(Var var) {
        return std::make_shared<VariableExpr>(var);
    }

    std::shared_ptr<FunctionNode> makeFun(Var arg, std::shared_ptr<Expr> body) {
        return std::make_shared<FunctionNode>(arg, std::move(body));
    }

    std::shared_ptr<ApplyExpr> makeApp(std::shared_ptr<Expr> function, std::shared_ptr<Expr> argument) {
        return std::make_shared<ApplyExpr>(std::move(function), std::move(argument));
    }

    std::shared_ptr<VariableType> makeVariableType(TypeVar typeVar) {
        return std::make_shared<VariableType>(typeVar);
    }
};

TEST_F(TypeInferenceTest, InfersInteger) {
    auto node = makeInt("3");

    TypeInference typeInference;
    typeInference.perform(*node);

    auto typedNode = makeInt("3");
    ASSERT_EQ(*node, *typedNode);
    ASSERT_EQ(typeInference.unbounded.size(), 0);
}

TEST_F(TypeInferenceTest, InfersDouble) {
    auto node = makeDouble("3");

    TypeInference typeInference;
    typeInference.perform(*node);

    auto typedNode = makeDouble("3");
    ASSERT_EQ(*node, *typedNode);
    ASSERT_EQ(typeInference.unbounded.size(), 0);
}

TEST_F(TypeInferenceTest, InfersIdentityFunction) {
    auto node = makeFun(
        Var(0),
        makeVar(Var(0))
    );

    TypeInference typeInference;
    typeInference.perform(*node);

    auto typedNode = makeFun(
        Var(0, makeVariableType(0)),
        makeVar(Var(0, makeVariableType(0)))
    );
    ASSERT_EQ(*node, *typedNode);
    ASSERT_EQ(typeInference.unbounded.size(), 1);
}

TEST_F(TypeInferenceTest, InfersSimpleApplication) {
    // (λx.x) 5
    auto x = Var(0);
    auto node = makeApp(
        makeFun(x, makeVar(x)),
        makeInt("5")
    );

    TypeInference typeInference;
    typeInference.perform(*node);

    // After inference, x should be an integer type
    auto typedX = Var(0, std::make_shared<IntegerType>());
    auto typedNode = makeApp(
        makeFun(typedX, makeVar(typedX)),
        makeInt("5")
    );

    ASSERT_EQ(*node, *typedNode);
    ASSERT_EQ(typeInference.unbounded.size(), 0);
}

TEST_F(TypeInferenceTest, InfersNestedApplication) {
    // (λx.λy.x) 3 4
    auto node = makeApp(
        makeApp(
            makeFun(Var(0), makeFun(Var(1), makeVar(Var(0)))),
            makeInt("3")
        ),
        makeInt("4")
    );

    TypeInference typeInference;
    typeInference.perform(*node);

    auto intType = std::make_shared<IntegerType>();
    auto typedX = Var(0, intType);
    auto typedY = Var(1, intType);

    auto typedNode = makeApp(
        makeApp(
            makeFun(typedX, makeFun(typedY, makeVar(typedX))),
            makeInt("3")
        ),
        makeInt("4")
    );

    ASSERT_EQ(*node, *typedNode);
    ASSERT_EQ(typeInference.unbounded.size(), 0);
}

TEST_F(TypeInferenceTest, InferenceShouldConstrainTypes) {
    // The function (λx.x x) should fail to type check because x can't be both a function and its argument
    auto x = Var(0);
    auto node = makeFun(
        x,
        makeApp(
            makeVar(x),
            makeVar(x)
        )
    );

    TypeInference typeInference;
    ASSERT_THROW({
        typeInference.perform(*node);
    }, InfiniteTypeError);
}
