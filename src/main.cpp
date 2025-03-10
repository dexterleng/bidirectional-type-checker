#include <iostream>

#include "ast.h"
#include "ast_pretty_printer.h"
#include "type_inference.h"

int main() {
  // Variable representations
  Var x(0); // x variable (first parameter)
  Var y(1); // y variable (second parameter)

  // Create variable nodes for x and y
  auto xVarNode = std::make_shared<VariableNode>(x);
  auto yVarNode = std::make_shared<VariableNode>(y);

  // Create the addition node x + y (body of the inner lambda)
  auto addNode = std::make_shared<AddNode>(xVarNode, yVarNode);

  // Create the inner lambda: y -> x + y
  auto innerLambda = std::make_shared<FunctionNode>(y, addNode);

  // Create the outer lambda: x -> (y -> x + y)
  auto outerLambda = std::make_shared<FunctionNode>(x, innerLambda);

  // Create the arguments
  auto arg1 = std::make_shared<DoubleNode>("3.0");
  auto arg2 = std::make_shared<DoubleNode>("4.1");

  // Apply the function to the first argument: (x -> y -> x + y)(3.0)
  auto firstApply = std::make_shared<ApplyNode>(outerLambda, arg1);

  // Apply the result to the second argument: ((x -> y -> x + y)(3.0))(4.1)
  auto secondApply = std::make_shared<ApplyNode>(firstApply, arg2);

  TypeInference inference;
  inference.perform(*secondApply);

  // Pretty print the resulting AST
  ASTPrettyPrinter printer(nullptr);
  std::cout << "Expression: (x -> y -> x + y)(3.0)(4.1)" << std::endl;
  std::cout << "AST:" << std::endl;
  printer.print(*secondApply);

  return 0;
}