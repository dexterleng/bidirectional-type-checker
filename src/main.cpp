#include <iostream>

#include "ast.h"
#include "ast_pretty_printer.h"
#include "ast_visitor.h"
#include "type_inference.h"

int main() {
  // Variable representation (assuming Var is an integer ID wrapper)
  Var x(0);

  auto varNode = std::make_shared<VariableNode>(x);
  auto funcNode = std::make_shared<FunctionNode>(x, varNode);
  auto intNode = std::make_shared<IntegerNode>("3");
  auto applyNode = std::make_shared<ApplyNode>(funcNode, intNode);

  TypeInference typeInference;
  typeInference.solve(*applyNode);

  ASTPrettyPrinter printer(&typeInference.unionFind);
  printer.print(*applyNode);

  return 0;
}
