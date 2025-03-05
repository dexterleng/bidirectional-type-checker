#include <iostream>

#include "ast.h"
#include "ast_visitor.h"
#include "type_inference.h"

int main() {
  auto intNode = std::make_shared<IntegerNode<Var>>("42");
  auto varNode = std::make_shared<VariableNode<Var>>(0);
  auto funcNode = std::make_shared<FunctionNode<Var>>(1, intNode);
  auto applyNode = std::make_shared<ApplyNode<Var>>(funcNode, intNode);

  PrettyPrinterVisitor printer;
  printer.print(*applyNode);

  TypeInference inference;
  inference.infer(*funcNode);

  return 0;
}
