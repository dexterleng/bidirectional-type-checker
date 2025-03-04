#include <iostream>

#include "ast.h"
#include "ast_visitor.h"
#include "type_inference.h"

int main() {
  std::cout << "Hello, World!" << std::endl;

  auto intNode = Integer<Var>("42");
  auto funcNode = Function<Var>("square", intNode);
  auto applyNode = Apply<Var>(funcNode, intNode);
  PrettyPrinterVisitor printer;
  printer.visit(applyNode);

  Infer infer;
  std::unordered_map<Var, Type> env;
  // infer.visit(intNode.get(), env);

  std::cout << std::endl;

  return 0;
}
