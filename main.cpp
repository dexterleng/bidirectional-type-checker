#include <iostream>

#include "ast.h"
#include "ast_visitor.h"
#include "type_inference.h"

int main() {
  std::cout << "Hello, World!" << std::endl;

  auto intNode = std::make_unique<Integer<Var>>("42");
  auto funcNode = std::make_unique<Function<Var>>("square", std::make_unique<Integer<Var>>("0"));
  auto applyNode = std::make_unique<Apply<Var>>(std::move(funcNode), std::move(intNode));
  PrettyPrinterVisitor printer;
  printer.visit(applyNode.get());

  Infer infer;
  std::unordered_map<Var, Type> env;
  infer.visit(intNode.get(), env);

  std::cout << std::endl;

  return 0;
}
