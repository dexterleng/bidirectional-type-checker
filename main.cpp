#include <iostream>

#include "ast.h"
#include "ast_visitor.h"
#include "type_inference.h"

int main() {
  std::cout << "Hello, World!" << std::endl;

  auto intNode = Integer<Var>("42");
  auto varNode = Variable<Var>(0);
  auto funcNode = Function<Var>("square", intNode);
  auto applyNode = Apply<Var>(funcNode, intNode);
  PrettyPrinterVisitor printer;
  printer.visit(applyNode);
  std::cout << std::endl;

  Infer infer;
  std::unordered_map<Var, Type> env;
  infer.visit(varNode, env);

  for(const auto& [key, value] : env) {
    std::cout << key << " : " << &value << std::endl;
  }


  // env[]

  std::cout << std::endl;

  return 0;
}
