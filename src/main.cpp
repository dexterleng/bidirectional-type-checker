#include <iostream>

#include "ast.h"
#include "ast_visitor.h"
#include "type_inference.h"

int main() {
  // Variable representation (assuming Var is an integer ID wrapper)
  Var x(0);

  // AST Nodes
  auto varNode = std::make_shared<VariableNode<Var>>(x);      // Ast::Var(Var(0))
  auto funcNode = std::make_shared<FunctionNode<Var>>(x, varNode); // Ast::fun(Var(0), Ast::Var(Var(0)))
  auto intNode = std::make_shared<IntegerNode<Var>>("3");     // Ast::Int(3)
  auto applyNode = std::make_shared<ApplyNode<Var>>(funcNode, intNode); // Ast::app(...)

  PrettyPrinterVisitor printer;
  printer.print(*applyNode);

  TypeInference inference;
  auto res = inference.infer(*applyNode);
  auto typedAst = res.typedAst;

  return 0;
}
