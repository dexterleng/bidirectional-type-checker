#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <vector>
#include "ast.h"
#include "type_constraint.h"

class GenOut {
public:
  std::vector<TypeConstraint> constraints;
  std::unique_ptr<ASTNode<TypedVar>> typed_ast;

  GenOut(std::vector<TypeConstraint> constraints, std::unique_ptr<ASTNode<TypedVar>> typed_ast)
    : constraints(std::move(constraints)),
      typed_ast(std::move(typed_ast)) {
  }
};

class TypeInference {
public:
  std::pair<GenOut, std::shared_ptr<Type>> infer(
    const std::unordered_map<Var, Type>& env,
    const std::shared_ptr<ASTNode<Var>>& ast
  ) {

    // if (auto intNode = dynamic_cast<const Integer<Var>*>(&ast)) {
    //   return {
    //     GenOut({}, std::make_shared<IntNode<TypedVar>>(int_node->value))
    //   }
    // }
    // else if (auto varNode = dynamic_cast<const Variable<Var>*>(&ast)) {
    // }
    // else if (auto funNode = dynamic_cast<const Function<Var>*>(&ast)) {
    // }
    // else if (auto appNode = dynamic_cast<const Apply<Var>*>(&ast)) {
    // }
    // else {
    //   throw std::runtime_error("Unknown AST node");
    // }
  }
};

#endif //TYPE_INFERENCE_H
