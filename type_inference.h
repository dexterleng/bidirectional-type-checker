#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <vector>
#include "ast.h"
#include "ast_visitor.h"
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

using InferOut = std::pair<GenOut, Type>;

class Infer: public ASTVisitor<
  Var, Infer, InferOut,
  std::unordered_map<Var, Type>&
> {
public:
  InferOut visitInteger(Integer<Var>& node, std::unordered_map<Var, Type>& env) {
    return {
      GenOut({}, std::make_unique<Integer<TypedVar>>(node.literal)),
      IntegerType{}
    };
  }

  // Ast::Var(v) => {
  //   let ty = &env[&v];
  //   (
  //     GenOut::new(
  //       vec![],
  //       // Return a `TypedVar` instead of `Var`
  //       Ast::Var(TypedVar(v, ty.clone())
  //     ),
  //     ty.clone(),
  //   )
  // },
  InferOut visitVariable(Variable<Var>& node, std::unordered_map<Var, Type>& env) {
    auto type = env[node.var];
    return {
      GenOut(
        {},
        std::make_unique<Variable<TypedVar>>(
          TypedVar {
            node.var,
            type
          }
        )
      ),
      type
    };
  }

  // InferOut visitFunction(Function<Var> *node, std::unordered_map<Var, Type>& env) {
    // visit(node->body.get());
  // }

  // InferOut visitApply(Apply<Var> *node, std::unordered_map<Var, Type>& env) {
    // visit(node->function.get());
    // visit(node->argument.get());
  // }
};

// class TypeInference {
// public:
//   std::pair<GenOut, std::shared_ptr<Type>> infer(
//     const std::unordered_map<Var, Type>& env,
//     const std::shared_ptr<ASTNode<Var>>& ast
//   ) {
//
//   }
// };

#endif //TYPE_INFERENCE_H
