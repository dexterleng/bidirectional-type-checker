#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <utility>
#include <vector>
#include "ast.h"
#include "type_constraint.h"

class GenOut {
public:
  std::vector<std::unique_ptr<TypeConstraint>> constraints;
  std::shared_ptr<ASTNode<TypedVar>> typed_ast;

  GenOut(std::vector<std::unique_ptr<TypeConstraint>> constraints, std::shared_ptr<ASTNode<TypedVar>> typed_ast)
    : constraints(std::move(constraints)),
      typed_ast(std::move(typed_ast)) {
  }
};

class InferOut {
public:
  std::unique_ptr<GenOut> genOut;
  std::shared_ptr<Type> type;

  InferOut(std::unique_ptr<GenOut> gen_out, std::shared_ptr<Type> type)
    : genOut(std::move(gen_out)),
      type(type) {
  }
};

class TypeInference {
public:
  std::unordered_map<Var, std::shared_ptr<Type>> env;

  TypeInference() = default;

  InferOut infer(
    ASTNode<Var>& node
  ) {
    switch (node.kind) {
      case ASTNodeKind::Integer:
        return inferInteger(static_cast<IntegerNode<Var>&>(node));
      case ASTNodeKind::Variable:
        return inferVariable(static_cast<VariableNode<Var>&>(node));
      case ASTNodeKind::Function:
        return inferFunction(static_cast<FunctionNode<Var>&>(node));
      case ASTNodeKind::Apply:
        return inferApply(static_cast<ApplyNode<Var>&>(node));
      default:
        throw std::runtime_error("Unknown AST node kind");
    }
  }

private:
  InferOut inferInteger(IntegerNode<Var>& node) {
    return InferOut(
      std::make_unique<GenOut>(
        std::vector<std::unique_ptr<TypeConstraint>>(),
        std::make_shared<IntegerNode<TypedVar>>(node.literal)
      ),
      std::make_shared<IntegerType>()
    );
  }

  InferOut inferVariable(VariableNode<Var>& node) {
    return InferOut(
      std::make_unique<GenOut>(
        std::vector<std::unique_ptr<TypeConstraint>>(),
        std::make_shared<VariableNode<TypedVar>>(
          TypedVar {
            node.var,
            env[node.var]
          }
        )
      ),
      env[node.var]
    );
  }

  InferOut inferFunction(FunctionNode<Var>& node) {
    // setup env to revert
    std::optional<std::shared_ptr<Type>> oldValue;
    auto it = env.find(node.arg);
    if (it != env.end()) {
      oldValue = std::make_optional(it->second);
    }

    // insert arg into env
    TypeVar argumentTypeVar = freshTypeVar();
    env[node.arg] = std::make_shared<VariableType>(argumentTypeVar);

    // infer the body
    auto bodyInferOut = infer(*node.body);

    // revert changes to env
    if (auto value = oldValue) {
      env[node.arg] = *value;
    } else {
      env.erase(node.arg);
    }

    auto functionNode = FunctionNode<TypedVar>(
      TypedVar(
        node.arg,
        std::make_shared<VariableType>(argumentTypeVar)
      ),
      bodyInferOut.genOut->typed_ast
    );

    return InferOut(
      std::make_unique<GenOut>(
        std::move(bodyInferOut.genOut->constraints),
        std::make_shared<FunctionNode<TypedVar>>(functionNode)
      ),
      std::make_unique<FunctionType>(
        std::make_shared<VariableType>(argumentTypeVar),
        bodyInferOut.type
      )
    );
  }

  InferOut inferApply(ApplyNode<Var>& node) {
    throw std::runtime_error("Unimplemented");
  //   // Infer the type of the argument
  //   auto argInferOut = infer(node.argument, env);
  //   GenOut argGenOut = std::move(argInferOut.first);
  //   auto argType = argInferOut.second;
  //
  //   // Generate a fresh return type variable
  //   TypeVar returnTypeVar = freshTypeVar();
  //   auto returnType = VariableType(returnTypeVar);
  //
  //   // Expected function type
  //   auto functionType = FunctionType(
  //     std::make_shared<Type>(argType),
  //     std::make_shared<Type>(returnType)
  //   );
  //
  //   // Check the function type
  //   GenOut funGenOut = check(node.function, env, functionType);
  //
  //   // Collect constraints from both argument and function
  //   std::vector<std::unique_ptr<TypeConstraint>> constraints;
  //   for (auto& constraint : argGenOut.constraints) {
  //     constraints.push_back(std::move(constraint));
  //   }
  //   for (auto& constraint : funGenOut.constraints) {
  //     constraints.push_back(std::move(constraint));
  //   }
  //
  //   // Construct the new Apply node with typed AST
  //   auto applyNode = ApplyNode<TypedVar>(
  //     *funGenOut.typed_ast,
  //     *argGenOut.typed_ast
  //   );
  //
  //   auto genOut = GenOut {
  //     std::move(constraints),
  //     std::make_shared<ApplyNode<TypedVar>>(applyNode)
  //   };
  //
  //   return { genOut, returnType };
  }

  GenOut check(
  ASTNode<Var>& node,
  std::unordered_map<Var, Type>& env
) {
    // switch (node.kind) {
    //   case ASTNodeKind::Integer:
    //     return inferInteger(static_cast<Integer<Var>&>(node), env);
    //   case ASTNodeKind::Variable:
    //     return inferVariable(static_cast<Variable<Var>&>(node), env);
    //   case ASTNodeKind::Function:
    //     return inferFunction(static_cast<Function<Var>&>(node), env);
    //   case ASTNodeKind::Apply:
    //     return inferApply(static_cast<Apply<Var>&>(node), env);
    //   default:
    //     throw std::runtime_error("Unknown AST node kind");
    // }
  }



//   fn check(
//   &mut self,
//   ast: Ast<Var>,
//   ty: Type
// ) -> GenOut {
//     match (ast, ty) {
//       // ...
//     }
//   }

  TypeVar freshTypeVar() {
    return 0;
  }
};

#endif //TYPE_INFERENCE_H
