#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <utility>
#include <vector>
#include "ast.h"
#include "ast_visitor.h"
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

using InferOut = std::pair<GenOut, Type>;

class Infer: public ASTVisitor<
  Var, Infer, InferOut,
  std::unordered_map<Var, Type>&
> {
public:
  InferOut visitInteger(Integer<Var>& node, std::unordered_map<Var, Type>& env) {
    return {
      GenOut(
        {},
        std::make_unique<Integer<TypedVar>>(node.literal)
      ),
      IntegerType{}
    };
  }

  InferOut visitVariable(Variable<Var>& node, std::unordered_map<Var, Type>& env) {
    Type& type = env[node.var];
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
      std::move(type)
    };
  }

  InferOut visitFunction(Function<Var>& node, std::unordered_map<Var, Type>& env) {
    // setup env to revert
    std::optional<Type> oldValue;
    auto it = env.find(node.arg);
    if (it != env.end()) {
      oldValue = std::make_optional(it->second);
    }

    // insert arg into env
    TypeVar argumentTypeVar = freshTypeVar();
    env[node.arg] = VariableType(argumentTypeVar);

    auto bodyInferOut = visit(*node.body, env);
    auto bodyGenOut = std::move(bodyInferOut.first);
    auto bodyType = bodyInferOut.second;

    // revert changes to env
    if (auto value = oldValue) {
      env[node.arg] = *value;
    } else {
      env.erase(node.arg);
    }

    auto functionNode = Function<TypedVar>(
      TypedVar(node.arg, VariableType(argumentTypeVar)),
      std::move(bodyGenOut.typed_ast)
    );
    auto genOut = GenOut { bodyGenOut.constraints, std::make_shared<Function<TypedVar>>(functionNode) };

    auto functionType = FunctionType(
      std::static_pointer_cast<Type>(
        std::make_shared<VariableType>(argumentTypeVar)
      ),
      std::make_shared<Type>(bodyType)
    );
    return {
      genOut,
      functionType
    };
  }

  TypeVar freshTypeVar() {
    return 0;
  }

  InferOut visitApply(Apply<Var>& node, std::unordered_map<Var, Type>& env) {
    // Infer the type of the argument
    auto argInferOut = visit(node.argument, env);
    GenOut argGenOut = std::move(argInferOut.first);
    auto argType = argInferOut.second;

    // Generate a fresh return type variable
    TypeVar returnTypeVar = freshTypeVar();
    auto returnType = VariableType(returnTypeVar);

    // Expected function type
    auto functionType = FunctionType(
      std::make_shared<Type>(argType),
      std::make_shared<Type>(returnType)
    );

    // Check the function type
    GenOut funGenOut = check(node.function, env, functionType);

    // Collect constraints from both argument and function
    std::vector<std::unique_ptr<TypeConstraint>> constraints;
    for (auto& constraint : argGenOut.constraints) {
      constraints.push_back(std::move(constraint));
    }
    for (auto& constraint : funGenOut.constraints) {
      constraints.push_back(std::move(constraint));
    }

    // Construct the new Apply node with typed AST
    auto applyNode = Apply<TypedVar>(
      *funGenOut.typed_ast,
      *argGenOut.typed_ast
    );

    auto genOut = GenOut {
      std::move(constraints),
      std::make_shared<Apply<TypedVar>>(applyNode)
    };

    return { genOut, returnType };
  }
};

class TypeInference {
public:
  std::pair<GenOut, std::shared_ptr<Type>> infer(
    const std::unordered_map<Var, Type>& env,
    const std::shared_ptr<ASTNode<Var>>& ast
  ) {

  }
};

#endif //TYPE_INFERENCE_H
