#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <vector>
#include "ast.h"
#include "type_constraint.h"

class GenOut {
public:
  std::vector<std::unique_ptr<TypeConstraint>> constraints;
  std::shared_ptr<ASTNode<TypedVar>> typedAst;

  GenOut(std::vector<std::unique_ptr<TypeConstraint>> constraints, std::shared_ptr<ASTNode<TypedVar>> typedAst)
    : constraints(std::move(constraints)),
      typedAst(std::move(typedAst)) {
  }
};

class InferOut {
public:
  std::unique_ptr<GenOut> genOut;
  std::shared_ptr<Type> type;

  InferOut(std::unique_ptr<GenOut> gen_out, std::shared_ptr<Type> type)
    : genOut(std::move(gen_out)),
      type(std::move(type)) {
  }
};

// Helper struct to store environment state for later restoration
struct EnvState {
  Var var;
  bool existed;
  std::shared_ptr<Type> oldValue;
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
  InferOut inferInteger(IntegerNode<Var> node) {
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
    TypeVar argumentTypeVar = freshTypeVar();
    auto argumentType = std::make_shared<VariableType>(argumentTypeVar);

    EnvState envState = extendEnv(node.arg, argumentType);
    auto bodyInferOut = infer(*node.body);
    restoreEnv(envState);

    auto functionNode = FunctionNode<TypedVar>(
      TypedVar(
        node.arg,
        std::make_shared<VariableType>(argumentTypeVar)
      ),
      bodyInferOut.genOut->typedAst
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
  //     *funGenOut.typedAst,
  //     *argGenOut.typedAst
  //   );
  //
  //   auto genOut = GenOut {
  //     std::move(constraints),
  //     std::make_shared<ApplyNode<TypedVar>>(applyNode)
  //   };
  //
  //   return { genOut, returnType };
  }

  std::unique_ptr<GenOut> check(
    ASTNode<Var>& node,
    Type& type
  ) {
    if (node.kind == ASTNodeKind::Integer && type.kind == TypeKind::Integer) {
      auto node = static_cast<IntegerNode<Var>&>(node);
      auto type = static_cast<IntegerType&>(type);
      return std::make_unique<GenOut>(
        std::vector<std::unique_ptr<TypeConstraint>>(),
        std::make_shared<IntegerNode<TypedVar>>(node.literal)
      );
    }

    if (node.kind == ASTNodeKind::Function && type.kind == TypeKind::Function) {
      auto node = static_cast<FunctionNode<Var>&>(node);
      auto type = static_cast<FunctionType&>(type);

      auto envState = extendEnv(node.arg, type.from);
      auto bodyCheckOut = check(*node.body, *type.to);
      restoreEnv(envState);

      auto functionNode = FunctionNode<TypedVar>(
        TypedVar(
          node.arg,
          type.from
        ),
        bodyCheckOut->typedAst
      );

      return std::make_unique<GenOut>(
        std::move(bodyCheckOut->constraints),
        std::make_shared<FunctionNode<TypedVar>>(functionNode)
      );
    }

    auto inferOut = infer(node);
    auto genOut = std::move(inferOut.genOut);
    auto constraint = std::make_unique<EqualTypeConstraint>(type, inferOut.type);
    genOut->constraints.push_back(std::move(constraint));
    return genOut;
  }

  TypeVar freshTypeVar() {
    return 0;
  }

  // Helper method to save environment state and set a new value
  EnvState extendEnv(const Var& var, std::shared_ptr<Type> type) {
    EnvState state{var, false, nullptr};
    auto it = env.find(var);

    if (it != env.end()) {
      state.existed = true;
      state.oldValue = it->second;
    }

    env[var] = std::move(type);
    return state;
  }

  // Helper method to restore previous environment state
  void restoreEnv(const EnvState& state) {
    if (state.existed) {
      env[state.var] = state.oldValue;
    } else {
      env.erase(state.var);
    }
  }
};

#endif //TYPE_INFERENCE_H
