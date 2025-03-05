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
    auto argumentType = std::make_shared<VariableType>(freshTypeVar());

    EnvState envState = extendEnv(node.arg, argumentType);
    auto bodyInferOut = infer(*node.body);
    restoreEnv(envState);

    auto functionNode = FunctionNode<TypedVar>(
      TypedVar(
        node.arg,
        argumentType
      ),
      bodyInferOut.genOut->typedAst
    );

    return InferOut(
      std::make_unique<GenOut>(
        std::move(bodyInferOut.genOut->constraints),
        std::make_shared<FunctionNode<TypedVar>>(functionNode)
      ),
      std::make_unique<FunctionType>(
        argumentType,
        bodyInferOut.type
      )
    );
  }

  InferOut inferApply(ApplyNode<Var>& node) {
    // construct a function type to check against the real function
    auto argInferOut = infer(*node.argument);
    auto argType = argInferOut.type;
    auto returnType = std::make_shared<VariableType>(freshTypeVar());
    auto functionType = std::make_shared<FunctionType>(argType, returnType);

    auto functionGenOut = check(*node.function, functionType);

    auto constraints = std::move(argInferOut.genOut->constraints);
    auto functionConstraints = std::move(functionGenOut->constraints);
    for (auto& constraint : functionConstraints) {
      constraints.push_back(std::move(constraint));
    }

    return InferOut(
      std::make_unique<GenOut>(
        std::move(constraints),
        std::make_shared<ApplyNode<TypedVar>>(
          functionGenOut->typedAst,
          argInferOut.genOut->typedAst
        )
      ),
      returnType
    );
  }

  std::unique_ptr<GenOut> check(
    ASTNode<Var>& _node,
    std::shared_ptr<Type> _type
  ) {
    if (_node.kind == ASTNodeKind::Integer && _type->kind == TypeKind::Integer) {
      auto node = static_cast<IntegerNode<Var>&>(_node);
      return std::make_unique<GenOut>(
        std::vector<std::unique_ptr<TypeConstraint>>(),
        std::make_shared<IntegerNode<TypedVar>>(node.literal)
      );
    }

    if (_node.kind == ASTNodeKind::Function && _type->kind == TypeKind::Function) {
      auto node = static_cast<FunctionNode<Var>&>(_node);
      auto type = static_pointer_cast<FunctionType>(_type);

      auto envState = extendEnv(node.arg, type->from);
      auto bodyCheckOut = check(*node.body, type->to);
      restoreEnv(envState);

      auto functionNode = FunctionNode<TypedVar>(
        TypedVar(
          node.arg,
          type->from
        ),
        bodyCheckOut->typedAst
      );

      return std::make_unique<GenOut>(
        std::move(bodyCheckOut->constraints),
        std::make_shared<FunctionNode<TypedVar>>(functionNode)
      );
    }

    auto inferOut = infer(_node);
    auto genOut = std::move(inferOut.genOut);
    auto constraint = std::make_unique<EqualTypeConstraint>(_type, inferOut.type);
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
