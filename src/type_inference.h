#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <vector>
#include "ast.h"
#include "type_constraint.h"
#include "union_find.h"

// Helper struct to store environment state for later restoration
struct EnvState {
  Var var;
  bool existed;
  std::shared_ptr<Type> oldValue;
};

class TypeInference {
public:
  std::unordered_map<VariableName, std::shared_ptr<Type>> env;
  std::vector<std::unique_ptr<TypeConstraint>> constraints;
  UnionFind unionFind;

  TypeInference() = default;

  void solve(
    ASTNode& node
  ) {
    auto type = infer(node);

    for (auto& _constraint : this->constraints) {
      switch (_constraint->kind) {
        case TypeConstraintKind::Equal: {
          auto constraint = static_cast<EqualTypeConstraint*>(_constraint.get());
          solveEqualTypeConstraint(*constraint);
          break;
        }
        default:
          throw std::runtime_error("Unknown TypeConstraintKind");
      }
    }
  }

  void solveEqualTypeConstraint(EqualTypeConstraint& constraint) {
    auto lhsType = normalizeType(constraint.lhs);
    auto rhsType = normalizeType(constraint.rhs);
    _solveEqualTypeConstraint(lhsType, rhsType);
  }

  void _solveEqualTypeConstraint(const std::shared_ptr<Type>& lhsType, const std::shared_ptr<Type>& rhsType) {
    if (lhsType->kind == TypeKind::Integer && rhsType->kind == TypeKind::Integer) {
      return;
    }

    if (lhsType->kind == TypeKind::Function && rhsType->kind == TypeKind::Function) {
      auto lhsFunctionType = static_pointer_cast<FunctionType>(lhsType);
      auto rhsFunctionType = static_pointer_cast<FunctionType>(rhsType);
      _solveEqualTypeConstraint(lhsFunctionType->from, rhsFunctionType->from);
      _solveEqualTypeConstraint(lhsFunctionType->to, rhsFunctionType->to);
      return;
    }

    if (lhsType->kind == TypeKind::Variable && rhsType->kind == TypeKind::Variable) {
      auto lhsVariableType = static_pointer_cast<VariableType>(lhsType);
      auto rhsVariableType = static_pointer_cast<VariableType>(rhsType);
      unionFind.join(lhsVariableType->typeVar, rhsVariableType->typeVar);
      return;
    }

    if (lhsType->kind == TypeKind::Variable || rhsType->kind == TypeKind::Variable) {
      auto variableType = std::static_pointer_cast<VariableType>(lhsType->kind == TypeKind::Variable ? lhsType : rhsType);
      auto type = lhsType->kind == TypeKind::Variable ? rhsType : lhsType;

      if (hasTypeVar(type, variableType->typeVar)) {
        throw std::runtime_error("infinite type error");
      }

      unionFind.setType(variableType->typeVar, type); // TODO: catch and throw a type not equal error
      return;
    }

    throw std::runtime_error("type not equal");
  }

  bool hasTypeVar(const std::shared_ptr<Type>& type, TypeVar var) {
    switch (type->kind) {
      case TypeKind::Integer:
        return false;
      case TypeKind::Variable: {
        auto varType = static_pointer_cast<VariableType>(type);
        return varType->typeVar == var;
      }
      case TypeKind::Function: {
        auto funType = static_pointer_cast<FunctionType>(type);
        return hasTypeVar(funType->from, var) || hasTypeVar(funType->to, var);
      }
      default:
        throw std::runtime_error("Unknown TypeKind in hasTypeVar");
    }
  }

  std::shared_ptr<Type> normalizeType(std::shared_ptr<Type> _type) {
    switch (_type->kind) {
      case TypeKind::Integer:
        return _type;
      case TypeKind::Variable: {
        auto type = static_pointer_cast<VariableType>(_type);
        auto normalizedType = unionFind.getType(type->typeVar);
        if (normalizedType.has_value()) {
          return normalizeType(*normalizedType);
        } else {
          auto typeVar = unionFind.find(type->typeVar);
          return std::make_shared<VariableType>(typeVar);
        }
      }
      case TypeKind::Function: {
        auto type = static_pointer_cast<FunctionType>(_type);
        auto fromType = normalizeType(type->from);
        auto toType = normalizeType(type->to);
        return std::make_shared<FunctionType>(fromType, toType);
      }
      default:
        throw std::runtime_error("Unknown TypeKind");
    }
  }

  TypeVar freshTypeVar() {
    return unionFind.insert(std::nullopt);
  }

private:
  std::shared_ptr<Type> infer(
    ASTNode& node
  ) {
    switch (node.kind) {
      case ASTNodeKind::Integer:
        return inferInteger(static_cast<IntegerNode&>(node));
      case ASTNodeKind::Variable:
        return inferVariable(static_cast<VariableNode&>(node));
      case ASTNodeKind::Function:
        return inferFunction(static_cast<FunctionNode&>(node));
      case ASTNodeKind::Apply:
        return inferApply(static_cast<ApplyNode&>(node));
      default:
        throw std::runtime_error("Unknown ASTNodeKind");
    }
  }

  std::shared_ptr<Type> inferInteger(IntegerNode& node) {
    return std::make_shared<IntegerType>();
  }

  std::shared_ptr<Type> inferVariable(VariableNode& node) {
    auto type = env[node.var.name];
    node.var.type = type;
    return type;
  }

  std::shared_ptr<Type> inferFunction(FunctionNode& node) {
    auto argumentType = std::make_shared<VariableType>(freshTypeVar());
    node.arg.type = argumentType;

    EnvState envState = extendEnv(node.arg, argumentType);
    auto bodyType = infer(*node.body);
    restoreEnv(envState);

    return std::make_unique<FunctionType>(
      argumentType,
      bodyType
    );
  }

  std::shared_ptr<Type> inferApply(ApplyNode& node) {
    // construct a function type to check against the real function
    auto argType = infer(*node.argument);
    auto returnType = std::make_shared<VariableType>(freshTypeVar());
    auto functionType = std::make_shared<FunctionType>(argType, returnType);
    check(*node.function, functionType);
    return returnType;
  }

  void check(
    ASTNode& node,
    const std::shared_ptr<Type>& type
  ) {
    if (node.kind == ASTNodeKind::Integer && type->kind == TypeKind::Integer) {
      return;
    }

    if (node.kind == ASTNodeKind::Function && type->kind == TypeKind::Function) {
      auto functionNode = static_cast<FunctionNode&>(node);
      auto functionType = static_pointer_cast<FunctionType>(type);

      auto envState = extendEnv(functionNode.arg, functionType->from);
      check(*functionNode.body, functionType->to);
      restoreEnv(envState);

      functionNode.arg.type = functionType->from;
      return;
    }

    auto inferredType = infer(node);
    auto constraint = std::make_unique<EqualTypeConstraint>(type, inferredType);
    this->constraints.push_back(std::move(constraint));
  }

  // Helper method to save environment state and set a new value
  EnvState extendEnv(const Var& var, std::shared_ptr<Type> type) {
    EnvState state{var, false, nullptr};
    auto it = env.find(var.name);

    if (it != env.end()) {
      state.existed = true;
      state.oldValue = it->second;
    }

    env[var.name] = std::move(type);
    return state;
  }

  // Helper method to restore previous environment state
  void restoreEnv(const EnvState& state) {
    if (state.existed) {
      env[state.var.name] = state.oldValue;
    } else {
      env.erase(state.var.name);
    }
  }
};

#endif //TYPE_INFERENCE_H
