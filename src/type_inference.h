#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <set>
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

class InfiniteTypeError : public Error {
public:
  InfiniteTypeError(const Type& variableType, const Type& cycleType)
    : Error("Infinite type detected: " + variableType.toString() +
            " occurs in " + cycleType.toString()) {}
};

class TypeNotEqualError : public Error {
public:
  TypeNotEqualError(const Type& typeA, const Type& typeB)
    : Error("Types are not equal: " + typeA.toString() + " and " + typeB.toString()) {}
};

class TypeInference {
public:
  std::unordered_map<VariableName, std::shared_ptr<Type>> env;
  std::vector<std::unique_ptr<TypeConstraint>> constraints;
  std::set<TypeVar> unbounded;
  UnionFind unionFind;

  TypeInference() = default;

  void perform(ASTNode& node) {
    auto type = infer(node);
    solveConstraints(node);
    substitute(type);
    substituteAst(node);
  }

private:
  std::shared_ptr<Type> substitute(std::shared_ptr<Type> ty) {
    switch (ty->kind) {
      case TypeKind::Integer:
      case TypeKind::Double: {
        // For integer and double types, return empty set of unbound variables and the original type
        return ty;
      }
      case TypeKind::Variable: {
        // For variable types, check if it's bound to a concrete type
        auto varType = static_pointer_cast<VariableType>(ty);
        auto root = unionFind.find(varType->typeVar);
        auto resolvedType = unionFind.getType(root);

        if (resolvedType.has_value()) {
          // If bound, recursively substitute
          return substitute(*resolvedType);
        } else {
          // If unbound, add to set of unbound variables
          unbounded.insert(root);

          return std::make_shared<VariableType>(root);
        }
      }
      case TypeKind::Function: {
        // For function types, recursively substitute in argument and return types
        auto funType = static_pointer_cast<FunctionType>(ty);
        auto argType = substitute(funType->from);
        auto retType = substitute(funType->to);
        return std::make_shared<FunctionType>(argType, retType);
      }
      default:
        throw std::runtime_error("Unhandled TypeKind");
    }
  }

  void substituteAst(ASTNode& node) {
    switch (node.kind) {
      case ASTNodeKind::Integer:
      case ASTNodeKind::Double: {
        break;
      }
      case ASTNodeKind::Variable: {
        // For variable nodes, substitute the type.
        auto& varNode = static_cast<VariableNode&>(node);
        auto substitutedType = substitute(varNode.var.type.value());
        varNode.var.type = substitutedType;
        break;
      }
      case ASTNodeKind::Function: {
        // For function nodes, substitute the argument type and then the body
        auto& funNode = static_cast<FunctionNode&>(node);
        // Substitute the argument type
        auto argType = substitute(funNode.arg.type.value());
        // Update function node with updated argument type
        funNode.arg.type = argType;
        // Recursively substitute in the body
        substituteAst(*funNode.body);
        break;
      }
      case ASTNodeKind::Apply: {
        // For apply nodes, substitute in both the function and argument parts
        auto& applyNode = static_cast<ApplyNode&>(node);
        // First substitute in the function
        substituteAst(*applyNode.function);
        // Then substitute in the argument
        substituteAst(*applyNode.argument);
        break;
      }
      case ASTNodeKind::Add: {
        auto& addNode = static_cast<AddNode&>(node);
        substituteAst(*addNode.left);
        substituteAst(*addNode.right);
        break;
      }
      default:
        throw std::runtime_error("Unknown ASTNodeKind in substituteAst");
    }
  }

  /*
   * Solve Constraints
   */
  void solveConstraints(
    ASTNode& node
  ) {
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

    if (lhsType->kind == TypeKind::Double && rhsType->kind == TypeKind::Double) {
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
        throw InfiniteTypeError(*variableType, *type);
      }

      unionFind.setType(variableType->typeVar, type);
      return;
    }

    throw TypeNotEqualError(*lhsType, *rhsType);
  }

  bool hasTypeVar(const std::shared_ptr<Type>& type, TypeVar var) {
    switch (type->kind) {
      case TypeKind::Integer:
      case TypeKind::Double:
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
      case TypeKind::Double:
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

  /*
   * Infer
   */
  std::shared_ptr<Type> infer(
    ASTNode& node
  ) {
    switch (node.kind) {
      case ASTNodeKind::Integer:
        return inferInteger(static_cast<IntegerNode&>(node));
      case ASTNodeKind::Double:
        return inferDouble(static_cast<DoubleNode&>(node));
      case ASTNodeKind::Variable:
        return inferVariable(static_cast<VariableNode&>(node));
      case ASTNodeKind::Function:
        return inferFunction(static_cast<FunctionNode&>(node));
      case ASTNodeKind::Apply:
        return inferApply(static_cast<ApplyNode&>(node));
      case ASTNodeKind::Add:
        return inferAdd(static_cast<AddNode&>(node));
      default:
        throw std::runtime_error("Unknown ASTNodeKind");
    }
  }

  std::shared_ptr<Type> inferInteger(IntegerNode& node) {
    return std::make_shared<IntegerType>();
  }

  std::shared_ptr<Type> inferDouble(DoubleNode& node) {
    return std::make_shared<DoubleType>();
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

  std::shared_ptr<Type> inferAdd(AddNode& node) {
    auto leftType = infer(*node.left);
    auto rightType = infer(*node.right);
    auto constraint = std::make_unique<EqualTypeConstraint>(leftType, rightType);
    this->constraints.push_back(std::move(constraint));
    return leftType;
  }

  TypeVar freshTypeVar() {
    return unionFind.insert(std::nullopt);
  }

  /*
   * Check
   */
  void check(
    ASTNode& node,
    const std::shared_ptr<Type>& type
  ) {
    if (node.kind == ASTNodeKind::Integer && type->kind == TypeKind::Integer) {
      return;
    }

    if (node.kind == ASTNodeKind::Double && type->kind == TypeKind::Double) {
      return;
    }

    if (node.kind == ASTNodeKind::Function && type->kind == TypeKind::Function) {
      auto& functionNode = static_cast<FunctionNode&>(node);
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

  /*
   * Env
   */
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
