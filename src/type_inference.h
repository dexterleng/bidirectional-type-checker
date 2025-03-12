#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include <ranges>
#include <set>
#include <vector>
#include "expr.h"
#include "stmt.h"
#include "type_constraint.h"
#include "union_find.h"

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
  // nil value represents declared but not defined. used to prevent referencing a variable in the same assignment statement.
  std::vector<std::unordered_map<VariableName, std::optional<std::shared_ptr<Type>>>> envs;
  std::vector<std::unique_ptr<TypeConstraint>> constraints;
  std::set<TypeVar> unbounded;
  // to check return type
  FunctionStmt* enclosingFunction = nullptr;
  UnionFind unionFind;

  TypeInference() = default;

  void perform(Stmt& node) {
    infer(node);
    solveConstraints();
    substituteAst(node);
  }

  // remove?
  void perform(Expr& node) {
    auto type = infer(node);
    solveConstraints();
    substitute(type);
    substituteAst(node);
  }

private:
  std::shared_ptr<Type> substitute(std::shared_ptr<Type> ty) {
    switch (ty->kind) {
      case TypeKind::Void:
      case TypeKind::Integer:
      case TypeKind::Double: {
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
        auto funType = static_pointer_cast<FunctionType>(ty);
        auto argType = substitute(funType->from);
        auto retType = substitute(funType->to);
        return std::make_shared<FunctionType>(argType, retType);
      }
      default:
        throw std::runtime_error("Unhandled TypeKind");
    }
  }

  void substituteAst(Expr& node) {
    switch (node.kind) {
      case ExprKind::Integer:
      case ExprKind::Double: {
        break;
      }
      case ExprKind::Variable: {
        auto& varNode = static_cast<VariableNode&>(node);
        auto substitutedType = substitute(varNode.var.type.value());
        varNode.var.type = substitutedType;
        break;
      }
      case ExprKind::Apply: {
        auto& applyNode = static_cast<ApplyNode&>(node);
        substituteAst(*applyNode.function);
        substituteAst(*applyNode.argument);
        break;
      }
      case ExprKind::Add: {
        auto& addNode = static_cast<AddNode&>(node);
        substituteAst(*addNode.left);
        substituteAst(*addNode.right);
        break;
      }
      default:
        throw std::runtime_error("Unknown ASTNodeKind in substituteAst");
    }
  }

  void substituteAst(Stmt& node) {
    switch (node.kind) {
      case StmtKind::Block: {
        auto& block = static_cast<BlockStmt&>(node);
        for (auto& stmt : block.statements) {
          substituteAst(*stmt);
        }
        break;
      }
      case StmtKind::Assign: {
        auto& assignStmt = static_cast<AssignStmt&>(node);
        auto argType = substitute(assignStmt.var.type.value());
        assignStmt.var.type = argType;
        break;
      }
      case StmtKind::Function: {
        auto& funStmt = static_cast<FunctionStmt&>(node);
        for (auto& param : funStmt.params) {
          auto paramType = substitute(param.type.value());
          param.type = paramType;
        }
        substituteAst(*funStmt.body);
        break;
      }
      case StmtKind::Return: {
        auto& returnStmt = static_cast<ReturnStmt&>(node);
        substituteAst(*returnStmt.expression);
        break;
      }
      default:
        throw std::runtime_error("Unknown StmtKind");
    }
  }

  /*
   * Solve Constraints
   */
  void solveConstraints() {
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
    if (lhsType->kind == TypeKind::Void && rhsType->kind == TypeKind::Void) {
      return;
    }

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
      case TypeKind::Void:
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
      case TypeKind::Void:
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
    Expr& node
  ) {
    switch (node.kind) {
      case ExprKind::Integer: {
        return std::make_shared<IntegerType>();
      }
      case ExprKind::Double: {
        return std::make_shared<DoubleType>();
      }
      case ExprKind::Variable: {
        auto& varNode = static_cast<VariableNode&>(node);
        auto type = lookup(varNode.var);
        varNode.var.type = type;
        return type;
      }
      case ExprKind::Apply: {
        auto& applyNode = static_cast<ApplyNode&>(node);
        // construct a function type to check against the real function
        auto argType = infer(*applyNode.argument);
        auto returnType = std::make_shared<VariableType>(freshTypeVar());
        auto functionType = std::make_shared<FunctionType>(argType, returnType);
        check(*applyNode.function, functionType);
        return returnType;
      }
      case ExprKind::Add: {
        auto& addNode = static_cast<AddNode&>(node);
        auto leftType = infer(*addNode.left);
        auto rightType = infer(*addNode.right);
        auto constraint = std::make_unique<EqualTypeConstraint>(leftType, rightType);
        this->constraints.push_back(std::move(constraint));
        return leftType;
      }
      default:
        throw std::runtime_error("Unknown ASTNodeKind");
    }
  }

  using FallsThrough = bool;
  FallsThrough infer(Stmt& node) {
    switch (node.kind) {
      case StmtKind::Block: {
        auto& block = static_cast<BlockStmt&>(node);
        beginScope();
        auto fallsThrough = true;
        for (auto& stmt : block.statements) {
          auto stmtFallsThrough = infer(*stmt);
          if (!stmtFallsThrough) {
            fallsThrough = false;
          }
        }
        endScope();
        return fallsThrough;
      }
      case StmtKind::Assign: {
        auto& assignStmt = static_cast<AssignStmt&>(node);
        declare(assignStmt.var);
        auto exprType = infer(*assignStmt.expression);
        define(assignStmt.var, exprType);
        assignStmt.var.type = exprType;
        return true;
      }
      case StmtKind::Function: {
        auto& funStmt = static_cast<FunctionStmt&>(node);

        beginScope();
        auto prevEnclosingFunction = enclosingFunction;
        enclosingFunction = &funStmt;

        for (auto& param : funStmt.params) {
          declare(param);
          define(param, param.type.value());
        }

        // Check if function falls through without returning
        auto bodyFallsThrough = infer(*funStmt.body);
        if (bodyFallsThrough && funStmt.returnType->kind != TypeKind::Void) {
          throw TypeNotEqualError(*funStmt.returnType, VoidType());
        }

        endScope();
        enclosingFunction = prevEnclosingFunction;

        return false;
      }
      case StmtKind::Return: {
        auto& returnStmt = static_cast<ReturnStmt&>(node);
        check(*returnStmt.expression, enclosingFunction->returnType);
        return false;
      }
      default:
        throw std::runtime_error("Unknown StmtKind");
    }
  }

  TypeVar freshTypeVar() {
    return unionFind.insert(std::nullopt);
  }

  /*
   * Check
   */
  void check(
    Expr& node,
    const std::shared_ptr<Type>& type
  ) {
    if (node.kind == ExprKind::Integer && type->kind == TypeKind::Integer) {
      return;
    }

    if (node.kind == ExprKind::Double && type->kind == TypeKind::Double) {
      return;
    }

    // if (node.kind == ExprKind::Function && type->kind == TypeKind::Function) {
    //   auto& functionNode = static_cast<FunctionNode&>(node);
    //   auto functionType = static_pointer_cast<FunctionType>(type);
    //
    //   beginScope();
    //   declare(functionNode.arg);
    //   define(functionNode.arg, functionType->from);
    //   check(*functionNode.body, functionType->to);
    //   endScope();
    //
    //   functionNode.arg.type = functionType->from;
    //   return;
    // }

    auto inferredType = infer(node);
    auto constraint = std::make_unique<EqualTypeConstraint>(type, inferredType);
    this->constraints.push_back(std::move(constraint));
  }

  /*
   * Env
   */
  void beginScope() {
    envs.emplace_back();
  }

  void endScope() {
    envs.pop_back();
  }

  void declare(const Var& var) {
    auto& env = envs.back();
    if (env.contains(var.name)) {
      throw std::runtime_error("Variable " + std::to_string(var.name) + " is already defined in this scope.");
    }
    env[var.name] = std::nullopt;
  }

  void define(const Var& var, std::shared_ptr<Type> type) {
    auto& env = envs.back();
    env[var.name] = std::move(type);
  }

  std::shared_ptr<Type> lookup(const Var& var) {
    for (auto & env : std::ranges::reverse_view(envs)) {
      auto found = env.find(var.name);
      if (found != env.end()) {
        auto typeOpt = found->second;
        if (typeOpt.has_value()) {
          return typeOpt.value();
        }
        throw std::runtime_error("Cannot read variable while it is being defined.");
      }
    }
    throw std::runtime_error("Variable " + std::to_string(var.name) + " not found in any scope.");
  }
};

#endif //TYPE_INFERENCE_H
