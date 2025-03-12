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

  void perform(Stmt& stmt) {
    infer(stmt);
    solveConstraints();
    substituteAst(stmt);
  }

  // remove?
  void perform(Expr& expr) {
    auto type = infer(expr);
    solveConstraints();
    substitute(type);
    substituteAst(expr);
  }

private:
  std::shared_ptr<Type> substitute(std::shared_ptr<Type> ty) {
    switch (ty->kind) {
      case TypeKind::Void:
      case TypeKind::Integer:
      case TypeKind::Double:
      case TypeKind::Boolean: {
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
        auto functionType = static_pointer_cast<FunctionType>(ty);

        std::vector<std::shared_ptr<Type>> paramTypes;
        for (const auto & parameter : functionType->parameters) {
          paramTypes.push_back(substitute(parameter));
        }
        auto retType = substitute(functionType->ret);
        return std::make_shared<FunctionType>(paramTypes, retType);
      }
      default:
        throw std::runtime_error("Unhandled TypeKind");
    }
  }

  void substituteAst(Expr& expr) {
    switch (expr.kind) {
      case ExprKind::Integer:
      case ExprKind::Double:
      case ExprKind::Boolean: {
        break;
      }
      case ExprKind::Variable: {
        auto& varExpr = static_cast<VariableExpr&>(expr);
        auto substitutedType = substitute(varExpr.var.type.value());
        varExpr.var.type = substitutedType;
        break;
      }
      case ExprKind::Apply: {
        auto& applyExpr = static_cast<ApplyExpr&>(expr);
        substituteAst(*applyExpr.function);
        for (auto& arg : applyExpr.arguments) {
          substituteAst(*arg);
        }
        break;
      }
      case ExprKind::Binary: {
        auto& binaryExpr = static_cast<BinaryExpr&>(expr);
        substituteAst(*binaryExpr.left);
        substituteAst(*binaryExpr.right);
        break;
      }
      case ExprKind::Unary: {
        auto& unaryExpr = static_cast<UnaryExpr&>(expr);
        substituteAst(*unaryExpr.operand);
        break;
      }
      default:
        throw std::runtime_error("Unknown ExprKind");
    }
  }

  void substituteAst(Stmt& stmt) {
    switch (stmt.kind) {
      case StmtKind::Block: {
        auto& block = static_cast<BlockStmt&>(stmt);
        for (auto& stmt : block.statements) {
          substituteAst(*stmt);
        }
        break;
      }
      case StmtKind::Declare: {
        auto& declStmt = static_cast<DeclareStmt&>(stmt);
        auto varType = substitute(declStmt.var.type.value());
        declStmt.var.type = varType;
        break;
      }
      case StmtKind::Assign: {
        auto& assignStmt = static_cast<AssignStmt&>(stmt);
        auto varType = substitute(assignStmt.var.type.value());
        assignStmt.var.type = varType;
        break;
      }
      case StmtKind::Function: {
        auto& funStmt = static_cast<FunctionStmt&>(stmt);
        for (auto& param : funStmt.params) {
          auto paramType = substitute(param.type.value());
          param.type = paramType;
        }
        substituteAst(*funStmt.body);
        break;
      }
      case StmtKind::Return: {
        auto& returnStmt = static_cast<ReturnStmt&>(stmt);
        substituteAst(*returnStmt.expression);
        break;
      }
      case StmtKind::If: {
        auto& ifStmt = static_cast<IfStmt&>(stmt);
        substituteAst(*ifStmt.condition);
        substituteAst(*ifStmt.thenBranch);
        if (ifStmt.elseBranch.has_value()) {
          substituteAst(**ifStmt.elseBranch);
        }
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

    if (lhsType->kind == TypeKind::Boolean && rhsType->kind == TypeKind::Boolean) {
      return;
    }

    if (lhsType->kind == TypeKind::Function && rhsType->kind == TypeKind::Function) {
      auto lhsFunctionType = static_pointer_cast<FunctionType>(lhsType);
      auto rhsFunctionType = static_pointer_cast<FunctionType>(rhsType);
      if (lhsFunctionType->parameters.size() != rhsFunctionType->parameters.size()) {
        throw TypeNotEqualError(*lhsType, *rhsType);
      }
      for (size_t i = 0; i < lhsFunctionType->parameters.size(); i++) {
        _solveEqualTypeConstraint(lhsFunctionType->parameters[i], rhsFunctionType->parameters[i]);
      }
      _solveEqualTypeConstraint(lhsFunctionType->ret, rhsFunctionType->ret);
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
      case TypeKind::Boolean:
        return false;
      case TypeKind::Variable: {
        auto varType = static_pointer_cast<VariableType>(type);
        return varType->typeVar == var;
      }
      case TypeKind::Function: {
        auto functionType = static_pointer_cast<FunctionType>(type);
        for (const auto & parameter : functionType->parameters) {
          if (hasTypeVar(parameter, var)) {
            return true;
          }
        }
        return hasTypeVar(functionType->ret, var);
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
      case TypeKind::Boolean:
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
        auto functionType = static_pointer_cast<FunctionType>(_type);
        std::vector<std::shared_ptr<Type>> paramTypes;
        for (auto& param : functionType->parameters) {
          auto resolvedParam = normalizeType(param);
          paramTypes.push_back(resolvedParam);
        }
        auto returnType = normalizeType(functionType->ret);
        return std::make_shared<FunctionType>(paramTypes, returnType);
      }
      default:
        throw std::runtime_error("Unknown TypeKind");
    }
  }

  /*
   * Infer
   */
  std::shared_ptr<Type> infer(
    Expr& expr
  ) {
    switch (expr.kind) {
      case ExprKind::Integer: {
        return std::make_shared<IntegerType>();
      }
      case ExprKind::Double: {
        return std::make_shared<DoubleType>();
      }
      case ExprKind::Boolean: {
        return std::make_shared<BoolType>();
      }
      case ExprKind::Variable: {
        auto& varExpr = static_cast<VariableExpr&>(expr);
        auto type = lookup(varExpr.var);
        varExpr.var.type = type;
        return type;
      }
      case ExprKind::Apply: {
        auto& applyExpr = static_cast<ApplyExpr&>(expr);

        std::vector<std::shared_ptr<Type>> argumentTypes;
        for (auto& arg : applyExpr.arguments) {
          auto argType = infer(*arg);
          argumentTypes.push_back(argType);
        }
        auto returnType = std::make_shared<VariableType>(freshTypeVar());
        auto expectedFunctionType = std::make_shared<FunctionType>(argumentTypes, returnType);

        check(*applyExpr.function, expectedFunctionType);
        return returnType;
      }
      case ExprKind::Binary: {
        auto& binaryExpr = static_cast<BinaryExpr&>(expr);
        auto leftType = infer(*binaryExpr.left);
        auto rightType = infer(*binaryExpr.right);
        switch (binaryExpr.op) {
          case BinaryOperator::Add:
          case BinaryOperator::Minus: {
            if (leftType->kind == TypeKind::Integer && rightType->kind == TypeKind::Integer) {
              return leftType;
            }
            if (leftType->kind == TypeKind::Double && rightType->kind == TypeKind::Double) {
              return leftType;
            }
            throw std::runtime_error("Invalid binary operand types");
          }
          case BinaryOperator::And:
          case BinaryOperator::Or:
            if (leftType->kind == TypeKind::Boolean && rightType->kind == TypeKind::Boolean) {
              return leftType;
            }
            throw std::runtime_error("Invalid binary operand types");
          default:
            throw std::runtime_error("Unknown BinaryOperator");
        }
      }
      case ExprKind::Unary: {
        auto& unary = static_cast<UnaryExpr&>(expr);
        auto operandType = infer(*unary.operand);
        switch (unary.op) {
          case UnaryOperator::Not: {
            if (operandType->kind == TypeKind::Boolean) {
              return operandType;
            }
            throw std::runtime_error("Invalid unary operand type");
          }
          case UnaryOperator::Negate: {
            if (operandType->kind == TypeKind::Integer || operandType->kind == TypeKind::Double) {
              return operandType;
            }
            throw std::runtime_error("Invalid unary operand type");
          }
          default:
            throw std::runtime_error("Unknown UnaryOperator");
        }
      }
      default:
        throw std::runtime_error("Unknown ExprKind");
    }
  }

  using FallsThrough = bool;
  FallsThrough infer(Stmt& stmt) {
    switch (stmt.kind) {
      case StmtKind::Block: {
        auto& block = static_cast<BlockStmt&>(stmt);
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
      case StmtKind::Declare: {
        auto& declStmt = static_cast<DeclareStmt&>(stmt);
        declare(declStmt.var);
        auto exprType = infer(*declStmt.expression);
        define(declStmt.var, exprType);
        declStmt.var.type = exprType;
        return true;
      }
      case StmtKind::Assign: {
        auto& assignStmt = static_cast<AssignStmt&>(stmt);
        auto varType = lookup(assignStmt.var);
        check(*assignStmt.expression, varType);
        assignStmt.var.type = varType;
        return true;
      }
      case StmtKind::Function: {
        auto& funStmt = static_cast<FunctionStmt&>(stmt);

        // declare and define first to allow recursion.
        declare(funStmt.name);

        std::vector<std::shared_ptr<Type>> paramTypes;
        paramTypes.reserve(funStmt.params.size());
        for (const auto& param : funStmt.params) {
          paramTypes.push_back(param.type.value());
        }
        auto functionType = std::make_shared<FunctionType>(paramTypes, funStmt.returnType);
        define(funStmt.name, functionType);

        beginScope();
        auto prevEnclosingFunction = enclosingFunction;
        enclosingFunction = &funStmt;

        for (auto& param : funStmt.params) {
          declare(param);
          define(param, param.type.value());
        }

        // All return statements in the body will be checked against the enclosing function's return type
        auto bodyFallsThrough = infer(*funStmt.body);
        // If the function body falls through, it must be a Void return type.
        if (bodyFallsThrough && funStmt.returnType->kind != TypeKind::Void) {
          throw TypeNotEqualError(*funStmt.returnType, VoidType());
        }

        endScope();
        enclosingFunction = prevEnclosingFunction;

        return false;
      }
      case StmtKind::Return: {
        auto& returnStmt = static_cast<ReturnStmt&>(stmt);
        check(*returnStmt.expression, enclosingFunction->returnType);
        return false;
      }
      case StmtKind::If: {
        auto& ifStmt = static_cast<IfStmt&>(stmt);
        auto conditionType = infer(*ifStmt.condition);
        if (conditionType->kind != TypeKind::Boolean) {
          throw std::runtime_error("If condition must be a boolean");
        }
        auto thenFallsThrough = infer(*ifStmt.thenBranch);
        // If there's no else branch, the if statement always falls through
        if (!ifStmt.elseBranch.has_value()) {
          return true;
        }
        auto elseFallsThrough = infer(**ifStmt.elseBranch);
        // The if-else falls through if any branch can fall through
        return thenFallsThrough || elseFallsThrough;
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
    Expr& expr,
    const std::shared_ptr<Type>& type
  ) {
    if (expr.kind == ExprKind::Integer && type->kind == TypeKind::Integer) {
      return;
    }

    if (expr.kind == ExprKind::Double && type->kind == TypeKind::Double) {
      return;
    }

    if (expr.kind == ExprKind::Boolean && type->kind == TypeKind::Boolean) {
      return;
    }

    auto inferredType = infer(expr);
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
