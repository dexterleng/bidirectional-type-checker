#ifndef STMT_H
#define STMT_H
#include <utility>

#include "expr.h"

// Enum for statement nodes
enum class StmtKind {
  Block,
  Assign,
  Function,
  Return
};

// Base class for statements
class Stmt {
public:
  StmtKind kind;

  explicit Stmt(StmtKind kind) : kind(kind) {}
  virtual ~Stmt() = default;

  virtual bool operator==(const Stmt& other) const {
    return kind == other.kind;
  }

  bool operator!=(const Stmt& other) const = default;
};

// Block statement containing a list of statements
class BlockStmt : public Stmt {
public:
  std::vector<std::shared_ptr<Stmt>> statements;

  explicit BlockStmt(std::vector<std::shared_ptr<Stmt>> statements)
    : Stmt(StmtKind::Block),
      statements(std::move(statements)) {}

  bool operator==(const Stmt& other) const override {
    if (kind != other.kind) {
      return false;
    }

    const auto& otherBlock = static_cast<const BlockStmt&>(other);

    // Check if both blocks have the same number of statements
    if (statements.size() != otherBlock.statements.size()) {
      return false;
    }

    // Compare each statement
    for (size_t i = 0; i < statements.size(); i++) {
      if (*statements[i] != *otherBlock.statements[i]) {
        return false;
      }
    }

    return true;
  }
};

// Assignment statement (var = expr)
class AssignStmt : public Stmt {
public:
  Var var;
  std::shared_ptr<Expr> expression;

  AssignStmt(Var  var, std::shared_ptr<Expr> expression)
    : Stmt(StmtKind::Assign),
      var(std::move(var)),
      expression(std::move(expression)) {}

  bool operator==(const Stmt& other) const override {
    if (kind != other.kind) {
      return false;
    }

    const auto& otherAssign = static_cast<const AssignStmt&>(other);

    // Compare variable and expression
    return var == otherAssign.var && *expression == *otherAssign.expression;
  }
};

class FunctionStmt : public Stmt {
public:
  Var name;
  std::vector<Var> params;
  std::shared_ptr<Type> returnType;
  std::vector<std::shared_ptr<Stmt>> body;

  FunctionStmt(Var name, std::vector<Var> params, std::shared_ptr<Type> returnType, std::vector<std::shared_ptr<Stmt>> body)
    : Stmt(StmtKind::Function),
      name(std::move(name)),
      params(std::move(params)),
      returnType(std::move(returnType)),
      body(std::move(body)) {}

  bool operator==(const Stmt& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherFunc = static_cast<const FunctionStmt&>(other);

    if (name != otherFunc.name) return false;
    if (params != otherFunc.params) return false;

    if (body.size() != otherFunc.body.size()) {
      return false;
    }

    for (size_t i = 0; i < body.size(); i++) {
      if (*body[i] != *otherFunc.body[i]) {
        return false;
      }
    }

    if (*returnType != *otherFunc.returnType) return false;

    return true;
  }
};

class ReturnStmt : public Stmt {
public:
  std::shared_ptr<Expr> expression;

  explicit ReturnStmt(std::shared_ptr<Expr> expression)
    : Stmt(StmtKind::Return), expression(std::move(expression)) {}

  bool operator==(const Stmt& other) const override {
    if (kind != other.kind) {
      return false;
    }

    const auto& otherReturn = static_cast<const ReturnStmt&>(other);
    return *expression == *otherReturn.expression;
  }
};

#endif //STMT_H
