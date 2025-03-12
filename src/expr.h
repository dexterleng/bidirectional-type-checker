#ifndef EXPR_H
#define EXPR_H

#include "type.h"

using VariableName = uint32_t;

struct Var {
  VariableName name;
  std::optional<std::shared_ptr<Type>> type;

  explicit Var(VariableName name)
    : name(name), type(std::nullopt) {}

  Var(VariableName name, std::optional<std::shared_ptr<Type>> type)
    : name(name), type(std::move(type)) {}

  bool operator==(const Var& other) const {
    if (name != other.name) return false;

    // If both types are nullopt, they're equal
    if (!type.has_value() && !other.type.has_value()) return true;

    // If one has a value and the other doesn't, they're not equal
    if (type.has_value() != other.type.has_value()) return false;

    // Both have values, compare the pointed-to Types
    return *type.value() == *other.type.value();
  }

  bool operator!=(const Var &other) const = default;
};

enum class ExprKind {
  Integer,
  Double,
  Boolean,
  Variable,
  Apply,
  Binary
};

class Expr {
public:
  ExprKind kind;

  explicit Expr(ExprKind kind) : kind(kind) {}
  virtual ~Expr() = default;

  virtual bool operator==(const Expr& other) const {
    return kind == other.kind;
  }

  bool operator!=(const Expr& other) const = default;
};

class IntegerExpr : public Expr {
public:
  std::string_view literal;

  explicit IntegerExpr(std::string_view literal)
    : Expr(ExprKind::Integer), literal(literal) {}

  int getValue() const {
    return std::stoi(std::string(literal));
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherInt = static_cast<const IntegerExpr&>(other);
    return literal == otherInt.literal;
  }
};

class DoubleExpr : public Expr {
public:
  std::string_view literal;

  explicit DoubleExpr(std::string_view literal)
    : Expr(ExprKind::Double), literal(literal) {}

  double getValue() const {
    return std::stod(std::string(literal));
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherDouble = static_cast<const DoubleExpr&>(other);
    return literal == otherDouble.literal;
  }
};

class BoolExpr : public Expr {
public:
  bool literal;

  explicit BoolExpr(bool literal)
    : Expr(ExprKind::Boolean), literal(literal) {}

  bool getValue() const {
    return literal;
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherBool = static_cast<const BoolExpr&>(other);
    return literal == otherBool.literal;
  }
};

class VariableExpr : public Expr {
public:
  Var var;

  explicit VariableExpr(const Var &var)
    : Expr(ExprKind::Variable),
      var(var) {
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherVar = static_cast<const VariableExpr&>(other);
    return var == otherVar.var;
  }
};

class ApplyExpr : public Expr {
public:
  std::shared_ptr<Expr> function;
  std::shared_ptr<Expr> argument;

  ApplyExpr(std::shared_ptr<Expr> function, std::shared_ptr<Expr> argument)
    : Expr(ExprKind::Apply),
      function(std::move(function)),
      argument(std::move(argument)) {
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherApply = static_cast<const ApplyExpr&>(other);

    // Compare function and argument
    auto funcEqual = *function == *otherApply.function;
    auto argEqual = *argument == *otherApply.argument;
    return funcEqual && argEqual;
  }
};

enum class BinaryOperator {
  Add,
  Minus,
  And,
  Or,
};

inline std::string toString(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::Add: return "+";
    case BinaryOperator::Minus: return "-";
    case BinaryOperator::And: return "&&";
    case BinaryOperator::Or: return "||";
    default: return "Unknown BinaryOperator";
  }
}

class BinaryExpr : public Expr {
public:
  std::shared_ptr<Expr> left;
  BinaryOperator op;
  std::shared_ptr<Expr> right;

  BinaryExpr(std::shared_ptr<Expr> left, BinaryOperator op, std::shared_ptr<Expr> right)
    : Expr(ExprKind::Binary),
      left(std::move(left)),
      op(op),
      right(std::move(right)) {
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherBinary = static_cast<const BinaryExpr&>(other);

    // Compare left and right operands
    auto leftEqual = *left == *otherBinary.left;
    auto rightEqual = *right == *otherBinary.right;
    return leftEqual && rightEqual;
  }
};

#endif //EXPR_H
