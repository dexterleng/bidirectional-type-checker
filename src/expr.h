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
  Variable,
  Apply,
  Add
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

class IntegerNode : public Expr {
public:
  std::string_view literal;

  explicit IntegerNode(std::string_view literal)
    : Expr(ExprKind::Integer), literal(literal) {}

  int getValue() const {
    return std::stoi(std::string(literal));
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherInt = static_cast<const IntegerNode&>(other);
    return literal == otherInt.literal;
  }
};

class DoubleNode : public Expr {
public:
  std::string_view literal;

  explicit DoubleNode(std::string_view literal)
    : Expr(ExprKind::Double), literal(literal) {}

  double getValue() const {
    return std::stod(std::string(literal));
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherDouble = static_cast<const DoubleNode&>(other);
    return literal == otherDouble.literal;
  }
};

class VariableNode : public Expr {
public:
  Var var;

  explicit VariableNode(const Var &var)
    : Expr(ExprKind::Variable),
      var(var) {
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherVar = static_cast<const VariableNode&>(other);
    return var == otherVar.var;
  }
};

class ApplyNode : public Expr {
public:
  std::shared_ptr<Expr> function;
  std::shared_ptr<Expr> argument;

  ApplyNode(std::shared_ptr<Expr> function, std::shared_ptr<Expr> argument)
    : Expr(ExprKind::Apply),
      function(std::move(function)),
      argument(std::move(argument)) {
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherApply = static_cast<const ApplyNode&>(other);

    // Compare function and argument
    auto funcEqual = *function == *otherApply.function;
    auto argEqual = *argument == *otherApply.argument;
    return funcEqual && argEqual;
  }
};

class AddNode : public Expr {
public:
  std::shared_ptr<Expr> left;
  std::shared_ptr<Expr> right;

  AddNode(std::shared_ptr<Expr> left, std::shared_ptr<Expr> right)
    : Expr(ExprKind::Add),
      left(std::move(left)),
      right(std::move(right)) {
  }

  bool operator==(const Expr& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherAdd = static_cast<const AddNode&>(other);

    // Compare left and right operands
    auto leftEqual = *left == *otherAdd.left;
    auto rightEqual = *right == *otherAdd.right;
    return leftEqual && rightEqual;
  }
};

#endif //EXPR_H
