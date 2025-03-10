#ifndef AST_H
#define AST_H

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

enum class ASTNodeKind {
  Integer,
  Double,
  Variable,
  Function,
  Apply,
  Add
};

class ASTNode {
public:
  ASTNodeKind kind;

  explicit ASTNode(ASTNodeKind kind) : kind(kind) {}
  virtual ~ASTNode() = default;

  virtual bool operator==(const ASTNode& other) const {
    return kind == other.kind;
  }

  bool operator!=(const ASTNode& other) const = default;
};

class IntegerNode : public ASTNode {
public:
  std::string_view literal;

  explicit IntegerNode(std::string_view literal)
    : ASTNode(ASTNodeKind::Integer), literal(literal) {}

  int getValue() const {
    return std::stoi(std::string(literal));
  }

  bool operator==(const ASTNode& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherInt = static_cast<const IntegerNode&>(other);
    return literal == otherInt.literal;
  }
};

class DoubleNode : public ASTNode {
public:
  std::string_view literal;

  explicit DoubleNode(std::string_view literal)
    : ASTNode(ASTNodeKind::Double), literal(literal) {}

  double getValue() const {
    return std::stod(std::string(literal));
  }

  bool operator==(const ASTNode& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherDouble = static_cast<const DoubleNode&>(other);
    return literal == otherDouble.literal;
  }
};

class VariableNode : public ASTNode {
public:
  Var var;

  explicit VariableNode(const Var &var)
    : ASTNode(ASTNodeKind::Variable),
      var(var) {
  }

  bool operator==(const ASTNode& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherVar = static_cast<const VariableNode&>(other);
    return var == otherVar.var;
  }
};

class FunctionNode : public ASTNode {
public:
  Var arg;
  std::shared_ptr<ASTNode> body;

  FunctionNode(const Var &arg, std::shared_ptr<ASTNode> body)
    : ASTNode(ASTNodeKind::Function),
      arg(arg),
      body(std::move(body)) {
  }

  bool operator==(const ASTNode& other) const override {
    if (kind != other.kind) {
      return false;
    }
    const auto& otherFunc = static_cast<const FunctionNode&>(other);

    // Compare arg
    if (!(arg == otherFunc.arg)) return false;

    // Compare body
    return *body == *otherFunc.body;
  }
};

class ApplyNode : public ASTNode {
public:
  std::shared_ptr<ASTNode> function;
  std::shared_ptr<ASTNode> argument;

  ApplyNode(std::shared_ptr<ASTNode> function, std::shared_ptr<ASTNode> argument)
    : ASTNode(ASTNodeKind::Apply),
      function(std::move(function)),
      argument(std::move(argument)) {
  }

  bool operator==(const ASTNode& other) const override {
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

class AddNode : public ASTNode {
public:
  std::shared_ptr<ASTNode> left;
  std::shared_ptr<ASTNode> right;

  AddNode(std::shared_ptr<ASTNode> left, std::shared_ptr<ASTNode> right)
    : ASTNode(ASTNodeKind::Add),
      left(std::move(left)),
      right(std::move(right)) {
  }

  bool operator==(const ASTNode& other) const override {
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

#endif //AST_H
