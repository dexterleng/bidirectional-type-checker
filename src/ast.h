#ifndef AST_H
#define AST_H

#include "type.h"

using VariableName = uint32_t;

struct Var {
  VariableName name;
  std::optional<std::shared_ptr<Type>> type;

  explicit Var(VariableName name)
    : name(name), type(std::nullopt) {}
};

enum class ASTNodeKind {
  Integer,
  Variable,
  Function,
  Apply
};

class ASTNode {
public:
  ASTNodeKind kind;

  explicit ASTNode(ASTNodeKind kind) : kind(kind) {}
  virtual ~ASTNode() = default;
};

class IntegerNode : public ASTNode {
public:
  std::string_view literal;

  explicit IntegerNode(std::string_view literal)
    : ASTNode(ASTNodeKind::Integer), literal(literal) {}

  int getValue() const {
    return std::stoi(std::string(literal));
  }
};

class VariableNode : public ASTNode {
public:
  Var var;

  explicit VariableNode(const Var &var)
    : ASTNode(ASTNodeKind::Variable),
      var(var) {
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
};

#endif //AST_H
