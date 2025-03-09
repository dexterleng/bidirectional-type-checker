#ifndef AST_H
#define AST_H
#include <utility>

#include "type.h"

using Var = uint32_t;

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
