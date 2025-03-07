#ifndef AST_H
#define AST_H
#include <utility>

#include "type.h"

using Var = uint32_t;

class TypedVar {
public:
  Var var;
  std::shared_ptr<Type> type;

  TypedVar(Var var, std::shared_ptr<Type> type)
    : var(var),
      type(std::move(type)) {
  }
};

enum class ASTNodeKind {
  Integer,
  Variable,
  Function,
  Apply
};

template <typename V>
class ASTNode {
public:
  ASTNodeKind kind;

  explicit ASTNode(ASTNodeKind kind) : kind(kind) {}
  virtual ~ASTNode() = default;
};

template <typename V>
class IntegerNode : public ASTNode<V> {
public:
  std::string_view literal;

  explicit IntegerNode(std::string_view literal)
    : ASTNode<V>(ASTNodeKind::Integer), literal(literal) {}

  int getValue() const {
    return std::stoi(std::string(literal));
  }
};

template <typename V>
class VariableNode : public ASTNode<V> {
public:
  V var;

  explicit VariableNode(const V &var)
    : ASTNode<V>(ASTNodeKind::Variable),
      var(var) {
  }
};

template <typename V>
class FunctionNode : public ASTNode<V> {
public:
  V arg;
  std::shared_ptr<ASTNode<V>> body;

  FunctionNode(const V &arg, std::shared_ptr<ASTNode<V>> body)
    : ASTNode<V>(ASTNodeKind::Function),
      arg(arg),
      body(std::move(body)) {
  }
};

template <typename V>
class ApplyNode : public ASTNode<V> {
public:
  std::shared_ptr<ASTNode<V>> function;
  std::shared_ptr<ASTNode<V>> argument;

  ApplyNode(std::shared_ptr<ASTNode<V>> function, std::shared_ptr<ASTNode<V>> argument)
    : ASTNode<V>(ASTNodeKind::Apply),
      function(std::move(function)),
      argument(std::move(argument)) {
  }
};

#endif //AST_H
