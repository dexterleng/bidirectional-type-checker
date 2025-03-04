#ifndef AST_H
#define AST_H
#include "type.h"

using Var = uint32_t;

class TypedVar {
public:
  Var var;
  std::unique_ptr<Type> type;

  TypedVar(Var var, std::unique_ptr<Type> type)
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
class Integer : public ASTNode<V> {
public:
  std::string_view literal;

  explicit Integer(std::string_view literal)
  : ASTNode<V>(ASTNodeKind::Integer), literal(literal) {}

  int getValue() const {
    return std::stoi(std::string(literal));
  }
};

template <typename V>
class Variable : public ASTNode<V> {
public:
  std::unique_ptr<V> var;

  explicit Variable(std::unique_ptr<V> var)
  : ASTNode<V>(ASTNodeKind::Variable), var(std::move(var)) {}
};

template <typename V>
class Function : public ASTNode<V> {
public:
  std::string_view name;
  std::unique_ptr<ASTNode<V>> body;

  explicit Function(std::string_view name, std::unique_ptr<ASTNode<V>> body)
    : ASTNode<V>(ASTNodeKind::Function), name(name), body(std::move(body)) {}
};

template <typename V>
class Apply : public ASTNode<V> {
public:
  std::unique_ptr<ASTNode<V>> function;
  std::unique_ptr<ASTNode<V>> argument;

  explicit Apply(std::unique_ptr<ASTNode<V>> function, std::unique_ptr<ASTNode<V>> argument)
    : ASTNode<V>(ASTNodeKind::Apply), function(std::move(function)), argument(std::move(argument)) {}
};

#endif //AST_H
