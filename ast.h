#ifndef AST_H
#define AST_H
#include "type.h"

using Var = uint32_t;

class TypedVar {
public:
  Var var;
  Type& type;

  TypedVar(Var var, Type &type)
    : var(var),
      type(type) {
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
  V var;

  Variable(const V &var)
    : ASTNode<V>(ASTNodeKind::Variable),
      var(var) {
  }
};

template <typename V>
class Function : public ASTNode<V> {
public:
  std::string_view name;
  ASTNode<V>& body;

  Function(const std::string_view &name, ASTNode<V> &body)
    : ASTNode<V>(ASTNodeKind::Function),
      name(name),
      body(body) {
  }
};

template <typename V>
class Apply : public ASTNode<V> {
public:
  ASTNode<V>& function;
  ASTNode<V>& argument;

  Apply(ASTNode<V> &function, ASTNode<V> &argument)
    : ASTNode<V>(ASTNodeKind::Apply),
      function(function),
      argument(argument) {
  }
};

#endif //AST_H
