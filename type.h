#ifndef TYPE_H
#define TYPE_H
#include <iostream>

using TypeVar = uint32_t;

enum class TypeKind {
  Integer,
  Variable,
  Function,
};

class Type {
public:
  TypeKind kind;

  explicit Type(TypeKind kind) : kind(kind) {}
  virtual ~Type() = default;

  virtual void print() const = 0;
};

class VariableType : public Type {
public:
  TypeVar typeVar;

  explicit VariableType(TypeVar typeVar)
    : Type(TypeKind::Variable), typeVar(typeVar) {}

  void print() const override {
    std::cout << "VariableType: " << typeVar << std::endl;
  }
};

class IntegerType : public Type {
public:
  explicit IntegerType() : Type(TypeKind::Integer) {}

  void print() const override {
    std::cout << "IntegerType" << std::endl;
  }
};

class FunctionType : public Type {
public:
  std::shared_ptr<Type> from;
  std::shared_ptr<Type> to;

  FunctionType(std::shared_ptr<Type> from, std::shared_ptr<Type> to)
    : Type(TypeKind::Function),
      from(std::move(from)),
      to(std::move(to)) {
  }

  void print() const override {
    std::cout << "FunctionType: from -> to" << std::endl;
    from->print();
    to->print();
  }
};

#endif //TYPE_H
