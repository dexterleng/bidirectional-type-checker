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

  virtual bool operator==(const Type &other) const {
    return kind == other.kind;
  }
  bool operator!=(const Type &other) const = default;

  virtual std::string toString() const = 0;
};

class VariableType : public Type {
public:
  TypeVar typeVar;

  explicit VariableType(TypeVar typeVar)
    : Type(TypeKind::Variable), typeVar(typeVar) {}

  bool operator==(const Type &other) const override {
    if (kind != other.kind) {
      return false;
    }
    auto otherType = static_cast<const VariableType&>(other);
    return typeVar == otherType.typeVar;
  }

  std::string toString() const override {
    return "VariableType(" + std::to_string(typeVar) + ")";
  }
};

class IntegerType : public Type {
public:
  explicit IntegerType() : Type(TypeKind::Integer) {}

  std::string toString() const override {
    return "IntegerType";
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

  bool operator==(const Type &other) const override {
    if (other.kind != TypeKind::Function) {
      return false;
    }
    auto otherType = static_cast<const FunctionType&>(other);
    auto fromEqual = *from == *(otherType.from);
    auto toEqual = *to == *(otherType.to);
    return fromEqual && toEqual;
  }

  std::string toString() const override {
    return "FunctionType(" + from->toString() + " -> " + to->toString() + ")";
  }
};

#endif //TYPE_H
