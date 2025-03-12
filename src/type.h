#ifndef TYPE_H
#define TYPE_H
#include <iostream>

using TypeVar = uint32_t;

enum class TypeKind {
  Void,
  Integer,
  Double,
  Boolean,
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

class VoidType : public Type {
public:
  explicit VoidType() : Type(TypeKind::Void) {}

  std::string toString() const override {
    return "VoidType";
  }
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
    auto& otherType = static_cast<const VariableType&>(other);
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

class DoubleType : public Type {
public:
  explicit DoubleType() : Type(TypeKind::Double) {}

  std::string toString() const override {
    return "DoubleType";
  }
};

class BoolType : public Type {
public:
  explicit BoolType() : Type(TypeKind::Boolean) {}

  std::string toString() const override {
    return "BoolType";
  }
};

class FunctionType : public Type {
public:
  std::vector<std::shared_ptr<Type>> parameters;
  std::shared_ptr<Type> ret;

  FunctionType(std::vector<std::shared_ptr<Type>> parameters, std::shared_ptr<Type> ret)
    : Type(TypeKind::Function),
      parameters(std::move(parameters)),
      ret(std::move(ret)) {
  }

  bool operator==(const Type &other) const override {
    if (other.kind != TypeKind::Function) {
      return false;
    }
    auto& otherType = static_cast<const FunctionType&>(other);

    if (parameters.size() != otherType.parameters.size()) {
      return false;
    }

    for (size_t i = 0; i < parameters.size(); i++) {
      if (*parameters[i] != *(otherType.parameters[i])) {
        return false;
      }
    }

    return *ret == *(otherType.ret);
  }

  std::string toString() const override {
    std::string result = "FunctionType(";

    for (size_t i = 0; i < parameters.size(); i++) {
      if (i > 0) {
        result += ", ";
      }
      result += parameters[i]->toString();
    }

    result += " -> " + ret->toString() + ")";
    return result;
  }
};

#endif //TYPE_H
