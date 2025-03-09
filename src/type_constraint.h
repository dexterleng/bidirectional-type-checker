#ifndef TYPE_CONSTRAINT_H
#define TYPE_CONSTRAINT_H
#include <iostream>
#include "type.h"

enum class TypeConstraintKind {
    Equal
};

class TypeConstraint {
public:
  TypeConstraintKind kind;

  explicit TypeConstraint(TypeConstraintKind kind) : kind(kind) {}
  virtual ~TypeConstraint() = default;
};

class EqualTypeConstraint : public TypeConstraint {
public:
  std::shared_ptr<Type> lhs;
  std::shared_ptr<Type> rhs;

  EqualTypeConstraint(std::shared_ptr<Type> lhs, std::shared_ptr<Type> rhs)
      : TypeConstraint(TypeConstraintKind::Equal),
        lhs(std::move(lhs)),
        rhs(std::move(rhs)) {}
};

#endif //TYPE_CONSTRAINT_H
