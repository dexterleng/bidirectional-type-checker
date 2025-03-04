#ifndef TYPE_CONSTRAINT_H
#define TYPE_CONSTRAINT_H
#include <iostream>
#include "type.h"

class TypeConstraint {
public:
  virtual ~TypeConstraint() = default;
  virtual void print() const = 0;
};

class TypeEqual : public TypeConstraint {
public:
  std::unique_ptr<Type> lhs;
  std::unique_ptr<Type> rhs;

  TypeEqual(std::unique_ptr<Type> lhs, std::unique_ptr<Type> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  void print() const override {
    std::cout << "TypeEqual Constraint:\n";
    // lhs->print();
    // rhs->print();
  }
};

#endif //TYPE_CONSTRAINT_H
