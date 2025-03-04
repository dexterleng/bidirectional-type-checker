#ifndef TYPE_H
#define TYPE_H
#include <iostream>

using TypeVar = uint32_t;

struct VariableType;
struct IntegerType;
struct FunctionType;
using Type = std::variant<VariableType, IntegerType, FunctionType>;

struct VariableType {
  TypeVar typeVar;
};

struct IntegerType {};

struct FunctionType {
  std::shared_ptr<Type> from;
  std::shared_ptr<Type> to;
};

//
// class Type {
// public:
//   virtual ~Type() = default;
//   virtual void print() const = 0;
// };
//
// class VariableType : public Type {
// public:
//   TypeVar typeVar;
//
//   explicit VariableType(TypeVar typeVar) : typeVar(typeVar) {}
//
//   void print() const override {
//     std::cout << "VariableType: " << typeVar << std::endl;
//   }
// };
//
// class IntegerType : public Type {
// public:
//   void print() const override {
//     std::cout << "IntegerType" << std::endl;
//   }
// };
//
// class FunctionType : public Type {
// public:
//   std::unique_ptr<Type> from;
//   std::unique_ptr<Type> to;
//
//   FunctionType(std::unique_ptr<Type> from, std::unique_ptr<Type> to)
//       : from(std::move(from)), to(std::move(to)) {}
//
//   void print() const override {
//     std::cout << "FunctionType: from -> to" << std::endl;
//     from->print();
//     to->print();
//   }
// };

#endif //TYPE_H
