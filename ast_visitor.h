#ifndef AST_VISITOR_H
#define AST_VISITOR_H
#include "ast.h"

template <typename V, typename ImplClass, typename RetTy = void, typename... Args>
class ASTVisitor {
public:
  RetTy visit(ASTNode<V> *node, Args... args) {
    switch (node->kind) {
      case ASTNodeKind::Integer:
        return static_cast<ImplClass*>(this)->visitInteger(static_cast<Integer<V>*>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Variable:
        return static_cast<ImplClass*>(this)->visitVariable(static_cast<Variable<V>*>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Function:
        return static_cast<ImplClass*>(this)->visitFunction(static_cast<Function<V>*>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Apply:
        return static_cast<ImplClass*>(this)->visitApply(static_cast<Apply<V>*>(node), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown AST node kind");
    }
  }
};


class PrettyPrinterVisitor : public ASTVisitor<Var, PrettyPrinterVisitor> {
public:
  void visitInteger(Integer<Var> *node) {
    std::cout << node->getValue();
  }

  void visitVariable(Variable<Var> *node) {
    std::cout << "Variable"; // Modify as needed to print variable names
  }

  void visitFunction(Function<Var> *node) {
    std::cout << "(function " << node->name << " ";
    visit(node->body.get());
    std::cout << ")";
  }

  void visitApply(Apply<Var> *node) {
    std::cout << "(";
    visit(node->function.get());
    std::cout << " ";
    visit(node->argument.get());
    std::cout << ")";
  }
};


#endif //AST_VISITOR_H
