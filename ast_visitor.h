#ifndef AST_VISITOR_H
#define AST_VISITOR_H
#include "ast.h"

template <typename V, typename ImplClass, typename RetTy = void, typename... Args>
class ASTVisitor {
public:
  RetTy visit(ASTNode<V>& node, Args... args) {
    switch (node.kind) {
      case ASTNodeKind::Integer:
        return static_cast<ImplClass*>(this)->visitInteger(static_cast<IntegerNode<V>&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Variable:
        return static_cast<ImplClass*>(this)->visitVariable(static_cast<VariableNode<V>&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Function:
        return static_cast<ImplClass*>(this)->visitFunction(static_cast<FunctionNode<V>&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Apply:
        return static_cast<ImplClass*>(this)->visitApply(static_cast<ApplyNode<V>&>(node), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown AST node kind");
    }
  }
};

class PrettyPrinterVisitor : public ASTVisitor<Var, PrettyPrinterVisitor> {
public:
  using ASTVisitor::visit; // Ensure we don't hide the base class `visit`

  void print(ASTNode<Var>& node) {
    ASTVisitor::visit(node);
    std::cout << std::endl;
  }

  void visitInteger(IntegerNode<Var>& node) {
    std::cout << node.getValue();
  }

  void visitVariable(VariableNode<Var>& node) {
    std::cout << "Variable"; // Modify as needed to print variable names
  }

  void visitFunction(FunctionNode<Var>& node) {
    std::cout << "(function " << node.arg << " ";
    visit(*node.body);
    std::cout << ")";
  }

  void visitApply(ApplyNode<Var>& node) {
    std::cout << "(";
    visit(*node.function);
    std::cout << " ";
    visit(*node.argument);
    std::cout << ")";
  }
};

#endif //AST_VISITOR_H
