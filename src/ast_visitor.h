#ifndef AST_VISITOR_H
#define AST_VISITOR_H
#include "ast.h"

template <typename ImplClass, typename RetTy = void, typename... Args>
class ASTVisitor {
public:
  RetTy visit(ASTNode& node, Args... args) {
    switch (node.kind) {
      case ASTNodeKind::Integer:
        return static_cast<ImplClass*>(this)->visitInteger(static_cast<IntegerNode&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Variable:
        return static_cast<ImplClass*>(this)->visitVariable(static_cast<VariableNode&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Function:
        return static_cast<ImplClass*>(this)->visitFunction(static_cast<FunctionNode&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Apply:
        return static_cast<ImplClass*>(this)->visitApply(static_cast<ApplyNode&>(node), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown AST node kind");
    }
  }
};

class PrettyPrinterVisitor : public ASTVisitor<PrettyPrinterVisitor> {
public:
  using ASTVisitor::visit; // Ensure we don't hide the base class `visit`

  void print(ASTNode& node) {
    ASTVisitor::visit(node);
    std::cout << std::endl;
  }

  void visitInteger(IntegerNode& node) {
    std::cout << node.getValue();
  }

  void visitVariable(VariableNode& node) {
    std::cout << "Variable"; // Modify as needed to print variable names
  }

  void visitFunction(FunctionNode& node) {
    std::cout << "(Function " << node.arg << " ";
    visit(*node.body);
    std::cout << ")";
  }

  void visitApply(ApplyNode& node) {
    std::cout << "(";
    visit(*node.function);
    std::cout << " ";
    visit(*node.argument);
    std::cout << ")";
  }
};

#endif //AST_VISITOR_H
