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
      case ASTNodeKind::Double:
        return static_cast<ImplClass*>(this)->visitDouble(static_cast<DoubleNode&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Variable:
        return static_cast<ImplClass*>(this)->visitVariable(static_cast<VariableNode&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Function:
        return static_cast<ImplClass*>(this)->visitFunction(static_cast<FunctionNode&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Apply:
        return static_cast<ImplClass*>(this)->visitApply(static_cast<ApplyNode&>(node), std::forward<Args>(args)...);
      case ASTNodeKind::Add:
        return static_cast<ImplClass*>(this)->visitAdd(static_cast<AddNode&>(node), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown AST node kind");
    }
  }
};

#endif //AST_VISITOR_H
