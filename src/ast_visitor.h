#ifndef AST_VISITOR_H
#define AST_VISITOR_H
#include "expr.h"
#include "stmt.h"

template <typename ImplClass,
          typename ExprRetTy = void,
          typename StmtRetTy = void,
          typename... Args>
class ASTVisitor {
public:
  ExprRetTy visit(Expr& node, Args... args) {
    switch (node.kind) {
      case ExprKind::Integer:
        return static_cast<ImplClass*>(this)->visitInteger(static_cast<IntegerNode&>(node), std::forward<Args>(args)...);
      case ExprKind::Double:
        return static_cast<ImplClass*>(this)->visitDouble(static_cast<DoubleNode&>(node), std::forward<Args>(args)...);
      case ExprKind::Variable:
        return static_cast<ImplClass*>(this)->visitVariable(static_cast<VariableNode&>(node), std::forward<Args>(args)...);
      case ExprKind::Apply:
        return static_cast<ImplClass*>(this)->visitApply(static_cast<ApplyNode&>(node), std::forward<Args>(args)...);
      case ExprKind::Add:
        return static_cast<ImplClass*>(this)->visitAdd(static_cast<AddNode&>(node), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown ExprKind");
    }
  }

  StmtRetTy visit(Stmt& node, Args... args) {
    switch (node.kind) {
      case StmtKind::Block:
        return static_cast<ImplClass*>(this)->visitBlock(static_cast<BlockStmt&>(node), std::forward<Args>(args)...);
      case StmtKind::Declare:
        return static_cast<ImplClass*>(this)->visitDeclare(static_cast<DeclareStmt&>(node), std::forward<Args>(args)...);
      case StmtKind::Function:
        return static_cast<ImplClass*>(this)->visitFunction(static_cast<FunctionStmt&>(node), std::forward<Args>(args)...);
      case StmtKind::Return:
        return static_cast<ImplClass*>(this)->visitReturn(static_cast<ReturnStmt&>(node), std::forward<Args>(args)...);
      case StmtKind::If:
        return static_cast<ImplClass*>(this)->visitIf(static_cast<IfStmt&>(node), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown StmtKind");
    }
  }
};

#endif //AST_VISITOR_H
