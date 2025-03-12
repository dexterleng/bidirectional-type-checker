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
  ExprRetTy visit(Expr& expr, Args... args) {
    switch (expr.kind) {
      case ExprKind::Integer:
        return static_cast<ImplClass*>(this)->visitInteger(static_cast<IntegerExpr&>(expr), std::forward<Args>(args)...);
      case ExprKind::Double:
        return static_cast<ImplClass*>(this)->visitDouble(static_cast<DoubleExpr&>(expr), std::forward<Args>(args)...);
      case ExprKind::Variable:
        return static_cast<ImplClass*>(this)->visitVariable(static_cast<VariableExpr&>(expr), std::forward<Args>(args)...);
      case ExprKind::Apply:
        return static_cast<ImplClass*>(this)->visitApply(static_cast<ApplyExpr&>(expr), std::forward<Args>(args)...);
      case ExprKind::Add:
        return static_cast<ImplClass*>(this)->visitAdd(static_cast<AddExpr&>(expr), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown ExprKind");
    }
  }

  StmtRetTy visit(Stmt& stmt, Args... args) {
    switch (stmt.kind) {
      case StmtKind::Block:
        return static_cast<ImplClass*>(this)->visitBlock(static_cast<BlockStmt&>(stmt), std::forward<Args>(args)...);
      case StmtKind::Declare:
        return static_cast<ImplClass*>(this)->visitDeclare(static_cast<DeclareStmt&>(stmt), std::forward<Args>(args)...);
      case StmtKind::Function:
        return static_cast<ImplClass*>(this)->visitFunction(static_cast<FunctionStmt&>(stmt), std::forward<Args>(args)...);
      case StmtKind::Return:
        return static_cast<ImplClass*>(this)->visitReturn(static_cast<ReturnStmt&>(stmt), std::forward<Args>(args)...);
      case StmtKind::If:
        return static_cast<ImplClass*>(this)->visitIf(static_cast<IfStmt&>(stmt), std::forward<Args>(args)...);
      default:
        throw std::runtime_error("Unknown StmtKind");
    }
  }
};

#endif //AST_VISITOR_H
