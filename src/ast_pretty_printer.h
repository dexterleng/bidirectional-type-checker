#ifndef AST_PRETTY_PRINTER_H
#define AST_PRETTY_PRINTER_H
#include "ast_visitor.h"
#include "union_find.h"
#include <iostream>
#include <string>
#include <vector>

class ASTPrettyPrinter : public ASTVisitor<ASTPrettyPrinter, void, void> {
public:
  using ASTVisitor::visit;     // Ensure we don't hide the base class expression `visit`

  UnionFind* unionFind;
  std::vector<bool> isLastChild; // Track whether each level is the last child

  explicit ASTPrettyPrinter(UnionFind* unionFind)
    : unionFind(unionFind) {
  }

  void print(Expr& expr) {
    isLastChild.clear();
    visit(expr);
    std::cout << std::endl;
  }

  void print(Stmt& stmt) {
    isLastChild.clear();
    visit(stmt);
    std::cout << std::endl;
  }

  // Expression visitors
  void visitIntegerExpr(IntegerExpr& expr) {
    printPrefix();
    std::cout << "Integer(" << expr.getValue() << ")" << std::endl;
  }

  void visitDoubleExpr(DoubleExpr& expr) {
    printPrefix();
    std::cout << "Double(" << expr.getValue() << ")" << std::endl;
  }

  void visitBoolExpr(BoolExpr& expr) {
    printPrefix();
    std::cout << "Bool(" << (expr.getValue() ? "true" : "false") << ")" << std::endl;
  }

  void visitVariableExpr(VariableExpr& expr) {
    printPrefix();
    std::cout << "Variable " << expr.var.name << std::endl;
  }

  void visitApplyExpr(ApplyExpr& expr) {
    printPrefix();
    std::cout << "Apply" << std::endl;

    isLastChild.push_back(false);
    visit(*expr.function);
    isLastChild.pop_back();

    isLastChild.push_back(true);
    visit(*expr.argument);
    isLastChild.pop_back();
  }

  void visitBinaryExpr(BinaryExpr& expr) {
    printPrefix();
    std::cout << "Binary (" << toString(expr.op) << ")" << std::endl;

    isLastChild.push_back(false);
    visit(*expr.left);
    isLastChild.pop_back();

    isLastChild.push_back(true);
    visit(*expr.right);
    isLastChild.pop_back();
  }

  // Statement visitors
  void visitBlockStmt(BlockStmt& stmt) {
    printPrefix();
    std::cout << "Block" << std::endl;

    for (size_t i = 0; i < stmt.statements.size(); i++) {
      bool isLast = (i == stmt.statements.size() - 1);
      isLastChild.push_back(isLast);
      visit(*stmt.statements[i]);
      isLastChild.pop_back();
    }
  }

  void visitDeclareStmt(DeclareStmt& stmt) {
    printPrefix();
    std::cout << "Declare " << stmt.var.name << std::endl;

    isLastChild.push_back(true);
    visit(*stmt.expression);
    isLastChild.pop_back();
  }

  void visitAssignStmt(AssignStmt& stmt) {
    printPrefix();
    std::cout << "Assign " << stmt.var.name << std::endl;

    isLastChild.push_back(true);
    visit(*stmt.expression);
    isLastChild.pop_back();
  }

  void visitFunctionStmt(FunctionStmt& stmt) {
    printPrefix();
    std::cout << "Function " << stmt.name.name << "(";

    for (size_t i = 0; i < stmt.params.size(); ++i) {
      std::cout << stmt.params[i].name;
      if (i < stmt.params.size() - 1) {
        std::cout << ", ";
      }
    }
    std::cout << ")" << std::endl;

    isLastChild.push_back(true);
    visit(*stmt.body);
    isLastChild.pop_back();
  }

  void visitReturnStmt(ReturnStmt& stmt) {
    printPrefix();
    std::cout << "Return" << std::endl;

    isLastChild.push_back(true);
    visit(*stmt.expression);
    isLastChild.pop_back();
  }

  void visitIfStmt(IfStmt& stmt) {
    printPrefix();
    std::cout << "If" << std::endl;

    // Print condition
    isLastChild.push_back(false); // Condition is not the last child
    printPrefix();
    std::cout << "Condition" << std::endl;
    isLastChild.push_back(true);  // Condition expression is the last in this branch
    visit(*stmt.condition);
    isLastChild.pop_back();
    isLastChild.pop_back();

    // Print then branch
    isLastChild.push_back(!stmt.elseBranch.has_value()); // Then branch is the last child only if no else branch
    printPrefix();
    std::cout << "Then" << std::endl;
    isLastChild.push_back(true);  // Then body is the last in this branch
    visit(*stmt.thenBranch);
    isLastChild.pop_back();
    isLastChild.pop_back();

    // Print else branch if it exists
    if (stmt.elseBranch.has_value()) {
      isLastChild.push_back(true); // Else branch is the last child
      printPrefix();
      std::cout << "Else" << std::endl;
      isLastChild.push_back(true); // Else body is the last in this branch
      visit(**stmt.elseBranch);
      isLastChild.pop_back();
      isLastChild.pop_back();
    }
  }

private:
  void printPrefix() {
    for (size_t i = 0; i < isLastChild.size(); i++) {
      if (i == isLastChild.size() - 1) {
        // Last level gets corner or tee
        std::cout << (isLastChild[i] ? " └─ " : " ├─ ");
      } else {
        // Upper levels get vertical bar or space
        std::cout << (isLastChild[i] ? "    " : " │  ");
      }
    }
  }
};

#endif //AST_PRETTY_PRINTER_H