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
  void visitInteger(IntegerExpr& expr) {
    printPrefix();
    std::cout << "Integer(" << expr.getValue() << ")" << std::endl;
  }

  void visitDouble(DoubleExpr& expr) {
    printPrefix();
    std::cout << "Double(" << expr.getValue() << ")" << std::endl;
  }

  void visitVariable(VariableExpr& expr) {
    printPrefix();
    std::cout << "Variable " << expr.var.name << std::endl;
  }

  void visitApply(ApplyExpr& expr) {
    printPrefix();
    std::cout << "Apply" << std::endl;

    isLastChild.push_back(false); // Function is not the last child
    visit(*expr.function);
    isLastChild.pop_back();

    isLastChild.push_back(true); // Argument is the last child
    visit(*expr.argument);
    isLastChild.pop_back();
  }

  void visitAdd(AddExpr& expr) {
    printPrefix();
    std::cout << "Add" << std::endl;

    isLastChild.push_back(false); // Left is not the last child
    visit(*expr.left);
    isLastChild.pop_back();

    isLastChild.push_back(true); // Right is the last child
    visit(*expr.right);
    isLastChild.pop_back();
  }

  // Statement visitors
  void visitBlock(BlockStmt& stmt) {
    printPrefix();
    std::cout << "Block" << std::endl;

    for (size_t i = 0; i < stmt.statements.size(); i++) {
      bool isLast = (i == stmt.statements.size() - 1);
      isLastChild.push_back(isLast);
      visit(*stmt.statements[i]);
      isLastChild.pop_back();
    }
  }

  void visitDeclare(DeclareStmt& stmt) {
    printPrefix();
    std::cout << "Declare " << stmt.var.name << std::endl;

    isLastChild.push_back(true); // Expression is the only child
    visit(*stmt.expression);
    isLastChild.pop_back();
  }

  void visitFunction(FunctionStmt& stmt) {
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

  void visitReturn(ReturnStmt& stmt) {
    printPrefix();
    std::cout << "Return" << std::endl;

    isLastChild.push_back(true);
    visit(*stmt.expression);
    isLastChild.pop_back();
  }

  void visitIf(IfStmt& stmt) {
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