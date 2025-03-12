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

  void print(Expr& node) {
    isLastChild.clear();
    visit(node);
    std::cout << std::endl;
  }

  void print(Stmt& node) {
    isLastChild.clear();
    visit(node);
    std::cout << std::endl;
  }

  // Expression visitors
  void visitInteger(IntegerNode& node) {
    printPrefix();
    std::cout << "Integer(" << node.getValue() << ")" << std::endl;
  }

  void visitDouble(DoubleNode& node) {
    printPrefix();
    std::cout << "Double(" << node.getValue() << ")" << std::endl;
  }

  void visitVariable(VariableNode& node) {
    printPrefix();
    std::cout << "Variable " << node.var.name << std::endl;
  }

  void visitApply(ApplyNode& node) {
    printPrefix();
    std::cout << "Apply" << std::endl;

    isLastChild.push_back(false); // Function is not the last child
    visit(*node.function);
    isLastChild.pop_back();

    isLastChild.push_back(true); // Argument is the last child
    visit(*node.argument);
    isLastChild.pop_back();
  }

  void visitAdd(AddNode& node) {
    printPrefix();
    std::cout << "Add" << std::endl;

    isLastChild.push_back(false); // Left is not the last child
    visit(*node.left);
    isLastChild.pop_back();

    isLastChild.push_back(true); // Right is the last child
    visit(*node.right);
    isLastChild.pop_back();
  }

  // Statement visitors
  void visitBlock(BlockStmt& node) {
    printPrefix();
    std::cout << "Block" << std::endl;

    for (size_t i = 0; i < node.statements.size(); i++) {
      bool isLast = (i == node.statements.size() - 1);
      isLastChild.push_back(isLast);
      visit(*node.statements[i]);
      isLastChild.pop_back();
    }
  }

  void visitDeclare(DeclareStmt& node) {
    printPrefix();
    std::cout << "Declare " << node.var.name << std::endl;

    isLastChild.push_back(true); // Expression is the only child
    visit(*node.expression);
    isLastChild.pop_back();
  }

  void visitFunction(FunctionStmt& node) {
    printPrefix();
    std::cout << "Function " << node.name.name << "(";

    for (size_t i = 0; i < node.params.size(); ++i) {
      std::cout << node.params[i].name;
      if (i < node.params.size() - 1) {
        std::cout << ", ";
      }
    }
    std::cout << ")" << std::endl;

    isLastChild.push_back(true);
    visit(*node.body);
    isLastChild.pop_back();
  }

  void visitReturn(ReturnStmt& node) {
    printPrefix();
    std::cout << "Return" << std::endl;

    isLastChild.push_back(true);
    visit(*node.expression);
    isLastChild.pop_back();
  }

  void visitIf(IfStmt& node) {
    printPrefix();
    std::cout << "If" << std::endl;

    // Print condition
    isLastChild.push_back(false); // Condition is not the last child
    printPrefix();
    std::cout << "Condition" << std::endl;
    isLastChild.push_back(true);  // Condition expression is the last in this branch
    visit(*node.condition);
    isLastChild.pop_back();
    isLastChild.pop_back();

    // Print then branch
    isLastChild.push_back(!node.elseBranch.has_value()); // Then branch is the last child only if no else branch
    printPrefix();
    std::cout << "Then" << std::endl;
    isLastChild.push_back(true);  // Then body is the last in this branch
    visit(*node.thenBranch);
    isLastChild.pop_back();
    isLastChild.pop_back();

    // Print else branch if it exists
    if (node.elseBranch.has_value()) {
      isLastChild.push_back(true); // Else branch is the last child
      printPrefix();
      std::cout << "Else" << std::endl;
      isLastChild.push_back(true); // Else body is the last in this branch
      visit(**node.elseBranch);
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