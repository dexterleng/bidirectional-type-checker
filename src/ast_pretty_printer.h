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
  using ASTVisitor::visitStmt; // Ensure we don't hide the base class statement `visitStmt`

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

  void printStatement(Stmt& node) {
    isLastChild.clear();
    visitStmt(node);
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

  void visitFunction(FunctionNode& node) {
    printPrefix();
    std::cout << "Function " << node.arg.name << std::endl;

    isLastChild.push_back(true); // Body is the only child (last)
    visit(*node.body);
    isLastChild.pop_back();
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
      visitStmt(*node.statements[i]);
      isLastChild.pop_back();
    }
  }

  void visitAssign(AssignStmt& node) {
    printPrefix();
    std::cout << "Assign " << node.var.name << std::endl;

    isLastChild.push_back(true); // Expression is the only child
    visit(*node.expression);
    isLastChild.pop_back();
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