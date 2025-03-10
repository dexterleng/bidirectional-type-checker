#ifndef AST_PRETTY_PRINTER_H
#define AST_PRETTY_PRINTER_H
#include "ast_visitor.h"
#include "union_find.h"

class ASTPrettyPrinter : public ASTVisitor<ASTPrettyPrinter, void, void> {
public:
  using ASTVisitor::visit; // Ensure we don't hide the base class `visit`

  UnionFind* unionFind;
  int indent = 0;

  explicit ASTPrettyPrinter(UnionFind* unionFind)
    : unionFind(unionFind) {
  }

  void print(Expr& node) {
    ASTVisitor::visit(node);
    std::cout << std::endl;
  }

  void printStatement(Stmt& node) {
    visitStmt(node);
    std::cout << std::endl;
  }

  void visitInteger(IntegerNode& node) {
    printIndent();
    std::cout << "Integer(" << node.getValue() << ")" << std::endl;
  }

  void visitDouble(DoubleNode& node) {
    printIndent();
    std::cout << "Double(" << node.getValue() << ")" << std::endl;
  }

  void visitVariable(VariableNode& node) {
    printIndent();
    std::cout << "Variable(" << node.var.name << ")" << std::endl;
  }

  void visitFunction(FunctionNode& node) {
    printIndent();
    std::cout << "Function " << node.arg.name << std::endl;

    indent++;
    visit(*node.body);
    indent--;
  }

  void visitApply(ApplyNode& node) {
    printIndent();
    std::cout << "Apply" << std::endl;

    indent++;
    visit(*node.function);
    visit(*node.argument);
    indent--;
  }

  void visitAdd(AddNode& node) {
    printIndent();
    std::cout << "Add" << std::endl;

    indent++;
    visit(*node.left);
    visit(*node.right);
    indent--;
  }

  // Statement visitors
  void visitBlock(BlockStmt& node) {
    printIndent();
    std::cout << "Block" << std::endl;

    indent++;
    for (const auto& stmt : node.statements) {
      visitStmt(*stmt);
    }
    indent--;
  }

  void visitAssign(AssignStmt& node) {
    printIndent();
    std::cout << "Assign " << node.var.name << std::endl;

    indent++;
    visit(*node.expression);
    indent--;
  }

private:
  void printIndent() const {
    std::cout << std::string(indent * 2, ' '); // 2 spaces per level
  }
};

#endif //AST_PRETTY_PRINTER_H
