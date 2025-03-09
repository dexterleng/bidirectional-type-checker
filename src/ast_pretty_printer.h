#ifndef AST_PRETTY_PRINTER_H
#define AST_PRETTY_PRINTER_H
#include "ast_visitor.h"
#include "union_find.h"

class ASTPrettyPrinter : public ASTVisitor<ASTPrettyPrinter, void> {
public:
  using ASTVisitor::visit; // Ensure we don't hide the base class `visit`

  UnionFind* unionFind;
  int indent = 0;

  explicit ASTPrettyPrinter(UnionFind* unionFind)
    : unionFind(unionFind) {
  }

  void print(ASTNode& node) {
    ASTVisitor::visit(node);
    std::cout << std::endl;
  }

  void visitInteger(IntegerNode& node) {
    printIndent();
    std::cout << "Integer(" << node.getValue() << ")" << std::endl;
  }

  void visitVariable(VariableNode& node) {
    printIndent();
    std::cout << "Variable(" << node.var.name << ")" << std::endl;
  }

  void visitFunction(FunctionNode& node) {
    printIndent();
    std::cout << "Function" << std::endl;

    indent++;

    printIndent();
    std::cout << "Parameters:" << std::endl;
    indent++;
    printIndent();
    std::cout << node.arg.name << std::endl;
    indent--;

    printIndent();
    std::cout << "Body" << std::endl;
    indent++;
    visit(*node.body);
    indent--;

    indent--;
  }

  void visitApply(ApplyNode& node) {
    printIndent();
    std::cout << "Apply" << std::endl;

    indent++;
    visit(*node.function);

    printIndent();
    std::cout << "Arguments:" << std::endl;
    indent++;
    visit(*node.argument);
    indent--;

    indent--;
  }

private:
  void printIndent() const {
    std::cout << std::string(indent * 2, ' '); // 2 spaces per level
  }
};

#endif //AST_PRETTY_PRINTER_H
