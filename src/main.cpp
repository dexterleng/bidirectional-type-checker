#include <iostream>

#include "expr.h"
#include "ast_pretty_printer.h"
#include "type_inference.h"

int main() {
  auto ast = std::make_shared<BlockStmt>(
    std::vector<std::shared_ptr<Stmt>>{
      std::make_shared<FunctionStmt>(
        Var(0),
        std::vector{ Var(1, std::make_shared<IntegerType>()), Var(2, std::make_shared<IntegerType>()) },
        std::make_shared<IntegerType>(),
        std::make_shared<BlockStmt>(
          std::vector<std::shared_ptr<Stmt>>{
            std::make_shared<DeclareStmt>(
              Var(3),
              std::make_shared<AddNode>(
                std::make_shared<VariableNode>(Var(1)),
                std::make_shared<VariableNode>(Var(2))
              )
            ),
            std::make_shared<DeclareStmt>(Var(4), std::make_shared<IntegerNode>("3")),
            std::make_shared<IfStmt>(
              std::make_shared<IntegerNode>("3"), // this should be a bool
                std::make_shared<BlockStmt>(
                  std::vector<std::shared_ptr<Stmt>>{
                    std::make_shared<DeclareStmt>(Var(4), std::make_shared<IntegerNode>("3")), // shadowing
                    // std::make_shared<ReturnStmt>(std::make_shared<VariableNode>(Var(4)))
                  }
                ),
                std::make_shared<BlockStmt>(
                  std::vector<std::shared_ptr<Stmt>>{
                    std::make_shared<DeclareStmt>(Var(4), std::make_shared<IntegerNode>("3")),
                    // std::make_shared<ReturnStmt>(std::make_shared<VariableNode>(Var(4)))
                  }
                )
            ),
            std::make_shared<ReturnStmt>(std::make_shared<VariableNode>(Var(4)))
          }
        )
      )
    }
  );

  ASTPrettyPrinter printer(nullptr);
  printer.print(*ast);

  TypeInference inference;
  inference.perform(*ast);

  return 0;
}